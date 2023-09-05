# header ------------------------------------------------------------------
# death: mortality rate

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# import and data preparation ---------------------------------------------

# death
dea <- read_csv(dea_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, dea = AnzSterWir) %>%
  mutate(sex = fact_if(SexCd, uni_s)) %>%
  group_by(year, age, sex) %>%
  summarize(
    dea = sum(dea),
    .groups = "drop"
  )

# birth (needed to calculate life expectancy)
bir <- read_csv(bir_od) %>%
  rename(year = EreignisDatJahr, bir = AnzGebuWir) %>%
  mutate(
    age = 0,
    sex = fact_if(SexCd, uni_s)
  ) %>%
  group_by(year, age, sex) %>%
  summarize(
    B = sum(bir),
    .groups = "drop"
  )

# population
# year: begin of year population
pop <- read_csv(pop_od) %>%
  rename(age = AlterVCd, pop = AnzBestWir) %>%
  mutate(
    year = StichtagDatJahr + 1,
    sex = fact_if(SexCd, uni_s)
  ) %>%
  group_by(year, age, sex) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )

# FSO data (used in the prediction)
# rate is converted to percent
mor_fso <- read_csv(dea_fso_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd) %>%
  filter(HerkunftCd == 0) %>%
  mutate(
    sex = fact_if(SexCd, uni_s),
    region = factor(text_r[2], uni_r),
    mor_yas = RateSterSta * 100
  ) %>%
  select(year, age, sex, region, KategorieCd, mor_yas)

# mortality ---------------------------------------------------------------

# Zurich (calculation based on all possible cases)
mor_zh_yas <- as_tibble(expand_grid(
  year = (date_start + 1):date_end,
  age = age_min:age_max,
  sex = uni_s,
  region = factor(text_r[1], uni_r)
)) %>%
  left_join(pop, by = c("year", "age", "sex")) %>%
  left_join(dea, by = c("year", "age", "sex")) %>%
  replace_na(list(pop = 0, dea = 0)) %>%
  mutate(mor_yas = if_else(pop == 0, NA_real_, round(dea / pop * 100, round_rate)))

# FSO, past (smoothed rates)
# category: (smoothed) data of the past
mor_fso_yas_past <- filter(mor_fso, KategorieCd == dea_fso_cat_past) %>%
  select(year, age, sex, region, mor_yas)

# FSO, future
# category: data of the future
# WHY filter on year? there are also predictions for years in the past
mor_fso_yas_future <- mor_fso %>%
  filter((KategorieCd == dea_fso_cat_future) & (year >= scen_begin) & (year <= scen_end)) %>%
  select(year, age, sex, region, mor_yas)

# FSO, past and future
mor_fso_yas <- bind_rows(mor_fso_yas_past, mor_fso_yas_future)

# with region (Zurich and FSO)
mor_yasr_zh_fso <- select(mor_zh_yas, year, age, sex, region, mor_yas) %>%
  bind_rows(mor_fso_yas) %>%
  rename(mor_yasr = mor_yas)

# with all possible cases
mor_yasr <- as_tibble(expand_grid(
  year = (date_start + 1):scen_end,
  age = age_min:age_max,
  sex = uni_s,
  region = uni_r
)) %>%
  left_join(mor_yasr_zh_fso, by = c("year", "age", "sex", "region"))

# # mortality: age plots 0201, 0202

# # mortality: year plots 0202, 0203

# life expectancy: based on deaths, births, population --------------------
# no function, since only used once

# age capped
pop_capped <- pop %>%
  mutate(age_capped = if_else(age >= dea_age_max_le, dea_age_max_le, age)) %>%
  group_by(year, age_capped, sex) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  ) %>%
  rename(age = age_capped)

dea_capped <- dea %>%
  mutate(age_capped = if_else(age >= dea_age_max_le, dea_age_max_le, age)) %>%
  group_by(year, age_capped, sex) %>%
  summarize(
    dx = sum(dea),
    .groups = "drop"
  ) %>%
  rename(age = age_capped)

# population at the end of the year
pop_end_year <- mutate(pop_capped, end_of_year = year - 1) %>%
  select(end_of_year, age, sex, pop) %>%
  rename(year = end_of_year, pop_end = pop)

# mean population per year
pop_mean <- as_tibble(expand_grid(
  year = (date_start + 1):date_end,
  age = age_min:dea_age_max_le,
  sex = uni_s
)) %>%
  left_join(pop_capped, by = c("year", "age", "sex")) %>%
  left_join(pop_end_year, by = c("year", "age", "sex")) %>%
  replace_na(list(pop = 0, pop_end = 0)) %>%
  mutate(Px = (pop + pop_end) / 2) %>%
  left_join(dea_capped, by = c("year", "age", "sex")) %>%
  left_join(bir, by = c("year", "age", "sex")) %>%
  replace_na(list(dx = 0, B = 0))

# preparation (for life expectancy calculation)

le_mult <- mutate(pop_mean,
  # mx: age-specific mortality rate, Yusuf et al. (2014), eq 7.1
  mx = if_else(Px > 0, dx / Px, NA_real_),
  # qx1: probability to die between age x and age x+1, Yusuf et al. (2014), eq 7.4, 7.5, 7.6
  # the last two age-values: qx should be 1
  # why? otherwise after the lag, some people 'survive' the last age
  qx1 = pmin(1, if_else(age == 0, if_else(B > 0, dx / B, NA_real_),
    if_else((age > 0) & (age < (dea_age_max_le - 1)), 2 * mx / (2 + mx), 1)
  )),
  # if there is no one at a certain age in the population (e.g. no 96 year old men),
  # then qx1 is NA. However, a value is needed to multiply the subsequent survival probabilities
  qx = if_else(is.na(qx1), dea_qx_NA_le, qx1),
  # survival
  px_ = 1 - qx
) %>%
  # cumulative product
  arrange(year, sex, age) %>%
  group_by(year, sex) %>%
  mutate(
    mult = cumprod(px_),
    multlag = lag(mult, default = 1)
  ) %>%
  ungroup() %>%
  mutate(
    lx = dea_radix * multlag,
    # expected deaths dx_, Yusuf et al. (2014), eq 7.8
    dx_ = lx * qx,
    # number of survivors lxp1, Yusuf et al. (2014), eq 7.9
    lxp1 = lx - dx_,
    # person-years lived, Yusuf et al. (2014), eq 7.12, 7.14, 7.15
    Lx_ = if_else(age == 0, 0.3 * lx + 0.7 * lxp1,
      if_else((age > 0) & (age < dea_age_max_le), 0.5 * lx + 0.5 * lxp1, dx / mx)
    )
  )

# check subgroups
le_mult_sub <- filter(le_mult, (year == 2018) & (sex == "female"))
plot(le_mult_sub$age, le_mult_sub$px_, type = "o")
plot(le_mult_sub$age, le_mult_sub$mult)

# life expectancy at certain age (e.g. birth)
le <- le_mult %>% 
  group_by(year, sex) %>%
  summarize(
    life_years = sum(Lx_[age >= dea_age_at], na.rm = TRUE),
    start_pop = min(lx[age == dea_age_at], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(life = dea_age_at + life_years / start_pop) %>%
  select(year, sex, life)

# life expectancy: based on mortality rate --------------------------------

# life expectancy
le_ysr <- life_exp(
  data = mor_yasr, mor = "mor_yasr",
  age = "age", group_cols = c("year", "sex", "region"),
  age_max = dea_age_max_le, qx_NA = dea_qx_NA_le,
  age_at = dea_age_at, radix = dea_radix
) %>%
  #' manual' correction
  # Zurich: missing values in the future
  # Switzerland: latest values not available yet
  mutate(le_ysr = if_else((region == "Zurich") & (year > date_end), NA_real_,
    if_else((region == "Switzerland") & !(year %in%
      c(dea_fso_date_start:dea_fso_date_end, scen_begin:scen_end)), NA_real_, life_exp)
  )) %>%
  select(-life_exp)


# method comparison (based on rate vs. death/birth/population)
# plot 0204: method differences (rate vs. deaths/births/population)
 
# plot0205, 0206: life expectancy

# mortality over base years (including tail correction) -------------------

# Zurich (tibble with all possible cases as input)
mor_zh_asr <- filter(mor_zh_yas, year %in% dea_base_begin:dea_base_end) %>%
  mutate(age_tail = if_else(age <= dea_lower, dea_lower,
    if_else(age >= dea_upper, dea_upper, age)
  )) %>%
  group_by(age_tail, sex, region) %>%
  summarize(
    dea = sum(dea),
    pop = sum(pop),
    .groups = "drop"
  ) %>%
  mutate(mor_asr = if_else(pop == 0, NA_real_, round(dea / pop * 100, round_rate)))

# Switzerland
# why slightly different code (compared to Zurich)?
# for Switzerland only mortality rates are available
# median over rates as an approximation

mor_ch_asr <- filter(mor_fso_yas_past, year %in% dea_base_begin:dea_base_end) %>%
  mutate(age_tail = if_else(age <= dea_lower, dea_lower,
    if_else(age >= dea_upper, dea_upper, age)
  )) %>%
  # over tails
  group_by(year, age_tail, sex, region) %>%
  summarize(
    mor_asr = median(mor_yas),
    .groups = "drop"
  ) %>%
  # over years (why not in tail-correction? if the median is changed to another function)
  group_by(age_tail, sex, region) %>%
  summarize(
    mor_asr = median(mor_asr),
    .groups = "drop"
  )

# both (Zurich and Switzerland), all cases
mor_asr_temp <- select(mor_zh_asr, age_tail, sex, region, mor_asr) %>%
  bind_rows(mor_ch_asr)

mor_asr <- as_tibble(expand_grid(
  age = as.double(age_min:age_max),
  sex = uni_s,
  region = uni_r
)) %>%
  mutate(age_tail = if_else(age <= dea_lower, dea_lower,
    if_else(age >= dea_upper, dea_upper, age)
  )) %>%
  left_join(mor_asr_temp, by = c("age_tail", "sex", "region"))

# plot 0207

# smoothing with LOESS ----------------------------------------------------
# fit
# WHY log? because the mortality rates vary substantially
mor_fit <- select(mor_asr, age, sex, region, mor_asr) %>%
  arrange(sex, region, age) %>%
  group_by(sex, region) %>%
  mutate(mor_fit = pmax(0, exp(predict(
    loess(log(mor_asr) ~ age, span = dea_mor_span, degree = 1, na.action = na.aggregate))))) %>%
  ungroup()
 
# # plot 0208

# ratio Zurich / Switzerland ----------------------------------------------

# ratio (Zurich / Switzerland)
ratio_as <- select(mor_fit, age, sex, region, mor_fit) %>%
  pivot_wider(names_from = "region", values_from = "mor_fit") %>%
  mutate(ratio = Zurich / Switzerland)

# plot 0209

# Zurich: future mortality rate -------------------------------------------

# ZH: future (based on ratio ZH / Switzerland)
# mortality in percent per year, not more than 100 %

mor_zh_yas_future <- select(ratio_as, age, sex, ratio) %>%
  right_join(mor_fso_yas_future, by = c("age", "sex")) %>%
  mutate(mor = pmin(100, mor_yas * ratio)) %>%
  select(year, age, sex, mor) %>%
  rename(mor_yas = mor)

# ZH: past and future
mor_zh_yas_past_future <- select(mor_zh_yas, year, age, sex, mor_yas) %>%
  bind_rows(mor_zh_yas_future)

# plot 0210

# export mortality rates --------------------------------------------------

# prepare the export data
dea_ex <- mutate(mor_zh_yas_past_future, mor = round(mor_yas, round_rate)) %>%
  filter(year >= scen_begin) %>%
  select(year, age, sex, mor) %>%
  arrange(year, age, sex)

# export
write_csv(dea_ex, paste0(exp_path, "/mortality_future.csv"))

# Zurich: life expectancy (including the model data) ----------------------
# why? visual check, if life expectancy based on predicted rates are ok
# plots 0211, 0212

# log info
cat_log(paste0(
  "mortality rate: ",
  capture.output(Sys.time() - t0)
))
