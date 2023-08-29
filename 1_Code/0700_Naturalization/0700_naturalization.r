# header ------------------------------------------------------------------
# naturalization

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# import and data preparation ---------------------------------------------

# naturalization and denaturalization
nat_denat <- read_csv(nat_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, nat = AnzEinbWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    origin_prev = factor(if_else(HerkunftBisherCd == 1, uni_o[1], uni_o[2]), uni_o),
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, origin_prev, origin, nat)

# naturalization
nat <- filter(nat_denat, (origin_prev == uni_o[2]) & (origin == uni_o[1])) %>%
  group_by(district, year, age, sex) %>%
  summarize(
    nat = sum(nat),
    .groups = "drop"
  )

# population (foreign population only)
# year: begin of year population

pop <- read_csv(pop_od) %>%
  rename(age = AlterVCd, pop = AnzBestWir) %>%
  filter(HerkunftCd == 2) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    year = StichtagDatJahr + 1,
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, pop) %>%
  group_by(district, year, age, sex) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )

# plot 0700: naturalization vs. denaturalization 

# naturalization and population -------------------------------------------

# nat and pop
nat_pop <- as_tibble(expand_grid(
  district = uni_d,
  year = (date_start + 1):date_end,
  age = age_min:age_max,
  sex = uni_s
)) %>%
  left_join(pop, by = c("district", "year", "age", "sex")) %>%
  left_join(nat, by = c("district", "year", "age", "sex")) %>%
  replace_na(list(pop = 0, nat = 0))

# plots: 0701, 0702, 0703, 0704, 0705, 0706

# base years --------------------------------------------------------------

# base years, aggregate (mean over base years)
nat_pop_das <- filter(
  nat_pop,
  (year >= nat_base_begin) & (year <= nat_base_end)
) %>%
  group_by(district, age, sex) %>%
  summarize(
    nat = mean(nat),
    pop = mean(pop),
    .groups = "drop"
  )

# das: smoothing (nat and pop) --------------------------------------------

# smoothing (direction: age, result should not be below zero)
nat_pop_das_smooth <- group_by(nat_pop_das, district, sex) %>%
  arrange(age) %>%
  mutate(
    pop_a = pmax(0, predict(loess(pop ~ age, span = 0.1, degree = 1, na.action = na.aggregate))),
    nat_a = pmax(0, predict(loess(nat ~ age, span = nat_nat_span_das, degree = 1, na.action = na.aggregate)))
  ) %>%
  ungroup()

# plot 0707

# das: naturalization rate ------------------------------------------------

# rate, threshold, smooth
nat_rate_das <- mutate(nat_pop_das_smooth,
  rate_das = if_else(pop_a == 0, NA_real_, round(nat_a / pop_a * 100, round_rate)),
  rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_das)
) %>%
  group_by(district, sex) %>%
  arrange(age) %>%
  mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
    span = nat_rate_span_das, degree = 1, na.action = na.aggregate
  )))) %>%
  ungroup()

# plot 0708

# ya: smooth processes (preparation for trend by ya) ----------------------

# WHY trend for ya?
# shown in plots by a -> age is relevant
# since naturalization is conditional to duration of residence
# moreover different regulations for families, children
# therefore, age is an essential variable
# district and sex not that different (look at plots)
# same regulations in entire city, and not differentiate by sex

# smooth processes (base years only)
nat_ya <-
  filter(nat_pop, (year >= nat_base_begin) &
    (year <= nat_base_end)) %>%
  group_by(year, age) %>%
  summarize(
    pop = sum(pop),
    nat = sum(nat),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  arrange(age) %>%
  mutate(
    pop_a = pmax(0, predict(loess(pop ~ age,
      span = nat_pop_span_ya,
      degree = 1,
      na.action = na.aggregate
    ))),
    nat_a = pmax(0, predict(loess(nat ~ age,
      span = nat_nat_span_ya,
      degree = 1,
      na.action = na.aggregate
    )))
  ) %>%
  ungroup()

# plot 0709, 0710

# ya: rate ----------------------------------------------------------------

# rate
nat_rate_ya <- mutate(nat_ya,
  rate_ya = if_else(pop_a == 0, NA_real_, round(nat_a / pop_a * 100, round_rate)),
  rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_ya)
) %>%
  group_by(year) %>%
  arrange(age) %>%
  mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
    span = nat_rate_span_ya, degree = 1, na.action = na.aggregate
  )))) %>%
  ungroup()

# plots 0711, 0712

# constrained regression --------------------------------------------------

# prediction
rate_ya_pred <- con_reg(
  data = nat_rate_ya, x = "year", y = "rate_smooth",
  group_cols = "age",
  window = nat_window_thres, base_t0 = nat_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = nat_prop_trend, thres_percent = nat_thres_percent,
  lower_thres = nat_lower_thres
)

# past and prediction
rate_ya_past_pred <- as_tibble(expand_grid(
  year = nat_base_begin:scen_end,
  age = age_min:age_max
)) %>%
  left_join(select(nat_rate_ya, year, age, rate_smooth),
    by = c("year", "age")
  ) %>%
  left_join(select(rate_ya_pred, year, age, pred_roll),
    by = c("year", "age")
  ) %>%
  mutate(rate_ya = if_else(year < scen_begin, rate_smooth, pred_roll))

# plots 0713, 0714

# smooth processes (preparation for the trend factor calculation) ---------

# smooth processes (base years only)
nat_a <- filter(nat_pop, (year >= nat_base_begin) & (year <= nat_base_end)) %>%
  group_by(age) %>%
  summarize(
    pop = mean(pop),
    nat = mean(nat),
    .groups = "drop"
  ) %>%
  arrange(age) %>%
  mutate(
    pop_a = pmax(0, predict(loess(pop ~ age,
      span = nat_pop_span_a,
      degree = 1,
      na.action = na.aggregate
    ))),
    nat_a = pmax(0, predict(loess(nat ~ age,
      span = nat_pop_span_a,
      degree = 1,
      na.action = na.aggregate
    )))
  )

# plot 0715

# rate --------------------------------------------------------------------

# rate
nat_rate_a <- mutate(nat_a,
  rate_a = if_else(pop_a == 0, NA_real_, round(nat_a / pop_a * 100, round_rate)),
  rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_a)
) %>%
  arrange(age) %>%
  mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
    span = nat_rate_span_a, degree = 1, na.action = na.aggregate
  ))))

# plot 0716

# trend factor ('rate ya' to 'rate a') ------------------------------------

# preparation
prep_rate_ya <- filter(rate_ya_past_pred, year >= scen_begin) %>%
  select(year, age, rate_ya)

prep_rate_a <- select(nat_rate_a, age, rate_smooth) %>%
  rename(rate_a = rate_smooth)


# trend factor (has to be at least 0, otherwise 'denaturalization')
tf_ya <- left_join(prep_rate_ya, prep_rate_a, by = "age") %>%
  mutate(tf_ya = pmax(0, if_else(rate_a <= nat_factor_thres, nat_factor_value, rate_ya / rate_a)))

# plot 0718

# future rate (dyas) ------------------------------------------------------

# future rate
rate_dyas_future <- as_tibble(expand_grid(
  district = uni_d,
  year = scen_begin:scen_end,
  age = age_min:age_max,
  sex = uni_s
)) %>%
  left_join(select(nat_rate_das, district, age, sex, rate_smooth),
    by = c("district", "age", "sex")
  ) %>%
  rename(rate_das = rate_smooth) %>%
  left_join(select(tf_ya, year, age, tf_ya), by = c("year", "age")) %>%
  mutate(rate_dyas = round(rate_das * tf_ya, round_rate)) %>%
  group_by(district, year, sex) %>%
  arrange(age) %>%
  mutate(rate_smoothed = pmax(0, predict(loess(rate_dyas ~ age,
    span = nat_rate_span_dyas, degree = 1, na.action = na.aggregate
  )))) %>%
  ungroup()

# plots 0719, 0720, 0721, 0722

# export the results ------------------------------------------------------

# rate: prediction
nat_ex_data <- mutate(rate_dyas_future, rate_nat_dyas = round(rate_smoothed, round_rate)) %>%
  select(district, year, age, sex, rate_nat_dyas) %>%
  arrange(district, year, age, sex)

# export
write_csv(nat_ex_data, paste0(exp_path, "/naturalization_future.csv"))

# log info
cat_log(paste0(
  "naturalization rate: ",
  capture.output(Sys.time() - t0)
))
