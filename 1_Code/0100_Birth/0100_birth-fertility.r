# header ------------------------------------------------------------------
# birth: fertility rate

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# import and data preparation ---------------------------------------------

# birth
# age: only births of women at 'fertile age'
# origin: here origin of mother

bir <- read_csv(bir_od) %>%
  rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) %>%
  filter((age >= bir_age_begin) & (age <= bir_age_end)) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    origin = factor(if_else(HerkunftMutterCd == 1, uni_o[1], uni_o[2]), uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, origin, bir) %>%
  group_by(district, year, age, origin) %>%
  summarize(
    bir = sum(bir),
    .groups = "drop"
  )

# population
# year: begin of year population (therefore: StichtagDatJahr + 1)
# age: only women at 'fertile age'

pop <- read_csv(pop_od) %>%
  rename(age = AlterVCd, pop = AnzBestWir) %>%
  filter((SexCd == 2) & (age >= bir_age_begin) & (age <= bir_age_end)) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    year = StichtagDatJahr + 1,
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, origin, pop) %>%
  group_by(district, year, age, origin) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )

# fertility ---------------------------------------------------------------

# values for all possible cases

# WHY with age categories? to calculate TFR by age category

cas <- as_tibble(expand_grid(
  district = uni_d,
  year = (date_start + 1):date_end,
  age = bir_age_begin:bir_age_end,
  origin = uni_o
)) %>%
  left_join(pop, by = c("district", "year", "age", "origin")) %>%
  left_join(bir, by = c("district", "year", "age", "origin")) %>%
  replace_na(list(pop = 0, bir = 0)) %>%
  left_join(look_a1, by = "age") %>%
  left_join(look_a2, by = "age")

# fertility by year, age
fer_ya <- group_by(cas, year, age) %>%
  summarize(
    pop = sum(pop),
    bir = sum(bir),
    .groups = "drop"
  ) %>%
  mutate(fer_ya = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>%
  select(year, age, fer_ya)

# fertility by year, age, origin
fer_yao <- group_by(cas, year, age, origin) %>%
  summarize(
    pop = sum(pop),
    bir = sum(bir),
    .groups = "drop"
  ) %>%
  mutate(fer_yao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>%
  select(year, age, origin, fer_yao)

# fertility by district, year, age, origin
# is already aggregated by district, year, age, origin
fer_dyao <- mutate(cas, fer_dyao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate)))

# TFR (total fertility rate) ----------------------------------------------

# WHY? plots to define the base period for fertility prediction
# plots 0100, 0101, 0102, 0103, 0104, 0105

# fertility plots (before any corrections) --------------------------------
# plots 0110, 0111, 0112

# fertility: replace values in tails, base period only --------------------

# cumulative sums vs. thresholds
# lowcum: cumulative sum from the lower tail of the age distribution
# upcum: cumulative sum from the upper tail of the age distribution

fer_tail <- filter(fer_dyao, year >= bir_base_begin) %>%
  left_join(fer_yao, by = c("year", "age", "origin")) %>%
  left_join(fer_ya, by = c("year", "age")) %>%
  arrange(district, year, origin, age) %>%
  group_by(district, year, origin) %>%
  mutate(
    low_cum = cumsum(pop),
    up_cum = rev(cumsum(rev(pop)))
  ) %>%
  ungroup() %>%
  mutate(
    min_cum = pmin(low_cum, up_cum),
    fer = if_else(min_cum < bir_thres_const, bir_thres_value,
      if_else(min_cum < bir_thres_overall, fer_ya,
        if_else(min_cum < bir_thres_origin, fer_yao, fer_dyao)
      )
    )
  )

# corrected fertility:
# plot 0120: initial vs. corrected fertility
# plot 0121: cumulative population from the tails

# smooth the corrected fertility rate -------------------------------------

# smoothing with loess
fer_fit <- arrange(fer_tail, district, year, origin, age) %>%
  group_by(district, year, origin) %>%
  mutate(fer_fit = pmax(0, predict(
    loess(fer ~ age, span = bir_fer_span, degree = 1, na.action = na.aggregate)
  ))) %>%
  ungroup()

# plot 0130

# prediction --------------------------------------------------------------

# constrained regression
# (proportion of linear model and mean, within bandwidth)

fer_pred <- con_reg(
  data = fer_fit, x = "year", y = "fer_fit",
  group_cols = c("district", "age", "origin"),
  window = bir_window_thres, base_t0 = bir_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = bir_prop_trend, thres_percent = bir_thres_percent,
  lower_thres = bir_lower_thres, upper_thres = bir_upper_thres
) %>%
  # with the input data (WHY? to assess the regression)
  left_join(select(fer_fit, district, year, age, origin, fer_fit),
    by = c("district", "year", "age", "origin")
  ) %>%
  # output generation, one rate variable for both future and past
  left_join(select(fer_dyao, district, year, age, origin, fer_dyao),
    by = c("district", "year", "age", "origin")
  ) %>%
  mutate(fer_all = if_else(year <= bir_base_end, fer_dyao, pred_roll))

# plot 0140: the predictions: age distribution by district and year

# plots 0141, 0142 the predictions: along year, for selected age

# smooth the future fertility rates ---------------------------------------

# smoothed (only the prediction years)
pred_fit <- filter(fer_pred, year >= scen_begin) %>%
  arrange(district, year, origin, age) %>%
  group_by(district, year, origin) %>%
  mutate(pred_fit = pmax(0, predict(
    loess(pred_roll ~ age, span = bir_fer_span_pred, degree = 1, na.action = na.aggregate)
  ))) %>%
  ungroup()

# # plot 0150: prediction for selected years

# export fertility rates --------------------------------------------------

# prepare the export data
fer_ex <- mutate(pred_fit, fer = round(pred_fit, round_rate)) %>%
  filter(year >= scen_begin) %>%
  select(district, year, age, origin, fer) %>%
  arrange(district, year, age, origin)

# export
write_csv(fer_ex, paste0(exp_path, "/birth_fertility_future.csv"))

# TFR (total fertility rate) ----------------------------------------------

# fertility rate of past and future
fer_ex_past <- rename(fer_dyao, fer = fer_dyao) %>%
  select(district, year, age, origin, fer) %>%
  arrange(district, year, age, origin)

# export the data of the past (for model evaluation later on)
write_csv(fer_ex_past, paste0(exp_path, "/birth_fertility_past.csv"))

# plot 0160

# plot 0161: TFR by age class

# plot 0162: TFR by age class (more detailled) 

# log info
cat_log(paste0(
  "fertility rate: ",
  capture.output(Sys.time() - t0)
))
