# header ------------------------------------------------------------------
# allocation

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# import, data preparation ------------------------------------------------

# allocation: data
# WHY living space file? same data as for living space
aca_dat <- read_csv(spa_od) %>%
  rename(year = StichtagDatJahr, apartments = AnzWhgStat, people = AnzBestWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    owner = fact_if(EigentuemerSSZPubl3Cd, uni_w),
    district = factor(distr, uni_d)
  ) %>% 
  group_by(year, district, owner) %>% 
  summarize(
    apartments = sum_NA(apartments),
    people = sum_NA(people), 
    .groups = "drop") %>% 
  select(year, district, owner, apartments, people)

# plot 1000

# allocation by year and owner --------------------------------------------

# allocation (yw)
aca_yw <- group_by(aca_dat, year, owner) %>%
  summarize(
    apartments = sum_NA(apartments),
    people = sum_NA(people)
  ) %>%
  ungroup() %>%
  mutate(aca_yw = round(people / apartments, round_aca))

# plot 1001

# allocation (and apartments, people, by dyw)  ----------------------------

# allocation (dyw)
aca_dyw <- group_by(aca_dat, district, year, owner) %>%
  summarize(
    apartments = sum_NA(apartments),
    people = sum_NA(people)
  ) %>%
  ungroup() %>%
  mutate(aca_dyw = round(people / apartments, round_aca))

# plot 1002, 1003, 1004

# prediction: entire city, by owner ---------------------------------------

# base years
aca_yw_base <- filter(
  aca_yw,
  (year >= aca_base_begin) & (year <= aca_base_end)
) %>%
  select(year, owner, aca_yw)

# prediction (no allocation below zero)
aca_yw_pred <- con_reg(
  data = aca_yw_base, x = "year", y = "aca_yw",
  group_cols = "owner",
  window = aca_window_thres, base_t0 = aca_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = aca_prop_trend, thres_percent = aca_thres_percent,
  lower_thres = 0
)

# past and prediction
aca_yw_past_pred <- as_tibble(expand_grid(
  year = (min(aca_yw$year)):scen_end,
  owner = uni_w
)) %>%
  left_join(select(aca_yw, year, owner, aca_yw), by = c("year", "owner")) %>%
  left_join(select(aca_yw_pred, year, owner, pred_roll),
    by = c("year", "owner")
  ) %>%
  mutate(aca_yw_all = if_else(year < scen_begin, aca_yw, pred_roll))

# plot 1005

# prediction: by district and owner ---------------------------------------

# base years
aca_dyw_base <- filter(
  aca_dyw,
  (year >= aca_base_begin) & (year <= aca_base_end)
) %>%
  select(district, year, owner, aca_dyw)

# prediction (no allocation below zero)
aca_dyw_pred <- con_reg(
  data = aca_dyw_base, x = "year", y = "aca_dyw",
  group_cols = c("district", "owner"),
  window = aca_window_thres, base_t0 = aca_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = aca_prop_trend, thres_percent = aca_thres_percent,
  lower_thres = 0
)

# past and prediction
aca_dyw_past_pred <- as_tibble(expand_grid(
  district = uni_d,
  year = (min(aca_dyw$year)):scen_end,
  owner = uni_w
)) %>%
  left_join(aca_dyw, by = c("district", "year", "owner")) %>%
  left_join(select(aca_dyw_pred, district, year, owner, pred_roll),
    by = c("district", "year", "owner")
  ) %>%
  mutate(aca_dyw_all = if_else(year < scen_begin, aca_dyw, pred_roll))

# plot 1006

# prediction: Escher Wyss, by owner ---------------------------------------

# WHY a different approach for the Escher Wyss district, private housing?
# same approach as in the living space model (see living space code)
# WHY with owner?
# con_reg function needs groups (so far)

# Escher Wyss (district number: 52)
aca_dyw_52 <- filter(aca_dyw, district == "Escher Wyss") %>%
  select(year, owner, aca_dyw)

# base years
aca_dyw_base_52 <- filter(
  aca_dyw_52,
  (year >= aca_base_begin_52p) & (year <= aca_base_end)
)

# prediction (no allocation below zero)
aca_dyw_pred_52 <- con_reg(
  data = aca_dyw_base_52, x = "year", y = "aca_dyw",
  group_cols = "owner",
  window = aca_window_thres, base_t0 = aca_base_begin_52p,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = aca_prop_trend_52p, thres_percent = aca_thres_percent,
  lower_thres = 0
)

# past and prediction
aca_yw_past_pred_52 <- as_tibble(expand_grid(
  year = (min(aca_yw$year)):scen_end,
  owner = uni_w
)) %>%
  left_join(select(aca_dyw_52, year, owner, aca_dyw), by = c("year", "owner")) %>%
  left_join(select(aca_dyw_pred_52, year, owner, pred_roll),
    by = c("year", "owner")
  ) %>%
  mutate(
    aca_dyw_all = if_else(year < scen_begin, aca_dyw, pred_roll),
    district = uni_d[uni_d == "Escher Wyss"]
  )

# plot 1007

# combine the different data sets -----------------------------------------

# district, year, owner
aca_prep_dyw <- select(aca_dyw_past_pred, district, year, owner, aca_dyw_all) %>%
  rename(aca_dyw = aca_dyw_all)

# year, owner
aca_prep_yw <- select(aca_yw_past_pred, year, owner, aca_yw_all) %>%
  rename(aca_yw = aca_yw_all)

# Escher Wyss (only future: values of the past already contained in 'aca_prep_dyo')
aca_prep_52p <- select(aca_yw_past_pred_52, district, year, owner, aca_dyw_all) %>%
  filter((owner == uni_w[2]) & (year >= scen_begin)) %>%
  rename(aca_52p = aca_dyw_all)

# apartment threshold (mean over base years)

# WHY with expand_grid?
# correct calculation if no apartments (by owner) in a certain district and year

aca_apart_thres <- as_tibble(expand_grid(
  district = uni_d,
  year = aca_base_begin:aca_base_end,
  owner = uni_w
)) %>%
  left_join(select(aca_dyw, district, year, owner, apartments),
    by = c("district", "year", "owner")
  ) %>%
  replace_na(list(apartments = 0)) %>%
  group_by(district, owner) %>%
  summarize(apart_thres = mean(apartments)) %>%
  ungroup()

# combine the data sets
aca_comb <- aca_prep_dyw %>%
  left_join(aca_prep_yw, by = c("year", "owner")) %>%
  left_join(aca_prep_52p, by = c("district", "year", "owner")) %>%
  left_join(aca_apart_thres, by = c("district", "owner")) %>%
  mutate(aca = if_else(year < scen_begin, aca_dyw,
    if_else((district == "Escher Wyss") & (owner == uni_w[2]), aca_52p,
      if_else(apart_thres < aca_apart, aca_yw, aca_dyw)
    )
  ))

# plots 1008, 1009

# export the results ------------------------------------------------------

# export data
aca_ex_data <- mutate(aca_comb, aca_dyw = round(aca, round_aca)) %>%
  select(district, year, owner, aca_dyw) %>%
  filter(year >= scen_begin) %>%
  arrange(district, year, owner)

# export
write_csv(aca_ex_data, paste0(exp_path, "/allocation_future.csv"))

# log info
cat_log(paste0(
  "allocation: ",
  capture.output(Sys.time() - t0)
))
