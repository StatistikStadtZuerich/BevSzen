# header ------------------------------------------------------------------
# living space

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# import, data preparation ------------------------------------------------

# living space: data
spa_dat <- read_csv(spa_od) %>%
  rename(year = StichtagDatJahr, area = Wohnflaeche, apartments = AnzWhgStat, people = AnzBestWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    owner = fact_if(EigentuemerSSZPubl3Cd_noDM, uni_w),
    district = factor(distr, uni_d)
  ) %>%
  group_by(year, district, owner) %>% 
  summarize(
    area = sum_NA(area),    
    apartments = sum_NA(apartments),
    people = sum_NA(people), 
    .groups = "drop") %>%   
  select(year, district, owner, area, apartments, people)

# plot 0900: living space by year
 
# living space by year and owner ------------------------------------------
 
# livings space (yw)
spa_yw <- group_by(spa_dat, year, owner) %>%
  summarize(
    area = sum_NA(area),
    people = sum_NA(people)
  ) %>%
  ungroup() %>%
  mutate(spa_yw = round(area / people, round_area))

# plot 0901

# living space (area, apartments, people, by dyw) -------------------------

# livings space (dyw)
spa_dyw <- group_by(spa_dat, district, year, owner) %>%
  summarize(
    area = sum_NA(area),
    area_ha = area / 10000,
    apartments = sum_NA(apartments),
    people = sum_NA(people)
  ) %>%
  ungroup() %>%
  mutate(spa_dyw = round(area / people, round_area))

# plots 0902, 0903, 0904, 0905

# prediction: entire city, by owner ---------------------------------------

# base years
spa_yw_base <- filter(
  spa_yw,
  (year >= spa_base_begin) & (year <= spa_base_end)
) %>%
  select(year, owner, spa_yw)

# prediction (no living space below zero)
spa_yw_pred <- con_reg(
  data = spa_yw_base, x = "year", y = "spa_yw",
  group_cols = "owner",
  window = spa_window_thres, base_t0 = spa_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = spa_prop_trend, thres_percent = spa_thres_percent,
  lower_thres = 0
)

# past and prediction
spa_yw_past_pred <- as_tibble(expand_grid(
  year = (min(spa_yw$year)):scen_end,
  owner = uni_w
)) %>%
  left_join(select(spa_yw, year, owner, spa_yw), by = c("year", "owner")) %>%
  left_join(select(spa_yw_pred, year, owner, pred_roll),
    by = c("year", "owner")
  ) %>%
  mutate(spa_yw_all = if_else(year < scen_begin, spa_yw, pred_roll))

# plot 0906

# prediction: by district and owner ---------------------------------------

# base years
spa_dyw_base <- filter(
  spa_dyw,
  (year >= spa_base_begin) & (year <= spa_base_end)
) %>%
  select(district, year, owner, spa_dyw)

# prediction (no living space below zero)
spa_dyw_pred <- con_reg(
  data = spa_dyw_base, x = "year", y = "spa_dyw",
  group_cols = c("district", "owner"),
  window = spa_window_thres, base_t0 = spa_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = spa_prop_trend, thres_percent = spa_thres_percent,
  lower_thres = 0
)

# past and prediction
spa_dyw_past_pred <- as_tibble(expand_grid(
  district = uni_d,
  year = (min(spa_dyw$year)):scen_end,
  owner = uni_w
)) %>%
  left_join(spa_dyw, by = c("district", "year", "owner")) %>%
  left_join(select(spa_dyw_pred, district, year, owner, pred_roll),
    by = c("district", "year", "owner")
  ) %>%
  mutate(spa_dyw_all = if_else(year < scen_begin, spa_dyw, pred_roll))

# plot 0907

# prediction: Escher Wyss, by owner ---------------------------------------

# WHY a different approach for the Escher Wyss district, private housing?
# the living space over time differs substantially from the other districts
# (increase until 2013, then decrease)
# reason: few apartments in 2008 then very intense construction activity
# numbers: less than 1500 apartments in 2008
# more than 3100 apartments in 2017
# after construction some apartments were finished, yet no one lived there yet
# for a certain moment the empty apartments increase the living space (based on buildings)
# due to the high ratio of new buildings (compared to existing buildings) the living space was increased
# this will not happen again in future (due to the higher amount of buildings)

# WHY with owner?
# con_reg function needs groups (so far)

# Escher Wyss (district number: 52)
spa_dyw_52 <- filter(spa_dyw, district == "Escher Wyss") %>%
  select(year, owner, spa_dyw)

# base years
spa_dyw_base_52 <- filter(
  spa_dyw_52,
  (year >= spa_base_begin_52p) & (year <= spa_base_end)
)

# prediction (no living space below zero)
spa_dyw_pred_52 <- con_reg(
  data = spa_dyw_base_52, x = "year", y = "spa_dyw",
  group_cols = "owner",
  window = spa_window_thres, base_t0 = spa_base_begin_52p,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = spa_prop_trend_52p, thres_percent = spa_thres_percent,
  lower_thres = 0
)

# past and prediction
spa_yw_past_pred_52 <- as_tibble(expand_grid(
  year = (min(spa_yw$year)):scen_end,
  owner = uni_w
)) %>%
  left_join(select(spa_dyw_52, year, owner, spa_dyw), by = c("year", "owner")) %>%
  left_join(select(spa_dyw_pred_52, year, owner, pred_roll),
    by = c("year", "owner")
  ) %>%
  mutate(
    spa_dyw_all = if_else(year < scen_begin, spa_dyw, pred_roll),
    district = uni_d[uni_d == "Escher Wyss"]
  )

# plot 0908

# combine the different data sets -----------------------------------------

# district, year, owner
spa_prep_dyw <- select(spa_dyw_past_pred, district, year, owner, spa_dyw_all) %>%
  rename(spa_dyw = spa_dyw_all)

# year, owner
spa_prep_yw <- select(spa_yw_past_pred, year, owner, spa_yw_all) %>%
  rename(spa_yw = spa_yw_all)

# Escher Wyss (only future: values of the past already contained in 'spa_prep_dyo')
spa_prep_52p <- select(spa_yw_past_pred_52, district, year, owner, spa_dyw_all) %>%
  filter((owner == uni_w[2]) & (year >= scen_begin)) %>%
  rename(spa_52p = spa_dyw_all)

# apartment threshold (mean over base years)

# WHY with expand_grid?
# correct calculation if no apartments (by owner) in a certain district and year

spa_apart_thres <- as_tibble(expand_grid(
  district = uni_d,
  year = spa_base_begin:spa_base_end,
  owner = uni_w
)) %>%
  left_join(select(spa_dyw, district, year, owner, apartments),
    by = c("district", "year", "owner")
  ) %>%
  replace_na(list(apartments = 0)) %>%
  group_by(district, owner) %>%
  summarize(apart_thres = mean(apartments)) %>%
  ungroup()

# combine the data sets
spa_comb <- spa_prep_dyw %>%
  left_join(spa_prep_yw, by = c("year", "owner")) %>%
  left_join(spa_prep_52p, by = c("district", "year", "owner")) %>%
  left_join(spa_apart_thres, by = c("district", "owner")) %>%
  mutate(spa = if_else(year < scen_begin, spa_dyw,
    if_else((district == "Escher Wyss") & (owner == uni_w[2]), spa_52p,
      if_else(apart_thres < spa_apart, spa_yw, spa_dyw)
    )
  ))

# plots 0909, 0910: past and prediction

# export the results ------------------------------------------------------

# export data
spa_ex_data <- mutate(spa_comb, spa_dyw = round(spa, round_area)) %>%
  select(district, year, owner, spa_dyw) %>%
  filter(year >= scen_begin) %>%
  arrange(district, year, owner)

# export
write_csv(spa_ex_data, paste0(exp_path, "/living-space_future.csv"))

# log info
cat_log(paste0(
  "living space: ",
  capture.output(Sys.time() - t0)
))
