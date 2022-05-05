# header ------------------------------------------------------------------
# ownership

# paths, general ----------------------------------------------------------

# general functions already available?
if (!exists("para")) {

  # working directory
  library(here)
  setwd(paste0(here(), "/2020phase2/"))

  # general functions (without dependence on parameters)
  source("1_Code/0000_General/0002_general_without-parameters.r")

  # parameters (depend on scenario)
  i_scen <- "middle"
  for (i_para in 1:nrow(para)) {
    assign(para$parameter[i_para], para[[i_scen]][i_para],
      envir = .GlobalEnv
    )
  }

  # general functions (with dependence on parameters)
  source(paste0(code_path, "/0000_General/0003_general_with-parameters.r"))
}

# start time
t0 <- Sys.time()

# import, data preparation ------------------------------------------------

# ownership: data
own_dat <- read_csv(spa_od) %>%
  rename(year = StichtagDatJahr, apartments = AnzWhg, people = PersInGeb) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    owner = factor(if_else(EigentumCd == 1, uni_w[1], uni_w[2]), uni_w),
    district = factor(distr, uni_d)
  ) %>%
  select(year, district, owner, apartments, people) %>%
  group_by(district, year, owner) %>%
  summarize(
    apartments = sum_NA(apartments),
    people = sum_NA(people)
  ungroup()



# proportion of cooperative housing ---------------------------------------

# proportions (based on apartments and people)
own_prop <- group_by(own_dat, district, year) %>%
  mutate(
    prop_apartments = round(apartments / sum_NA(apartments) * 100, round_prop),
    prop_people = round(people / sum_NA(people) * 100, round_prop)
  ) %>%
  filter(owner == uni_w[1]) %>%
  select(district, year, prop_apartments, prop_people) %>%
  pivot_longer(
    cols = starts_with("prop"), names_prefix = "prop_",
    names_to = "category", values_to = "prop"
  )

# plot
sszplot(own_prop,
  aes_x = "year", aes_y = "prop", aes_col = "category",
  labs_y = "proportion of cooperative housing (in %)",
  wrap = "district", ncol = 4,
  scale_y = c(0, NA),
  name = "1200_proportion-cooperative-housing",
  width = 12, height = 14
)



# prediction --------------------------------------------------------------

# category: people
# WHY? prediction is based on different sources
# projects: apartments
# reserves: m2
# however, the combined prediction is for personen
# therefore, the proportion based on people

own_cat <- filter(own_prop, category == "people") %>%
  select(district, year, prop) %>%
  ungroup()
tail(own_cat)

# base years
# WHY not in previous pipe? The previous tibble will be used later for plotting
own_base <- filter(own_cat, (year >= own_base_begin) & (year <= own_base_end))

# prediction
own_pred <- con_reg(
  data = own_base, x = "year", y = "prop",
  group_cols = "district",
  window = own_window_thres, base_t0 = own_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = own_prop_trend, thres_percent = own_thres_percent,
  lower_thres = own_lower_thres, upper_thres = own_upper_thres
)

# past and prediction
own_past_pred <- as_tibble(expand_grid(
  district = uni_d,
  year = (min(own_cat$year)):scen_end
)) %>%
  left_join(own_cat, by = c("district", "year")) %>%
  left_join(select(own_pred, district, year, pred_roll),
    by = c("district", "year")
  ) %>%
  mutate(own_all = if_else(year < scen_begin, prop, pred_roll))

# plot
sszplot(own_past_pred,
  aes_x = "year", aes_y = "own_all",
  labs_y = "proportion of cooperative housing (in %)",
  wrap = "district", ncol = 4,
  i_x = c(own_base_begin, own_base_end),
  scale_y = c(0, NA),
  name = "1201_proportion-cooperative-housing_prediction",
  width = 12, height = 14
)



# export the results ------------------------------------------------------

# data of past and future
# WHY? to calculate population (by ownership) in the past
# since the people in the open data dataset (with ownership)
# is only people in apartments

# export data
own_ex_data <- mutate(own_past_pred, prop = round(own_all, round_prop)) %>%
  select(district, year, prop) %>%
  arrange(district, year)

# export
write_csv(own_ex_data, paste0(exp_path, "/ownership_past_future.csv"))

# log info
cat_log(paste0(
  "ownership: ",
  capture.output(Sys.time() - t0)
))
