# header ------------------------------------------------------------------
# projects (i.e. construction projects)

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# temporary path (since data not on open data yet)
pro_path <- paste0(here::here(), "/2_Data/1_Input/BEV347OD3470.csv")

# start time
t0 <- Sys.time()

# import, data preparation ------------------------------------------------

# projects: data
pro_dat <- read_csv(pro_path) %>%
  rename(year = StichtagDatJahr) %>%
  left_join(look_pro, by = c("StatusCd" = "code")) %>%
  mutate(distnum = as.numeric(QuarCd)) %>%
  left_join(look_reg, by = "distnum") %>%
  mutate(owner = fact_if(EigentumCd, uni_w)) %>%
  select(district, year, owner, status, WhgNeu, WhgAbbruch) %>%
  pivot_longer(
    cols = c("WhgNeu", "WhgAbbruch"), names_to = "ind",
    values_to = "apartments"
  ) %>%
  mutate(indicator = fact_if(ind, uni_i, "WhgNeu")) %>%
  select(district, year, owner, status, indicator, apartments)

# with all possible cases
pro_all <- as_tibble(expand_grid(
  district = uni_d,
  year = min(pro_dat$year):max(pro_dat$year),
  owner = uni_w,
  status = uni_t,
  indicator = uni_i
)) %>%
  left_join(pro_dat, by = c("district", "year", "owner", "status", "indicator")) %>%
  replace_na(list(apartments = 0))


# plots 1100, 1101, 1102, 1103, 1104, 1105: indicators

# not all projects realized -----------------------------------------------

# not realized
pro_not <- left_join(pro_all, look_not,
  by = "status"
) %>%
  mutate(realized = apartments * (100 - not_realized) / 100)


# delayed -----------------------------------------------------------------

# lambda: non-linear model
# WHY lambda positive?
# to allow transformation before the regression

data_points <- tibble(
  year = c(pro_begin, pro_end),
  lambda = c(pro_lambda_begin, pro_lambda_end)^(1 / pro_transfo)
)

lm_ <- lm(lambda ~ year, data = data_points)

lambda_y <- tibble(year = pro_begin:pro_end) %>%
  add_predictions(lm_) %>%
  mutate(lambda = pred^pro_transfo) %>%
  select(year, lambda)

plot(lambda_y$year, lambda_y$lambda)

# delay in years (1 to 'pro_max_delay')
# WHY plus 1? since 'no delay' is also possible, one additional category

delay <- as_tibble(expand_grid(
  year = pro_begin:pro_end,
  delta = 1:(pro_max_delay + 1)
)) %>%
  left_join(lambda_y, by = "year") %>%
  mutate(
    y = exp(-lambda * delta),
    delay = delta - 1,
    delayText = if_else(delay == 1, paste0(delay, " year"),
      paste0(delay, " years")
    )
  ) %>%
  group_by(year) %>%
  mutate(ynorm = y / sum(y) * 100) %>%
  ungroup()

# plot 1106

# projects and delay
pro_delay <- as_tibble(expand_grid(
  district = uni_d,
  year = pro_begin:pro_end,
  owner = uni_w,
  status = pro_category,
  indicator = uni_i,
  delay = as.double(0:5)
)) %>%
  left_join(select(pro_not, c(district, year, owner, status, indicator, realized)),
    by = c("district", "year", "owner", "status", "indicator")
  ) %>%
  left_join(select(delay, year, delay, ynorm),
    by = c("year", "delay")
  ) %>%
  mutate(
    year_new = year + delay,
    realized_new = realized * ynorm / 100
  ) %>%
  group_by(district, year_new, owner, status, indicator) %>%
  summarize(
    apartments = sum(realized_new),
    .groups = "drop"
  ) %>%
  rename(year = year_new)

# control
sum(pro_not$realized)
sum(pro_delay$apartments)

# plot 1107: initial vs. 'not realized'/'delayed' 

# summarize (status not needed anymore) -----------------------------------

pro_sum <- pro_delay %>%
  group_by(district, year, owner, indicator) %>%
  summarize(apartments = sum(apartments),
  .groups = "drop")


# export the results (projects: new/removed, by category) -----------------

# export data
pro_ex_data <- arrange(pro_sum, district, year, owner, indicator)

# export
write_csv(pro_ex_data, paste0(exp_path, "/projects_future.csv"))

# log info
cat_log(paste0(
  "projects: ",
  capture.output(Sys.time() - t0)
))
