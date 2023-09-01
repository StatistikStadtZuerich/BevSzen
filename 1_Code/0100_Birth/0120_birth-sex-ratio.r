# header ------------------------------------------------------------------
# birth: sex ratio

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# import, proportion male -------------------------------------------------

# proportion male
pro_male <- read_csv(bir_od) %>%
  rename(year = EreignisDatJahr, bir = AnzGebuWir) %>%
  mutate(sex = fact_if(SexCd, uni_s)) %>%
  select(year, sex, bir) %>%
  group_by(year, sex) %>%
  summarize(
    bir = sum(bir),
    .groups = "drop"
  ) %>%
  spread(key = sex, value = bir) %>%
  mutate(pro_male = round(male / (male + female) * 100, round_rate))

# plot 0190

# prediction --------------------------------------------------------------

# model
# the sex proportion could depend on age
# see: https://www.pnas.org/content/112/16/E2102, Figure 4
# however, a simple model is used: mean only

# mean
pred_mean <- pro_male %>%
  filter(year >= bir_sex_ratio_begin & year <= bir_sex_ratio_end) %>%
  summarize(
    pred_mean = mean(pro_male),
    .groups = "drop"
  )

# plot 0191

# export the results ------------------------------------------------------

# proportion male: prediction
pro_male_pred <- tibble(
  year = scen_begin:scen_end,
  pro_male = round(pred_mean$pred_mean, round_rate)
)

# export
write_csv(pro_male_pred, paste0(exp_path, "/birth_sex-ratio_future.csv"))

# log info
cat_log(paste0(
  "proportion male babies: ",
  capture.output(Sys.time() - t0)
))
