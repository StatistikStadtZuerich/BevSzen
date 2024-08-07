# header ------------------------------------------------------------------
# immigration


# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()


# immigration*: rate per district and year --------------------------------

# prediction: immigration* rate
ims_rate_dy <- mig_rate_dy(
  # paths, names
  mig_path = imm_od,
  mig_vari = "AnzZuzuWir",
  mig_district = "QuarCd",
  mig_name = "immigration",
  mig_number = "03",
  ex_path = paste0(exp_path, "/immigration-star_rate-dy_future.csv"),
  mis_base_begin = ims_base_begin,
  mis_base_end = ims_base_end,
  mis_rate_window_thres = ims_rate_window_thres,
  mis_rate_prop_trend = ims_rate_prop_trend,
  mis_rate_thres_percent = ims_rate_thres_percent,
  mis_rate_lower_thres = ims_rate_lower_thres
)


# immigration*: proportion of sex and origin ------------------------------
# by district and year

# prediction: proportion of sex an origin
ims_prop_so_dy <- mig_prop_so_dy(
  # paths, names
  mig_path = imm_od,
  mig_vari = "AnzZuzuWir",
  mig_district = "QuarCd",
  mig_name = "immigration",
  mig_number = "03",
  ex_path = paste0(exp_path, "/immigration-star_prop-so-dy_future.csv"),
  mis_so_base_begin = ims_so_base_begin,
  mis_so_base_end = ims_so_base_end,
  mis_so_window_thres = ims_so_window_thres,
  mis_so_prop_trend = ims_so_prop_trend,
  mis_so_thres_percent = ims_so_thres_percent,
  mis_so_lower_thres = ims_so_lower_thres
)


# immigration*: proportion of age -----------------------------------------
# per district, year, sex, origin

# prediction: proportion of age (duration: approx. 8 minutes)
ims_prop_a_dyso <- mig_prop_a_dyso(
  mig_path = imm_od,
  mig_vari = "AnzZuzuWir",
  mig_district = "QuarCd",
  mig_name = "immigration",
  mig_number = "03",
  ex_path = paste0(exp_path, "/immigration-star_prop-a-dyso_future.csv"),
  mis_age_min = age_min,
  mis_age_max = age_max,
  mis_span_y = ims_span_y,
  mis_span_a = ims_span_a,
  mis_age_base_begin = ims_age_base_begin,
  mis_age_base_end = ims_age_base_end,
  mis_age_window_thres = ims_age_window_thres,
  mis_age_prop_trend = ims_age_prop_trend,
  mis_age_thres_percent = ims_age_thres_percent,
  mis_age_lower_thres = ims_age_lower_thres
)

# log info
cat_log(paste0(
  "immigration* rate: ",
  capture.output(Sys.time() - t0)
))
