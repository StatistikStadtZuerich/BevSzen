# header ------------------------------------------------------------------
# Emigration


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

# emigration*: rate per district and year ---------------------------------

# prediction: emigration* rate
ems_rate_dy <- mig_rate_dy(
  # paths, names
  mig_path = emi_od,
  mig_vari = "AnzWezuWir",
  mig_district = "QuarBisherCd",
  mig_name = "emigration",
  mig_number = "04",
  ex_path = paste0(exp_path, "/emigration-star_rate-dy_future.csv"),
  mis_base_begin = ems_base_begin,
  mis_base_end = ems_base_end,
  mis_rate_window_thres = ems_rate_window_thres,
  mis_rate_prop_trend = ems_rate_prop_trend,
  mis_rate_thres_percent = ems_rate_thres_percent,
  mis_rate_lower_thres = ems_rate_lower_thres
)


# emigration*: proportion of sex and origin -------------------------------
# by district and year

# prediction: proportion of sex an origin
ems_prop_so_dy <- mig_prop_so_dy(
  # paths, names
  mig_path = emi_od,
  mig_vari = "AnzWezuWir",
  mig_district = "QuarBisherCd",
  mig_name = "emigration",
  mig_number = "04",
  ex_path = paste0(exp_path, "/emigration-star_prop-so-dy_future.csv"),
  mis_so_base_begin = ems_so_base_begin,
  mis_so_base_end = ems_so_base_end,
  mis_so_window_thres = ems_so_window_thres,
  mis_so_prop_trend = ems_so_prop_trend,
  mis_so_thres_percent = ems_so_thres_percent,
  mis_so_lower_thres = ems_so_lower_thres
)


# emigration*: proportion of age ------------------------------------------
# per district, year, sex, origin

# prediction: proportion of age (duration: approx. 8 minutes)
ems_prop_a_dyso <- mig_prop_a_dyso(
  mig_path = emi_od,
  mig_vari = "AnzWezuWir",
  mig_district = "QuarBisherCd",
  mig_name = "emigration",
  mig_number = "04",
  ex_path = paste0(exp_path, "/emigration-star_prop-a-dyso_future.csv"),
  mis_age_min = age_min,
  mis_age_max = age_max,
  mis_span_y = ems_span_y,
  mis_span_a = ems_span_a,
  mis_age_base_begin = ems_age_base_begin,
  mis_age_base_end = ems_age_base_end,
  mis_age_window_thres = ems_age_window_thres,
  mis_age_prop_trend = ems_age_prop_trend,
  mis_age_thres_percent = ems_age_thres_percent,
  mis_age_lower_thres = ems_age_lower_thres
)


# log info
cat_log(paste0(
  "emigration* rate: ",
  capture.output(Sys.time() - t0)
))
