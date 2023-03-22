# header ------------------------------------------------------------------
# relocation proportion of immigration*


# paths, general ----------------------------------------------------------

source(paste0(here::here(), "/1_code/0000_general/general_utils.R"))
util_gf()

# start time
t0 <- Sys.time()


# proportion of relocation (based on immigration*) ------------------------

ims_rel_prop <- rel_prop(
  mig_path = imm_od,
  mig_vari = "AnzZuzuWir",
  mig_district = "QuarCd",
  mig_name = "immigration",
  rem_number = "05",
  ex_path = paste0(exp_path, "/relocation_immigration_future.csv"),
  rem_base_begin = rei_base_begin,
  rem_base_end = rei_base_end,
  rem_age_max = rei_age_max,
  rem_mis_span_dyao = rei_ims_span_dyao,
  rem_rel_span_dyao = rei_rel_span_dyao,
  rem_mis_span_dao = rei_ims_span_dao,
  rem_rel_span_dao = rei_rel_span_dao,
  rem_mis_thres_y = rei_ims_thres_y,
  rem_prop_span = rei_prop_span,
  rem_window_thres = rei_window_thres,
  rem_prop_trend = rei_prop_trend,
  rem_thres_percent = rei_thres_percent,
  rem_lower_thres = rei_lower_thres,
  rem_upper_thres = rei_upper_thres,
  rem_pred_span = rei_pred_span
)


# log info
cat_log(paste0(
  "relocation and immigration: ",
  capture.output(Sys.time() - t0)
))
