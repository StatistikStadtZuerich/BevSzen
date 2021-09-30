#-------------------------------------------------------------------
#Function: Relocation proportion of emigration*
#
#
#
#
#rok/bad, July 2021
#-------------------------------------------------------------------



if (!exists("para")) {
  #-------------------------------------------------------------------
  #paths, general
  #-------------------------------------------------------------------
  
  #working directory
  library(here)
  setwd(paste0(here(), "/2020phase2/"))
  
  #general (e.g. packages, colors)
  source("1_Code/0000_General/0000_general_phase2.r")
}


#-------------------------------------------------------------------
#proportion of relocation (based on immigration*)
#-------------------------------------------------------------------

#proportion
    ems_rel_prop <- rel_prop(
                mig_path = emi_od,
                mig_vari = "AnzWezuWir", 
                mig_district = "QuarBisherCd", 
                mig_name = "emigration",
                rem_number = "06", 
                ex_path = paste0(exp_path, "/relocation_emigration_future.csv"),
                rem_base_begin = ree_base_begin, 
                rem_base_end = ree_base_end, 
                rem_age_max = ree_age_max,                
                rem_mis_span_dyao = ree_ems_span_dyao,
                rem_rel_span_dyao = ree_rel_span_dyao, 
                rem_mis_span_dao = ree_ems_span_dao,
                rem_rel_span_dao = ree_rel_span_dao, 
                rem_mis_thres_y = ree_ems_thres_y,
                rem_prop_span = ree_prop_span,
                rem_window_thres = ree_window_thres,
                rem_prop_trend = ree_prop_trend,
                rem_thres_percent = ree_thres_percent,
                rem_lower_thres = ree_lower_thres,
                rem_upper_thres = ree_upper_thres,
                rem_pred_span = ree_pred_span)                
                
                

 