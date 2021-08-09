#-------------------------------------------------------------------
#Function: Relocation proportion of emigration*
#
#
#
#
#rok/bad, July 2021
#-------------------------------------------------------------------





#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#working directory
    setwd("O:/Projekte/BevSzen/2020_sandkasten_bad-rok/2020phase2/")

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")




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
                graph_path = "3_Results/0600_Relocation-Emigration/",
                ex_path = "2_Data/4_Rates/relocation_emigration_future.csv",
                rem_base_begin = ree_base_begin, 
                rem_base_end = ree_base_end, 
                rem_age_max = ree_age_max,                
                rem_mis_span_dyao = ree_ems_span_dyao,
                rem_rel_span_dyao = ree_rel_span_dyao, 
                rem_mis_span_yao = ree_ems_span_yao,
                rem_rel_span_yao = ree_rel_span_yao, 
                rem_mis_span_ya = ree_ems_span_ya,
                rem_rel_span_ya = ree_rel_span_ya,
                rem_mis_thres_d = ree_ems_thres_d,
                rem_mis_thres_o = ree_ems_thres_o,
                rem_prop_span = ree_prop_span,
                rem_window_thres = ree_window_thres,
                rem_prop_trend = ree_prop_trend,
                rem_thres_percent = ree_thres_percent,
                rem_lower_thres = ree_lower_thres,
                rem_upper_thres = ree_upper_thres,
                rem_pred_span = ree_pred_span)                
                
                

      


 