#-------------------------------------------------------------------
#General: levels, colors, functions
#(with dependence on parameters, i.e. scenarios)
#
#
#rok/bad, December 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#unique levels
#-------------------------------------------------------------------

#year: base period
    uniy_bir_base <- bir_base_begin:bir_base_end

#year: future
    uniy_szen <- szen_begin:szen_end

    
#-------------------------------------------------------------------
#lookup tables
#-------------------------------------------------------------------
    
#projects not realized
    look_not <- tibble(status = factor(pro_category, levels = pro_category),
                       not_realized = c(pro_not_scheduled, pro_not_submitted, 
                                        pro_not_approved, pro_not_started,
                                        pro_not_completed, pro_not_onhold))
    
    
#-------------------------------------------------------------------
#colors
#-------------------------------------------------------------------

#color for year (base period)
    col_y_base <- colorRampPalette(col_6)(length(uniy_bir_base))

#colour for time distributions
    col_time <- c(
        rep(col_grey, length(uniy_bir_base)),
        colorRampPalette(col_6[1:5])(length(uniy_szen)))



#-------------------------------------------------------------------
#specific functions
#-------------------------------------------------------------------

#migration functions
    source(paste0(code_path, "/0000_General/0013_migration-functions.r"))

#relocation functions
    source(paste0(code_path, "/0000_General/0014_relocation-function.r"))
    
    
    
