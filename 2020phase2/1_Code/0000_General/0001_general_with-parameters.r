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
    
    
    
