#-------------------------------------------------------------------
#Model (control flows)
#
#
#
#rok/bad, December 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, parameters, functions
#-------------------------------------------------------------------

#working directory
    library(here)
    setwd(paste0(here(), "/2020phase2/"))
    
#general functions (without dependence on parameters)    
    source("1_Code/0000_General/0000_general_without-parameters.r")    



#function to source all the code files

run_scen <- function(modules = c("all"))

    #assign values to parameters
        for (ipara in 1:nrow(para)) {
            assign(para$parameter[ipara], para$value[ipara])}    
        
    #general functions (with dependence on parameters)    
        source(paste0(code_path, "/0000_General/0001_general_with-parameters.r"))
        
        
    




                    

    
