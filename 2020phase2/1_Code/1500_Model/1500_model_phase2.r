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
    
#general functions without dependence on parameters    
    source("1_Code/0000_General/0000_general_without-parameters.r")    
    

#-------------------------------------------------------------------
#source all the code files
#-------------------------------------------------------------------
    
#function for different scenarios and modules

    #possible values    
        #scenarios: lower, middle, upper
        #modules
            # all 
                # dem (demography-modules), 
                    # bir (birth), dea (death), 
                    # ims (immigration*), ems (emigration*)
                    # rei (relocation-immigration), ree (relocation-emigration)
                    # nat (naturalization)
                # hom (housing-modules)
                    # car (capacity-reserves), pro (projects)
                    # spa (living space), aca (allocation)
                    # own (ownership)
                    # hou (housing model)
                # deh (demography and housing model)

    run_scen <- function(scenarios, modules){
        
        #different scenarios
            for (i_scen in scenarios){
                
                #assign values to parameters
                    for (i_para in 1:nrow(para)) {
                        assign(para$parameter[i_para], para[[i_scen]][i_para])}  
                
                #general functions (with dependence on parameters)    
                    source(paste0(code_path, "/0000_General/0001_general_with-parameters.r"))                        
            
            #birth                
                if (modules %in% c("all", "dem", "bir")) {
                    source(paste0(code_path, "0100_Birth/0100_birth-fertility.r"))
                    source(paste0(code_path, "0100_Birth/0110_birth-origin.r"))
                    source(paste0(code_path, "0100_Birth/0120_birth-sex-ratio.r"))
                }

                
            #end: different scenarios                    
                }
    
        #end of scenario function             
            }
            
#execute the function
    run_scen(
        scenarios = c("middle"),
        modules = c("bir"))            
            
        




                    

    
