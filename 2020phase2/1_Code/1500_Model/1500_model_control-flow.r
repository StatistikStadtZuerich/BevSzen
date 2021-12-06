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
#source the code files
#-------------------------------------------------------------------
    
#function for different scenarios and modules

    #possible values    
        #scenarios: lower, middle, upper
        #modules
            # all 
                # dem (demography-modules), 
                    # bir (birth)
                    # dea (death) 
                    # ims (immigration*)
                    # ems (emigration*)
                    # rei (relocation-immigration)
                    # ree (relocation-emigration)
                    # nat (naturalization)
                # hom (housing-modules)
                    # car (capacity-reserves)
                    # spa (living space)
                    # aca (allocation)    
                    # pro (projects)
                    # own (ownership)
                    # hou (housing model)
                # deh (demography and housing model)

    
    
    run_scen <- function(scenarios, modules){
        
 
        #different scenarios
            for (i_scen in scenarios){
                #i_scen <- "middle"
                
                #assign values to parameters 
                    #to global environment
                    #WHY? will be used in functions outside this function
                
                    for (i_para in 1:nrow(para)) {
                        assign(para$parameter[i_para], para[[i_scen]][i_para], 
                               envir = .GlobalEnv)} 
                
                #same with the scenario name
                    assign("i_scen", i_scen, envir = .GlobalEnv)
                
                #general functions (with dependence on parameters)
                    source(paste0(code_path, "/0000_General/0001_general_with-parameters.r"))
                    
                #birth
                    if (modules %in% c("all", "dem", "bir")) {
                        
                        t0 <- Sys.time()
                        source(paste0(code_path, "0100_Birth/0100_birth-fertility.r"))
                        cat_log(paste0("fertility rate: ", Sys.time() - t0))
  
                        source(paste0(code_path, "0100_Birth/0110_birth-origin.r"))
                        source(paste0(code_path, "0100_Birth/0120_birth-sex-ratio.r"))
                        
                    log_print("birth module completed") 
                    
                    }
                    
                #death
                    if (modules %in% c("all", "dem", "dea")) {
                        source(paste0(code_path, "0200_Death/0200_death.r"))
                        
                    log_print("death module completed") 
                    
                    }
                                        
                #immigration*
                    if (modules %in% c("all", "dem", "ims")) {
                        source(paste0(code_path, "0300_Immigration/0300_immigration.r"))
                    }
                                        
                #emigration*
                    if (modules %in% c("all", "dem", "ems")) {
                        source(paste0(code_path, "0400_Emigration/0400_emigration.r"))
                    }   
                       
                #relocation-immigration
                    if (modules %in% c("all", "dem", "rei")) {
                        source(paste0(code_path, "0500_Relocation-Immigration/0500_relocation-immigration.r"))
                    }                       
                        
                #relocation-emigration
                    if (modules %in% c("all", "dem", "ree")) {
                        source(paste0(code_path, "0600_Relocation-Emigration/0600_relocation-emigration.r"))
                    }                       
                        
                #naturalization
                    if (modules %in% c("all", "dem", "nat")) {
                        source(paste0(code_path, "0700_Naturalization/0700_naturalization.r"))
                    }   
                    
                #capacity, reserves
                    if (modules %in% c("all", "hom", "car")) {
                        source(paste0(code_path, "0800_Capacity-Reserves/0800_capacity-reserves.r"))
                    }                       
                    
                #living space
                    if (modules %in% c("all", "hom", "spa")) {
                        source(paste0(code_path, "0900_Living-Space/0900_living-space.r"))
                    }                       
                                        
                #allocation
                    if (modules %in% c("all", "hom", "aca")) {
                        source(paste0(code_path, "1000_Allocation/1000_allocation.r"))
                    }                       
                       
                #projects
                    if (modules %in% c("all", "hom", "pro")) {
                        source(paste0(code_path, "1100_Projects/1100_projects.r"))
                    }                       
                       
                #ownership
                    if (modules %in% c("all", "hom", "own")) {
                        source(paste0(code_path, "1200_Ownership/1200_ownership.r"))
                    }                       
                                                               
                #housing model
                    if (modules %in% c("all", "hom", "hou")) {
                        source(paste0(code_path, "1300_Housing-Model/1300_housing-model.r"))
                    }                       
                                             
            #end: different scenarios                    
            }
        
        #close the log file
            log_close()                      
                                   
        #end of scenario function             
            }
           

#execute the function
    run_scen(
        scenarios = c("lower"),
        modules = c("bir", "dea"))            
            
        




                    

    
