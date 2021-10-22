#-------------------------------------------------------------------
#Housing Model
#
#
#
#rok/bad, October 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#general functions already available?
    if (!exists("para")) {
      
        #working directory
            library(here)
            setwd(paste0(here(), "/2020phase2/"))
        
        #general (e.g. packages, colors)
            source("1_Code/0000_General/0000_general_phase2.r")
            
    }

    
#-------------------------------------------------------------------
#import, data preparation
#-------------------------------------------------------------------

#projects (apartments)
    pro_dat <- read_csv(paste0(exp_path, "/projects_future.csv"))
    
#allocation (persons per apartment)
    aca_dat <- read_csv(paste0(exp_path, "/allocation_future.csv"))     
      
    
    
    
#capacity, reserves (m2 usage)  
    car_dat <- read_csv(paste0(exp_path, "/usage_area.csv"))    
    
#living space (m2 per person)   
    spa_dat <- read_csv(paste0(exp_path, "/living-space_future.csv"))    
 

    
#-------------------------------------------------------------------
#import, data preparation
#-------------------------------------------------------------------
    
    
    

