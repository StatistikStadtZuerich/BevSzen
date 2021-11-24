#-------------------------------------------------------------------
#Demography and Housing Model
#
#
#
#rok/bad, November 2021
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

# #projects (apartments, dyw)
#     pro_dat <- read_csv(paste0(exp_path, "/projects_future.csv"))
#     
# #allocation (persons per apartment, dyw)
#     aca_dat <- read_csv(paste0(exp_path, "/allocation_future.csv"))     
#       
# #capacity/reserves (m2, dyw)  
#     car_dat <- read_csv(paste0(exp_path, "/usage_area.csv"))    
#     
# #living space (m2 per person, dyw)   
#     spa_dat <- read_csv(paste0(exp_path, "/living-space_future.csv"))    
#  
# #ownership (% cooperative housing)
#     own_dat <- read_csv(paste0(exp_path, "/ownership_past_future.csv"))
#     tail(own_dat)
#     
# #population   
#     #why population not from the housing open data  file?
#     #there only people in apartments (and not in care centers etc)
#     #the population number in the housing open data is below the total 
#     #amount of people in Zurich
#     
#     pop <- read_csv(pop_od) %>%   
#         rename(year = StichtagDatJahr, pop = AnzBestWir) %>%  
#         left_join(look_dis, by = "QuarCd") %>% 
#         mutate(district = factor(distr, uni_d)) %>%      
#         select(district, year, pop) %>%    
#         group_by(district, year) %>% 
#             summarize(pop = sum(pop),
#                       .groups = "drop")          
          

    
    
    
