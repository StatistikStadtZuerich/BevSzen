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
      
#capacity/reserves (m2 usage)  
    car_dat <- read_csv(paste0(exp_path, "/usage_area.csv"))    
    
#living space (m2 per person)   
    spa_dat <- read_csv(paste0(exp_path, "/living-space_future.csv"))    
 
#ownership
    own_dat <- read_csv(paste0(exp_path, "/ownership_future.csv")) 
    
# #people (also per ownership category)
#     pop_dyw <- read_csv(spa_od) %>% 
#         rename(year = StichtagDatJahr, apartments = AnzWhg, people = PersInGeb) %>% 
#         filter(year == date_end) %>% 
#         left_join(look_dis, by = "QuarCd") %>% 
#         mutate(owner = factor(if_else(EigentumCd== 1, uni_w[1], uni_w[2]), uni_w),
#             district = factor(distr, uni_d)) %>% 
#         select(year, district, owner, apartments, people) %>% 
#         group_by(district, year, owner) %>% 
#             summarize(apartments = sum_NA(apartments),
#                 people = sum_NA(people)) %>% 
#         ungroup() 
#         
    

    
#-------------------------------------------------------------------
#projects and allocation
#-------------------------------------------------------------------

#calculate amount of people 
    pro_aca <- left_join(pro_dat, aca_dat, 
            by = c("district", "year", "owner")) %>% 
        mutate(people = apartments * aca_dyw)
    
    
    
#-------------------------------------------------------------------
#capacity/reserves and living space
#-------------------------------------------------------------------

#calculate amount of people    
    #units: ha * 10,000 m2/ha / (m2/person) = person    
    
    car_aca <- left_join(car_dat, spa_dat, 
            by = c("district", "year", "owner")) %>% 
        mutate(people = usage_ha * 10000 / spa_dyw)
    
    
    
    
    
