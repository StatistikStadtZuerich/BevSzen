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

#population
    
    #in contrast to rate calculations: population at the end of the year 
    #WHY: the rates will be applied here to the population of the previous year
  
    pop <- read_csv(pop_od) %>% 
        rename(year = StichtagDatJahr, age = AlterVCd, pop = AnzBestWir) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(district = factor(distr, uni_d),
               sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
               origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o)) %>% 
        select(district, year, age, sex, origin, pop) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup()   
    
#birth: fertility rate
    fer <- read_csv(paste0(exp_path, "/birth_fertility_future.csv"))
    
#birth: origin change    
    cha <- read_csv(paste0(exp_path, "/birth_origin-change_future.csv"))
        
#birth: proportion male 
    pro_male <- read_csv(paste0(exp_path, "/birth_sex-ratio_future.csv"))
       
                        
                    
                    
    
    

      
      







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
          

    
    
    
