#-------------------------------------------------------------------
#Living Space
#
#
#
#rok/bad, September 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#working directory
    library(here)
    setwd(paste0(here(), "/2020phase2/"))

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")

#result path (for images)
    spa_res <- "3_Results/0900_Living-Space/"
    
#export path (for future rates)
    spa_exp <- "2_Data/4_Rates/" 
    


#-------------------------------------------------------------------
#import, data preparation
#-------------------------------------------------------------------
  
#living space: data
    spa_dat <- read_csv(spa_od) %>% 
        rename(year = StichtagDatJahr, area = Wohnungsfl, apartments = AnzWhg, people = PersInGeb) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(owner = factor(if_else(EigentumCd== 1, uni_w[1], uni_w[2]), uni_w),
            district = factor(distr, uni_d)) %>% 
        select(year, district, owner, area, apartments, people)
    
    
#-------------------------------------------------------------------
#living space by year
#-------------------------------------------------------------------
      
#livings space (y)
    #WHY? e.g. to support the base year decision
    
    spa_y <- group_by(spa_dat, year) %>% 
            summarize(area = sum_NA(area),
                people = sum_NA(people)) %>% 
        ungroup() %>% 
        mutate(spa_y = round(area / people, round_prop))    
    
    sszplot(spa_y,
            aes_x = "year", aes_y = "spa_y",
            labs_y = "living space (m² per person)",
            scale_y = c(0, NA),
            name = "0900_living-space_y",
            width = 8, height = 5)   


#-------------------------------------------------------------------
#living space by year and owner
#-------------------------------------------------------------------
      
#livings space (yo)
    
    spa_yo <- group_by(spa_dat, year, owner) %>% 
            summarize(area = sum_NA(area),
                people = sum_NA(people)) %>% 
        ungroup() %>% 
        mutate(spa_yo = round(area / people, round_prop))    
    
    sszplot(spa_yo,
            aes_x = "year", aes_y = "spa_yo", aes_col = "owner",
            labs_y = "living space (m² per person)",
            scale_y = c(0, NA),
            name = "0901_living-space_yo",
            width = 8, height = 5)   
    

#-------------------------------------------------------------------
#living space (and area, apartments, people) by district, year and owner
#-------------------------------------------------------------------
      
#livings space (dyo)
    spa_dyo <- group_by(spa_dat, district, year, owner) %>% 
            summarize(area = sum_NA(area),
                area_ha = area/10000,
                apartments = sum_NA(apartments),
                people = sum_NA(people)) %>% 
        ungroup() %>% 
        mutate(spa_dyo = round(area / people, round_prop))    
    
#plot: living space    
    sszplot(spa_dyo,
            aes_x = "year", aes_y = "spa_dyo", aes_col = "owner",
            labs_y = "living space (m² per person)",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA),            
            name = "0902_living-space_dyo",
            width = 12, height = 14)      
    
#plot: area    
    sszplot(spa_dyo,
            aes_x = "year", aes_y = "area_ha", aes_col = "owner",
            labs_y = "living area (in ha)",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA),            
            name = "0903_area_dyo",
            width = 12, height = 14)   
    
#plot: apartments    
    sszplot(spa_dyo,
            aes_x = "year", aes_y = "apartments", aes_col = "owner",
            labs_y = "apartments",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA),            
            name = "0904_apartments_dyo",
            width = 12, height = 14)   
   
#plot: people    
    sszplot(spa_dyo,
            aes_x = "year", aes_y = "people", aes_col = "owner",
            labs_y = "people",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA),            
            name = "0905_people_dyo",
            width = 12, height = 14)   
   

    
    
    
    
    
    
    