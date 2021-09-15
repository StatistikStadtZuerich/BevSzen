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
        select(year, district, owner, area, people)
    
    
#-------------------------------------------------------------------
#living space by year
#-------------------------------------------------------------------
      
#livings space (y)
    #WHY? plots to support the base year decision
    
    spa_y <- group_by(spa_dat, year) %>% 
            summarize(area = sum_NA(area),
                people = sum_NA(people)) %>% 
        ungroup() %>% 
        mutate(spa_y = round(area / people, round_prop))    
    
    sszplot(spa_y,
            aes_x = "year", aes_y = "spa_y",
            labs_y = "living space (m² per person)",
            name = "0900_living-space_y",
            width = 8, height = 6)   


    
    
    
    

    
    
    

    

   
    
#     
# #plot
#     p900 <- ggplot(data = total) + 
#         geom_line(aes(x = year, y = usage_prop, color = owner)) + 
#         facet_wrap(~district, ncol = 4) +      
#         scale_colour_manual(values = col_w) +  
#         scale_x_continuous(breaks = pretty_breaks()) + 
#         labs(x = "", y = "usage proportion (in % of the reserves per year)", color = "") +  
#         expand_limits(y = 0) +         
#         neutral +
#         theme(panel.spacing = unit(1.5, "lines"))      
#     
#     ggsave(paste0(spa_res, "0900_living-space_by-district-year.pdf"), 
#         plot = p900, width = 12, height = 14)     
#     
#     
    
    
    
    