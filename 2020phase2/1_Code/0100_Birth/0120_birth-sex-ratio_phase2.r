#-------------------------------------------------------------------
#Birth: Sex Ratio, phase 2
#
#
#
#rok/bad, February 2021
#-------------------------------------------------------------------


if (!exists("para")) {
    #-------------------------------------------------------------------
    #paths, general
    #-------------------------------------------------------------------
    
    #working directory
    library(here)
    setwd(paste0(here(), "/2020phase2/"))
    
    #general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")
}
    
#birth: export path (for future rates)
    bir_exp <- "2_Data/4_Rates/"  


    
#-------------------------------------------------------------------
#import, proportion male
#-------------------------------------------------------------------

#proportion male
    pro_male <- read_csv(bir_od) %>% 
        rename(year = EreignisDatJahr, bir = AnzGebuWir) %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s)) %>% 
        select(year, sex, bir) %>%     
        group_by(year, sex) %>% 
            summarize(bir = sum(bir)) %>% 
        ungroup() %>% 
        spread(key = sex, value = bir) %>% 
        mutate(pro_male = round(male / (male + female) * 100, round_rate))
    
#plot   
    year5 <- pro_male$year[pro_male$year %% 5 == 0]

    sszplot(pro_male,
            aes_x = "year", aes_y = "pro_male",
            geom = c("line", "point"),
            i_x = year5, i_y = 50,
            labs_y = "proportion male in %",
            scale_y = c(0, 70), breaks = seq(0, 70, 10),
            name = "0190_sex-ratio_by-year")  
    
#-------------------------------------------------------------------
#prediction
#-------------------------------------------------------------------
   
#model
    #the sex proportion could depend on age 
    #see: https://www.pnas.org/content/112/16/E2102, Figure 4
    #however, a simple model is used: mean only
    
#mean
    pred_mean <- filter(pro_male, (year >= bir_sex_ratio_begin) & (year <= bir_sex_ratio_end)) %>% 
        summarize(pred_mean = mean(pro_male))
    
#plot with mean
    sszplot(pro_male,
            aes_x = "year", aes_y = "pro_male",
            geom = c("line", "point"),
            i_x = year5, i_y = pred_mean$pred_mean,
            labs_y = "proportion male in %",
            scale_y = c(0, 70), breaks = seq(0, 70, 10),
            name = "0191_sex-ratio_by-year_with-mean")  
    
     

#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#proportion male: prediction   
    pro_male_pred <- tibble(year = szen_begin:szen_end, 
        pro_male = round(pred_mean$pred_mean, round_rate))     
        
#export
    write_csv(pro_male_pred, paste0(bir_exp, "birth_sex-ratio_future.csv"))    


    
    
