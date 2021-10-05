#-------------------------------------------------------------------
#Allocation
#
#
#
#rok/bad, October 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------
if (!exists("para")) {
#working directory
    library(here)
    setwd(paste0(here(), "/2020phase2/"))

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")
}

#export path (for future rates)
    aca_exp <- exp_path 
    


#-------------------------------------------------------------------
#import, data preparation
#-------------------------------------------------------------------
  
#allocation: data
    #WHY living space file? same data as for living space
    aca_dat <- read_csv(spa_od) %>% 
        rename(year = StichtagDatJahr, apartments = AnzWhg, people = PersInGeb) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(owner = factor(if_else(EigentumCd== 1, uni_w[1], uni_w[2]), uni_w),
            district = factor(distr, uni_d)) %>% 
        select(year, district, owner, apartments, people)
    
    
#-------------------------------------------------------------------
#allocation by year
#-------------------------------------------------------------------
      
#allocation (y)
    #WHY? e.g. to support the base year decision 
    
    aca_y <- group_by(aca_dat, year) %>% 
            summarize(apartments = sum_NA(apartments),
                people = sum_NA(people)) %>% 
        ungroup() %>% 
        mutate(aca_y = round(people / apartments, round_aca))    
    
    sszplot(aca_y,
            aes_x = "year", aes_y = "aca_y",
            labs_y = "allocation (people per apartment)",
            scale_y = c(0, NA),
            name = "1000_allocation_y",
            width = 8, height = 5)   


#-------------------------------------------------------------------
#allocation by year and owner
#-------------------------------------------------------------------
      
#allocation (yw)
    aca_yw <- group_by(aca_dat, year, owner) %>% 
            summarize(apartments = sum_NA(apartments),
                people = sum_NA(people)) %>% 
        ungroup() %>% 
      mutate(aca_yw = round(people / apartments, round_aca))  
    
    tail(aca_yw)
    
    sszplot(aca_yw,
            aes_x = "year", aes_y = "aca_yw", aes_col = "owner",
            labs_y = "allocation (people per apartment)",
            scale_y = c(0, NA),
            name = "1001_allocation_yw",
            width = 8, height = 5)   
    

#-------------------------------------------------------------------
#allocation (and apartments, people) by district, year and owner
#-------------------------------------------------------------------
      
#allocation (dyw)
    aca_dyw <- group_by(aca_dat, district, year, owner) %>% 
            summarize(
                apartments = sum_NA(apartments),
                people = sum_NA(people)) %>% 
        ungroup() %>% 
        mutate(aca_dyw = round(people / apartments, round_aca))    
      
#look at certain data points    
    
    # test_c <- filter(aca_dyw, (year == 2020) & (owner == "cooperative housing") & (apartments < 500)) %>%
    #     arrange(apartments)
    # as.data.frame(test_c)
    # 
    # test_p <- filter(aca_dyw, (year == 2020) & (owner == "private housing")) %>%
    #      arrange(apartments)
    # 
    # ew <- filter(aca_dyw, (district == "Escher Wyss") & (owner == "private housing")) %>%
    #      arrange(year)

#plot: allocation    
    sszplot(aca_dyw,
            aes_x = "year", aes_y = "aca_dyw", aes_col = "owner",
            labs_y = "allocation (people per apartment)",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA),            
            name = "1002_allocation_dyw",
            width = 12, height = 14)   
    
#plot: apartments    
    sszplot(aca_dyw,
            aes_x = "year", aes_y = "apartments", aes_col = "owner",
            labs_y = "apartments",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA),            
            name = "1003_apartments_dyw",
            width = 12, height = 14)   
   
#plot: people    
    sszplot(aca_dyw,
            aes_x = "year", aes_y = "people", aes_col = "owner",
            labs_y = "people",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA),            
            name = "1004_people_dyw",
            width = 12, height = 14)   
   
    

#-------------------------------------------------------------------
#prediction: entire city, by owner
#-------------------------------------------------------------------
        
#base years 
    aca_yw_base <- filter(aca_yw, 
            (year >= aca_base_begin) & (year <= aca_base_end)) %>% 
        select(year, owner, aca_yw)
    
#prediction (no allocation below zero)   
    aca_yw_pred <- con_reg(data = aca_yw_base, x = "year", y = "aca_yw",
              group_cols = "owner",
              window = aca_window_thres, base_t0 = aca_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end,
              prop_trend = aca_prop_trend, thres_percent = aca_thres_percent,
              lower_thres = 0)   
    
#past and prediction
    aca_yw_past_pred <- as_tibble(expand_grid(
            year = (min(aca_yw$year)):szen_end,
            owner = uni_w)) %>% 
        left_join(select(aca_yw, year, owner, aca_yw), by = c("year", "owner")) %>%     
        left_join(select(aca_yw_pred, year, owner, pred_roll),
            by = c("year", "owner")) %>% 
        mutate(aca_yw_all = if_else(year < szen_begin, aca_yw, pred_roll))    
    
#plot
    sszplot(aca_yw_past_pred,
            aes_x = "year", aes_y = "aca_yw_all", aes_col = "owner",
            labs_y = "allocation (people per apartment)",
            i_x = c(aca_base_begin, aca_base_end), 
            scale_y = c(0, NA),               
            name = "1005_allocation_prediction_yw",
            width = 10, height = 7)      
    
    
#-------------------------------------------------------------------
#prediction: by district and owner
#-------------------------------------------------------------------
        
#base years 
    aca_dyw_base <- filter(aca_dyw, 
            (year >= aca_base_begin) & (year <= aca_base_end)) %>% 
        select(district, year, owner, aca_dyw)    
    
#prediction (no allocation below zero)   
    aca_dyw_pred <- con_reg(data = aca_dyw_base, x = "year", y = "aca_dyw",
              group_cols = c("district", "owner"),
              window = aca_window_thres, base_t0 = aca_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end,
              prop_trend = aca_prop_trend, thres_percent = aca_thres_percent,
              lower_thres = 0)   
    
#past and prediction
    aca_dyw_past_pred <- as_tibble(expand_grid(
            district = uni_d,
            year = (min(aca_dyw$year)):szen_end,
            owner = uni_w)) %>% 
        left_join(aca_dyw, by = c("district", "year", "owner")) %>%     
        left_join(select(aca_dyw_pred, district, year, owner, pred_roll),
            by = c("district", "year", "owner")) %>% 
        mutate(aca_dyw_all = if_else(year < szen_begin, aca_dyw, pred_roll))      
    
#plot   
    sszplot(aca_dyw_past_pred,
            aes_x = "year", aes_y = "aca_dyw_all", aes_col = "owner",
            labs_y = "allocation (people per apartment)",            
            wrap = "district", ncol = 4,
            i_x = c(aca_base_begin, aca_base_end),           
            scale_y = c(0, NA),            
            name = "1006_allocation_precition_dyw",
            width = 12, height = 14)     
    
    
    
#-------------------------------------------------------------------
#prediction: Escher Wyss, by owner
#-------------------------------------------------------------------
   
        
#WHY a different approach for the Escher Wyss district, private housing?
    #same approach as in the living space model (see living space code)

#WHY with owner?
    #con_reg function needs groups (so far)
     
#Escher Wyss (district number: 52)
    aca_dyw_52 <- filter(aca_dyw, district == "Escher Wyss") %>% 
        select(year, owner, aca_dyw)
    
#base years 
    aca_dyw_base_52 <- filter(aca_dyw_52, 
            (year >= aca_base_begin_52p) & (year <= aca_base_end))
    
#prediction (no allocation below zero)    
    aca_dyw_pred_52 <- con_reg(data = aca_dyw_base_52, x = "year", y = "aca_dyw",
              group_cols = "owner",  
              window = aca_window_thres, base_t0 = aca_base_begin_52p,
              szen_t0 = szen_begin, szen_t1 = szen_end,
              prop_trend = aca_prop_trend_52p, thres_percent = aca_thres_percent,
              lower_thres = 0) 
    
#past and prediction
    aca_yw_past_pred_52 <- as_tibble(expand_grid(
            year = (min(aca_yw$year)):szen_end,
            owner = uni_w)) %>% 
        left_join(select(aca_dyw_52, year, owner, aca_dyw), by = c("year", "owner")) %>%     
        left_join(select(aca_dyw_pred_52, year, owner, pred_roll),
            by = c("year", "owner")) %>% 
        mutate(aca_dyw_all = if_else(year < szen_begin, aca_dyw, pred_roll), 
            district = uni_d[uni_d == "Escher Wyss"])
    
#plot
    sszplot(aca_yw_past_pred_52,
            aes_x = "year", aes_y = "aca_dyw_all", aes_col = "owner",
            labs_y = "allocation (people per apartment)",
            i_x = c(aca_base_begin_52p, aca_base_end), 
            scale_y = c(0, NA),               
            name = "1007_allocation_prediction_52yw",
            width = 10, height = 7)  
    
    
#-------------------------------------------------------------------
#combine the different data sets  
#-------------------------------------------------------------------

#district, year, owner
    aca_prep_dyw <- select(aca_dyw_past_pred, district, year, owner, aca_dyw_all) %>%  
        rename(aca_dyw = aca_dyw_all)

#year, owner
    aca_prep_yw <- select(aca_yw_past_pred, year, owner, aca_yw_all) %>%  
        rename(aca_yw = aca_yw_all)
    
#Escher Wyss (only future: values of the past already contained in 'aca_prep_dyo')
    aca_prep_52p <- select(aca_yw_past_pred_52, district, year, owner, aca_dyw_all) %>%  
        filter((owner == uni_w[2]) & (year >= szen_begin)) %>% 
        rename(aca_52p = aca_dyw_all)
    
#apartment threshold (mean over base years)
    
    #WHY with expand_grid?
    #correct calculation if no apartments (by owner) in a certain district and year
    
    aca_apart_thres <- as_tibble(expand_grid(
            district = uni_d,
            year = aca_base_begin:aca_base_end,
            owner = uni_w)) %>% 
        left_join(select(aca_dyw, district, year, owner, apartments), 
            by = c("district", "year", "owner")) %>% 
        replace_na(list(apartments = 0)) %>% 
        group_by(district, owner) %>% 
            summarize(apart_thres = mean(apartments)) %>% 
        ungroup()
        
#combine the data sets
    aca_comb <- aca_prep_dyw %>% 
        left_join(aca_prep_yw, by = c("year", "owner")) %>% 
        left_join(aca_prep_52p, by = c("district", "year", "owner")) %>% 
        left_join(aca_apart_thres, by = c("district", "owner")) %>% 
        mutate(aca = if_else(year < szen_begin, aca_dyw, 
            if_else((district == "Escher Wyss") & (owner == uni_w[2]), aca_52p, 
                if_else(apart_thres < aca_apart, aca_yw, aca_dyw))))
   
#plot: past and prediction
    sszplot(aca_comb,
            aes_x = "year", aes_y = "aca", aes_col = "owner",
            labs_y = "allocation (people per apartment)",            
            wrap = "district", ncol = 4,
            i_x = c(spa_base_begin, spa_base_end),           
            scale_y = c(0, NA),            
            name = "1008_allocation_precition_dyw_different-datasets",
            width = 12, height = 14) 
    
#plot both (i.e. per district, and entire city)
    
    names_before <- c("aca_dyw", "aca_yw", "aca")
    names_plot <- c("by district", "entire city", "prediction")
    
    aca_both <- select(aca_comb, district, year, owner, aca_dyw, aca_yw, aca) %>% 
        pivot_longer(cols = names_before, names_to = "category", values_to = "aca") %>% 
        mutate(cat = factor(case_when(category ==  names_before[1] ~ names_plot[1], 
                                            category  ==  names_before[2] ~ names_plot[2],
                                            TRUE ~ names_plot[3]), levels = names_plot))
    
#same content, different plot
    sszplot(aca_both,
            aes_x = "year", aes_y = "aca", aes_col = "cat", 
            grid = c(".", "owner"),
            labs_y = "allocation (people per apartment)",
            i_x = c(spa_base_begin, spa_base_end),           
            scale_y = c(0, NA),             
            name = "1009_allocation_dyw-yw",
            width = 8, height = 4,
            multi = uni_d)     
    
    
#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#export data   
    aca_ex_data <- mutate(aca_comb, aca_dyw = round(aca, round_aca)) %>% 
        select(district, year, owner, aca_dyw) %>%
        filter(year >= szen_begin) %>% 
        arrange(district, year, owner)
      
#export
    write_csv(aca_ex_data, paste0(aca_exp, "/allocation_future.csv"))    

    