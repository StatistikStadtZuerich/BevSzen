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
        mutate(spa_y = round(area / people, round_area))    
    
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
        mutate(spa_yo = round(area / people, round_area))  
    
    tail(spa_yo)
    
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
        mutate(spa_dyo = round(area / people, round_area)) 
    
#look at certain data points    
    
    # test_c <- filter(spa_dyo, (year == 2020) & (owner == "cooperative housing") & (apartments < 500))
    # as.data.frame(test_c)
    # 
    # test_p <- filter(spa_dyo, (year == 2020) & (owner == "private housing")) %>% 
    #      arrange(apartments)
    # 
    # ew <- filter(spa_dyo, (district == "Escher Wyss") & (owner == "private housing")) %>% 
    #      arrange(year)
    
    
    
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
   
    

#-------------------------------------------------------------------
#prediction: entire city, by owner
#-------------------------------------------------------------------
        
#base years 
    spa_yo_base <- filter(spa_yo, 
            (year >= spa_base_begin) & (year <= spa_base_end)) %>% 
        select(year, owner, spa_yo)
    
#prediction (no living space below zero)   
    spa_yo_pred <- con_reg(data = spa_yo_base, x = "year", y = "spa_yo",
              group_cols = "owner",
              window = spa_window_thres, base_t0 = spa_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end,
              prop_trend = spa_prop_trend, thres_percent = spa_thres_percent,
              lower_thres = 0)   
    
#past and prediction
    spa_yo_past_pred <- as_tibble(expand_grid(
            year = (min(spa_yo$year)):szen_end,
            owner = uni_w)) %>% 
        left_join(select(spa_yo, year, owner, spa_yo), by = c("year", "owner")) %>%     
        left_join(select(spa_yo_pred, year, owner, pred_roll),
            by = c("year", "owner")) %>% 
        mutate(spa_yo_all = if_else(year < szen_begin, spa_yo, pred_roll))    
    
#plot
    sszplot(spa_yo_past_pred,
            aes_x = "year", aes_y = "spa_yo_all", aes_col = "owner",
            labs_y = "living space (m² per person)",
            i_x = c(spa_base_begin, spa_base_end), 
            scale_y = c(0, NA),               
            name = "0906_living-space_prediction_yo",
            width = 10, height = 7)      
    
    
#-------------------------------------------------------------------
#prediction: by district and owner
#-------------------------------------------------------------------
        
#base years 
    spa_dyo_base <- filter(spa_dyo, 
            (year >= spa_base_begin) & (year <= spa_base_end)) %>% 
        select(district, year, owner, spa_dyo)    
    
#prediction (no living space below zero)   
    spa_dyo_pred <- con_reg(data = spa_dyo_base, x = "year", y = "spa_dyo",
              group_cols = c("district", "owner"),
              window = spa_window_thres, base_t0 = spa_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end,
              prop_trend = spa_prop_trend, thres_percent = spa_thres_percent,
              lower_thres = 0)   
    
#past and prediction
    spa_dyo_past_pred <- as_tibble(expand_grid(
            district = uni_d,
            year = (min(spa_dyo$year)):szen_end,
            owner = uni_w)) %>% 
        left_join(spa_dyo, by = c("district", "year", "owner")) %>%     
        left_join(select(spa_dyo_pred, district, year, owner, pred_roll),
            by = c("district", "year", "owner")) %>% 
        mutate(spa_dyo_all = if_else(year < szen_begin, spa_dyo, pred_roll))      
    
#plot   
    sszplot(spa_dyo_past_pred,
            aes_x = "year", aes_y = "spa_dyo_all", aes_col = "owner",
            labs_y = "living space (m² per person)",            
            wrap = "district", ncol = 4,
            i_x = c(spa_base_begin, spa_base_end),           
            scale_y = c(0, NA),            
            name = "0907_living-space_precition_dyo",
            width = 12, height = 14)     
    
    
    
#-------------------------------------------------------------------
#prediction: Escher Wyss, by owner
#-------------------------------------------------------------------
   
        
#WHY a diffent approach for the Escher Wyss district, private housing?
    # the living space over time differs substantially from the other districts
    # (increase until 2013, then decrease)
    # reason: few apartments in 2008 then very intense construction activity
    # numbers: less than 1500 apartments in 2008
    # more than 3100 apartments in 2017
    # after construction some apartments were finished, yet no one lived there yet
    # for a certain moment the empty apartments increase the living space (based on buildings)
    # due to the high ratio of new buildings (compared to existing buildings) the living space was increased
    # this will not happen again in future (due to the higher amount of buildings)
    
#WHY with owner?
    #con_reg function needs groups (so far)
     
#Escher Wyss (district number: 52)
    spa_dyo_52 <- filter(spa_dyo, district == "Escher Wyss") %>% 
        select(year, owner, spa_dyo)
    
#base years 
    spa_dyo_base_52 <- filter(spa_dyo_52, 
            (year >= spa_base_begin_52p) & (year <= spa_base_end))
    
#prediction (no living space below zero)   
    spa_dyo_pred_52 <- con_reg(data = spa_dyo_base_52, x = "year", y = "spa_dyo",
              group_cols = "owner",  
              window = spa_window_thres, base_t0 = spa_base_begin_52p,
              szen_t0 = szen_begin, szen_t1 = szen_end,
              prop_trend = spa_prop_trend_52p, thres_percent = spa_thres_percent,
              lower_thres = 0) 
    
#past and prediction
    spa_yo_past_pred_52 <- as_tibble(expand_grid(
            year = (min(spa_yo$year)):szen_end,
            owner = uni_w)) %>% 
        left_join(select(spa_dyo_52, year, owner, spa_dyo), by = c("year", "owner")) %>%     
        left_join(select(spa_dyo_pred_52, year, owner, pred_roll),
            by = c("year", "owner")) %>% 
        mutate(spa_dyo_all = if_else(year < szen_begin, spa_dyo, pred_roll), 
            district = uni_d[uni_d == "Escher Wyss"])
    
#plot
    sszplot(spa_yo_past_pred_52,
            aes_x = "year", aes_y = "spa_dyo_all", aes_col = "owner",
            labs_y = "living space (m² per person)",
            i_x = c(spa_base_begin_52p, spa_base_end), 
            scale_y = c(0, NA),               
            name = "0908_living-space_prediction_52yo",
            width = 10, height = 7)  
    
    
#-------------------------------------------------------------------
#combine the different data sets
#-------------------------------------------------------------------

#district, year, owner
    spa_prep_dyo <- select(spa_dyo_past_pred, district, year, owner, spa_dyo_all) %>%  
        rename(spa_dyo = spa_dyo_all)

#year, owner
    spa_prep_yo <- select(spa_yo_past_pred, year, owner, spa_yo_all) %>%  
        rename(spa_yo = spa_yo_all)
    
#Escher Wyss (only future: values of the past already contained in 'spa_prep_dyo')
    spa_prep_52p <- select(spa_yo_past_pred_52, district, year, owner, spa_dyo_all) %>%  
        filter((owner == uni_w[2]) & (year >= szen_begin)) %>% 
        rename(spa_52p = spa_dyo_all)
    
    
#apartment threshold (mean over base years)
    
    #WHY with expand_grid?
    #correct calculation if no apartments (by owner) in a certain district and year
    
    spa_apart_thres <- as_tibble(expand_grid(
            district = uni_d,
            year = spa_base_begin:spa_base_end,
            owner = uni_w)) %>% 
        left_join(select(spa_dyo, district, year, owner, apartments), 
            by = c("district", "year", "owner")) %>% 
        replace_na(list(apartments = 0)) %>% 
        group_by(district, owner) %>% 
            summarize(apart_thres = mean(apartments)) %>% 
        ungroup()
        
#combine the data sets
    spa_comb <- spa_prep_dyo %>% 
        left_join(spa_prep_yo, by = c("year", "owner")) %>% 
        left_join(spa_prep_52p, by = c("district", "year", "owner")) %>% 
        left_join(spa_apart_thres, by = c("district", "owner")) %>% 
        mutate(spa = if_else(year < szen_begin, spa_dyo, 
            if_else((district == "Escher Wyss") & (owner == uni_w[2]), spa_52p, 
                if_else(apart_thres < spa_apart, spa_yo, spa_dyo))))
   
#plot: past and prediction
    sszplot(spa_comb,
            aes_x = "year", aes_y = "spa", aes_col = "owner",
            labs_y = "living space (m² per person)",            
            wrap = "district", ncol = 4,
            i_x = c(spa_base_begin, spa_base_end),           
            scale_y = c(0, NA),            
            name = "0909_living-space_precition_dyo_different-datasets",
            width = 12, height = 14) 
    
#plot both (i.e. per district, and entire city)
    
    names_before <- c("spa_dyo", "spa_yo", "spa")
    names_plot <- c("by district", "entire city", "prediction")
    
    spa_both <- select(spa_comb, district, year, owner, spa_dyo, spa_yo, spa) %>% 
        pivot_longer(cols = names_before, names_to = "category", values_to = "spa") %>% 
        mutate(cat = factor(case_when(category ==  names_before[1] ~ names_plot[1], 
                                            category  ==  names_before[2] ~ names_plot[2],
                                            TRUE ~ names_plot[3]), levels = names_plot))
    
#same content, different plot
    sszplot(spa_both,
            aes_x = "year", aes_y = "spa", aes_col = "cat", 
            grid = c(".", "owner"),
            labs_y = "living space (m² per person)",
            i_x = c(spa_base_begin, spa_base_end),           
            scale_y = c(0, NA),             
            name = "0910_living-space_dyo-yo",
            width = 8, height = 4,
            multi = uni_d)     
    
    
#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#export data   
    spa_ex_data <- mutate(spa_comb, spa_dya = round(spa, round_area)) %>% 
        select(district, year, owner, spa_dya) %>%
        filter(year >= szen_begin) %>% 
        arrange(district, year, owner)
      
#export
    write_csv(spa_ex_data, paste0(spa_exp, "living-space_future.csv"))    

    