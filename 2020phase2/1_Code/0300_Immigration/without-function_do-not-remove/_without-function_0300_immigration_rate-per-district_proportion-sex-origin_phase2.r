#-------------------------------------------------------------------
#Immigration
#
#immigration* rate (per district and year)
#proportion of sex and origin (per district and year)
#
#
#rok/bad, May 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#working directory
    setwd("O:/Projekte/BevSzen/2020_sandkasten_bad-rok/2020phase2/")

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")

#result path (for images)
    imm_res <- "3_Results/0300_Immigration/"
    
#export path (for future rates)
    imm_exp <- "2_Data/4_Rates/" 
    


#-------------------------------------------------------------------
#import and data preparation
#-------------------------------------------------------------------
    
#immigration  
    imm <- read_csv(imm_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, imm = AnzZuzuWir) %>%    
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, sex, origin, imm) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(imm = sum(imm)) %>% 
        ungroup()     
    
#relocation
    #only migration to a certain district
    #WHY? this is needed to calculate immigration* (i.e. migration to a certain district)
    #WHY rename QuarCd to dis? That the code can be applied to emigration as well
    
    rel <- read_csv(rel_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, dis = QuarCd, rel = AnzUmzuWir) %>% 
        left_join(look_dis, c("dis" = "QuarCd")) %>%     
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>%     
        select(district, year, age, sex, origin, rel) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(rel = sum(rel)) %>% 
        ungroup()         
    
#immigration* (i.e. migration to a certain district, 'immigration star' = ims)    
    ims <- bind_rows(rename(imm, ims = imm), 
        rename(rel, ims = rel)) %>% 
        group_by(district, year, age, sex, origin) %>% 
            summarize(ims = sum(ims)) %>% 
        ungroup()          
    
    
#population
    #year: begin of year population
  
    pop <- read_csv(pop_od) %>%   
        rename(age = AlterVCd, pop = AnzBestWir) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(year = StichtagDatJahr + 1,
            sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>%     
        select(district, year, age, sex, origin, pop) %>%     
        group_by(district, year, age, sex, origin) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup() 
    

    
#-------------------------------------------------------------------
#immigration* rate (per district and year)
#-------------------------------------------------------------------
   
#ims and pop: aggregate
    ims_dy <- group_by(ims, district, year) %>% 
            summarize(ims = sum(ims)) %>% 
        ungroup()          
            
    pop_dy <- group_by(pop, district, year) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup()          
                
#immigration* rate (based on all possible cases)    
    ims_rate_dy <- as_tibble(expand_grid(
        district = uni_d, 
        year = (date_start+1):date_end)) %>% 
        left_join(pop_dy, by = c("district", "year")) %>% 
        left_join(ims_dy, by = c("district", "year")) %>% 
        replace_na(list(pop = 0, ims = 0)) %>% 
        mutate(ims_rate_dy = if_else(pop == 0, NA_real_, round(ims / pop * 100, round_rate)))     

    
#years of the past (for plot)
    year_past <- (date_start+1):date_end
    year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))    
        
    
#plot
    p300 <- ggplot(data = ims_rate_dy) + 
        geom_vline(xintercept = year_past_5, color = col_grey) +      
        geom_line(aes(x = year, y = ims_rate_dy), color = col_6[1]) +  
        facet_wrap(~district, ncol = 4) +
        labs (x = "year", y = "immigration* rate (in % per year)") +  
        neutral

    ggsave(paste0(imm_res, "0300_immigration-star-rate_by-district-year.pdf"), 
        plot = p300, width = 12, height = 14)      
    
    
    
#-------------------------------------------------------------------
#prediction: future immigration* rate (per year and district)
#-------------------------------------------------------------------
       
#base years
    ims_base <- filter(ims_rate_dy, 
            (year >= ims_base_begin) & (year <= ims_base_end))
    
#prediction: constrained regression 
    ims_pred <- con_reg(data = ims_base, x = "year", y = "ims_rate_dy", 
              group_cols = "district",
              window = ims_rate_window_thres, base_t0 = ims_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end, 
              prop_trend = ims_rate_prop_trend, thres_percent = ims_rate_thres_percent, 
              lower_thres = ims_rate_lower_thres, upper_thres = NA, 
              window_thres = ims_rate_window_thres)
    
#past and prediction
    ims_past_pred <- as_tibble(expand_grid(
        district = uni_d, 
        year = (date_start+1):szen_end)) %>% 
        left_join(select(ims_rate_dy, district, year, ims_rate_dy), 
            by = c("district", "year")) %>% 
        left_join(ims_pred, by = c("district", "year")) %>% 
        mutate(rate_all = if_else(year <= ims_base_end, ims_rate_dy, pred_roll))    
    
    
#plot
    
    #levels    
        time_lev <- c("past", "future")
    
    #plot data    
        plot_dat_ims_pred <- select(ims_past_pred, district, year, rate_all) %>% 
            mutate(time = factor(if_else(year <= ims_base_end, 
                time_lev[1], time_lev[2]), levels = time_lev))          

    p301 <- ggplot(data = plot_dat_ims_pred) + 
        geom_vline(xintercept = c(ims_base_begin, ims_base_end), color = col_grey, linetype = 1) +      
        geom_line(aes(x = year, y = rate_all, linetype = time), color = col_6[1]) +  
        facet_wrap(~district, ncol = 4) +
        labs (x = "year", y = "immigration* rate (in % per year)", linetype = "") +  
        neutral

    ggsave(paste0(imm_res, "0301_immigration-star-rate_by-district-year_predicition.pdf"), 
        plot = p301, width = 12, height = 14)      
    
    
#plot (more detailed: regression and limits)
    p302 <- ggplot(data = ims_past_pred) +  
        geom_vline(xintercept = c(ims_base_begin, ims_base_end), color = col_grey, linetype = 1) +        
        geom_point(aes(x = year, y = ims_rate_dy), color = col_6[1]) +
        geom_line(aes(x = year, y = pred), linetype = 2, color = col_6[1]) +   
        geom_line(aes(x = year, y = pred_mean), col = col_6[1], linetype = 3) +            
        geom_line(aes(x = year, y = pred_roll), col = col_6[1]) +       
        facet_wrap(~district, ncol = 4) +
        labs (x = "year", y = "immigration* rate (in % per year)") +  
        neutral    

    ggsave(paste0(imm_res, "0302_immigration-star-rate_by-district-year_predicition-details.pdf"), 
        plot = p302, width = 12, height = 14)     
        

    
#-------------------------------------------------------------------
#immigration: distribution of sex and origin
#-------------------------------------------------------------------
         
#possible cases
    cas_dyso <- as_tibble(expand_grid(
        district = uni_d, 
        year = (date_start+1):date_end,
        sex = uni_s,
        origin = uni_o))
    
     
#distribution of sex and origin per year and district
    
    #comment: immigration only, not population
    #therefore, a proportion not a rate
    
    ims_dyso <- group_by(ims, district, year, sex, origin) %>% 
            summarize(ims_dyso = sum(ims)) %>% 
        ungroup() %>% 
        left_join(rename(ims_dy, ims_dy = ims), 
            by = c("district", "year")) %>% 
        right_join(cas_dyso, by = c("district", "year", "sex", "origin")) %>% 
        replace_na(list(ims_dyso = 0, ims_dy = 0)) %>% 
        mutate(ims_prop_dyso = if_else(ims_dy == 0, NA_real_, round(ims_dyso / ims_dy * 100, round_prop)))    
    
 
#plot
    p303 <- ggplot(data = ims_dyso) + 
        geom_vline(xintercept = year_past_5, color = col_grey) +          
        geom_line(aes(x = year, y = ims_prop_dyso, color = sex, linetype = origin)) +  
        facet_wrap(~district, ncol = 4) +
        scale_colour_manual(values = col_s) +       
        labs (x = "year", y = "proportion in % (per district and year, in immigration*)", color = "", linetype = "") +  
        neutral

    ggsave(paste0(imm_res, "0303_immigration_proportion-sex-origin_by-district-year.pdf"), 
        plot = p303, width = 12, height = 14)      
        

    
    
#-------------------------------------------------------------------
#distribution of sex and origin: prediction
#-------------------------------------------------------------------
       
#base years
    ims_so_base <- filter(ims_dyso, 
            (year >= ims_so_base_begin) & (year <= ims_so_base_end))
    
  
#prediction: constrained regression 
    
    #Why no upper threshold? 
    #proportions per district/year will be standardized to 100 percent anyways
    
    ims_so_pred <- con_reg(data = ims_so_base, x = "year", y = "ims_prop_dyso", 
              group_cols = c("district", "sex", "origin"),
              window = ims_so_window_thres, base_t0 = ims_so_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end, 
              prop_trend = ims_so_prop_trend, thres_percent = ims_so_thres_percent, 
              lower_thres = ims_so_lower_thres, upper_thres = NA, 
              window_thres = ims_so_window_thres)
    
#standardize proportions per district/year to 100 percent 
    ims_so_pred_stand <- group_by(ims_so_pred, district, year) %>% 
            summarize(pred_roll_sum = sum_NA(pred_roll)) %>% 
        ungroup() %>% 
        right_join(ims_so_pred, by = c("district", "year")) %>% 
        mutate(pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, round(pred_roll / pred_roll_sum * 100, round_prop)))     
   
#past and prediction
    ims_so_past_pred <- as_tibble(expand_grid(
        district = uni_d, 
        year = (date_start+1):szen_end,
        sex = uni_s,
        origin = uni_o)) %>% 
        left_join(select(ims_dyso, district, year, sex, origin, ims_prop_dyso), 
            by = c("district", "year", "sex", "origin")) %>% 
        left_join(ims_so_pred_stand, by = c("district", "year", "sex", "origin")) %>% 
        mutate(prop_all = if_else(year <= ims_so_base_end, ims_prop_dyso, pred_roll_stand))      
    
    
#plot
    p304 <- ggplot(data = ims_so_past_pred) + 
        geom_vline(xintercept = c(ims_so_base_begin, ims_so_base_end), color = col_grey, linetype = 1) +           
        geom_line(aes(x = year, y = prop_all, color = sex, linetype = origin)) +  
        facet_wrap(~district, ncol = 4) +
        scale_colour_manual(values = col_s) +       
        labs (x = "year", y = "proportion in % (per district and year, in immigration*)", color = "", linetype = "") +  
        neutral

    ggsave(paste0(imm_res, "0304_immigration_proportion-sex-origin_by-district-year_prediction.pdf"), 
        plot = p304, width = 12, height = 14)      
        
   
     
    
    
    
    
    
    
    
    
            
        
         
    