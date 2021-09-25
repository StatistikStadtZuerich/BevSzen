#-------------------------------------------------------------------
#Birth: Fertility Rate, phase 2
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
  source(paste0(code_path, "/0000_General/0000_general_phase2.r"))
}
    
#birth: export path (for future fertility rates)
    bir_exp <- exp_path  


    
#-------------------------------------------------------------------
#import and data preparation
#-------------------------------------------------------------------

#birth
    #age: only births of women at 'fertile age'
    #origin: here origin of mother
    
    bir <- read_csv(bir_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) %>% 
        filter((age >= bir_age_begin) & (age <= bir_age_end)) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(origin = factor(if_else(HerkunftMutterCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, origin, bir) %>%     
        group_by(district, year, age, origin) %>% 
            summarize(bir = sum(bir), 
                      .groups = "drop") 
  
    
#population
    #year: begin of year population
    #age: only women at 'fertile age'
  
    pop <- read_csv(pop_od) %>%   
        rename(age = AlterVCd, pop = AnzBestWir) %>%   
        filter((SexCd == 2) & (age >= bir_age_begin) & (age <= bir_age_end)) %>%       
        left_join(look_dis, by = "QuarCd") %>%       
        mutate(year = StichtagDatJahr + 1,
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>%   
        select(district, year, age, origin, pop) %>%    
        group_by(district, year, age, origin) %>% 
            summarize(pop = sum(pop),
                      .groups = "drop")
    

#-------------------------------------------------------------------
#fertility
#-------------------------------------------------------------------
          
#values for all possible cases 
    
    #WHY with age categories? to calculate TFR by age category    

    cas <- as_tibble(expand_grid(district = uni_d,
        year = (date_start+1):date_end,
        age = bir_age_begin:bir_age_end,
        origin = uni_o)) %>% 
        left_join(pop, by = c("district", "year", "age", "origin")) %>% 
        left_join(bir, by = c("district", "year", "age", "origin")) %>% 
        replace_na(list(pop = 0, bir = 0)) %>% 
        left_join(look_a1, by = "age") %>% 
        left_join(look_a2, by = "age")    
    
#fertility by year, age
    fer_ya <- group_by(cas, age, year) %>%
      summarize(pop = sum(pop),
                bir = sum(bir),
                .groups = "drop") %>%
      mutate(fer_ya = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>%
      select(age, year, fer_ya)      
    
#fertility by year, age, origin
    fer_yao <- group_by(cas, year, age, origin) %>%
      summarize(pop = sum(pop),
                bir = sum(bir),
                .groups = "drop") %>%
      mutate(fer_yao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>%
      select(year, age, origin, fer_yao)    
    
#fertility by district, year, age, origin
    #is already aggregated by district, year, age, origin
    fer_dyao <- mutate(cas, fer_dyao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate)))      

    
#-------------------------------------------------------------------
#TFR (total fertility rate) 
#-------------------------------------------------------------------
        
#WHY? plots to define the base period for fertility prediction    
    
#TFR by year
    tfr_y <- group_by(fer_ya, year) %>%
      summarize(tfr_y = sum_NA(fer_ya / 100),
                .groups = "drop")  
    
    sszplot(tfr_y, aes_x = "year", aes_y = "tfr_y",
            labs_y = "TFR", i_x = "5",
            geom = c("line", "point"),
            name = "0100_TFR_by-year")
    
    
#TFR by year, origin
    tfr_yo <- group_by(fer_yao, year, origin) %>%
      summarize(tfr_yo = sum_NA(fer_yao / 100),
                .groups = "drop")   
    
    sszplot(tfr_yo, aes_x = "year", aes_y = "tfr_yo", aes_col = "origin",
            labs_y = "TFR", i_x = "5",
            geom = c("line", "point"),
            name = "0101_TFR_by-year-origin")        
    
#TFR by year, age1
    tfr_ya1 <- left_join(fer_ya, look_a1, by = "age") %>%
      group_by(year, age_1) %>%
      summarize(tfr_ya1 = sum_NA(fer_ya / 100),
                .groups = "drop")
    
    sszplot(tfr_ya1, aes_x = "year", aes_y = "tfr_ya1", aes_col = "age_1",
            i_x = "5", labs_y = "TFR",
            geom = c("line", "point"),
            name = "0102_TFR_by-year-age1")  
    
    
#TFR by year, age2   
    tfr_ya2 <- left_join(fer_ya, look_a2, by = "age") %>%
      group_by(year, age_2) %>%
      summarize(tfr_ya2 = sum_NA(fer_ya / 100),
                .groups = "drop")
    
    sszplot(tfr_ya2, aes_x = "year", aes_y = "tfr_ya2", aes_col = "age_2",
            i_x = "5", labs_y = "TFR",
            geom = c("line", "point"),
            name = "0103_TFR_by-year-age2")      
    
    
#TFR by year, age1, origin  
    tfr_ya1o <- left_join(fer_yao, look_a1, by = "age") %>%
      group_by(year, age_1, origin) %>%
      summarize(tfr_ya1o = sum_NA(fer_yao / 100),
                .groups = "drop")
    
    sszplot(tfr_ya1o, aes_x = "year", aes_y = "tfr_ya1o", aes_col = "origin",
            grid = c("age_1", "."), labs_y = "TFR", i_x = "5",
            geom = c("line", "point"),
            name = "0104_TFR_by-year-age1-origin",
            width = 12) 
    
#TFR by year, age2, origin  
    tfr_ya2o <- left_join(fer_yao, look_a2, by = "age") %>%
      group_by(year, age_2, origin) %>%
      summarize(tfr_ya2o = sum_NA(fer_yao / 100),
                .groups = "drop")
        
    sszplot(tfr_ya2o, aes_x = "year", aes_y = "tfr_ya2o", aes_col = "origin",
            wrap = "age_2", ncol = nlevels(tfr_ya2o$age_2),
            labs_y = "TFR", i_x = "5",
            geom = c("line", "point"),
            name = "0105_TFR-by-year-age2",
            width = 16, height = 5) 

#-------------------------------------------------------------------
#fertility plots (before any corrections)
#-------------------------------------------------------------------

#plot: fertility by district, year, age, origin
    sszplot(filter(fer_dyao, year >= bir_base_begin),
            aes_x = "age", aes_y = "fer_dyao", aes_col = "origin",
            wrap = "as.factor(year)", labs_y = "fertility rate (in % per year)",
            name = "0110_fertility_by-district-year-age-origin",
            width = 9, height = 6,
            multi = uni_d) 
       
#plot: fertility by year, age, origin
    sszplot(filter(fer_yao, year >= bir_base_begin),
            aes_x = "age", aes_y = "fer_yao", aes_col = "origin",
            wrap = "as.factor(year)", labs_y = "fertility rate (in % per year)",
            name = "0111_fertility_by-year-age-origin")
    
#plot: fertility by year, age
    sszplot(filter(fer_ya, year >= bir_base_begin),
            aes_x = "age", aes_y = "fer_ya", wrap = "as.factor(year)",
            labs_y = "fertility rate (in % per year)",
            name = "0112_fertility_by-year-age") 
    
    
#-------------------------------------------------------------------
#fertility: replace values in tails, base period only
#-------------------------------------------------------------------
                     
#cumulative sums vs. thresholds
    #lowcum: cumulative sum from the lower tail of the age distribution
    #upcum: cumulative sum from the supper tail of the age distribution
    
    fer_tail <- filter(fer_dyao, year >= bir_base_begin) %>% 
        left_join(fer_yao, by = c("year", "age", "origin")) %>% 
        left_join(fer_ya, by = c("year", "age")) %>% 
        arrange(district, year, origin, age) %>% 
        group_by(district, year, origin) %>% 
            mutate(low_cum = cumsum(pop),
                   up_cum = rev(cumsum(rev(pop)))) %>% 
        ungroup() %>% 
        mutate(min_cum = pmin(low_cum, up_cum),
               fer = if_else(min_cum < bir_thres_const, bir_thres_value, 
                          if_else(min_cum < bir_thres_overall, fer_ya, 
                              if_else(min_cum < bir_thres_origin, fer_yao, fer_dyao))))

#corrected fertility: plot preparation
    cor_level <- c("initial", "corrected")    
    
    fer_cor <- select(fer_tail, district, year, origin, age, fer_dyao, fer) %>% 
        gather(`fer_dyao`, `fer`, key = category, value = fer) %>% 
        mutate(cat = factor(if_else(category == "fer_dyao", 
            cor_level[1], cor_level[2]), levels = cor_level)) %>% 
        select(district, year, origin, age, cat, fer)
    
#plot: initial vs. corrected fertility
    sszplot(fer_cor,
            aes_x = "age", aes_y = "fer", aes_col = "origin", aes_ltyp = "cat",
            wrap = "as.factor(year)",
            labs_y = "fertility rate (in % per year)",
            name = "0120_fertility_tail-correction",
            width = 13, height = 8,
            multi = uni_d) 
        
#plot: cumulative population from the tails
    sszplot(fer_tail,
            aes_x = "age", aes_y = "low_cum", aes_col = "origin",
            wrap = "as.factor(year)",
            i_y = c(bir_thres_origin, bir_thres_overall, bir_thres_const),
            labs_y = "cumulative population from tails",
            multi = uni_d,
            name = "0121_cumulative-population-from_tails",
            width = 13, height = 8,
            quotes = quote(geom_line(aes(x = age, y = up_cum, color = origin))))
    
#-------------------------------------------------------------------
#fit gam to corrected fertility rate
#-------------------------------------------------------------------
     
#with gam   
    
    # smoothing parameter see: https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/smooth.terms.html    
        
    # bs="tp". These are low rank isotropic smoothers of any number of covariates. 
    # By isotropic is meant that rotation of the covariate coordinate system will not change the result of smoothing. 
    # By low rank is meant that they have far fewer coefficients than there are data to smooth. 
    # They are reduced rank versions of the thin plate splines and use the thin plate spline penalty. 
    # They are the default smooth for s terms because there is a defined sense in which they are 
    # the optimal smoother of any given basis dimension/rank (Wood, 2003). 
       
    fer_fit <- arrange(fer_tail, district, year, origin, age) %>% 
        group_by(district, year, origin) %>% 
            mutate(fer_fit = pmax(0, gam(fer ~ s(age, bs = "tp"))$fitted.values)) %>% 
        ungroup()
    
#plot preparation
    fit_lev <- c("corrected", "with gam")
    
    fit_dat <- select(fer_fit, district, year, origin, age, fer, fer_fit) %>% 
        gather(`fer`, `fer_fit`, key = category, value = fer) %>% 
        mutate(cat = factor(if_else(category == "fer", 
            fit_lev[1], fit_lev[2]), levels = fit_lev)) %>% 
        select(district, year, age, origin, cat, fer)  
    
# #plot: 
    sszplot(fit_dat, 
            aes_x = "age", aes_y = "fer", aes_col = "origin", aes_ltyp = "cat",
            wrap = "as.factor(year)",
            labs_y = "fertility rate (in % per year)",
            name = "0130_fertility_fit",
            width = 13, height = 8,
            multi = uni_d)  
    
#-------------------------------------------------------------------
#prediction
#-------------------------------------------------------------------

#constrained regression 
    #(proportion of linear model and mean, within bandwidth)

    fer_pred <- con_reg(data = fer_fit, x = "year", y = "fer_fit", 
              group_cols = c("district", "age", "origin"),
              window = bir_window_thres, base_t0 = bir_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end, 
              prop_trend = bir_prop_trend, thres_percent = bir_thres_percent, 
              lower_thres = bir_lower_thres, upper_thres = bir_upper_thres) %>% 
        #with the input data (WHY? to assess the regression)
            left_join(select(fer_fit, district, year, age, origin, fer_fit), 
                by = c("district", "year", "age", "origin")) %>%       
        #output generation, one rate variable for both future and past
            left_join(select(fer_dyao, district, year, age, origin, fer_dyao), 
                by = c("district", "year", "age", "origin")) %>% 
            mutate(fer_all = if_else(year <= bir_base_end, fer_dyao, pred_roll)) 
        

#-------------------------------------------------------------------
#plot the predictions: age distribution by district and year
#-------------------------------------------------------------------
  
#plot
    sszplot(fer_pred, 
            aes_x = "age", aes_y = "fer_all", aes_col = "year",
            wrap = "district",
            labs_y = "fertility rate (in % per year)",
            scale_y = c(0, bir_plot_lim),
            name = "0140_fertility-prediction_by-district",
            width = 12, height = 14,
            multi = uni_o)  
        
#-------------------------------------------------------------------
#plot the precitions: along year, for selected age
#-------------------------------------------------------------------
     
#selected age
    uni_age <- sort(unique(fer_pred$age))
    sel_age <- uni_age[(uni_age %% 5) == 0]
    
#plot: Swiss
    age_dat <- filter(fer_pred, (origin == uni_o[1]) & (age %in% sel_age))    
    sszplot(age_dat,
            aes_x = "year", aes_y = "fer_fit",
            labs_y = "fertility rate (in % per year)",
            wrap = "age", ncol = 4,
            scale_y = c(0, bir_plot_lim),
            title = paste0("as.character(paste0('", levels(uni_o)[uni_o == uni_o[1]], ": ', x))"),
            geom = "point",
            name = "0141_fertility-prediction_by-district_along-year_Swiss",
            width = 11, height = 5,
            multi = uni_d,
            quotes = c(quote(geom_line(aes(x = year, y = pred), linetype = 2)),
                       quote(geom_line(aes(x = year, y = pred_mean), linetype = 3)),
                       quote(geom_line(aes(x = year, y = pred_roll), linetype = 1))))
    
  #plot: foreign
    age_dat <- filter(fer_pred, (origin == uni_o[2]) & (age %in% sel_age))    
    sszplot(age_dat,
            aes_x = "year", aes_y = "fer_fit",
            labs_y = "fertility rate (in % per year)",
            wrap = "age", ncol = 4,
            scale_y = c(0, bir_plot_lim),
            title = paste0("as.character(paste0('", levels(uni_o)[uni_o == uni_o[2]], ": ', x))"),
            geom = "point",
            name = "0142_fertility-prediction_by-district_along-year_foreign",
            width = 11, height = 5,
            multi = uni_d,
            quotes = c(quote(geom_line(aes(x = year, y = pred), linetype = 2)),
                       quote(geom_line(aes(x = year, y = pred_mean), linetype = 3)),
                       quote(geom_line(aes(x = year, y = pred_roll), linetype = 1))))     

#-------------------------------------------------------------------
#fit gam to future fertility rates
#-------------------------------------------------------------------
    
#with gam (only for prediction years)    
    pred_fit <- filter(fer_pred, year >= szen_begin) %>% 
        arrange(district, year, origin, age) %>%        
        group_by(district, year, origin) %>%    
            mutate(pred_fit = pmax(0, gam(pred_roll ~ s(age, bs = "tp"))$fitted.values)) %>% 
        ungroup()
    
#plot prediction for selected years
    sel_years <- uniy_szen[(uniy_szen %% 10) == 0]
    
    sel_lev <- c("initial", "with gam")

    sel_dat <- gather(pred_fit, `pred_roll`, `pred_fit`, key = category, value = fer) %>% 
        mutate(cat = factor(if_else(category == "pred_roll", 
            sel_lev[1], sel_lev[2]), levels = sel_lev)) %>% 
        filter(year %in% sel_years)      
  
#plot
    sszplot(sel_dat,
            aes_x = "age", aes_y = "fer", aes_col = "cat",
            grid = c("origin", "year"),
            labs_y = "fertility rate (in % per year)",
            name = "0150_fertility-prediction_before-after-fit",
            width = 10, height = 6,
            multi = uni_d) 
       
#-------------------------------------------------------------------
#export fertility rates
#-------------------------------------------------------------------

#prepare the export data
    fer_ex <- mutate(pred_fit, fer = round(pred_fit, round_rate)) %>% 
        filter(year >= szen_begin) %>%       
        select(district, year, age, origin, fer) %>% 
        arrange(district, year, age, origin)

#export
    write_csv(fer_ex, paste0(bir_exp, "/birth_fertility_future.csv"))
   
    
#-------------------------------------------------------------------
#TFR (total fertility rate) 
#-------------------------------------------------------------------
       
#fertility rate of past and future
    fer_ex_past <- rename(fer_dyao, fer = fer_dyao) %>% 
        select(district, year, age, origin, fer) %>% 
        arrange(district, year, age, origin)

#export the data of the past (for model evaluation later on)    
    write_csv(fer_ex_past, paste0(bir_exp, "/birth_fertility_past.csv"))

#TFR
    tfr_dyo <- bind_rows(fer_ex_past, fer_ex) %>%
      group_by(district, year, origin) %>%
      summarize(tfr = sum_NA(fer / 100),
                .groups = "drop")
    
#plot
    sszplot(tfr_dyo,
            aes_x = "year", aes_y = "tfr", aes_col = "origin",
            i_x = c(bir_base_begin, szen_begin),
            wrap = "district", ncol = 4,
            name = "0160_TFR_by-district-origin",
            width = 12, height = 14)   
   
#-------------------------------------------------------------------
#TFR by age class 
#-------------------------------------------------------------------
      
#TFR by age class
    tfr_a1 <- bind_rows(fer_ex_past, fer_ex) %>% 
        left_join(look_a1, by = "age") %>%      
        group_by(district, year, origin, age_1) %>% 
            summarize(tfr = sum_NA(fer / 100), 
        .groups = "drop")
    
#plot
    sszplot(tfr_a1,
            aes_x = "year", aes_y = "tfr", aes_col = "age_1",
            i_x = c(bir_base_begin, szen_begin),
            labs_col = "age",
            wrap = "district", ncol = 4,
            name = "0161_TFR_by-district-origin-age1",
            width = 12, height = 14,
            multi = uni_o)      
     
#-------------------------------------------------------------------
#TFR by age class (more detailled)
#-------------------------------------------------------------------
      
#TFR by age class
    tfr_a2 <- bind_rows(fer_ex_past, fer_ex) %>% 
        left_join(look_a2, by = "age") %>%        
        group_by(district, year, origin, age_2) %>% 
            summarize(tfr = sum_NA(fer / 100), 
        .groups = "drop")
    
#plot
    sszplot(tfr_a2,
            aes_x = "year", aes_y = "tfr", aes_col = "age_2",
            i_x = c(bir_base_begin, szen_begin),
            labs_col = "age",
            wrap = "district", ncol = 4,
            name = "0162_TFR_by-district-origin-age2",
            width = 12, height = 14,
            multi = uni_o)   
    