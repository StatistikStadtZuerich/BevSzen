#-------------------------------------------------------------------
#Death: Mortality Rate, phase 2
#
#
#
#rok/bad, April 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#general functions already available?
    if (!exists("para")) {
      
        #working directory
            library(here)
            setwd(paste0(here(), "/2020phase2/"))
        
        #general functions (without dependence on parameters)
            source("1_Code/0000_General/0000_general_without-parameters.r")
            
        #parameters (depend on scenario)
            i_scen <- "middle"
            for(i_para in 1:nrow(para)){
                assign(para$parameter[i_para], para[[i_scen]][i_para],
                       envir = .GlobalEnv)}

        #general functions (with dependence on parameters)
            source(paste0(code_path, "/0000_General/0001_general_with-parameters.r"))
    
    }

#start time
    t0 <- Sys.time() 


#-------------------------------------------------------------------
#import and data preparation
#-------------------------------------------------------------------

#death
    dea <- read_csv(dea_od) %>%
        rename(year = EreignisDatJahr, age = AlterVCd, dea = AnzSterWir) %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s)) %>%    
        group_by(year, age, sex) %>% 
            summarize(dea = sum(dea),
                      .groups = "drop")
    
#birth (needed to calculate life expectancy)  
    bir <- read_csv(bir_od) %>% 
        rename(year = EreignisDatJahr, bir = AnzGebuWir) %>% 
        mutate(age = 0,
            sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s)) %>% 
        group_by(year, age, sex) %>% 
            summarize(B = sum(bir),
                      .groups = "drop")
  
#population
    #year: begin of year population
  
    pop <- read_csv(pop_od) %>%   
        rename(age = AlterVCd, pop = AnzBestWir) %>%   
        mutate(year = StichtagDatJahr + 1,
            sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s)) %>%
        group_by(year, age, sex) %>% 
            summarize(pop = sum(pop),
                      .groups = "drop")
    
#FSO data (used in the prediction)
    
    #rate is converted to percent

    mor_fso <- read_csv(dea_fso_od) %>%
        rename(year = EreignisDatJahr, age = AlterVCd) %>%
        filter(HerkunftCd == 0) %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
            region = factor(text_r[2], uni_r),
            mor_yas = RateSterSta * 100) %>% 
        select(year, age, sex, region, KategorieCd, mor_yas)

    #fso: maximum age in the data of the past?
    # temp <- filter(mor_fso, (year %in% dea_fso_date_start:dea_fso_date_end) & (KategorieCd == 2))
    # max(temp$age)
    
    
#-------------------------------------------------------------------
#mortality
#-------------------------------------------------------------------
    
#Zurich (calculation based on all possible cases)
    mor_zh_yas <- as_tibble(expand_grid(
        year = (date_start+1):date_end,
        age = dea_age_min:dea_age_max,
        sex = uni_s,
        region = factor(text_r[1], uni_r))) %>%
        left_join(pop, by = c("year", "age", "sex")) %>%
        left_join(dea, by = c("year", "age", "sex")) %>%
        replace_na(list(pop = 0, dea = 0)) %>%
        mutate(mor_yas = if_else(pop == 0, NA_real_, round(dea / pop * 100, round_rate)))
    
#FSO, past (smoothed rates)
    #category: (smoothed) data of the past
    mor_fso_yas_past <- filter(mor_fso, KategorieCd == dea_fso_cat_past) %>%
        select(year, age, sex, region, mor_yas)

#FSO, future
    #category: data of the future
    #WHY filter on year? there are also predictions for years in the past

    mor_fso_yas_future <- filter(mor_fso, (KategorieCd == dea_fso_cat_future) & (year >= szen_begin) & (year <= szen_end)) %>%
        select(year, age, sex, region, mor_yas)

#FSO, past and future
    mor_fso_yas <- bind_rows(mor_fso_yas_past, mor_fso_yas_future)

#with region (Zurich and FSO)
    mor_yasr_zh_fso <- select(mor_zh_yas, year, age, sex, region, mor_yas) %>%
        bind_rows(mor_fso_yas) %>% 
        rename(mor_yasr = mor_yas)
    
#with all possible cases
    mor_yasr <- as_tibble(expand_grid(
        year = (date_start+1):szen_end,
        age = dea_age_min:dea_age_max,
        sex = uni_s,
        region = uni_r)) %>%
        left_join(mor_yasr_zh_fso, by = c("year", "age", "sex", "region"))


#-------------------------------------------------------------------
#mortality: age plots
#-------------------------------------------------------------------

#years (past only)
    year_past <- (date_start+1):date_end
    year_5 <- sort(unique(year_past[year_past %% 5 == 0]))

#focus: difference in mortality by sex
    sszplot(filter(mor_yasr, year %in% year_5),
            aes_x = "age", aes_y = "mor_yasr", aes_col = "sex",
            labs_y = "mortality rate (in % per year)",
            scale_y = "log",
            grid = c("region", "year"),
            name = "0200_mortality_by-year-age-sex-region_focus-age-sex",
            width = 12, height = 7)

#focus: difference in mortality by region
    sszplot(filter(mor_yasr, year %in% year_5),
            aes_x = "age", aes_y = "mor_yasr", aes_col = "region",
            labs_y = "mortality rate (in % per year)",
            scale_y = "log",
            grid = c("sex", "year"),
            name = "0201_mortality_by-year-age-sex-region_focus-age-region",
            width = 12, height = 7)

#-------------------------------------------------------------------
#mortality: year plots
#-------------------------------------------------------------------

#age
    age_select <- c(0, 20, 40, 50, 60, 70, 80)

#focus: difference in mortality by sex
    sszplot(filter(mor_yasr, age %in% age_select),
            aes_x = "year", aes_y = "mor_yasr", aes_col = "sex",
            labs_y = "mortality rate (in % per year)",
            scale_y = "log",
            grid = c("region", "age"),
            name = "0202_mortality_by-year-age-sex-region_focus-year-sex",
            width = 14, height = 7, 
            angle = 90)
    

#focus: difference in mortality by region
    sszplot(filter(mor_yasr, age %in% age_select),
            aes_x = "year", aes_y = "mor_yasr", aes_col = "region",
            labs_y = "mortality rate (in % per year)",
            scale_y = "log",
            grid = c("sex", "age"),
            name = "0203_mortality_by-year-age-sex-region_focus-year-region",
            width = 14, height = 7, 
            angle = 90)
    
#-------------------------------------------------------------------
#life expectancy: based on deaths, births, population
#-------------------------------------------------------------------

#no function, since only used once
    
#age capped
    pop_capped <- mutate(pop, age_capped = if_else(age >= dea_age_max_le, dea_age_max_le, age)) %>%
        group_by(year, age_capped, sex) %>%
            summarize(pop = sum(pop),
                      .groups = "drop") %>%
        rename(age = age_capped)
    
    dea_capped <- mutate(dea, age_capped = if_else(age >= dea_age_max_le, dea_age_max_le, age)) %>%
        group_by(year, age_capped, sex) %>%
            summarize(dx = sum(dea),
                      .groups = "drop") %>%
        rename(age = age_capped)

#population at the end of the year
    pop_end_year <- mutate(pop_capped, end_of_year = year - 1) %>%
        select(end_of_year, age, sex, pop) %>%
        rename(year = end_of_year, pop_end = pop)

#mean population per year
    pop_mean <- as_tibble(expand_grid(
        year = (date_start+1):date_end,
        age = dea_age_min:dea_age_max_le,
        sex = uni_s)) %>%
        left_join(pop_capped, by = c("year", "age", "sex")) %>%
        left_join(pop_end_year, by = c("year", "age", "sex")) %>%
        replace_na(list(pop = 0, pop_end = 0)) %>%
        mutate(Px = (pop + pop_end)/2) %>%
        left_join(dea_capped, by = c("year", "age", "sex")) %>%
        left_join(bir, by = c("year", "age", "sex")) %>%
        replace_na(list(dx = 0, B = 0))

#life expectancy

    #life expectancy at certain age (e.g. at birth)
        age_at <- 0

    #radix (since not relevant: not a model parameter)
        radix <- 100000

    le <- mutate(pop_mean,
        #mx: age-specific mortality rate, Yusuf et al. (2014), eq 7.1
        mx = if_else(Px > 0, dx / Px, NA_real_),
        #qx1: probability to die between age x and age x+1, Yusuf et al. (2014), eq 7.4, 7.5, 7.6
        #the last two age-values: qx should bei 1
        #why? otherwise after the lag, some people 'survive' the last age        
        qx1 = pmin(1, if_else(age == 0, if_else(B > 0, dx / B, NA_real_),
                if_else((age > 0) & (age < (dea_age_max_le-1)), 2 * mx / (2 + mx), 1))),
        #if there is no one at a certain age in the population (e.g. no 96 year old men), 
        #then qx1 is NA. However, a value is need to multiply the subsequent survival probabilities        
        qx = if_else(is.na(qx1), dea_qx_NA_le, qx1),
        #survival
        px_ = 1 - qx) %>%
        #cumulative product
        arrange(year, sex, age) %>%
        group_by(year, sex) %>%
            mutate(mult = cumprod(px_),
                multlag = lag(mult, default = 1)) %>%
        ungroup() %>%
        mutate(radix = radix,
            lx = radix * multlag,
            #expected deaths dx_, Yusuf et al. (2014), eq 7.8
            dx_ = lx * qx,
            #number of survivors lxp1, Yusuf et al. (2014), eq 7.9
            lxp1 = lx - dx_,
            #person-years lived, Yusuf et al. (2014), eq 7.12, 7.14, 7.15
            Lx_ = if_else(age == 0, 0.3 * lx + 0.7 * lxp1,
                if_else((age > 0) & (age < dea_age_max_le), 0.5 * lx + 0.5 * lxp1, dx / mx))) %>%
        #life expectancy at certain age (e.g. birth)
        group_by(year, sex) %>%
            summarize(life_years = sum(Lx_[age >= age_at], na.rm = TRUE),
                start_pop = min(lx[age == age_at], na.rm = TRUE),
                .groups = "drop") %>%
        mutate(life = age_at + life_years / start_pop) %>%
        select(year, sex, life)
    
    # temp <- filter(le, (year == 2018) & (sex == "female"))
    # plot(temp$age, temp$px_, type = "o")
    # plot(temp$age, temp$mult)    
    
    
#-------------------------------------------------------------------
#life expectancy: based on mortality rate
#-------------------------------------------------------------------

#life expectancy    
    le_ysr <- life_exp(data = mor_yasr, mor = "mor_yasr", 
        age = "age", group_cols = c("year", "sex", "region"), 
        age_max = dea_age_max_le, qx_NA = dea_qx_NA_le, 
        age_at = 0, radix = 100000) %>% 
        #'manual' correction
        #Zurich: missing values in the future
        #Switzerland: latest values not available yet
        mutate(le_ysr = if_else((region == "Zurich") & (year > date_end), NA_real_, 
            if_else((region == "Switzerland") & !(year %in% 
                c(dea_fso_date_start:dea_fso_date_end, szen_begin:szen_end)), NA_real_, life_exp))) %>% 
        select(-life_exp)

    # temp <- filter(le_ysr, (year == 2018) & (sex == "male") & (region == "Zurich"))
    # plot(temp$age, temp$px_, type = "o")
    # plot(temp$age, temp$mult)
    # plot(temp$age, temp$Lx_)        
        

#-------------------------------------------------------------------
#method comparison (based on rate vs. death/birth/population)
#-------------------------------------------------------------------

#method differences (rate vs. deaths/births/population)
    #details: see life expectancy function definition
    
#plot data
    le_methods <- filter(le_ysr, region == "Zurich") %>%
        right_join(le, by = c("year", "sex")) %>%
        rename(basic = life, rate = le_ysr) %>%
        gather(`rate`, `basic`, key = "method", value = "life")
    
#plot
    sszplot(le_methods,
            aes_x = "year", aes_y = "life", aes_col = "method",
            labs_y = "life expectancy at birth",
            grid = c(".", "sex"),
            name = "0204_life-expectancy-at-birth_by-year-method",
            width = 9, height = 5)    

#-------------------------------------------------------------------
#plot: life expectancy
#-------------------------------------------------------------------

#years (all: past and future)
    year_all <- (date_start+1):szen_end
    year_all_5 <- sort(unique(year_all[year_all %% 5 == 0]))    
    
    
#focus: differences by sex
    sszplot(le_ysr,
            aes_x = "year", aes_y = "le_ysr", aes_col = "sex",
            i_x = year_all_5,
            labs_y = "life expectancy at birth",
            grid = c(".", "region"),
            name = "0205_life-expectancy-at-birth_by-year-sex-region_focus-sex",
            width = 10, height = 6)

#focus: differences by region
    sszplot(le_ysr,
            aes_x = "year", aes_y = "le_ysr", aes_col = "region",
            i_x = year_all_5,
            labs_y = "life expectancy at birth",
            grid = c(".", "sex"),
            name = "0206_life-expectancy-at-birth_by-year-sex-region_focus-region",
            width = 10, height = 6)    
    
#-------------------------------------------------------------------
#mortality over base years (including tail correction)
#-------------------------------------------------------------------
 
#Zurich (tibble with all possible cases as input)   
    mor_zh_asr <- filter(mor_zh_yas, year %in% dea_base_begin:dea_base_end) %>% 
        mutate(age_tail = if_else(age <= dea_lower, dea_lower, 
            if_else(age >= dea_upper, dea_upper, age))) %>% 
            group_by(age_tail, sex, region) %>% 
                summarize(dea = sum(dea),
                    pop = sum(pop),
                    .groups = "drop") %>%
            mutate(mor_asr = if_else(pop == 0, NA_real_, round(dea / pop * 100, round_rate)))        
    
    
#Switzerland
    mor_ch_asr <- filter(mor_fso_yas_past, year %in% dea_base_begin:dea_base_end) %>%     
        mutate(age_tail = if_else(age <= dea_lower, dea_lower, 
            if_else(age >= dea_upper, dea_upper, age))) %>% 
            #over tails
            group_by(year, age_tail, sex, region) %>%             
                summarize(mor_asr = median(mor_yas),
                          .groups = "drop") %>%
            #over years (why not in tail-correction? if the median is changed to another function)
            group_by(age_tail, sex, region) %>% 
                summarize(mor_asr = median(mor_asr),
                          .groups = "drop")
    
#both (Zurich and Switzerland), all cases
    mor_asr_temp <- select(mor_zh_asr, age_tail, sex, region, mor_asr) %>%
        bind_rows(mor_ch_asr)

    mor_asr <- as_tibble(expand_grid(
        age = as.double(dea_age_min:dea_age_max),
        sex = uni_s,
        region = factor(text_r, uni_r))) %>% 
        mutate(age_tail = if_else(age <= dea_lower, dea_lower, 
            if_else(age >= dea_upper, dea_upper, age))) %>% 
        left_join(mor_asr_temp, by = c("age_tail", "sex", "region"))

#plot
    sszplot(mor_asr,
            aes_x = "age", aes_y = "mor_asr", aes_col = "region",
            labs_y = "mortality rate (in % per year)",
            scale_y = "log",
            grid = c("sex", "."),
            name = "0207_mortality_by-age-sex-region_tails-corrected",
            width = 8, height = 7)     
    

#-------------------------------------------------------------------
#fit: gam
#-------------------------------------------------------------------

#fit
    #WHY log? because the mortality rates vary substantially
    mor_fit <- select(mor_asr, age, sex, region, mor_asr) %>%
        arrange(sex, region, age) %>%
        group_by(sex, region) %>%
            mutate(mor_fit = pmax(0, exp(gam(log(mor_asr) ~ s(age, bs = "tp"))$fitted.values))) %>%
        ungroup()

#plot

    #fit: levels
        fit_lev <- c("before", "with gam")

    #plot data
        plot_dat_fit <- gather(mor_fit, `mor_asr`, `mor_fit`, key = category, value = mor) %>%
            mutate(cat = factor(if_else(category == "mor_asr",
                fit_lev[1], fit_lev[2]), levels = fit_lev))

    #plot
    sszplot(plot_dat_fit,
            aes_x = "age", aes_y = "mor", aes_col = "region", aes_ltyp = "cat",
            labs_y = "mortality rate (in % per year)",
            scale_y = "log",
            grid = c("sex", "."),
            name = "0208_mortality_by-age-sex-region_fitted",
            width = 8, height = 7)       

#-------------------------------------------------------------------
#ratio Zurich / Switzerland
#-------------------------------------------------------------------

#ratio (Zurich / Switzerland)
    ratio_as <- select(mor_fit, age, sex, region, mor_fit) %>% 
        spread(key = region, value = mor_fit) %>% 
        mutate(ratio = Zurich / Switzerland)      
      
        #Test influences of the tails
        # mutate(ratio = 1)
        # mutate(ratio = if_else((age <= (dea_lower + 10)) | (age >= dea_upper), 1, ratio))
    
#plot
    sszplot(ratio_as,
            aes_x = "age", aes_y = "ratio", aes_col = "sex",
            i_x = c(dea_lower, dea_upper), i_y = 1,
            labs_y = "mortality ratio (Zurich / Switzerland)",
            scale_y = c(0, NA),
            name = "0209_ratio-Zurich-Switzerland_by-age-sex",
            width = 10, height = 5)  
    
  
#-------------------------------------------------------------------
#Zurich: future mortality rate
#-------------------------------------------------------------------

#ZH: future (based on ratio ZH / Switzerland)
    #mortality in percent per year, not more than 100 %
    
    mor_zh_yas_future <- select(ratio_as, age, sex, ratio) %>%
        right_join(mor_fso_yas_future, by = c("age", "sex")) %>%
        mutate(mor = pmin(100, mor_yas * ratio)) %>%
        select(year, age, sex, mor) %>%
        rename(mor_yas = mor)

#ZH: past and future
    mor_zh_yas_past_future <- select(mor_zh_yas, year, age, sex, mor_yas) %>%
        bind_rows(mor_zh_yas_future)

#plot
    sszplot(mor_zh_yas_past_future,
            aes_x = "age", aes_y = "mor_yas", aes_col = "year",
            labs_y = "mortality rate (in % per year)",
            scale_y = "log",
            grid = c("sex", "."),
            name = "0210_mortality-prediction_by-year-age-sex",
            width = 12, height = 8)  

#-------------------------------------------------------------------
#export mortality rates
#-------------------------------------------------------------------

#prepare the export data
    dea_ex <- mutate(mor_zh_yas_past_future, mor = round(mor_yas, round_rate)) %>% 
        filter(year >= szen_begin) %>%       
        select(year, age, sex, mor) %>% 
        arrange(year, age, sex)
    
#export
    write_csv(dea_ex, paste0(exp_path, "/mortality_future.csv"))
    
#-------------------------------------------------------------------
#Zurich: life expectancy (including the model data)
#-------------------------------------------------------------------

#ZH: life expectancy    
    le_ys_ZH <- life_exp(data = mor_zh_yas_past_future, mor = "mor_yas", 
            age = "age", group_cols = c("year", "sex"), 
            age_max = dea_age_max_le, qx_NA = dea_qx_NA_le, 
            age_at = 0, radix = 100000) %>% 
        mutate(region = factor(text_r[1], uni_r)) %>% 
        rename(le_ysr = life_exp) %>% 
        select(year, sex, region, le_ysr)
    
#plot data
    le_ysr_model <- filter(le_ysr, region != "Zurich") %>%
        bind_rows(le_ys_ZH) %>%
        arrange(year, sex, region)


#plots

    #focus: differences by sex
        sszplot(le_ysr_model,
                aes_x = "year", aes_y = "le_ysr", aes_col = "sex",
                labs_y = "life expectancy at birth",
                grid = c(".", "region"),
                name = "0211_life-expectancy-at-birth_by-year-sex-region_focus-sex_model",
                width = 10, height = 6) 

    #focus: differences by region
        sszplot(le_ysr_model,
                aes_x = "year", aes_y = "le_ysr", aes_col = "region",
                labs_y = "life expectancy at birth",
                grid = c(".", "sex"),
                name = "0212_life-expectancy-at-birth_by-year-sex-region_focus-region_model",
                width = 10, height = 6) 
        
        
#log info    
    cat_log(paste0("mortality rate: ", 
        capture.output(Sys.time() - t0)))
        
        
        
        
         
    