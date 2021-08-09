#-------------------------------------------------------------------
#Immigration
#
#proportion of age (per district, year, sex, origin)
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
    
    rel <- read_csv(rel_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, rel = AnzUmzuWir) %>% 
        left_join(look_dis, by = "QuarCd") %>%     
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
    

    
    
#-------------------------------------------------------------------
#immigration*: per district, year, sex, origin
#-------------------------------------------------------------------
    
#WHY? to see where the denominator of the age proportion is low    
    
#possible cases: dyso
    cas_dyso <- as_tibble(expand_grid(
        district = uni_d, 
        year = date_start:date_end,
        sex = uni_s,
        origin = uni_o))
    
#immigration*: dyso    
    ims_dyso <- group_by(ims, district, year, sex, origin) %>% 
            summarize(ims_dyso = sum(ims)) %>% 
        ungroup() %>% 
        right_join(cas_dyso, by = c("district", "year", "sex", "origin")) %>% 
        replace_na(list(ims_dyso = 0))  

#years of the past (for plot)
    year_past <- date_start:date_end
    year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))    
            
#plot
    p310 <- ggplot(data = ims_dyso) + 
        geom_vline(xintercept = year_past_5, color = col_grey) +          
        geom_line(aes(x = year, y = ims_dyso, color = sex, linetype = origin)) +  
        facet_wrap(~district, ncol = 4) +
        scale_colour_manual(values = col_s) +       
        labs(x = "year", y = "immigration* per year", color = "", linetype = "") +  
        neutral

    ggsave(paste0(imm_res, "0310_immigration-star_per-district-year-sex-origin.pdf"), 
        plot = p310, width = 12, height = 14)      
            
    
#plot (as p310, but free scales per facet)
    p311 <- ggplot(data = ims_dyso) + 
        geom_vline(xintercept = year_past_5, color = col_grey) +          
        geom_line(aes(x = year, y = ims_dyso, color = sex, linetype = origin)) +  
        facet_wrap(~district, ncol = 4, scales = "free") +
        expand_limits(y = 0) + 
        scale_colour_manual(values = col_s) +       
        labs(x = "year", y = "immigration* per year", color = "", linetype = "") +  
        neutral

    ggsave(paste0(imm_res, "0311_immigration-star_per-district-year-sex-origin_free-scales.pdf"), 
        plot = p311, width = 12, height = 14)      
            
        
#-------------------------------------------------------------------
#age proportion: per district, year, sex, origin
#-------------------------------------------------------------------
        
#immigration*: dyaso (already summarized before)
    ims_dyaso <- as_tibble(expand_grid(
        district = uni_d, 
        year = date_start:date_end,
        age = ims_age_min:ims_age_max,
        sex = uni_s,
        origin = uni_o)) %>% 
        left_join(ims, by = c("district", "year", "age", "sex", "origin")) %>% 
        rename(ims_dyaso = ims) %>% 
        replace_na(list(ims_dyaso = 0)) %>% 
        left_join(ims_dyso, by = c("district", "year", "sex", "origin")) %>% 
        arrange(district, year, sex, origin, age) %>% 
        mutate(ims_prop_a = if_else(ims_dyso == 0, NA_real_, round(ims_dyaso / ims_dyso * 100, round_prop)))    
        
    
#plot: focus age distribution
    #test: x <- uni_d[2]
    
    #years (subjectively selected)
        #WHY with rev? To have the last year in the plot
        years_plot <- rev(seq(date_end, date_start, by = -8))
    
    #colors
        col_years <- colorRampPalette(col_6[1:5])(length(years_plot))    
        

    p312 <- function(x){
        ggplot(data = filter(ims_dyaso, (district == x) & (year %in% years_plot))) +
            geom_line(aes(x = age, y = ims_prop_a, color = as.factor(year))) +
            facet_grid(sex ~ origin) +
            scale_colour_manual(values = col_years) +   
            labs(x = "age", y = "proportion in %", color = "year") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(imm_res, "0312_age-proportion_per-district-year-sex-origin_focus-age.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p312)

    dev.off()
    

    
#plot: focus years
    
    #age (subjectively selected)
        age_plot <- seq(0, 60, by = 20)    
    
    #colors
        col_age <- colorRampPalette(col_6[1:5])(length(age_plot))    
            

    p313 <- function(x){
        ggplot(data = filter(ims_dyaso, (district == x) & (age %in% age_plot))) +
            geom_line(aes(x = year, y = ims_prop_a, color = as.factor(age))) +
            facet_grid(sex ~ origin) +
            scale_colour_manual(values = col_age) +   
            labs(x = "year", y = "proportion in %", color = "age") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(imm_res, "0313_age-proportion_per-district-year-sex-origin_focus-years.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p313)

    dev.off()
     
    
    
    
    
    
    
#-------------------------------------------------------------------
#moving average over years (by district, age, sex, origin)
#-------------------------------------------------------------------
 
#moving average  
    ims_ma <- select(ims_dyaso, district, year, age, sex, origin, ims_dyaso) %>% 
        group_by(district, age, sex, origin) %>% 
            arrange(year) %>% 
            mutate(ims_roll = rollmean(ims_dyaso, k = ims_age_window_years, fill = NA)) %>% 
        ungroup()
    
#plot preparation
    ma_lev <- c("initial", "moving average")
    
    ims_ma_plot <- gather(ims_ma, `ims_dyaso`, `ims_roll`, key = category, value = ims) %>% 
        mutate(cat = factor(if_else(category == "ims_dyaso", 
            ma_lev[1], ma_lev[2]), levels = ma_lev)) %>% 
        select(district, year, age, sex, origin, cat, ims)    
        
    
#plot: focus age distribution
    #test: x <- uni_d[3]
    
    #years (subjectively selected)
        years_not_NA <- sort(unique(ims_ma$year[!is.na(ims_ma$ims_roll)]))
        years_plot_ma <- seq(min(years_not_NA), max(years_not_NA), by = 6)
    
    p314 <- function(x){
        ggplot(data = filter(ims_ma_plot, (district == x) & (year %in% years_plot_ma))) +
            geom_line(aes(x = age, y = ims, color = cat)) +
            facet_grid(as.factor(year) ~ origin*sex) +
            scale_colour_manual(values = col_6[1:2]) +  
            labs(x = "age", y = "immigration* (per year)", color = "") +
            ggtitle(as.character(x)) +
            neutral}
    
    pdf(paste0(imm_res, "0314_moving-average-over-year_focus-age.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p314)

    dev.off()
    
        
#plot: focus years
    
    #age (subjectively selected)
        age_plot_ma <- seq(0, 60, by = 20) 
        
    p315 <- function(x){
        ggplot(data = filter(ims_ma_plot, (district == x) & (age %in% age_plot_ma))) +
            geom_line(aes(x = year, y = ims, color = cat)) +
            facet_grid(as.factor(age) ~ origin*sex) +
            scale_colour_manual(values = col_age) +   
            labs(x = "year", y = "immigration* (per year)", color = "") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(imm_res, "0315_moving-average-over-year_focus-years.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p315)

    dev.off()
                
        
        
#-------------------------------------------------------------------
#age proportion (after moving average)
#-------------------------------------------------------------------
     
#preparation (e.g. without years with NA)
    ims_ma_prep <- filter(ims_ma, year %in% years_not_NA) %>%
        select(district, year, age, sex, origin, ims_roll) %>% 
        rename(ims_dyaso = ims_roll)
    
#age proportion     
    ims_age_prop_ma <- group_by(ims_ma_prep, district, year, sex, origin) %>% 
                summarize(ims_dyso = sum(ims_dyaso)) %>% 
            ungroup() %>% 
        right_join(ims_ma_prep, by = c("district", "year", "sex", "origin")) %>%  
        mutate(prop_a_ma = if_else(ims_dyso == 0, NA_real_, round(ims_dyaso / ims_dyso * 100, round_prop))) %>% 
        select(district, year, age, sex, origin, prop_a_ma) %>% 
        arrange(district, year, sex, origin, age) 

    
#plot: focus age distribution
    #test: x <- uni_d[3]

    #years
        #WHY like this with rev? To have the last year with data in the plot    
        years_plot_ma_prop <- rev(seq(max(years_not_NA), min(years_not_NA), by = -6))        

    #colors
        col_years_ma_prop <- colorRampPalette(col_6[1:5])(length(years_plot_ma_prop))

    p316 <- function(x){
        ggplot(data = filter(ims_age_prop_ma, (district == x) & (year %in% years_plot_ma_prop))) +
            geom_line(aes(x = age, y = prop_a_ma, color = as.factor(year))) +
            facet_grid(sex ~ origin) +
            scale_colour_manual(values = col_years_ma_prop) +
            labs(x = "age", y = "proportion in %", color = "year") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(imm_res, "0316_age-proportion_after-moving-average_focus-age.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p316)

    dev.off()


#plot: focus years

    #age (subjectively selected)
        age_plot_ma_prop <- seq(0, 60, by = 20)

    #colors
        col_age_ma_prop <- colorRampPalette(col_6[1:5])(length(age_plot_ma_prop))

    p317 <- function(x){
        ggplot(data = filter(ims_age_prop_ma, (district == x) & (age %in% age_plot_ma_prop))) +
            geom_line(aes(x = year, y = prop_a_ma, color = as.factor(age))) +
            facet_grid(sex ~ origin) +
            scale_colour_manual(values = col_age_ma_prop) +
            labs(x = "year", y = "proportion in %", color = "age") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(imm_res, "0317_age-proportion_after-moving-average_focus-years.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p317)

    dev.off()


    
#-------------------------------------------------------------------
#fit gam to age proportion
#-------------------------------------------------------------------
     
#with gam (duration: approx. 6 minutes)  
    
    # details see: https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/smooth.terms.html 
    # initial smooth term 'tp' did not provide an appropriate fit ('bumps')
    # therefore changed to 'ad' (adaptive smoother)
    # ad: should be applied, when 'the degree of smoothing should itself vary with the covariates to be smoothed'
        
    t0 <- Sys.time()
    
    prop_fit <- arrange(ims_age_prop_ma, district, year, sex, origin, age) %>% 
        group_by(district, year, sex, origin) %>% 
            mutate(prop_fit = pmax(0, gam(prop_a_ma ~ s(age, bs = "ad"))$fitted.values)) %>% 
        ungroup()  
    
    t1 <- Sys.time()
    t1 - t0
   
#plot preparation
    fit_lev <- c("initial", "with gam")
    
    ims_fit_plot <- gather(prop_fit, `prop_a_ma`, `prop_fit`, key = category, value = prop) %>% 
        mutate(cat = factor(if_else(category == "prop_a_ma", 
            fit_lev[1], fit_lev[2]), levels = fit_lev)) %>% 
        select(district, year, age, sex, origin, cat, prop)    
        
#plot: focus age distribution
    #test: x <- uni_d[3]
    
    #years (subjectively selected)
        years_plot_fit <- seq(min(years_not_NA), max(years_not_NA), by = 6)
    
    p318 <- function(x){
        ggplot(data = filter(ims_fit_plot, (district == x) & (year %in% years_plot_fit))) +
            geom_line(aes(x = age, y = prop, color = cat)) +
            facet_grid(as.factor(year) ~ origin*sex) +
            scale_colour_manual(values = col_6[1:2]) +  
            labs(x = "age", y = "proportion in %", color = "") +
            ggtitle(as.character(x)) +
            neutral}
    
    pdf(paste0(imm_res, "0318_proportion_with-gam_focus-age.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p318)

    dev.off()
    

#-------------------------------------------------------------------
#Constrained regression
#-------------------------------------------------------------------
  
#years: base period
    years_temp <- ims_age_base_begin:ims_age_base_end
    years_base <- years_temp[years_temp %in% years_not_NA]
      
    prop_base <- filter(prop_fit, year %in% years_base)    
    
    
#prediction (duration: approx. 30 seconds)   
    
    #Why no upper threshold? 
    #proportions will be standardized to 100 percent anyways    
    
    prop_pred <- con_reg(data = prop_base, x = "year", y = "prop_fit", 
              group_cols = c("district", "age", "sex", "origin"),
              window = ims_age_window_thres, base_t0 = min(years_base),
              szen_t0 = max(years_base)+1, szen_t1 = szen_end, 
              prop_trend = ims_age_prop_trend, thres_percent = ims_age_thres_percent, 
              lower_thres = ims_age_lower_thres, upper_thres = NA, 
              window_thres = ims_age_window_thres)
    
#limit prediction period
    prop_pred_begin <- filter(prop_pred, year >= szen_begin)
    
#standardize the sum of the proportions to 100 percent
    prop_stand <- group_by(prop_pred_begin, district, year, sex, origin) %>% 
            summarize(pred_roll_sum = sum_NA(pred_roll)) %>%             
        ungroup() %>%  
        right_join(prop_pred_begin, by = c("district", "year", "sex", "origin")) %>%     
        mutate(pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, round(pred_roll / pred_roll_sum * 100, round_prop)))
       
#past and prediction
    ims_a_past_pred <- as_tibble(expand_grid(
        district = uni_d, 
        year = (date_start+1):szen_end,
        age = ims_age_min:ims_age_max,
        sex = uni_s,
        origin = uni_o)) %>% 
        left_join(select(ims_dyaso, district, year, age, sex, origin, ims_prop_a), 
            by = c("district", "year", "age", "sex", "origin")) %>%     
        left_join(select(prop_stand, district, year, age, sex, origin, pred_roll_stand),
            by = c("district", "year", "age", "sex", "origin")) %>% 
        mutate(prop_a = if_else(year <= ims_age_base_end, ims_prop_a, pred_roll_stand))         
        
    
#colors
    col_time <- c(rep(col_grey, length((date_start+1):date_end)), 
        colorRampPalette(col_6[1:5])(length(szen_begin:szen_end)))
    # plot(1:length(col_time), 1:length(col_time), col = col_time, pch = 16, cex = 2)

    
#plot: focus age distribution
    #test: x <- uni_d[3]
    
    p319 <- function(x){
        ggplot(data = filter(ims_a_past_pred, district == x)) +
            geom_line(aes(x = age, y = prop_a, color = as.factor(year))) +
            facet_grid(origin ~ sex) +
            scale_colour_manual(values = col_time) +  
            labs(x = "age", y = "proportion in %", color = "") +
            ggtitle(as.character(x)) +
            neutral}
    
    pdf(paste0(imm_res, "0319_age-proportion_by-district-year-sex-origin_past-future.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p319)

    dev.off()    
    
    
#plot levels    
    time_lev <- c("past", "future")
    
#plot data    
    plot_a_past_pred <- mutate(ims_a_past_pred, 
        time = factor(if_else(year <= ims_age_base_end, 
        time_lev[1], time_lev[2]), levels = time_lev))      

#plot: focus age distribution
    #test: x <- uni_d[3]
    #WHY this plot: it is recommendable to look precisely at the age plots over years
    #therefore, a plot that focuses on certain years

    #years
        year_temp <- (date_start + 1):szen_end
        year_plot <- seq(min(year_temp), max(year_temp), by = 8)        
        
    #colors
        col_years_plot <- colorRampPalette(col_6[1:5])(length(year_plot))

    p320 <- function(x){
        ggplot(data = filter(plot_a_past_pred, (district == x) & (year %in% year_plot))) +
            geom_line(aes(x = age, y = prop_a, color = as.factor(year), size = time, alpha = time)) +
            facet_grid(sex ~ origin) +
            scale_colour_manual(values = col_years_plot) +
            scale_size_manual(values = c(0.2, 1.2)) + 
            scale_alpha_manual(values = c(0.4, 1)) + 
            labs(x = "age", y = "proportion in %", color = "year", size = "") +
            guides(alpha = FALSE) +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(imm_res, "0320_age-proportion_by-district-year-sex-origin_past-future_focus-age.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p320)

    dev.off()


    
    
     
#plot: focus years

    #age (subjectively selected)
        age_plot_pred <- seq(0, 60, by = 20)

    #colors
        col_age_pred <- colorRampPalette(col_6[1:5])(length(age_plot_pred))

    p321 <- function(x){
        ggplot(data = filter(plot_a_past_pred, (district == x) & (age %in% age_plot_ma_prop))) +
            geom_vline(xintercept = c(ims_age_base_begin, ims_age_base_end), color = col_grey, linetype = 1) +              
            geom_line(aes(x = year, y = prop_a, color = as.factor(age))) +
            facet_grid(sex ~ origin) +
            scale_colour_manual(values = col_age_pred) +
            labs(x = "year", y = "proportion in %", color = "age") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(imm_res, "0321_age-proportion_by-district-year-sex-origin_past-future_focus-years.pdf"),
        width = 11, height = 8)

        lapply(uni_d, p321)

    dev.off()




        
        
        



     
        
    
    
             
    
    
    
    
    