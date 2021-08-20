#-------------------------------------------------------------------
#Relocation
#
#relocation proportion (of immigration*)
#
#
#rok/bad, July 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#working directory
    setwd("C:/GitTransfer/BevSzen/2020phase2/")

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")

#result path (for images)
    rel_res <- "3_Results/0500_Relocation-Immigration/dao/"
    
#export path (for future rates)
    rel_exp <- "2_Data/4_Rates/" 


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
    
    
#immigration* and relocation (based on all possible cases)    
    ims_rel <- as_tibble(expand_grid(
        district = uni_d, 
        year = date_start:date_end,
        age = age_min:age_max,
        sex = uni_s,
        origin = uni_o)) %>% 
        left_join(ims, by = c("district", "year", "age", "sex", "origin")) %>% 
        left_join(rel, by = c("district", "year", "age", "sex", "origin")) %>% 
        replace_na(list(ims = 0, rel = 0))
    

       
#-------------------------------------------------------------------
#check differences by sex (sum over years)
#-------------------------------------------------------------------
      
#WHY? Check if minor differences between sex? Because omitted in precition
    #theoretically this should be evaluated with all interactions (i.e. over years)
    #however, there are not enough data points available
            
#proportion by daso
    rel_prop_daso <- group_by(ims_rel, district, age, sex, origin) %>% 
            summarize(ims = sum(ims), 
                rel = sum(rel)) %>% 
        ungroup() %>% 
        mutate(rel_prop_daso = if_else(ims == 0, NA_real_, round(rel / ims * 100, round_prop)))  
                     
        
#plot: focus sex
    p500 <- function(x){
        ggplot(data = filter(rel_prop_daso, origin == x)) + 
            geom_line(aes(x = age, y = rel_prop_daso, color = sex)) +  
            facet_wrap(~district, ncol = 4) +
            scale_colour_manual(values = col_s) +       
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0500_proportion-daso_focus-sex.pdf"),
        width = 12, height = 14)

        lapply(uni_o, p500)

    dev.off() 
    
    
#-------------------------------------------------------------------
#differences by origin (sum over years and sex)
#-------------------------------------------------------------------
          
#WHY?
    #first: to emphasize the need of origin in the model
    #second: and the plots can be used to get an idea the value of an age threshold 

#proportion by dao
    rel_prop_dao <- group_by(ims_rel, district, age, origin) %>% 
            summarize(ims = sum(ims), 
                rel = sum(rel)) %>% 
        ungroup() %>% 
        mutate(rel_prop_dao = if_else(ims == 0, NA_real_, round(rel / ims * 100, round_prop)))  
                     
#plot
    p501 <- ggplot(data = rel_prop_dao) + 
        geom_line(aes(x = age, y = rel_prop_dao, color = origin)) +  
        facet_wrap(~district, ncol = 4) +
        scale_colour_manual(values = col_o) +       
        labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
        neutral
    
    ggsave(paste0(rel_res, "0501_proportion-dao_focus-origin.pdf"), 
        plot = p501, width = 11, height = 10)      
                
    

            
#-------------------------------------------------------------------
#Which base years?
#-------------------------------------------------------------------

#year and origin    
              
    #WHY? simplest plot to determine the base years
    
    #proportion by yo
        rel_prop_yo <- group_by(ims_rel, year, origin) %>% 
                summarize(ims = sum(ims), 
                    rel = sum(rel)) %>% 
            ungroup() %>% 
            mutate(rel_prop_yo = if_else(ims == 0, NA_real_, round(rel / ims * 100, round_prop)))  
                         
    
    #plot
        p502 <- ggplot(data = rel_prop_yo) +
            geom_line(aes(x = year, y = rel_prop_yo, color = origin)) +
            scale_colour_manual(values = col_o) +  
            scale_x_continuous(breaks = pretty_breaks()) + 
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "") +
            expand_limits(y = 0) +           
            neutral
        
        ggsave(paste0(rel_res, "0502_proportion-yo.pdf"), 
            plot = p502, width = 8, height = 6)      
              
              
#year, age, origin 
      
    #proportion by yao
        rel_prop_yao <- group_by(ims_rel, year, age, origin) %>% 
                summarize(ims = sum(ims), 
                    rel = sum(rel)) %>% 
            ungroup() %>% 
            rename(age_num = age) %>% 
            mutate(rel_prop_yao = if_else(ims == 0, NA_real_, round(rel / ims * 100, round_prop)),
                age = as.factor(age_num))  
                         
    #age (subjectively selected)
        age_plot <- seq(0, 80, by = 10)  
    
    #plot        
        p503 <- ggplot(data = filter(rel_prop_yao, age_num %in% age_plot)) +
            geom_line(aes(x = year, y = rel_prop_yao, color = origin)) +
            facet_grid(age ~ ., labeller = labeller(age = label_both)) +
            scale_colour_manual(values = col_o) +  
            scale_x_continuous(breaks = pretty_breaks()) +           
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "") +
            neutral
        
        ggsave(paste0(rel_res, "0503_proportion-yao.pdf"), 
            plot = p503, width = 11, height = 14)      
              
        

        
#-------------------------------------------------------------------
#aggregate over sex
#-------------------------------------------------------------------
        
#This plot is the starting point (before smoothing, age corrections)        
        
#aggregate over sex
    rei_dyao <- group_by(ims_rel, district, year, age, origin) %>% 
            summarize(ims = sum(ims),
                rel = sum(rel)) %>% 
        ungroup()
        
#plot preparation
    process_lev <- c("relocation", "immigration*")
    
    process_plot <- gather(rei_dyao, `rel`, `ims`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel", 
            process_lev[1], process_lev[2]), levels = process_lev)) %>% 
        select(district, year, age, origin, cat, count) 
        

    
#plot: focus age 
    
    #years (subjectively, but last year in the plot)
        year_plot <- seq(date_end, date_start, by = -8)       
        
    p504 <- function(x){
        ggplot(data = filter(process_plot, (district == x) & (year %in% year_plot))) + 
            geom_line(aes(x = age, y = count, color = cat)) +  
            facet_grid(year ~ origin) +
            scale_colour_manual(values = col_6[1:2]) + 
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "quantity per year", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0504_processes_dyao_focus-age.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p504)

    dev.off()  
    
    
#plot: focus years
    
    #age (subjectively selected)
        age_plot <- seq(0, 80, by = 20) 
        
    #with age as factor
        process_plot_age <- rename(process_plot, age_num = age) %>%
            mutate(age = as.factor(age_num))
          
        
    p505 <- function(x){
        ggplot(data = filter(process_plot_age, (district == x) & (age_num %in% age_plot))) + 
            geom_line(aes(x = year, y = count, color = cat)) +  
            facet_grid(age ~ origin, labeller = labeller(age = label_both)) +
            scale_colour_manual(values = col_6[1:2]) +  
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "year", y = "quantity per year", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0505_processes_dyao_focus-years.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p505)

    dev.off()      
    
 
    
    
#-------------------------------------------------------------------
#maximum age, base years
#-------------------------------------------------------------------
    
#WHY mean and not sum over high age? result is mean age 
#is more appropriate e.g. in plots
#and can be explained (e.g. the mean amount of relocations from people as of age x)
#in order to avoid a 'peak value' at age max (therefore mean and not sum)
           
#aggregate
    dyao_amax_ybase <- filter(rei_dyao, (year >= rei_base_begin) & (year <= rei_base_end)) %>% 
        mutate(age_new = pmin(rei_age_max, age)) %>% 
        group_by(district, year, age_new, origin) %>% 
            summarize(ims = mean(ims),
                rel = mean(rel)) %>% 
        ungroup() %>% 
        rename(age = age_new)


    
#-------------------------------------------------------------------
#dyao: smoothing
#-------------------------------------------------------------------
 
#smoothing (direction: age, result should not be below zero)
    dyao_smooth <- group_by(dyao_amax_ybase, district, year, origin) %>%
            arrange(age) %>% 
            mutate(ims_a = pmax(0, predict(loess(ims ~ age, span = rei_ims_span_dyao, degree = 1, na.action = na.aggregate))),
                rel_a = pmax(0, predict(loess(rel ~ age, span = rei_rel_span_dyao, degree = 1, na.action = na.aggregate)))) %>% 
        ungroup()

#plot preparation
    smooth_lev <- c("initial", "smoothed")    
    
    temp_initial <- gather(dyao_smooth, `rel`, `ims`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[1], levels = smooth_lev)) %>% 
        select(district, year, age, origin, cat, smooth, count) 
        
    temp_smooth <- gather(dyao_smooth, `rel_a`, `ims_a`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel_a", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[2], levels = smooth_lev)) %>% 
        select(district, year, age, origin, cat, smooth, count) 
    
    smooth_plot <- bind_rows(temp_initial, temp_smooth)    
    
    
#plot: focus age 
    
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -7)       
      
    #plot 
        p506 <- function(x){
            ggplot(data = filter(smooth_plot, (district == x) & (year %in% year_plot))) + 
                geom_line(aes(x = age, y = count, color = cat, linetype = smooth, alpha = smooth)) +  
                facet_grid(origin ~  year) +
                scale_colour_manual(values = col_6[1:2]) + 
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "age", y = "quantity per year", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0506_processes_dyao_smooth_focus-age.pdf"),
            width = 15, height = 8)
    
            lapply(uni_d, p506)
    
        dev.off()   
        
        
    
#plot: focus years
    
    #age (subjectively selected)
        age_plot <- seq(0, 60, by = 20) 
      
    #plot 
        p507 <- function(x){
            ggplot(data = filter(smooth_plot, (district == x) & (age %in% age_plot))) + 
                geom_line(aes(x = year, y = count, color = cat, linetype = smooth, alpha = smooth)) +  
                facet_grid(origin ~  age) +
                scale_colour_manual(values = col_6[1:2]) + 
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "year", y = "quantity per year", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0507_processes_dyao_smooth_focus-years.pdf"),
            width = 15, height = 8)
    
            lapply(uni_d, p507)
    
        dev.off()   
        
        
#-------------------------------------------------------------------
#dyao: proportion after smoothing
#-------------------------------------------------------------------

#proportion (within the range from 0 to 100 percent)
    prop_dyao_smooth <- mutate(dyao_smooth, rel_prop_dyao = pmax(0, pmin(100, 
        if_else(ims_a == 0, NA_real_, round(rel_a / ims_a * 100, round_prop)))))      
        

#plot: focus age
        
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)           
        
    #plot        
        p508 <- function(x){
                ggplot(data = filter(prop_dyao_smooth, (year == x))) + 
                    geom_line(aes(x = age, y = rel_prop_dyao, color = origin)) +  
                    facet_wrap(~district, ncol = 4) +
                    scale_colour_manual(values = col_o) +       
                    labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
                    ggtitle(as.character(x)) +
                    neutral}
        
            pdf(paste0(rel_res, "0508_proportion_dyao_smooth_focus-age.pdf"),
                width = 11, height = 10)
        
                lapply(year_plot, p508)
        
            dev.off()      
            
    
#plot: focus year
        
    #age (subjectively selected)
        age_plot <- seq(0, 70, by = 10)        
        
    #plot        
        p509 <- function(x){
                ggplot(data = filter(prop_dyao_smooth, (age == x))) + 
                    geom_line(aes(x = year, y = rel_prop_dyao, color = origin)) +  
                    facet_wrap(~district, ncol = 4) +
                    scale_colour_manual(values = col_o) +       
                    labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "") +  
                    ggtitle(paste0("age: ", as.character(x))) +
                    neutral}
        
            pdf(paste0(rel_res, "0509_proportion_dyao_smooth_focus-years.pdf"),
                width = 11, height = 10)
        
                lapply(age_plot, p509)
        
            dev.off()      
            

#-------------------------------------------------------------------
#dao (without y)
#-------------------------------------------------------------------
          
#aggregate (mean per year)
    dao <- group_by(dyao_amax_ybase, district, age, origin) %>% 
            summarize(ims = mean(ims),
                rel = mean(rel)) %>% 
        ungroup()
    
#smoothing (direction: age)
    dao_smooth <- group_by(dao, district, origin) %>%
            arrange(age) %>% 
            mutate(ims_a = pmax(0, predict(loess(ims ~ age, span = rei_ims_span_dao, degree = 1, na.action = na.aggregate))),
                rel_a = pmax(0, predict(loess(rel ~ age, span = rei_rel_span_dao, degree = 1, na.action = na.aggregate)))) %>% 
        ungroup()    
    
#plot preparation
    temp_initial <- gather(dao_smooth, `rel`, `ims`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[1], levels = smooth_lev)) %>% 
        select(district, age, origin, cat, smooth, count) 
        
    temp_smooth <- gather(dao_smooth, `rel_a`, `ims_a`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel_a", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[2], levels = smooth_lev)) %>% 
        select(district, age, origin, cat, smooth, count) 
    
    dao_smooth_plot <- bind_rows(temp_initial, temp_smooth)        
    

#plot 
    p510 <- function(x){
        ggplot(data = filter(dao_smooth_plot, (district == x))) + 
            geom_line(aes(x = age, y = count, color = cat, linetype = smooth, alpha = smooth)) +  
            facet_grid(origin ~  .) +
            scale_colour_manual(values = col_6[1:2]) + 
            scale_alpha_manual(values = c(0.3, 1)) +
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "quantity per year", color = "", linetype = "", alpha = "") + 
            guides(alpha = FALSE) +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0510_processes_dao_smooth.pdf"),
        width = 10, height = 8)

        lapply(uni_d, p510)

    dev.off()  


    
#proportion after smoothing
    prop_dao_smooth <- mutate(dao_smooth, rel_prop_dao = pmax(0, pmin(100, 
        if_else(ims_a == 0, NA_real_, round(rel_a / ims_a * 100, round_prop)))))      
        
    
#plot        
    p511 <- ggplot(data = prop_dao_smooth) +
        geom_line(aes(x = age, y = rel_prop_dao, color = origin)) +  
        facet_wrap(~district, ncol = 4) +
        scale_colour_manual(values = col_o) +       
        labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
        neutral
    
    ggsave(paste0(rel_res, "0511_proportion_dao_smooth.pdf"), 
        plot = p511, width = 11, height = 10)      
                
   
  

#-------------------------------------------------------------------
#proportion from different aggregation levels
#-------------------------------------------------------------------
       
#threshold test
    # rei_ims_thres_y <- 0.8 

#proportion    
    temp_dyao <- select(prop_dyao_smooth, district, year, age, origin, ims_a, rel_prop_dyao) %>% 
        rename(ims = ims_a)
    temp_dao <- select(prop_dao_smooth, district, age, origin, rel_prop_dao)    
    
    sources <- c("dyao", "dao")    
    
    prop_agg <- left_join(temp_dyao, temp_dao, by = c("district", "age", "origin")) %>% 
        mutate(rel_prop = if_else(ims >= rei_ims_thres_y, rel_prop_dyao, rel_prop_dao), 
            rel_prop_source = factor(if_else(ims >= rei_ims_thres_y, sources[1], sources[2]), levels = sources))               

    #look at certain data points (to understand the values in the numerator of the proportion)    
        test <- filter(prop_agg, (district == "Wollishofen") & (year == 2014) & (origin == "foreign") & (rel_prop_dyao > 70))    
        test     

#plot: entire curves (different aggregations levels)
    entire_curves <- select(prop_agg, district, year, age, origin, rel_prop_dyao, rel_prop_dao) %>% 
        rename(dyao = rel_prop_dyao, dao = rel_prop_dao) %>%    
        gather(`dyao`, `dao`, key = category, value = count) %>% 
        mutate(cat = factor(category, levels = sources))
    

   
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)      
        
    p512 <- function(x){
        ggplot(data = filter(entire_curves, (district == x) & (year %in% year_plot))) + 
            geom_line(aes(x = age, y = count, color = cat)) +         
            facet_grid(year ~ origin) +
            scale_colour_manual(values = col_6[1:length(sources)]) + 
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0512_proportion_different-aggregation-levels.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p512)

    dev.off()      
    

#plot: focus age 
    
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)      
        
    p513 <- function(x){
        ggplot(data = filter(prop_agg, (district == x) & (year %in% year_plot))) + 
            geom_line(aes(x = age, y = rel_prop), color = "black") +         
            geom_point(aes(x = age, y = rel_prop, color = rel_prop_source)) +  
            facet_grid(year ~ origin) +
            scale_colour_manual(values = col_6[1:length(sources)]) + 
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0513_one-proportion-from-different-aggregation-levels_focus-age.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p513)

    dev.off()  
    
    
#plot: focus years 
    
    #age (subjectively selected)
        age_plot <- seq(0, 60, by = 20)      
        
    p514 <- function(x){
        ggplot(data = filter(prop_agg, (district == x) & (age %in% age_plot))) + 
            geom_line(aes(x = year, y = rel_prop), color = "black") +         
            geom_point(aes(x = year, y = rel_prop, color = rel_prop_source)) +  
            facet_grid(age ~ origin) +
            scale_colour_manual(values = col_6[1:length(sources)]) + 
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0514_one-proportion-from-different-aggregation-levels_focus-years.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p514)

    dev.off()  
    
    
#-------------------------------------------------------------------
#smoothing (after different levels of aggregation were brought together)
#-------------------------------------------------------------------
 
#smoothing (direction: age)
    prop_agg_smooth <- select(prop_agg, district, year, age, origin, rel_prop) %>% 
          group_by(district, year, origin) %>%
              arrange(age) %>% 
              mutate(prop_smooth = pmax(0, pmin(100, predict(loess(rel_prop ~ age, span = rei_prop_span, 
                  degree = 1, na.action = na.aggregate))))) %>% 
        ungroup()        
   
#plot preparation
    plot_agg_smooth <- gather(prop_agg_smooth, `rel_prop`, `prop_smooth`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel_prop", 
            smooth_lev[1], smooth_lev[2]), levels = smooth_lev)) %>% 
        select(district, year, age, origin, cat, count)     

#plot: focus age 
    
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)       
      
    #plot 
        p515 <- function(x){
            ggplot(data = filter(plot_agg_smooth, (district == x) & (year %in% year_plot))) + 
                geom_line(aes(x = age, y = count, linetype = cat, alpha = cat), color = col_6[1]) +  
                facet_grid(year ~ origin) +
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0515_one-proportion_smoothing_focus-age.pdf"),
            width = 12, height = 8)
    
            lapply(uni_d, p515)
    
        dev.off() 
        
        
    
#plot: focus years
    
    #age (subjectively selected)
        age_plot <- seq(0, 60, by = 20) 
      
    #plot 
        p516 <- function(x){
            ggplot(data = filter(plot_agg_smooth, (district == x) & (age %in% age_plot))) + 
                geom_line(aes(x = year, y = count, linetype = cat, alpha = cat), color = col_6[1]) +  
                facet_grid(origin ~  age) +
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0516_one-proportion_smoothing_focus-years.pdf"),
            width = 15, height = 8)
    
            lapply(uni_d, p516)
    
        dev.off()   

        
#-------------------------------------------------------------------
#Constrained regression
#-------------------------------------------------------------------

#prediction (duration: approx. 20 seconds)

    #Why no upper threshold?
    #proportions will be standardized to 100 percent anyways

    t0 <- Sys.time()        
        
    prop_pred <- con_reg(data = prop_agg_smooth, x = "year", y = "prop_smooth",
              group_cols = c("district", "age", "origin"),
              window = rei_window_thres, base_t0 = rei_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end,
              prop_trend = rei_prop_trend, thres_percent = rei_thres_percent,
              lower_thres = rei_lower_thres, upper_thres = rei_upper_thres)

    t1 <- Sys.time() 
    t1 - t0
    

#limit prediction period
    prop_pred_begin <- filter(prop_pred, year >= szen_begin)

    
    
#-------------------------------------------------------------------
#expand to ages beyond age threshold
#-------------------------------------------------------------------
    
#proportion at age threshold
    prop_age_thres <- filter(prop_pred_begin, age == rei_age_max) %>% 
        rename(pred_age_thres = pred_roll) %>% 
        select(district, year, origin, pred_age_thres)
    
#at ages beyond age threshold: adopt proportion
    prop_age <- as_tibble(expand_grid(
            district = uni_d,
            year = szen_begin:szen_end,
            age = age_min:age_max,
            origin = uni_o)) %>% 
        left_join(select(prop_pred_begin, district, year, age, origin, pred_roll),
            by = c("district", "year", "age", "origin")) %>% 
        left_join(prop_age_thres, by = c("district", "year", "origin")) %>% 
        mutate(pred_all_age = if_else(age > rei_age_max, pred_age_thres, pred_roll))
    
    
    
#smoothing (direction: age)
    pred_smooth <- group_by(prop_age, district, year, origin) %>%
            arrange(age) %>% 
            mutate(pred_smooth = pmax(0, pmin(100, predict(loess(pred_all_age ~ age, 
                span = rei_pred_span, degree = 1, na.action = na.aggregate))))) %>% 
        ungroup()
    

#plot preparation
    plot_pred_smooth <- select(pred_smooth, district, year, age, origin, pred_all_age, pred_smooth) %>% 
        gather(pred_smooth, `pred_all_age`, `pred_smooth`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "pred_all_age", 
            smooth_lev[1], smooth_lev[2]), levels = smooth_lev)) %>% 
        select(district, year, age, origin, cat, count) 
    
#plot 
    
    #base years (subjectively)
        year_plot <- seq(szen_begin, szen_end, by = 7)       
      
    #plot 
        p517 <- function(x){
            ggplot(data = filter(plot_pred_smooth, (district == x) & (year %in% year_plot))) + 
                geom_line(aes(x = age, y = count, linetype = cat, alpha = cat), color = col_6[1]) +  
                facet_grid(origin ~  year) +
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0517_prediction_dyao_smooth.pdf"),
            width = 15, height = 8)
    
            lapply(uni_d, p517)
    
        dev.off()   
            
    
    
    
#-------------------------------------------------------------------
#plot the final predictions
#-------------------------------------------------------------------
    
#past rate (before smoothing)    
    rei_dyao_not_smoothed <- mutate(rei_dyao, 
            rel_prop_dyao = pmax(0, pmin(100, if_else(ims == 0, NA_real_, round(rel / ims * 100, round_prop))))) %>% 
        select(district, year, age, origin, rel_prop_dyao)
    
#past and prediction
    rei_past_pred <- as_tibble(expand_grid(
        district = uni_d,
        year = date_start:szen_end,
        age = age_min:age_max,
        origin = uni_o)) %>% 
        left_join(rei_dyao_not_smoothed, by = c("district", "year", "age", "origin")) %>% 
        left_join(select(pred_smooth, district, year, age, origin, pred_smooth),
            by = c("district", "year", "age", "origin")) %>% 
        mutate(prop = if_else(year < szen_begin, rel_prop_dyao, pred_smooth))

#colors
    col_time <- c(rep(col_grey, length(date_start:date_end)),
        colorRampPalette(col_6[1:5])(length(szen_begin:szen_end)))
    # plot(1:length(col_time), 1:length(col_time), col = col_time, pch = 16, cex = 2)
       
#plot: focus age distribution
    p518 <- function(x){
        ggplot(data = filter(rei_past_pred, district == x)) +
            geom_line(aes(x = age, y = prop, color = as.factor(year))) +
            facet_grid(. ~ origin) +
            scale_colour_manual(values = col_time) +
            scale_x_continuous(breaks = pretty_breaks()) +           
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +
            ggtitle(as.character(x)) +
            neutral}

      pdf(paste0(rel_res, "0518_proportion_dyao_past-future.pdf"),
          width = 14, height = 7)
  
          lapply(uni_d, p518)
  
      dev.off()   
      
    
#plot levels
    time_lev <- c("past", "future")

#plot data
    plot_a_past_pred <- mutate(rei_past_pred,
        time = factor(if_else(year < szen_begin,
        time_lev[1], time_lev[2]), levels = time_lev))

#plot: focus age distribution
    #test: x <- uni_d[3]
    #WHY this plot: it is recommendable to look precisely at the age plots over years
    #therefore, a plot that focuses on certain years

    #years
        year_temp <- date_start:szen_end
        year_plot <- seq(min(year_temp), max(year_temp), by = 8)

    #colors
        col_years_plot <- colorRampPalette(col_6[1:5])(length(year_plot))

    p519 <- function(x){
        ggplot(data = filter(plot_a_past_pred, (district == x) & (year %in% year_plot))) +
            geom_line(aes(x = age, y = prop, color = as.factor(year), size = time, alpha = time)) +
            facet_grid(. ~ origin ) +
            scale_colour_manual(values = col_years_plot) +
            scale_size_manual(values = c(0.2, 1.2)) +
            scale_alpha_manual(values = c(0.4, 1)) +
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "year", size = "") +
            guides(alpha = FALSE) +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0519_proportion_dyao_past-future_focus-age.pdf"),
        width = 14, height = 7)

        lapply(uni_d, p519)

    dev.off()   
            

#plot: focus years

    #age (subjectively selected)
        age_plot_pred <- seq(0, 60, by = 20)

    #colors
        col_age_pred <- colorRampPalette(col_6[1:5])(length(age_plot_pred))

    p520 <- function(x){
        ggplot(data = filter(plot_a_past_pred, (district == x) & (age %in% age_plot_pred))) +
            geom_vline(xintercept = c(rei_base_begin, rei_base_end), color = col_grey, linetype = 1) +
            geom_line(aes(x = year, y = prop, color = as.factor(age))) +
            facet_grid(. ~ origin) +
            scale_colour_manual(values = col_age_pred) +
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "age") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0520_proportion_dyao_past-future_focus-years.pdf"),
        width = 14, height = 6)

        lapply(uni_d, p520)

    dev.off()   
            

#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#proportion: prediction   
    rel_ex_data <- mutate(pred_smooth, prop_rel_dyao = round(pred_smooth, round_prop)) %>% 
        select(district, year, age, origin, prop_rel_dyao) %>% 
        arrange(district, year, age, origin)
      
#export
    write_csv(rel_ex_data, paste0(rel_exp, "recolation_immigration_future.csv"))    


      

      


 