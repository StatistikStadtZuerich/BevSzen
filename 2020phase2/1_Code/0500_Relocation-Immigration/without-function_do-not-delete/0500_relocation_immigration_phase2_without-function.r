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
    setwd("O:/Projekte/BevSzen/2020_sandkasten_bad-rok/2020phase2/")

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")

#result path (for images)
    rel_res <- "3_Results/0500_Relocation/"
    
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
#and can be explaned (e.g. the mean amount of relocations from people as of age x)
           
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
#yao (without d)
#-------------------------------------------------------------------
          
#aggregate
    yao <- group_by(dyao_amax_ybase, year, age, origin) %>% 
            summarize(ims = sum(ims),
                rel = sum(rel)) %>% 
        ungroup()
    
#smoothing (direction: age)
    yao_smooth <- group_by(yao, year, origin) %>%
            arrange(age) %>% 
            mutate(ims_a = pmax(0, predict(loess(ims ~ age, span = rei_ims_span_yao, degree = 1, na.action = na.aggregate))),
                rel_a = pmax(0, predict(loess(rel ~ age, span = rei_rel_span_yao, degree = 1, na.action = na.aggregate)))) %>% 
        ungroup()    
    
#plot preparation
    temp_initial <- gather(yao_smooth, `rel`, `ims`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[1], levels = smooth_lev)) %>% 
        select(year, age, origin, cat, smooth, count) 
        
    temp_smooth <- gather(yao_smooth, `rel_a`, `ims_a`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel_a", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[2], levels = smooth_lev)) %>% 
        select(year, age, origin, cat, smooth, count) 
    
    yao_smooth_plot <- bind_rows(temp_initial, temp_smooth)        
    
    
#plot: focus age 
    
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -7)       
      
    #plot 
        p510 <- ggplot(data = filter(yao_smooth_plot, year %in% year_plot)) + 
            geom_line(aes(x = age, y = count, color = cat, linetype = smooth, alpha = smooth)) +  
            facet_grid(origin ~  year) +
            scale_colour_manual(values = col_6[1:2]) + 
            scale_alpha_manual(values = c(0.3, 1)) +
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "quantity per year", color = "", linetype = "", alpha = "") + 
            guides(alpha = FALSE) +
            neutral
        
    ggsave(paste0(rel_res, "0510_processes_yao_smooth_focus-age.pdf"), 
        plot = p510, width = 15, height = 8)      
                
 
    
#plot: focus years 
    
    #age (subjectively selected)
        age_plot <- seq(0, 60, by = 20) 
          
    #plot 
        p511 <- ggplot(data = filter(yao_smooth_plot, age %in% age_plot)) + 
            geom_line(aes(x = year, y = count, color = cat, linetype = smooth, alpha = smooth)) +  
            facet_grid(origin ~  age) +
            scale_colour_manual(values = col_6[1:2]) + 
            scale_alpha_manual(values = c(0.3, 1)) +
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "year", y = "quantity per year", color = "", linetype = "", alpha = "") + 
            guides(alpha = FALSE) +
            neutral
        
    ggsave(paste0(rel_res, "0511_processes_yao_smooth_focus-years.pdf"), 
        plot = p511, width = 15, height = 8)      
                    
    
#proportion after smoothing
    prop_yao_smooth <- mutate(yao_smooth, rel_prop_yao = pmax(0, pmin(100, 
        if_else(ims_a == 0, NA_real_, round(rel_a / ims_a * 100, round_prop)))))      
        
    

#plot: focus age
        
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)           
        
    #plot        
        p512 <- ggplot(data = filter(prop_yao_smooth, year %in% year_plot)) + 
            geom_line(aes(x = age, y = rel_prop_yao, color = origin)) +  
            facet_wrap(~ as.factor(year), ncol = 3) +
            scale_colour_manual(values = col_o) +       
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
            expand_limits(y = 0) +               
            neutral
        
    ggsave(paste0(rel_res, "0512_proportion_yao_smooth_focus-age.pdf"), 
        plot = p512, width = 15, height = 8)             
        
        
#plot: focus years
        
    #age (subjectively selected)
        age_plot <- seq(0, 70, by = 10)           
        
    #plot        
        p513 <- ggplot(data = filter(prop_yao_smooth, age %in% age_plot)) + 
            geom_line(aes(x = year, y = rel_prop_yao, color = origin)) +  
            facet_wrap(~ as.factor(age), ncol = 4) +
            scale_colour_manual(values = col_o) +       
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "") +  
            expand_limits(y = 0) +               
            neutral
        
    ggsave(paste0(rel_res, "0513_proportion_yao_smooth_focus-years.pdf"), 
        plot = p513, width = 15, height = 8)             
        
        
#-------------------------------------------------------------------
#ya (without do)
#-------------------------------------------------------------------
    
#Does this make sense? Is aggregation over origin helpful?
#Yes, this could be helpful, more often for the foreign than the Swiss categories
          
#aggregate
    ya <- group_by(dyao_amax_ybase, year, age) %>% 
            summarize(ims = sum(ims),
                rel = sum(rel)) %>% 
        ungroup()
            
#smoothing (direction: age)
    ya_smooth <- group_by(ya, year) %>%
            arrange(age) %>% 
            mutate(ims_a = pmax(0, predict(loess(ims ~ age, span = rei_ims_span_ya, degree = 1, na.action = na.aggregate))),
                rel_a = pmax(0, predict(loess(rel ~ age, span = rei_rel_span_ya, degree = 1, na.action = na.aggregate)))) %>% 
        ungroup()    
       
#plot preparation
    temp_initial <- gather(ya_smooth, `rel`, `ims`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[1], levels = smooth_lev)) %>% 
        select(year, age, cat, smooth, count) 
        
    temp_smooth <- gather(ya_smooth, `rel_a`, `ims_a`, key = category, value = count) %>% 
        mutate(cat = factor(if_else(category == "rel_a", 
            process_lev[1], process_lev[2]), levels = process_lev),
            smooth = factor(smooth_lev[2], levels = smooth_lev)) %>% 
        select(year, age, cat, smooth, count) 
    
    ya_smooth_plot <- bind_rows(temp_initial, temp_smooth)            
            
#plot: focus age 
    
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)       
      
    #plot 
        p514 <- ggplot(data = filter(ya_smooth_plot, year %in% year_plot)) + 
            geom_line(aes(x = age, y = count, color = cat, linetype = smooth, alpha = smooth)) +  
            facet_wrap( ~year, ncol = 3) +
            scale_colour_manual(values = col_6[1:2]) + 
            scale_alpha_manual(values = c(0.3, 1)) +
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "quantity per year", color = "", linetype = "", alpha = "") + 
            guides(alpha = FALSE) +
            neutral   
    
    ggsave(paste0(rel_res, "0514_processes_ya_smooth_focus-age.pdf"), 
        plot = p514, width = 15, height = 8)    
    
#plot: focus years 
    
    #age (subjectively selected)
        age_plot <- seq(0, 70, by = 10) 
          
    #plot 
        p515 <- ggplot(data = filter(ya_smooth_plot, age %in% age_plot)) + 
            geom_line(aes(x = year, y = count, color = cat, linetype = smooth, alpha = smooth)) +  
             facet_wrap( ~age, ncol = 4) +
            scale_colour_manual(values = col_6[1:2]) + 
            scale_alpha_manual(values = c(0.3, 1)) +
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "year", y = "quantity per year", color = "", linetype = "", alpha = "") + 
            guides(alpha = FALSE) +
            neutral
        
    ggsave(paste0(rel_res, "0515_processes_ya_smooth_focus-years.pdf"), 
        plot = p515, width = 15, height = 8)      
                        
    
#proportion after smoothing
    prop_ya_smooth <- mutate(ya_smooth, rel_prop_ya = pmax(0, pmin(100, 
        if_else(ims_a == 0, NA_real_, round(rel_a / ims_a * 100, round_prop)))))      
        

#plot: focus age
        
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)           
        
    #plot        
        p516 <- ggplot(data = filter(prop_ya_smooth, year %in% year_plot)) + 
            geom_line(aes(x = age, y = rel_prop_ya), color = col_6[1]) +  
            facet_wrap(~ as.factor(year), ncol = 3) +
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
            expand_limits(y = 0) +               
            neutral
        
    ggsave(paste0(rel_res, "0516_proportion_ya_smooth_focus-age.pdf"), 
        plot = p516, width = 15, height = 8)              
    
#plot: focus years
        
    #age (subjectively selected)
        age_plot <- seq(0, 70, by = 10)           
        
    #plot        
        p517 <- ggplot(data = filter(prop_ya_smooth, age %in% age_plot)) + 
            geom_line(aes(x = year, y = rel_prop_ya), color = col_6[1]) +  
            facet_wrap(~ as.factor(age), ncol = 4) +
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "") +  
            expand_limits(y = 0) +               
            neutral
        
    ggsave(paste0(rel_res, "0517_proportion_ya_smooth_focus-years.pdf"), 
        plot = p517, width = 15, height = 8)             
            
    
#-------------------------------------------------------------------
#proportion from different aggregation levels
#-------------------------------------------------------------------
       
#threshold tests
    # rei_ims_thres_d <- 0.8 
    # rei_rel_thres_o <- 0.1  
    
#proportion    
    temp_dyao <- select(prop_dyao_smooth, district, year, age, origin, ims_a, rel_prop_dyao) %>% 
        rename(ims = ims_a)
    temp_yao <- select(prop_yao_smooth, year, age, origin, rel_prop_yao)    
    temp_ya <- select(prop_ya_smooth, year, age, rel_prop_ya)    
      
    sources <- c("dyao", "yao", "ya")    
    
    prop_agg <- left_join(temp_dyao, temp_yao, by = c("year", "age", "origin")) %>% 
        left_join(temp_ya, by = c("year", "age")) %>% 
        mutate(rel_prop = case_when(ims >= rei_ims_thres_d ~ rel_prop_dyao,
                                    ims >= rei_ims_thres_o ~ rel_prop_yao,
                                    TRUE ~ rel_prop_ya), 
               rel_prop_source = factor(case_when(ims >= rei_ims_thres_d ~ sources[1],
                                    ims >= rei_ims_thres_o ~ sources[2],
                                    TRUE ~ sources[3]), levels = sources))
    
    
    #look at certain data points (to understand the values in the numerator of the proportion)    
        test <- filter(prop_agg, (district == "Wollishofen") & (year == 2014) & (origin == "foreign") & (rel_prop_dyao > 70))    
        test     

#plot: entire curves (different aggregations levels)
    entire_curves <- select(prop_agg, district, year, age, origin, rel_prop_dyao, rel_prop_yao, rel_prop_ya) %>% 
        rename(dyao = rel_prop_dyao, yao = rel_prop_yao, ya = rel_prop_ya) %>%    
        gather(`dyao`, `yao`, `ya`, key = category, value = count) %>% 
        mutate(cat = factor(category, levels = sources))
    

   
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)      
        
    p518 <- function(x){
        ggplot(data = filter(entire_curves, (district == x) & (year %in% year_plot))) + 
            geom_line(aes(x = age, y = count, color = cat)) +         
            facet_grid(year ~ origin) +
            scale_colour_manual(values = col_6[1:length(sources)]) + 
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0518_proportion_different-aggregation-levels.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p518)

    dev.off()      
    

#plot: focus age 
    
    #base years (subjectively, but last year in the plot)
        year_plot <- seq(rei_base_end, rei_base_begin, by = -3)      
        
    p519 <- function(x){
        ggplot(data = filter(prop_agg, (district == x) & (year %in% year_plot))) + 
            geom_line(aes(x = age, y = rel_prop), color = "black") +         
            geom_point(aes(x = age, y = rel_prop, color = rel_prop_source)) +  
            facet_grid(year ~ origin) +
            scale_colour_manual(values = col_6[1:length(sources)]) + 
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0519_one-proportion-from-different-aggregation-levels_focus-age.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p519)

    dev.off()  
    
    
#plot: focus years 
    
    #age (subjectively selected)
        age_plot <- seq(0, 60, by = 20)      
        
    p520 <- function(x){
        ggplot(data = filter(prop_agg, (district == x) & (age %in% age_plot))) + 
            geom_line(aes(x = year, y = rel_prop), color = "black") +         
            geom_point(aes(x = year, y = rel_prop, color = rel_prop_source)) +  
            facet_grid(age ~ origin) +
            scale_colour_manual(values = col_6[1:length(sources)]) + 
            scale_x_continuous(breaks = pretty_breaks()) +               
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "") +  
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0520_one-proportion-from-different-aggregation-levels_focus-years.pdf"),
        width = 12, height = 8)

        lapply(uni_d, p520)

    dev.off()  
    
    
#-------------------------------------------------------------------
#smoothing (after different levels of aggregation were brought together
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
        p521 <- function(x){
            ggplot(data = filter(plot_agg_smooth, (district == x) & (year %in% year_plot))) + 
                geom_line(aes(x = age, y = count, linetype = cat, alpha = cat), color = col_6[1]) +  
                facet_grid(year ~ origin) +
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0521_one-proportion_smoothing_focus-age.pdf"),
            width = 12, height = 8)
    
            lapply(uni_d, p521)
    
        dev.off() 
        
        
    
#plot: focus years
    
    #age (subjectively selected)
        age_plot <- seq(0, 60, by = 20) 
      
    #plot 
        p522 <- function(x){
            ggplot(data = filter(plot_agg_smooth, (district == x) & (age %in% age_plot))) + 
                geom_line(aes(x = year, y = count, linetype = cat, alpha = cat), color = col_6[1]) +  
                facet_grid(origin ~  age) +
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0522_one-proportion_smoothing_focus-years.pdf"),
            width = 15, height = 8)
    
            lapply(uni_d, p522)
    
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
    
#age at age threshold
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
        p523 <- function(x){
            ggplot(data = filter(plot_pred_smooth, (district == x) & (year %in% year_plot))) + 
                geom_line(aes(x = age, y = count, linetype = cat, alpha = cat), color = col_6[1]) +  
                facet_grid(origin ~  year) +
                scale_alpha_manual(values = c(0.3, 1)) +
                scale_x_continuous(breaks = pretty_breaks()) +               
                labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "", linetype = "", alpha = "") + 
                guides(alpha = FALSE) +
                ggtitle(as.character(x)) +
                neutral}
    
        pdf(paste0(rel_res, "0523_prediction_dyao_smooth.pdf"),
            width = 15, height = 8)
    
            lapply(uni_d, p523)
    
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
    p524 <- function(x){
        ggplot(data = filter(rei_past_pred, district == x)) +
            geom_line(aes(x = age, y = prop, color = as.factor(year))) +
            facet_grid(. ~ origin) +
            scale_colour_manual(values = col_time) +
            scale_x_continuous(breaks = pretty_breaks()) +           
            labs(x = "age", y = "proportion in % (relocation on immigration*)", color = "") +
            ggtitle(as.character(x)) +
            neutral}

      pdf(paste0(rel_res, "0524_proportion_dyao_past-future.pdf"),
          width = 14, height = 7)
  
          lapply(uni_d, p524)
  
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

    p525 <- function(x){
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

    pdf(paste0(rel_res, "0525_proportion_dyao_past-future_focus-age.pdf"),
        width = 14, height = 7)

        lapply(uni_d, p525)

    dev.off()   
            

#plot: focus years

    #age (subjectively selected)
        age_plot_pred <- seq(0, 60, by = 20)

    #colors
        col_age_pred <- colorRampPalette(col_6[1:5])(length(age_plot_pred))

    p526 <- function(x){
        ggplot(data = filter(plot_a_past_pred, (district == x) & (age %in% age_plot_pred))) +
            geom_vline(xintercept = c(rei_base_begin, rei_base_end), color = col_grey, linetype = 1) +
            geom_line(aes(x = year, y = prop, color = as.factor(age))) +
            facet_grid(. ~ origin) +
            scale_colour_manual(values = col_age_pred) +
            labs(x = "year", y = "proportion in % (relocation on immigration*)", color = "age") +
            ggtitle(as.character(x)) +
            neutral}

    pdf(paste0(rel_res, "0526_proportion_dyao_past-future_focus-years.pdf"),
        width = 14, height = 6)

        lapply(uni_d, p526)

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


      

      


 