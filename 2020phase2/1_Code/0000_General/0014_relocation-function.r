#-------------------------------------------------------------------
#Function: Relocation proportion of migration* 
#(immigration* or emigration*)
#
#
#
#rok/bad, July 2021
#-------------------------------------------------------------------



#proportion of relocation based on migration*
      
   rel_prop <- function(mig_path, mig_vari, mig_district,
                          mig_name, rem_number, graph_path, ex_path, 
                          rem_base_begin, rem_base_end, rem_age_max,
                          rem_mis_span_dyao,rem_rel_span_dyao,
                          rem_mis_span_yao, rem_rel_span_yao,
                          rem_mis_span_ya, rem_rel_span_ya,
                          rem_mis_thres_d, rem_mis_thres_o,
                          rem_prop_span, 
                          rem_window_thres, rem_prop_trend, rem_thres_percent,
                          rem_lower_thres, rem_upper_thres, 
                          rem_pred_span){
     
     
            #variables
                #WHY only migration path and variable name?
                #that are the only variables changed (when evaluation immigration or emigration)
                #relocation and population variables remain the same

     
            #-------------------------------------------------------------------
            #import and data preparation
            #-------------------------------------------------------------------
            
            #migration (immigration or emigration)  
                mig <- read_csv(mig_path) %>%
                    rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>%
                    left_join(look_dis, by = "QuarCd") %>%
                    mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                        origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                        district = factor(distr, uni_d)) %>%
                    select(district, year, age, sex, origin, mig) %>%
                    group_by(district, year, age, sex, origin) %>%
                        summarize(mig = sum(mig)) %>%
                    ungroup()
                
            #relocation
                #only migration to a certain district
                #WHY? this is needed to calculate immigration* (i.e. migration to a certain district)

                rel <- read_csv(rel_od) %>%
                    rename(year = EreignisDatJahr, age = AlterVCd, dis = mig_district, rel = AnzUmzuWir) %>%
                    left_join(look_dis, c("dis" = "QuarCd")) %>% 
                    mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                        origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                        district = factor(distr, uni_d)) %>%
                    select(district, year, age, sex, origin, rel) %>%
                    group_by(district, year, age, sex, origin) %>%
                        summarize(rel = sum(rel)) %>%
                    ungroup()
                
            #migration* (i.e. migration to a certain district, 'migration star' = mis)
                mis <- bind_rows(rename(mig, mis = mig),
                    rename(rel, mis = rel)) %>%
                    group_by(district, year, age, sex, origin) %>%
                        summarize(mis = sum(mis)) %>%
                    ungroup()                
                                      
            #migration* (immigration* or emigration*) and relocation (based on all possible cases)
                mis_rel <- as_tibble(expand_grid(
                    district = uni_d,
                    year = date_start:date_end,
                    age = age_min:age_max,
                    sex = uni_s,
                    origin = uni_o)) %>%
                    left_join(mis, by = c("district", "year", "age", "sex", "origin")) %>%
                    left_join(rel, by = c("district", "year", "age", "sex", "origin")) %>%
                    replace_na(list(mis = 0, rel = 0))

            
            #-------------------------------------------------------------------
            #check differences by sex (sum over years)
            #-------------------------------------------------------------------
            
            #WHY? Check if minor differences between sex? Because omitted in precition
                #theoretically this should be evaluated with all interactions (i.e. over years)
                #however, there are not enough data points available
            
            #proportion by daso
                rel_prop_daso <- group_by(mis_rel, district, age, sex, origin) %>%
                        summarize(mis = sum(mis),
                            rel = sum(rel)) %>%
                    ungroup() %>%
                    mutate(rel_prop_daso = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)))
            
            
            #plot: focus sex
                p00 <- function(x){
                    ggplot(data = filter(rel_prop_daso, origin == x)) +
                        geom_line(aes(x = age, y = rel_prop_daso, color = sex)) +
                        facet_wrap(~district, ncol = 4) +
                        scale_colour_manual(values = col_s) +
                        labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        ggtitle(as.character(x)) +
                        neutral}
                
                
                pdf(paste0(graph_path, rem_number, "00_proportion-daso_focus-sex.pdf"),
                    width = 12, height = 14)
    
                    # lapply(uni_d, p00) #when this code is within a function: pdf cannot be opened
                    for(i in 1:length(uni_o)){plot(p00(uni_o[i]))}
    
                dev.off()
                



            #-------------------------------------------------------------------
            #differences by origin (sum over years and sex)
            #-------------------------------------------------------------------
            
            #WHY?
                #first: to emphasize the need of origin in the model
                #second: and the plots can be used to get an idea the value of an age threshold
            
            #proportion by dao
                rel_prop_dao <- group_by(mis_rel, district, age, origin) %>%
                        summarize(mis = sum(mis),
                            rel = sum(rel)) %>%
                    ungroup() %>%
                    mutate(rel_prop_dao = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)))
            
            #plot
                p01 <- ggplot(data = rel_prop_dao) +
                    geom_line(aes(x = age, y = rel_prop_dao, color = origin)) +
                    facet_wrap(~district, ncol = 4) +
                    scale_colour_manual(values = col_o) +
                    labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                    neutral
            
                ggsave(paste0(graph_path, rem_number, "01_proportion-dao_focus-origin.pdf"),
                    plot = p01, width = 11, height = 10)

            
            #-------------------------------------------------------------------
            #Which base years?
            #-------------------------------------------------------------------
            
            #year and origin
            
                #WHY? simplest plot to determine the base years
            
                #proportion by yo
                    rel_prop_yo <- group_by(mis_rel, year, origin) %>%
                            summarize(mis = sum(mis),
                                rel = sum(rel)) %>%
                        ungroup() %>%
                        mutate(rel_prop_yo = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)))
            
            
                #plot
                    p02 <- ggplot(data = rel_prop_yo) +
                        geom_line(aes(x = year, y = rel_prop_yo, color = origin)) +
                        scale_colour_manual(values = col_o) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        expand_limits(y = 0) +
                        neutral
                    
                    ggsave(paste0(graph_path, rem_number, "02_proportion-yo.pdf"),
                        plot = p02, width = 8, height = 6)                    
            
            
            #year, age, origin
            
                #proportion by yao
                    rel_prop_yao <- group_by(mis_rel, year, age, origin) %>%
                            summarize(mis = sum(mis),
                                rel = sum(rel)) %>%
                        ungroup() %>%
                        rename(age_num = age) %>%
                        mutate(rel_prop_yao = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)),
                            age = as.factor(age_num))
            
                #age (subjectively selected)
                    age_plot <- seq(0, 80, by = 10)
            
                #plot
                    p03 <- ggplot(data = filter(rel_prop_yao, age_num %in% age_plot)) +
                        geom_line(aes(x = year, y = rel_prop_yao, color = origin)) +
                        facet_grid(age ~ ., labeller = labeller(age = label_both)) +
                        scale_colour_manual(values = col_o) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        neutral
            
                    ggsave(paste0(graph_path, rem_number, "03_proportion-yao.pdf"),
                        plot = p03, width = 11, height = 14)                    
                

          
          #-------------------------------------------------------------------
          #aggregate over sex
          #-------------------------------------------------------------------
          
          #This plot is the starting point (before smoothing, age corrections)
          
          #aggregate over sex
              rem_dyao <- group_by(mis_rel, district, year, age, origin) %>%
                      summarize(mis = sum(mis),
                          rel = sum(rel)) %>%
                  ungroup()
          
          #plot preparation
              process_lev <- c("relocation", paste0(mig_name, "*"))
          
              process_plot <- gather(rem_dyao, `rel`, `mis`, key = category, value = count) %>%
                  mutate(cat = factor(if_else(category == "rel",
                      process_lev[1], process_lev[2]), levels = process_lev)) %>%
                  select(district, year, age, origin, cat, count)
          
          
          #plot: focus age
          
              #years (subjectively, but last year in the plot)
                  year_plot <- seq(date_end, date_start, by = -8)
          
              p04 <- function(x){
                  ggplot(data = filter(process_plot, (district == x) & (year %in% year_plot))) +
                      geom_line(aes(x = age, y = count, color = cat)) +
                      facet_grid(year ~ origin) +
                      scale_colour_manual(values = col_6[1:2]) +
                      scale_x_continuous(breaks = pretty_breaks()) +
                      labs(x = "age", y = "quantity per year", color = "") +
                      ggtitle(as.character(x)) +
                      neutral}
          

              pdf(paste0(graph_path, rem_number, "04_processes_dyao_focus-age.pdf"),
                  width = 12, height = 8)

                  for(i in 1:length(uni_d)){plot(p04(uni_d[i]))}

              dev.off()
                              
              
          
          
          #plot: focus years
          
              #age (subjectively selected)
                  age_plot <- seq(0, 80, by = 20)
          
              #with age as factor
                  process_plot_age <- rename(process_plot, age_num = age) %>%
                      mutate(age = as.factor(age_num))
          
          
              p05 <- function(x){
                  ggplot(data = filter(process_plot_age, (district == x) & (age_num %in% age_plot))) +
                      geom_line(aes(x = year, y = count, color = cat)) +
                      facet_grid(age ~ origin, labeller = labeller(age = label_both)) +
                      scale_colour_manual(values = col_6[1:2]) +
                      scale_x_continuous(breaks = pretty_breaks()) +
                      labs(x = "year", y = "quantity per year", color = "") +
                      ggtitle(as.character(x)) +
                      neutral}
          

              pdf(paste0(graph_path, rem_number, "05_processes_dyao_focus-years.pdf"),
                  width = 12, height = 8)

                  for(i in 1:length(uni_d)){plot(p05(uni_d[i]))}

              dev.off()
              
            
            
            #-------------------------------------------------------------------
            #maximum age, base years
            #-------------------------------------------------------------------
            
            #WHY mean and not sum over high age? result is mean age
            #is more appropriate e.g. in plots
            #and can be explaned (e.g. the mean amount of relocations from people as of age x)
            
            #aggregate
                dyao_amax_ybase <- filter(rem_dyao, (year >= rem_base_begin) & (year <= rem_base_end)) %>%
                    mutate(age_new = pmin(rem_age_max, age)) %>%
                    group_by(district, year, age_new, origin) %>%
                        summarize(mis = mean(mis),
                            rel = mean(rel)) %>%
                    ungroup() %>%
                    rename(age = age_new)
            

            #-------------------------------------------------------------------
            #dyao: smoothing
            #-------------------------------------------------------------------
            
            #smoothing (direction: age, result should not be below zero)
                dyao_smooth <- group_by(dyao_amax_ybase, district, year, origin) %>%
                        arrange(age) %>%
                        mutate(mis_a = pmax(0, predict(loess(mis ~ age, span = rem_mis_span_dyao, degree = 1, na.action = na.aggregate))),
                            rel_a = pmax(0, predict(loess(rel ~ age, span = rem_rel_span_dyao, degree = 1, na.action = na.aggregate)))) %>%
                    ungroup()
            
            #plot preparation
                smooth_lev <- c("initial", "smoothed")

                temp_initial <- gather(dyao_smooth, `rel`, `mis`, key = category, value = count) %>%
                    mutate(cat = factor(if_else(category == "rel",
                        process_lev[1], process_lev[2]), levels = process_lev),
                        smooth = factor(smooth_lev[1], levels = smooth_lev)) %>%
                    select(district, year, age, origin, cat, smooth, count)

                temp_smooth <- gather(dyao_smooth, `rel_a`, `mis_a`, key = category, value = count) %>%
                    mutate(cat = factor(if_else(category == "rel_a",
                        process_lev[1], process_lev[2]), levels = process_lev),
                        smooth = factor(smooth_lev[2], levels = smooth_lev)) %>%
                    select(district, year, age, origin, cat, smooth, count)

                smooth_plot <- bind_rows(temp_initial, temp_smooth)


            #plot: focus age

                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(rem_base_end, rem_base_begin, by = -7)

                #plot
                    p06 <- function(x){
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

                    pdf(paste0(graph_path, rem_number, "06_processes_dyao_smooth_focus-age.pdf"),
                        width = 15, height = 8)
        
                        for(i in 1:length(uni_d)){plot(p06(uni_d[i]))}
        
                    dev.off()
                    



            #plot: focus years

                #age (subjectively selected)
                    age_plot <- seq(0, 60, by = 20)

                #plot
                    p07 <- function(x){
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

                    
                    pdf(paste0(graph_path, rem_number, "07_processes_dyao_smooth_focus-years.pdf"),
                        width = 15, height = 8)
        
                        for(i in 1:length(uni_d)){plot(p07(uni_d[i]))}
        
                    dev.off()
                    

            
            #-------------------------------------------------------------------
            #dyao: proportion after smoothing
            #-------------------------------------------------------------------
            
            #proportion (within the range from 0 to 100 percent)
                prop_dyao_smooth <- mutate(dyao_smooth, rel_prop_dyao = pmax(0, pmin(100,
                    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop)))))
            
            
            #plot: focus age
            
                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(rem_base_end, rem_base_begin, by = -3)
            
                #plot
                    p08 <- function(x){
                            ggplot(data = filter(prop_dyao_smooth, (year == x))) +
                                geom_line(aes(x = age, y = rel_prop_dyao, color = origin)) +
                                facet_wrap(~district, ncol = 4) +
                                scale_colour_manual(values = col_o) +
                                labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                                ggtitle(as.character(x)) +
                                neutral}
                        
                        pdf(paste0(graph_path, rem_number, "08_proportion_dyao_smooth_focus-age.pdf"),
                            width = 11, height = 10)
            
                            for(i in 1:length(year_plot)){plot(p08(year_plot[i]))}
            
                        dev.off()
                        
            
            
            #plot: focus year
            
                #age (subjectively selected)
                    age_plot <- seq(0, 70, by = 10)
            
                #plot
                    p09 <- function(x){
                            ggplot(data = filter(prop_dyao_smooth, (age == x))) +
                                geom_line(aes(x = year, y = rel_prop_dyao, color = origin)) +
                                facet_wrap(~district, ncol = 4) +
                                scale_colour_manual(values = col_o) +
                                labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                                ggtitle(paste0("age: ", as.character(x))) +
                                neutral}
            

                        pdf(paste0(graph_path, rem_number, "09_proportion_dyao_smooth_focus-years.pdf"),
                            width = 11, height = 10)
            
                            for(i in 1:length(age_plot)){plot(p09(age_plot[i]))}
            
                        dev.off()                    
                    
    

           
          #-------------------------------------------------------------------
          #yao (without d)
          #-------------------------------------------------------------------
          
          #aggregate
              yao <- group_by(dyao_amax_ybase, year, age, origin) %>%
                      summarize(mis = sum(mis),
                          rel = sum(rel)) %>%
                  ungroup()
          
          #smoothing (direction: age)
              yao_smooth <- group_by(yao, year, origin) %>%
                      arrange(age) %>%
                      mutate(mis_a = pmax(0, predict(loess(mis ~ age, span = rem_mis_span_yao, degree = 1, na.action = na.aggregate))),
                          rel_a = pmax(0, predict(loess(rel ~ age, span = rem_rel_span_yao, degree = 1, na.action = na.aggregate)))) %>%
                  ungroup()
          
          #plot preparation
              temp_initial <- gather(yao_smooth, `rel`, `mis`, key = category, value = count) %>%
                  mutate(cat = factor(if_else(category == "rel",
                      process_lev[1], process_lev[2]), levels = process_lev),
                      smooth = factor(smooth_lev[1], levels = smooth_lev)) %>%
                  select(year, age, origin, cat, smooth, count)
          
              temp_smooth <- gather(yao_smooth, `rel_a`, `mis_a`, key = category, value = count) %>%
                  mutate(cat = factor(if_else(category == "rel_a",
                      process_lev[1], process_lev[2]), levels = process_lev),
                      smooth = factor(smooth_lev[2], levels = smooth_lev)) %>%
                  select(year, age, origin, cat, smooth, count)
          
              yao_smooth_plot <- bind_rows(temp_initial, temp_smooth)
          
          
          #plot: focus age
          
              #base years (subjectively, but last year in the plot)
                  year_plot <- seq(rem_base_end, rem_base_begin, by = -7)
          
              #plot
                  p10 <- ggplot(data = filter(yao_smooth_plot, year %in% year_plot)) +
                      geom_line(aes(x = age, y = count, color = cat, linetype = smooth, alpha = smooth)) +
                      facet_grid(origin ~  year) +
                      scale_colour_manual(values = col_6[1:2]) +
                      scale_alpha_manual(values = c(0.3, 1)) +
                      scale_x_continuous(breaks = pretty_breaks()) +
                      labs(x = "age", y = "quantity per year", color = "", linetype = "", alpha = "") +
                      guides(alpha = FALSE) +
                      neutral
              
              ggsave(paste0(graph_path, rem_number, "10_processes_yao_smooth_focus-age.pdf"),
                  plot = p10, width = 15, height = 8)
          
          
          
          #plot: focus years
          
              #age (subjectively selected)
                  age_plot <- seq(0, 60, by = 20)
          
              #plot
                  p11 <- ggplot(data = filter(yao_smooth_plot, age %in% age_plot)) +
                      geom_line(aes(x = year, y = count, color = cat, linetype = smooth, alpha = smooth)) +
                      facet_grid(origin ~  age) +
                      scale_colour_manual(values = col_6[1:2]) +
                      scale_alpha_manual(values = c(0.3, 1)) +
                      scale_x_continuous(breaks = pretty_breaks()) +
                      labs(x = "year", y = "quantity per year", color = "", linetype = "", alpha = "") +
                      guides(alpha = FALSE) +
                      neutral
              
              ggsave(paste0(graph_path, rem_number, "11_processes_yao_smooth_focus-years.pdf"),
                  plot = p11, width = 15, height = 8)              
        
          
          #proportion after smoothing
              prop_yao_smooth <- mutate(yao_smooth, rel_prop_yao = pmax(0, pmin(100,
                  if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop)))))
          
          
          
          #plot: focus age
          
              #base years (subjectively, but last year in the plot)
                  year_plot <- seq(rem_base_end, rem_base_begin, by = -3)
          
              #plot
                  p12 <- ggplot(data = filter(prop_yao_smooth, year %in% year_plot)) +
                      geom_line(aes(x = age, y = rel_prop_yao, color = origin)) +
                      facet_wrap(~ as.factor(year), ncol = 3) +
                      scale_colour_manual(values = col_o) +
                      labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                      expand_limits(y = 0) +
                      neutral
              
              ggsave(paste0(graph_path, rem_number, "12_proportion_yao_smooth_focus-age.pdf"),
                  plot = p12, width = 15, height = 8)              
        
          
          #plot: focus years
          
              #age (subjectively selected)
                  age_plot <- seq(0, 70, by = 10)
          
              #plot
                  p13 <- ggplot(data = filter(prop_yao_smooth, age %in% age_plot)) +
                      geom_line(aes(x = year, y = rel_prop_yao, color = origin)) +
                      facet_wrap(~ as.factor(age), ncol = 4) +
                      scale_colour_manual(values = col_o) +
                      labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                      expand_limits(y = 0) +
                      neutral
        
              ggsave(paste0(graph_path, rem_number, "13_proportion_yao_smooth_focus-years.pdf"),
                  plot = p13, width = 15, height = 8)          

                        
              
 
            #-------------------------------------------------------------------
            #ya (without do)
            #-------------------------------------------------------------------
            
            #Does this make sense? Is aggregation over origin helpful?
            #Yes, this could be helpful, more often for the foreign than the Swiss categories
            
            #aggregate
                ya <- group_by(dyao_amax_ybase, year, age) %>%
                        summarize(mis = sum(mis),
                            rel = sum(rel)) %>%
                    ungroup()
            
            #smoothing (direction: age)
                ya_smooth <- group_by(ya, year) %>%
                        arrange(age) %>%
                        mutate(mis_a = pmax(0, predict(loess(mis ~ age, span = rem_mis_span_ya, degree = 1, na.action = na.aggregate))),
                            rel_a = pmax(0, predict(loess(rel ~ age, span = rem_rel_span_ya, degree = 1, na.action = na.aggregate)))) %>%
                    ungroup()
            
            #plot preparation
                temp_initial <- gather(ya_smooth, `rel`, `mis`, key = category, value = count) %>%
                    mutate(cat = factor(if_else(category == "rel",
                        process_lev[1], process_lev[2]), levels = process_lev),
                        smooth = factor(smooth_lev[1], levels = smooth_lev)) %>%
                    select(year, age, cat, smooth, count)
            
                temp_smooth <- gather(ya_smooth, `rel_a`, `mis_a`, key = category, value = count) %>%
                    mutate(cat = factor(if_else(category == "rel_a",
                        process_lev[1], process_lev[2]), levels = process_lev),
                        smooth = factor(smooth_lev[2], levels = smooth_lev)) %>%
                    select(year, age, cat, smooth, count)
            
                ya_smooth_plot <- bind_rows(temp_initial, temp_smooth)
            
            #plot: focus age
            
                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(rem_base_end, rem_base_begin, by = -3)
            
                #plot
                    p14 <- ggplot(data = filter(ya_smooth_plot, year %in% year_plot)) +
                        geom_line(aes(x = age, y = count, color = cat, linetype = smooth, alpha = smooth)) +
                        facet_wrap( ~year, ncol = 3) +
                        scale_colour_manual(values = col_6[1:2]) +
                        scale_alpha_manual(values = c(0.3, 1)) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "age", y = "quantity per year", color = "", linetype = "", alpha = "") +
                        guides(alpha = FALSE) +
                        neutral
                
                ggsave(paste0(graph_path, rem_number, "14_processes_ya_smooth_focus-age.pdf"),
                    plot = p14, width = 15, height = 8)                
            
            #plot: focus years
            
                #age (subjectively selected)
                    age_plot <- seq(0, 70, by = 10)
            
                #plot
                    p15 <- ggplot(data = filter(ya_smooth_plot, age %in% age_plot)) +
                        geom_line(aes(x = year, y = count, color = cat, linetype = smooth, alpha = smooth)) +
                        facet_wrap( ~age, ncol = 4) +
                        scale_colour_manual(values = col_6[1:2]) +
                        scale_alpha_manual(values = c(0.3, 1)) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "year", y = "quantity per year", color = "", linetype = "", alpha = "") +
                        guides(alpha = FALSE) +
                        neutral
                
                ggsave(paste0(graph_path, rem_number, "15_processes_ya_smooth_focus-years.pdf"),
                    plot = p15, width = 15, height = 8)                
            
            
            #proportion after smoothing
                prop_ya_smooth <- mutate(ya_smooth, rel_prop_ya = pmax(0, pmin(100,
                    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop)))))
            
            
            #plot: focus age
            
                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(rem_base_end, rem_base_begin, by = -3)
            
                #plot
                    p16 <- ggplot(data = filter(prop_ya_smooth, year %in% year_plot)) +
                        geom_line(aes(x = age, y = rel_prop_ya), color = col_6[1]) +
                        facet_wrap(~ as.factor(year), ncol = 3) +
                        labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        expand_limits(y = 0) +
                        neutral
            
                ggsave(paste0(graph_path, rem_number, "16_proportion_ya_smooth_focus-age.pdf"),
                    plot = p16, width = 15, height = 8)                
                
            
            #plot: focus years
            
                #age (subjectively selected)
                    age_plot <- seq(0, 70, by = 10)
            
                #plot
                    p17 <- ggplot(data = filter(prop_ya_smooth, age %in% age_plot)) +
                        geom_line(aes(x = year, y = rel_prop_ya), color = col_6[1]) +
                        facet_wrap(~ as.factor(age), ncol = 4) +
                        labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        expand_limits(y = 0) +
                        neutral
              
                ggsave(paste0(graph_path, rem_number, "17_proportion_ya_smooth_focus-years.pdf"),
                    plot = p17, width = 15, height = 8)
              
            
            #-------------------------------------------------------------------
            #proportion from different aggregation levels
            #-------------------------------------------------------------------
            
            
            #proportion
                temp_dyao <- select(prop_dyao_smooth, district, year, age, origin, mis_a, rel_prop_dyao) %>%
                    rename(mis = mis_a)
                temp_yao <- select(prop_yao_smooth, year, age, origin, rel_prop_yao)
                temp_ya <- select(prop_ya_smooth, year, age, rel_prop_ya)
            
                sources <- c("dyao", "yao", "ya")
            
                prop_agg <- left_join(temp_dyao, temp_yao, by = c("year", "age", "origin")) %>%
                    left_join(temp_ya, by = c("year", "age")) %>%
                    mutate(rel_prop = case_when(mis >= rem_mis_thres_d ~ rel_prop_dyao,
                                                mis >= rem_mis_thres_o ~ rel_prop_yao,
                                                TRUE ~ rel_prop_ya),
                           rel_prop_source = factor(
                                      case_when(mis >= rem_mis_thres_d ~ sources[1],
                                                mis >= rem_mis_thres_o ~ sources[2],
                                                TRUE ~ sources[3]), levels = sources))
            
            
            #plot: entire curves (different aggregations levels)
                entire_curves <- select(prop_agg, district, year, age, origin, rel_prop_dyao, rel_prop_yao, rel_prop_ya) %>%
                    rename(dyao = rel_prop_dyao, yao = rel_prop_yao, ya = rel_prop_ya) %>%
                    gather(`dyao`, `yao`, `ya`, key = category, value = count) %>%
                    mutate(cat = factor(category, levels = sources))
            
            
            
                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(rem_base_end, rem_base_begin, by = -3)
            
                p18 <- function(x){
                    ggplot(data = filter(entire_curves, (district == x) & (year %in% year_plot))) +
                        geom_line(aes(x = age, y = count, color = cat)) +
                        facet_grid(year ~ origin) +
                        scale_colour_manual(values = col_6[1:length(sources)]) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        ggtitle(as.character(x)) +
                        neutral}
            
                
                pdf(paste0(graph_path, rem_number, "18_proportion_different-aggregation-levels.pdf"),
                    width = 12, height = 8)
    
                    for(i in 1:length(uni_d)){plot(p18(uni_d[i]))}
    
                dev.off()                
                
            
            
            #plot: focus age
            
                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(rem_base_end, rem_base_begin, by = -3)
            
                p19 <- function(x){
                    ggplot(data = filter(prop_agg, (district == x) & (year %in% year_plot))) +
                        geom_line(aes(x = age, y = rel_prop), color = "black") +
                        geom_point(aes(x = age, y = rel_prop, color = rel_prop_source)) +
                        facet_grid(year ~ origin) +
                        scale_colour_manual(values = col_6[1:length(sources)]) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        ggtitle(as.character(x)) +
                        neutral}
            
                
                pdf(paste0(graph_path, rem_number, "19_one-proportion-from-different-aggregation-levels_focus-age.pdf"),
                    width = 12, height = 8)
    
                    for(i in 1:length(uni_d)){plot(p19(uni_d[i]))}
    
                dev.off()
                
            
            
            #plot: focus years
            
                #age (subjectively selected)
                    age_plot <- seq(0, 60, by = 20)
            
                p20 <- function(x){
                    ggplot(data = filter(prop_agg, (district == x) & (age %in% age_plot))) +
                        geom_line(aes(x = year, y = rel_prop), color = "black") +
                        geom_point(aes(x = year, y = rel_prop, color = rel_prop_source)) +
                        facet_grid(age ~ origin) +
                        scale_colour_manual(values = col_6[1:length(sources)]) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        ggtitle(as.character(x)) +
                        neutral}
            
                pdf(paste0(graph_path, rem_number, "20_one-proportion-from-different-aggregation-levels_focus-years.pdf"),
                    width = 12, height = 8)
    
                    for(i in 1:length(uni_d)){plot(p20(uni_d[i]))}
    
                dev.off()
                
   

            #-------------------------------------------------------------------
            #smoothing (after different levels of aggregation were brought together
            #-------------------------------------------------------------------
            
            #smoothing (direction: age)
                prop_agg_smooth <- select(prop_agg, district, year, age, origin, rel_prop) %>%
                      group_by(district, year, origin) %>%
                          arrange(age) %>%
                          mutate(prop_smooth = pmax(0, pmin(100, predict(loess(rel_prop ~ age, span = rem_prop_span,
                              degree = 1, na.action = na.aggregate))))) %>%
                    ungroup()
            
            #plot preparation
                plot_agg_smooth <- gather(prop_agg_smooth, `rel_prop`, `prop_smooth`, key = category, value = count) %>%
                    mutate(cat = factor(if_else(category == "rel_prop",
                        smooth_lev[1], smooth_lev[2]), levels = smooth_lev)) %>%
                    select(district, year, age, origin, cat, count)
            
            #plot: focus age
            
                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(rem_base_end, rem_base_begin, by = -3)
            
                #plot
                    p21 <- function(x){
                        ggplot(data = filter(plot_agg_smooth, (district == x) & (year %in% year_plot))) +
                            geom_line(aes(x = age, y = count, linetype = cat, alpha = cat), color = col_6[1]) +
                            facet_grid(year ~ origin) +
                            scale_alpha_manual(values = c(0.3, 1)) +
                            scale_x_continuous(breaks = pretty_breaks()) +
                            labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), 
                                color = "", linetype = "", alpha = "") +
                            guides(alpha = FALSE) +
                            ggtitle(as.character(x)) +
                            neutral}
                    
                    pdf(paste0(graph_path, rem_number, "21_one-proportion_smoothing_focus-age.pdf"),
                        width = 12, height = 8)
        
                        for(i in 1:length(uni_d)){plot(p21(uni_d[i]))}
        
                    dev.off()                    
            
            
            
            #plot: focus years
            
                #age (subjectively selected)
                    age_plot <- seq(0, 60, by = 20)
            
                #plot
                    p22 <- function(x){
                        ggplot(data = filter(plot_agg_smooth, (district == x) & (age %in% age_plot))) +
                            geom_line(aes(x = year, y = count, linetype = cat, alpha = cat), color = col_6[1]) +
                            facet_grid(origin ~  age) +
                            scale_alpha_manual(values = c(0.3, 1)) +
                            scale_x_continuous(breaks = pretty_breaks()) +
                            labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), 
                                 color = "", linetype = "", alpha = "") +
                            guides(alpha = FALSE) +
                            ggtitle(as.character(x)) +
                            neutral}
   
                
                    pdf(paste0(graph_path, rem_number, "22_one-proportion_smoothing_focus-years.pdf"),
                        width = 15, height = 8)
        
                        for(i in 1:length(uni_d)){plot(p22(uni_d[i]))}
        
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
                          window = rem_window_thres, base_t0 = rem_base_begin,
                          szen_t0 = szen_begin, szen_t1 = szen_end,
                          prop_trend = rem_prop_trend, thres_percent = rem_thres_percent,
                          lower_thres = rem_lower_thres, upper_thres = rem_upper_thres)
            
                t1 <- Sys.time()
                t1 - t0
            
            
            #limit prediction period
                prop_pred_begin <- filter(prop_pred, year >= szen_begin)
            
            
            
            #-------------------------------------------------------------------
            #expand to ages beyond age threshold
            #-------------------------------------------------------------------
            
            #age at age threshold
                prop_age_thres <- filter(prop_pred_begin, age == rem_age_max) %>%
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
                    mutate(pred_all_age = if_else(age > rem_age_max, pred_age_thres, pred_roll))
            
            #smoothing (direction: age)
                pred_smooth <- group_by(prop_age, district, year, origin) %>%
                        arrange(age) %>%
                        mutate(pred_smooth = pmax(0, pmin(100, predict(loess(pred_all_age ~ age,
                            span = rem_pred_span, degree = 1, na.action = na.aggregate))))) %>%
                    ungroup()
            
            
            #plot preparation
                plot_pred_smooth <- select(pred_smooth, district, year, age, origin, pred_all_age, pred_smooth) %>%
                    gather(pred_smooth, `pred_all_age`, `pred_smooth`, key = category, value = count) %>%
                    mutate(cat = factor(if_else(category == "pred_all_age",
                        smooth_lev[1], smooth_lev[2]), levels = smooth_lev)) %>%
                    select(district, year, age, origin, cat, count)
            
            #plot: focus age
            
                #base years (subjectively, but last year in the plot)
                    year_plot <- seq(szen_begin, szen_end, by = 7)
            
                #plot
                    p23 <- function(x){
                        ggplot(data = filter(plot_pred_smooth, (district == x) & (year %in% year_plot))) +
                            geom_line(aes(x = age, y = count, linetype = cat, alpha = cat), color = col_6[1]) +
                            facet_grid(origin ~  year) +
                            scale_alpha_manual(values = c(0.3, 1)) +
                            scale_x_continuous(breaks = pretty_breaks()) +
                            labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), 
                                 color = "", linetype = "", alpha = "") +
                            guides(alpha = FALSE) +
                            ggtitle(as.character(x)) +
                            neutral}
            
            
                    pdf(paste0(graph_path, rem_number, "23_prediction_dyao_smooth.pdf"),
                        width = 15, height = 8)
        
                        for(i in 1:length(uni_d)){plot(p23(uni_d[i]))}
        
                    dev.off()            

                    
            
            #-------------------------------------------------------------------
            #plot the final predictions
            #-------------------------------------------------------------------
            
            #past rate (before smoothing)
                rem_dyao_not_smoothed <- mutate(rem_dyao,
                        rel_prop_dyao = pmax(0, pmin(100, if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop))))) %>%
                    select(district, year, age, origin, rel_prop_dyao)
            
            #past and prediction
                rem_past_pred <- as_tibble(expand_grid(
                    district = uni_d,
                    year = date_start:szen_end,
                    age = age_min:age_max,
                    origin = uni_o)) %>%
                    left_join(rem_dyao_not_smoothed, by = c("district", "year", "age", "origin")) %>%
                    left_join(select(pred_smooth, district, year, age, origin, pred_smooth),
                        by = c("district", "year", "age", "origin")) %>%
                    mutate(prop = if_else(year < szen_begin, rel_prop_dyao, pred_smooth))
            
            #colors
                col_time <- c(rep(col_grey, length(date_start:date_end)),
                    colorRampPalette(col_6[1:5])(length(szen_begin:szen_end)))
                # plot(1:length(col_time), 1:length(col_time), col = col_time, pch = 16, cex = 2)
            
            #plot: focus age distribution
                p24 <- function(x){
                    ggplot(data = filter(rem_past_pred, district == x)) +
                        geom_line(aes(x = age, y = prop, color = as.factor(year))) +
                        facet_grid(. ~ origin) +
                        scale_colour_manual(values = col_time) +
                        scale_x_continuous(breaks = pretty_breaks()) +
                        labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "") +
                        ggtitle(as.character(x)) +
                        neutral}
                
                  pdf(paste0(graph_path, rem_number, "24_proportion_dyao_past-future.pdf"),
                      width = 14, height = 7)
      
                      for(i in 1:length(uni_d)){plot(p24(uni_d[i]))}
      
                  dev.off()
                 
            #plot levels
                time_lev <- c("past", "future")
            
            #plot data
                plot_a_past_pred <- mutate(rem_past_pred,
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
            
                p25 <- function(x){
                    ggplot(data = filter(plot_a_past_pred, (district == x) & (year %in% year_plot))) +
                        geom_line(aes(x = age, y = prop, color = as.factor(year), size = time, alpha = time)) +
                        facet_grid(. ~ origin ) +
                        scale_colour_manual(values = col_years_plot) +
                        scale_size_manual(values = c(0.2, 1.2)) +
                        scale_alpha_manual(values = c(0.4, 1)) +
                        labs(x = "age", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "year", size = "") +
                        guides(alpha = FALSE) +
                        ggtitle(as.character(x)) +
                        neutral}
                
                pdf(paste0(graph_path, rem_number, "25_proportion_dyao_past-future_focus-age.pdf"),
                    width = 14, height = 7)
    
                    for(i in 1:length(uni_d)){plot(p25(uni_d[i]))}
    
                dev.off()                
            
            
            
            #plot: focus years
            
                #age (subjectively selected)
                    age_plot_pred <- seq(0, 60, by = 20)
            
                #colors
                    col_age_pred <- colorRampPalette(col_6[1:5])(length(age_plot_pred))
            
                p26 <- function(x){
                    ggplot(data = filter(plot_a_past_pred, (district == x) & (age %in% age_plot_pred))) +
                        geom_vline(xintercept = c(rem_base_begin, rem_base_end), color = col_grey, linetype = 1) +
                        geom_line(aes(x = year, y = prop, color = as.factor(age))) +
                        facet_grid(. ~ origin) +
                        scale_colour_manual(values = col_age_pred) +
                        labs(x = "year", y = paste0("proportion in % (relocation on ", mig_name, "*)"), color = "age") +
                        ggtitle(as.character(x)) +
                        neutral}

            
                pdf(paste0(graph_path, rem_number, "26_proportion_dyao_past-future_focus-years.pdf"),
                    width = 14, height = 6)
    
                    for(i in 1:length(uni_d)){plot(p26(uni_d[i]))}
    
                dev.off()  
                
            
            #-------------------------------------------------------------------
            #export the results
            #-------------------------------------------------------------------
            
            #proportion: prediction
                rel_ex_data <- mutate(pred_smooth, prop_rel_dyao = round(pred_smooth, round_prop)) %>%
                    select(district, year, age, origin, prop_rel_dyao) %>%
                    arrange(district, year, age, origin)
            
            #export
                write_csv(rel_ex_data, ex_path)
            
                    
        #output (to get an idea of the exported output)
            return(list(rel_ex_data))

                }
     
