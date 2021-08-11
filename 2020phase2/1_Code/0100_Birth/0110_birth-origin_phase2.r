#-------------------------------------------------------------------
#Birth: Origin of the Babies, phase 2
#
#
#
#rok/bad, February 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#working directory
  setwd(paste0(here::here(), "/2020phase2/"))

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")

#birth: result path (for images)
    bir_res <- "3_Results/0100_Birth/"
    
#birth: export path (for future rates)
    bir_exp <- "2_Data/4_Rates/"  

    
#-------------------------------------------------------------------
#import and data preparation
#-------------------------------------------------------------------

#birth
    #age: only births of women at 'fertile age'
    #originb: origin of baby
    #origin: origin of mother
    
    bir <- read_csv(bir_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) %>% 
        filter((age >= bir_age_begin) & (age <= bir_age_end)) %>%         
        left_join(look_dis, by = "QuarCd") %>%         
        mutate(originb = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            origin = factor(if_else(HerkunftMutterCd == 1, uni_o[1], uni_o[2]), uni_o),               
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, originb, origin, bir) %>%     
        group_by(district, year, age, originb, origin) %>% 
            summarize(bir = sum(bir)) %>% 
        ungroup() 

       
#-------------------------------------------------------------------
#origin change: plots
#-------------------------------------------------------------------
      
#WHY plots?
#find out the relevant processes/variables
          
#origin change: data preparation
    cha <- spread(bir, key = originb, value = bir) %>%    
        replace_na(list(Swiss = 0, foreign = 0)) %>% 
        mutate(change = if_else(origin == "Swiss", foreign, Swiss),
            nochange = if_else(origin == "Swiss", Swiss, foreign),
            total = change + nochange)  
    
#change by year, origin
    cha_yo <- group_by(cha, year, origin) %>% 
            summarize(change = sum(change), 
                      total = sum(total)) %>% 
        ungroup() %>% 
        mutate(cha_yo = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))
    
    year5 <- cha_yo$year[cha_yo$year %% 5 == 0]    
    
    # p170 <- ggplot(data = cha_yo) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +     
    #     geom_line(aes(x = year, y = cha_yo, color = origin)) +
    #     geom_point(aes(x = year, y = cha_yo, color = origin)) +      
    #     labs(x = "year", y = "origin change in %", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +      
    #     scale_colour_manual(values = col_o) +
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0170_origin-change_by-year-origin.pdf"), 
    #     plot = p170, width = 7, height = 4)   
    
    sszplot(cha_yo,
            aes_x = "year", aes_y = "cha_yo", aes_col = "origin",
            i_x = year5,
            labs_x = "year", labs_y = "origin change in %",
            name = "0170_origin-change_by-year-origin")
    
#change by year, age1, origin
    cha_ya1o <- left_join(cha, look_a1, by = "age") %>%  
        group_by(year, age_1, origin) %>% 
            summarize(change = sum(change), 
                      total = sum(total)) %>% 
        ungroup() %>% 
        mutate(cha_ya1o = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))    
    
    # p171 <- ggplot(data = cha_ya1o) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") + 
    #     geom_line(aes(x = year, y = cha_ya1o, color = origin)) +
    #     geom_point(aes(x = year, y = cha_ya1o, color = origin)) +      
    #     facet_grid(~age_1) + 
    #     labs(x = "year", y = "origin change in %", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +      
    #     scale_colour_manual(values = col_o) +
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0171_origin-change_by-year-age1-origin.pdf"), 
    #     plot = p171, width = 12, height = 4)   
    
    sszplot(cha_ya1o,
            aes_x = "year", aes_y = "cha_ya1o", aes_col = "origin",
            i_x = year5,
            labs_x = "year", labs_y = "origin change in %",
            wrap = "age_1",
            name = "0171_origin-change_by-year-age1-origin", width = 12)
            
    
#change by year, age2, origin
    cha_ya2o <- left_join(cha, look_a2, by = "age") %>%  
        group_by(year, age_2, origin) %>% 
            summarize(change = sum(change), 
                      total = sum(total)) %>% 
        ungroup() %>% 
        mutate(cha_ya2o = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))    
    
    # p172 <- ggplot(data = cha_ya2o) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") + 
    #     geom_line(aes(x = year, y = cha_ya2o, color = origin)) +
    #     geom_point(aes(x = year, y = cha_ya2o, color = origin)) +      
    #     facet_grid(~age_2) + 
    #     labs(x = "year", y = "origin change in %", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +      
    #     scale_colour_manual(values = col_o) +
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0172_origin-change_by-year-age2-origin.pdf"), 
    #     plot = p172, width = 12, height = 4) 
    
    sszplot(cha_ya2o,
            aes_x = "year", aes_y = "cha_ya2o", aes_col = "origin",
            i_x = year5,
            labs_x = "year", labs_y = "origin change in %",
            wrap = "age_2", col = nlevels(cha_ya2o$age_2),
            name = "0172_origin-change_by-year-age2-origin", width = 12)
    
                
    
#change by district, year, origin
    cha_dyo <- group_by(cha, district, year, origin) %>% 
            summarize(change = sum(change), 
                      total = sum(total)) %>% 
        ungroup() %>% 
        mutate(cha_dyo = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))    
    
    # p173 <- ggplot(data = cha_dyo) + 
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +       
    #     geom_line(aes(x = year, y = cha_dyo, color = origin)) + 
    #     geom_point(aes(x = year, y = cha_dyo, color = origin)) +        
    #     facet_wrap(~district, ncol = 4) +
    #     labs (x = "age", y = "origin change in %", color = "") +  
    #     scale_colour_manual(values = col_o) + 
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0173_origin-change_by-district-year-origin.pdf"), 
    #     plot = p173, width = 12, height = 14)     
    
    sszplot(cha_dyo,
            aes_x = "year", aes_y = "cha_dyo", aes_col = "origin",
            i_x = year5,
            labs_x = "age", labs_y = "origin change in %",
            wrap = "district", col = 4,
            name = "0173_origin-change_by-district-year-origin", width = 12, height = 14)
    
#change by district, year, age1 (foreign only, since few cases for Swiss)
    cha_dya1f <- left_join(cha, look_a1, by = "age") %>% 
        filter(origin == "foreign") %>% 
        group_by(district, year, age_1) %>% 
            summarize(change = sum(change), 
                      total = sum(total)) %>% 
        ungroup() %>% 
        mutate(cha_dya1f = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))        
    
    
    # p174 <- ggplot(data = cha_dya1f) + 
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +         
    #     geom_line(aes(x = year, y = cha_dya1f, color = age_1)) + 
    #     geom_point(aes(x = year, y = cha_dya1f, color = age_1)) +   
    #     scale_colour_manual(values = colorRampPalette(col_6)(length(age_1t))) + 
    #     facet_wrap(~district, ncol = 4) +
    #     labs (x = "", y = "origin change in %", color = "age") + 
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0174_origin-change_by-district-year-age1-foreign.pdf"), 
    #     plot = p174, width = 12, height = 14)      
    
    sszplot(cha_dya1f,
            aes_x = "year", aes_y = "cha_dya1f", aes_col = "age_1",
            i_x = year5,
            labs_x = "age", labs_y = "origin change in %", labs_col = "age",
            wrap = "district", col = 4,
            name = "0174_origin-change_by-district-year-age1-foreign", width = 12, height = 14)
    
       

#-------------------------------------------------------------------
#origin change (cha): model
#-------------------------------------------------------------------
    
#base years
    cha_base <- filter(cha_dyo, (year >= bir_cha_base_begin) & (year <= bir_cha_base_end))

#constrained regression 
    #(proportion of linear model and mean, within bandwidth)

    cha_pred <- con_reg(data = cha_base, x = "year", y = "cha_dyo", 
              group_cols = c("district", "origin"),
              window = bir_cha_window_thres, base_t0 = bir_cha_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end, 
              prop_trend = bir_cha_prop_trend, thres_percent = bir_cha_thres_percent, 
              lower_thres = bir_cha_lower_thres, upper_thres = bir_cha_upper_thres) %>% 
        #with the input data (WHY? to assess the regression)
            full_join(select(cha_dyo, district, year, origin, cha_dyo), 
                by = c("district", "year", "origin")) %>%  
        #output generation, one rate variable for both future and past
            mutate(cha_all = if_else(year <= bir_cha_base_end, cha_dyo, pred_roll)) %>% 
            arrange(district, year, origin)
  
  
#plot
    # p176 <- ggplot(data = cha_pred) + 
    #     geom_vline(xintercept = c(bir_cha_base_begin, bir_cha_base_end), col = col_grey) +       
    #     geom_line(aes(x = year, y = cha_all, color = origin)) + 
    #     facet_wrap(~district, ncol = 4) +
    #     labs (x = "age", y = "origin change in %", color = "") +  
    #     scale_colour_manual(values = col_o) + 
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0175_origin-change_by-district-year-origin_past-future.pdf"), 
    #     plot = p176, width = 12, height = 14)      

    sszplot(cha_pred,
        aes_x = "year", aes_y = "cha_all", aes_col = "origin",
        i_x = c(bir_cha_base_begin, bir_cha_base_end),
        labs_x = "age", labs_y = "origin change in %",
        wrap = "district", col = 4,
        geom = "line",
        name = "0175_origin-change_by-district-year-origin_past-future", width = 12, height = 14)
    

       
#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#prepare the export data
    cha_ex <- mutate(cha_pred, cha = round(cha_all, round_rate)) %>% 
        filter(year >= szen_begin) %>%       
        select(district, year, origin, cha) %>% 
        arrange(district, year, origin)

#export
    write_csv(cha_ex, paste0(bir_exp, "birth_origin-change_future.csv"))
   
        
    
