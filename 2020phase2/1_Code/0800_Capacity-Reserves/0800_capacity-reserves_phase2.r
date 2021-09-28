#-------------------------------------------------------------------
#Capacity, Reserves
#
#
#
#rok/bad, August 2021
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
    car_res <- "3_Results/0800_Capacity-Reserves/"
    
#export path (for future rates)
    car_exp <- "2_Data/4_Rates/" 
    


#-------------------------------------------------------------------
#import and data preparation
#-------------------------------------------------------------------
    
#temporary path (since data not on open data yet)    
    car_path <- "2_Data/1_Input/KaReB.csv"
    

#capacity and reserves: data
    car_dat <- read_csv(car_path) %>%
      rename(year = PublJahr) %>%
      pivot_longer(cols = car_initial,
                   names_to = "initial",
                   values_to = "area") %>%
      left_join(look_car, by = "initial") %>%
      mutate(distnum = as.numeric(QuarCd)) %>%
      left_join(look_reg, by = "distnum") %>%
      mutate(
        cat = factor(category, car_category),
        residence = factor(
          case_when(WohnanteilCd ==  1 ~ uni_e[1],
                    WohnanteilCd ==  2 ~ uni_e[2],
                    TRUE ~ uni_e[3])
        ),
        plot = factor(if_else(ArealCd == 1, uni_p[1], uni_p[2]), uni_p),
        owner = factor(if_else(EigentumGrundstkCd == 1, uni_w[1], uni_w[2]), uni_w),
        ha = area / 10000
      ) %>%
      select(year, residence, plot, district, owner, cat, ha)
    
        
    
#-------------------------------------------------------------------
#plot (entire city, all owner categories)
#-------------------------------------------------------------------
   
#aggregate (year, residence, plot)
    car_yep <- group_by(car_dat, year, residence, plot, cat) %>%
      summarize(ha = sum(ha), .groups = "drop")
    
#plot    
    sszplot(car_yep,
            aes_x = "year", aes_y = "ha", aes_col = "residence", aes_ltyp = "plot",
            geom = c("line", "point"),
            wrap = "cat", ncol = 2, gridscale = "free",
            labs_x = "year of data delivery", labs_y = "area (in ha)",
            scale_y = c(0, NA),
            name = "0800_entire-city",
            width = 10, height = 8)

    
#-------------------------------------------------------------------
#plot (entire city, by owner)
#-------------------------------------------------------------------
       
#aggregate (year, residence, plot, owner)
    car_yepw <- group_by(car_dat, year, residence, plot, owner, cat) %>%
      summarize(ha = sum(ha), .groups = "drop")
    
#plot    
    sszplot(car_yepw,
            aes_x = "year", aes_y = "ha", aes_col = "residence", aes_ltyp = "plot",
            geom = c("line", "point"),
            grid = c("cat", "owner"), gridscale = "free_y",
            labs_x = "year of data delivery", labs_y = "area (in ha)",
            scale_y = c(0, NA),
            name = "0801_entire-city_by-owner",
            width = 10, height = 8)    
    
#-------------------------------------------------------------------
#plot (by district, all owner categories)
#-------------------------------------------------------------------
   
#aggregate (district, year, residence, plot)
    car_dyep <- group_by(car_dat, district, year, residence, plot, cat) %>% 
            summarize(ha = sum(ha), .groups = "drop")
    
#plot  
    sszplot(car_dyep,
            aes_x = "year", aes_y = "ha", aes_col = "residence", aes_ltyp = "plot",
            geom = c("line", "point"),
            wrap = "cat", gridscale = "free", ncol = 2,
            labs_x = "year of data delivery", labs_y = "area (in ha)",
            scale_y = c(0, NA),
            name = "0802_districts",
            width = 10, height = 8,
            multi = uni_d)           
      
    
#-------------------------------------------------------------------
#plot (by district, by owner)
#-------------------------------------------------------------------
       
#aggregate (district, year, residence, plot, owner)
    car_dyepw <- group_by(car_dat, district, year, residence, plot, owner, cat) %>% 
            summarize(ha = sum(ha), .groups = "drop")
    
#plot    
    sszplot(car_dyepw,
            aes_x = "year", aes_y = "ha", aes_col = "residence", aes_ltyp = "plot",
            geom = c("line", "point"),
            grid = c("cat", "owner"), gridscale = "free_y",
            labs_x = "year of data delivery", labs_y = "area (in ha)",
            scale_y = c(0, NA),
            name = "0803_districts_by-owner",
            width = 10, height = 8,
            multi = uni_d)
    
#-------------------------------------------------------------------
#combine information (with the parameters)
#-------------------------------------------------------------------
   
#current data (and no area values below zero)    
    curr <- mutate(car_dyepw, area = pmax(0, ha)) %>% 
        filter(year == max(year)) %>% 
        select(-c(year, ha))     
    
#residence portion
     residence <- mutate(curr, resi = case_when(residence == uni_e[1] ~ "min", 
        residence ==  uni_e[2] ~ "real", TRUE ~ "max")) %>% 
        select(-residence) %>% 
        pivot_wider(names_from = resi, values_from = area) %>% 
        mutate(car_resid = car_resi, 
            area = if_else(car_resid >= 0, real + (max - real) * car_resid/100,
            real + (real - min) * car_resid/100)) %>% 
        select(district, plot, owner, cat, area)    
   
#plot construction
    pcon <- mutate(residence, pcon = if_else(plot == uni_p[1], "with", "without")) %>% 
        select(-plot) %>% 
        pivot_wider(names_from = pcon, values_from = area) %>% 
        mutate(area = without + (with - without) * car_plot/100) %>% 
        select(district, owner, cat, area)    
    
#conversion from total to living area (e.g. elevators, stairs)
    living <- mutate(pcon, living = area / ((100 + car_sc)/100)) %>% 
        select(-area)
    
#degree of utilization, proportion of usage
    usage_prop <- mutate(living, area = if_else(cat %in% car_category[c(1, 4)], 
        living / car_uti_input * car_uti, living)) %>%
        select(-living) %>% 
        pivot_wider(names_from = cat, values_from = area) %>% 
        #new reserves, usage proportion
        mutate(reserve_new = capacity - buildings, 
            usage_prop = pmax(0, pmin(100, 
                if_else(reserve_new == 0, 0, usage / reserve_new * 100) + car_pp)))
    

#plot: reserves (area)
    p804 <- ggplot(data = usage_prop, 
                aes(x = district, y = reserve_new, fill = owner)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        scale_x_discrete(limits = rev(uni_d)) +
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_w) +  
        coord_flip() +  
        labs(x = "", y = "reserves (in ha)", fill = "") +        
        neutral
      
    ggsave(paste0(car_res, "0804_reserves_by-district.pdf"), 
        plot = p804, width = 8, height = 7)
    
    sszplot(usage_prop,
            aes_x = "district", aes_y = "reserve_new", aes_fill = "owner",
            geom = "col",
            labs_y = "reserves (in ha)", labs_x = "",
            scale_x = rev(uni_d),
            name = "0804_reserves_by-district",
            width = 8, height = 7)
    

#plot: usage (proportion)
    p805 <- ggplot(data = usage_prop, 
                aes(x = district, y = usage_prop, fill = owner)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        scale_x_discrete(limits = rev(uni_d)) +
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_w) +  
        coord_flip() +  
        labs(x = "", y = "usage proportion (in % of the reserves)", fill = "") +        
        neutral    

    ggsave(paste0(car_res, "0805_usage-proportion_by-district.pdf"), 
        plot = p805, width = 8, height = 7) 
        
    

#plot: usage (area)
    p806 <- ggplot(data = usage_prop, 
                aes(x = district, y = usage, fill = owner)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        scale_x_discrete(limits = rev(uni_d)) +
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_w) +  
        coord_flip() +  
        labs(x = "", y = "usage area (in ha)", fill = "") +        
        neutral 
      
    ggsave(paste0(car_res, "0806_usage-area_by-district.pdf"), 
        plot = p806, width = 8, height = 7) 
        
            
    
    
#-------------------------------------------------------------------
#usage per year
#-------------------------------------------------------------------

#exp-distribution over years
    exp_y <- tibble(year = szen_begin:szen_end) %>% 
        mutate(delta = year - date_end, 
            exp_y = exp(car_lamda * delta))

#used reserves until a certain year (parameter: car_y)
    exp_y_sum <- filter(exp_y, year <= car_y) %>% 
        summarize(exp_y_sum = sum(exp_y))
    
    # tail(exp_y)
    # sum(exp_y$exp_y)
    
    
#usage: proportion of the reserves per year
    usage_y_prop <- as_tibble(expand_grid(
            district = uni_d, 
            year = szen_begin:szen_end,
            owner = uni_w, 
            exp_y_sum = exp_y_sum$exp_y_sum)) %>% 
        left_join(select(usage_prop, district, owner, reserve_new, usage_prop), 
            by = c("district", "owner")) %>% 
        left_join(select(exp_y, year, exp_y), by = "year") %>% 
        mutate(usage_y_prop = usage_prop * exp_y / exp_y_sum)
    
   # tail(usage_y_prop)
    
#check: sum until year 'car_y' 
    # check if sum of the proportions per year equals the total proportion
    check <- filter(usage_y_prop, year <= car_y) %>% 
        group_by(district, owner) %>% 
        summarize(usage_prop = max(usage_prop),
            usage_prop_y_sum = sum(usage_y_prop)) %>% 
        ungroup()
    
#the total sum (until year 'szen_end') 
    #the total should not exceed 100 percent of the reserves
    
    total <- group_by(usage_y_prop, district, owner) %>% 
            summarize(total = sum(usage_y_prop)) %>% 
        ungroup() %>% 
        filter(total > 100) %>% 
        mutate(diff = total - 100) %>% 
        left_join(usage_y_prop, by = c("district", "owner")) %>% 
        group_by(district, owner) %>% 
            arrange(district, owner, desc(year)) %>% 
            mutate(usage_cumu = cumsum(usage_y_prop),
                usage_corr = if_else(usage_cumu < diff, 0, usage_y_prop)) %>% 
        ungroup() %>% 
        select(district, owner, year, usage_corr) %>% 
        right_join(usage_y_prop, by = c("district", "owner", "year")) %>% 
        arrange(district, owner, year) %>% 
        mutate(usage_y_prop_corr = if_else(is.na(usage_corr), usage_y_prop, usage_corr),
            usage_area = reserve_new * usage_y_prop_corr) %>% 
        select(district, year, owner, reserve_new, usage_y_prop_corr, usage_area) %>% 
        rename(reserve = reserve_new, usage_prop = usage_y_prop_corr)
    
    
#plot: usage (proportion)
    p807 <- ggplot(data = total) + 
        geom_line(aes(x = year, y = usage_prop, color = owner)) + 
        facet_wrap(~district, ncol = 4) +      
        scale_colour_manual(values = col_w) +  
        scale_x_continuous(breaks = pretty_breaks()) + 
        labs(x = "", y = "usage proportion (in % of the reserves per year)", color = "") +  
        expand_limits(y = 0) +         
        neutral +
        theme(panel.spacing = unit(1.5, "lines"))      
    
    ggsave(paste0(car_res, "0807_usage-proportion_by-district-year.pdf"), 
        plot = p807, width = 12, height = 14) 
    
    
#plot: usage (area)
    p808 <- ggplot(data = total) + 
        geom_line(aes(x = year, y = usage_area, color = owner)) + 
        facet_wrap(~district, ncol = 4) +      
        scale_colour_manual(values = col_w) +  
        scale_x_continuous(breaks = pretty_breaks()) + 
        labs(x = "", y = "usage area (in ha per year)", color = "") +  
        expand_limits(y = 0) +         
        neutral +
        theme(panel.spacing = unit(1.5, "lines"))
    
    ggsave(paste0(car_res, "0808_usage-area_by-district-year.pdf"), 
        plot = p808, width = 12, height = 14) 
    
    
#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#usage (area in ha)  
    car_ex_data <- mutate(total, usage_ha = round(usage_area, round_area)) %>% 
        select(district, year, owner, usage_ha) %>% 
        arrange(district, year, owner)
      
#export
    write_csv(car_ex_data, paste0(car_exp, "usage_area.csv"))    


    
    
    
    
    
    
    
    
    
    