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
        pivot_longer(cols = car_initial, names_to = "initial", values_to = "area") %>%      
        left_join(look_car, by = "initial") %>% 
        mutate(distnum = as.numeric(QuarCd)) %>% 
        left_join(look_reg, by = "distnum") %>% 
        mutate(cat = factor(category, car_category),
            residence = factor(case_when(WohnanteilCd ==  1 ~ uni_e[1], 
                                            WohnanteilCd ==  2 ~ uni_e[2],
                                            TRUE ~ uni_e[3])), 
            plot = factor(if_else(ArealCd == 1, uni_p[1], uni_p[2]), uni_p), 
            owner = factor(if_else(EigentumGrundstkCd == 1, uni_w[1], uni_w[2]), uni_w),
            ha = area / 10000) %>% 
        select(year, residence, plot, district, owner, cat, ha)
    
        
    
#-------------------------------------------------------------------
#plot (entire city, all owner categories)
#-------------------------------------------------------------------
   
#aggregate (year, residence, plot)
    car_yep <- group_by(car_dat, year, residence, plot, cat) %>% 
            summarize(ha = sum(ha)) %>% 
        ungroup()
    
#plot    
    p800 <- ggplot(data = car_yep, aes(x = year, y = ha, 
            color = residence, linetype = plot)) + 
        geom_line() +  
        geom_point() +
        facet_wrap(~cat, scales = "free", ncol = 2) +
        scale_y_continuous(breaks = pretty_breaks()) +  
        scale_colour_manual(values = col_e) +        
        labs(x = "year of data delivery", y = "area (in ha)", 
            color = "", linetype = "") + 
        expand_limits(y = 0) +          
        neutral + theme(panel.spacing.x = unit(1, "lines"))
    
    ggsave(paste0(car_res, "0800_entire-city.pdf"), 
        plot = p800, width = 10, height = 8)  

    
#-------------------------------------------------------------------
#plot (entire city, by owner)
#-------------------------------------------------------------------
       
#aggregate (year, residence, plot, owner)
    car_yepw <- group_by(car_dat, year, residence, plot, owner, cat) %>% 
            summarize(ha = sum(ha)) %>% 
        ungroup()
    
#plot    
    p801 <- ggplot(data = car_yepw, aes(x = year, y = ha, 
            color = residence, linetype = plot)) + 
        geom_line() +  
        geom_point() +
        facet_grid(cat ~ owner, scales = "free_y") +
        scale_y_continuous(breaks = pretty_breaks()) +  
        scale_colour_manual(values = col_e) +        
        labs(x = "year of data delivery", y = "area (in ha)", 
            color = "", linetype = "") + 
        expand_limits(y = 0) +          
        neutral + theme(panel.spacing.x = unit(1, "lines"))    
    
    ggsave(paste0(car_res, "0801_entire-city_by-owner.pdf"), 
        plot = p801, width = 8, height = 10) 
    
    
    
#-------------------------------------------------------------------
#plot (by district, all owner categories)
#-------------------------------------------------------------------
   
#aggregate (district, year, residence, plot)
    car_dyep <- group_by(car_dat, district, year, residence, plot, cat) %>% 
            summarize(ha = sum(ha)) %>% 
        ungroup()
    
#plot  
    p802 <- function(x){
        ggplot(data = filter(car_dyep, district == x), 
            aes(x = year, y = ha, color = residence, linetype = plot)) + 
        geom_line() +  
        geom_point() +
        facet_wrap(~cat, scales = "free", ncol = 2) +
        scale_y_continuous(breaks = pretty_breaks()) +  
        scale_colour_manual(values = col_e) +        
        labs(x = "year of data delivery", y = "area (in ha)", 
            color = "", linetype = "") + 
        expand_limits(y = 0) +
        ggtitle(as.character(x)) +        
        neutral + theme(panel.spacing.x = unit(1, "lines"))}    
      
    pdf(paste0(car_res, "0802_districts.pdf"),
        width = 10, height = 8)

        lapply(uni_d, p802)

    dev.off()   
          
      
    
#-------------------------------------------------------------------
#plot (by district, by owner)
#-------------------------------------------------------------------
       
#aggregate (district, year, residence, plot, owner)
    car_dyepw <- group_by(car_dat, district, year, residence, plot, owner, cat) %>% 
            summarize(ha = sum(ha)) %>% 
        ungroup()
    
#plot    
    p803 <- function(x){
        ggplot(data = filter(car_dyepw, district == x), 
            aes(x = year, y = ha, color = residence, linetype = plot)) + 
        geom_line() +  
        geom_point() +
        facet_grid(cat ~ owner, scales = "free_y") +
        scale_y_continuous(breaks = pretty_breaks()) +  
        scale_colour_manual(values = col_e) +        
        labs(x = "year of data delivery", y = "area (in ha)", 
            color = "", linetype = "") + 
        expand_limits(y = 0) +          
        ggtitle(as.character(x)) +        
        neutral + theme(panel.spacing.x = unit(1, "lines"))}     
    
    pdf(paste0(car_res, "0803_districts_by-owner.pdf"),
        width = 10, height = 8)

        lapply(uni_d, p803)

    dev.off()  
    
    
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
    
#conversion from total to living area
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
            if_else(usage == 0, 0, usage / reserve_new * 100) + car_pp)))
    
    
    
    
    
    
    
    
        
    
    
    
    car_uti
      tmp4$flaeche.kap <- tmp4$flaeche.kap / 85 * kareb.ausbau
    tmp4$flaeche.ina <- tmp4$flaeche.ina / 85 * kareb.ausbau  
  
    