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
    setwd("O:/Projekte/BevSzen/2020_sandkasten_bad-rok/2020phase2/")

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
    car_path <- "O:/Projekte/BevSzen/2020_sandkasten_bad-rok/2020phase2/2_Data/1_Input/KaReB.csv"
    
    
#capacity and reserves: data
    car_dat <- read_csv(car_path) %>% 
        rename(year = PublJahr) %>% 
        gather(car_initial, key = initial, value = area) %>% 
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
    