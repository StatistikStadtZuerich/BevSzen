#-------------------------------------------------------------------
#Projects (i.e. construction projects)
#
#
#
#rok/bad, October 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------
if (!exists("para")) {
#working directory
    library(here)
    setwd(paste0(here(), "/2020phase2/"))

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")
}
#result path (for images)
    pro_res <- "3_Results/1100_Projects/"
    
#export path (for future rates)
    pro_exp <- exp_path
    
    #temporary path (since data not on open data yet)    
    pro_path <- "2_Data/1_Input/BEV347OD3470.csv"

#-------------------------------------------------------------------
#import, data preparation
#-------------------------------------------------------------------
  
#indicator 
    
    
#projects: data
    pro_dat <- read_csv(pro_path) %>% 
        rename(year = StichtagDatJahr) %>% 
        left_join(look_pro, by = c("StatusCd" = "code")) %>% 
        mutate(distnum = as.numeric(QuarCd)) %>%       
        left_join(look_reg, by = "distnum") %>%    
        mutate(owner = factor(if_else(EigentumCd == 1, uni_w[1], uni_w[2]), uni_w)) %>% 
        select(district, year, owner, status, WhgNeu, WhgAbbruch) %>%        
        pivot_longer(cols = c("WhgNeu", "WhgAbbruch"), names_to = "ind", 
            values_to = "apartments") %>%      
        mutate(indicator = factor(if_else(ind == "WhgNeu", uni_i[1], uni_i[2]), uni_i)) %>%     
        select(district, year, owner, status, indicator, apartments)  
    
#with all possible cases
    pro_all <- as_tibble(expand_grid(district = uni_d,
                                     year = min(pro_dat$year):max(pro_dat$year),
                                     owner = uni_w,
                                     status = uni_t,
                                     indicator = uni_i)) %>%
      left_join(pro_dat,
                by = c("district", "year", "owner", "status", "indicator")) %>%
      replace_na(list(apartments = 0))
      
#-------------------------------------------------------------------
#indicators (new/removed apartments) by year
#-------------------------------------------------------------------
      
#by year
    pro_y <- group_by(pro_all, year, indicator) %>% 
            summarize(apartments = sum_NA(apartments),
                      .groups = "drop")
    
#plot    
    sszplot(pro_y,
            aes_x = "year", aes_y = "apartments", aes_fill = "indicator",
            geom = "col",
            labs_x = "",
            name = "1100_projects_by-year",
            width = 7, height = 4)   
    
#-------------------------------------------------------------------
#indicators by year and status
#-------------------------------------------------------------------
      
#by year, status
    pro_yt <- group_by(pro_all, year, status, indicator) %>% 
            summarize(apartments = sum_NA(apartments)) %>% 
        ungroup()
    
#plot    
    p1101 <- ggplot(data = pro_yt, 
                aes(x = as.factor(year), y = apartments, fill = indicator)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        labs(x = "", y = "apartments", fill = "") +       
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_i) + 
        facet_grid(status ~ .) +
        neutral
      
    ggsave(paste0(pro_res, "1101_projects_by-year-status.pdf"), 
        plot = p1101, width = 6, height = 12)       
    
    sszplot(pro_yt,
            aes_x = "year", aes_y = "apartments", aes_fill = "indicator",
            geom = "col",
            labs_x = "",
            name = "1101_projects_by-year-status",
            width = 7, height = 4)
    
#-------------------------------------------------------------------
#indicators by year and owner
#-------------------------------------------------------------------
      
#by year, owner
    pro_yw <- group_by(pro_all, year, owner, indicator) %>% 
            summarize(apartments = sum_NA(apartments)) %>% 
        ungroup()
    
#plot    
    p1102 <- ggplot(data = pro_yw, 
                aes(x = as.factor(year), y = apartments, fill = indicator)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        labs(x = "", y = "apartments", fill = "") +       
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_i) + 
        facet_grid(. ~ owner) +
        neutral
      
    ggsave(paste0(pro_res, "1102_projects_by-year-owner.pdf"), 
        plot = p1102, width = 10, height = 4)       
    
  
          
#-------------------------------------------------------------------
#indicators by year, owner, and status
#-------------------------------------------------------------------
      
#by year, owner, status
    pro_ywt <- group_by(pro_all, year, owner, status, indicator) %>% 
            summarize(apartments = sum_NA(apartments)) %>% 
        ungroup()
    
#plot    
    p1103 <- ggplot(data = pro_ywt, 
                aes(x = as.factor(year), y = apartments, fill = indicator)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        labs(x = "", y = "apartments", fill = "") +       
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_i) + 
        facet_grid(status ~ owner) +
        neutral
      
    ggsave(paste0(pro_res, "1103_projects_by-year-owner-status.pdf"), 
        plot = p1103, width = 10, height = 12)       
    
        
#-------------------------------------------------------------------
#indicators by district and year
#-------------------------------------------------------------------
      
#by district, year
    pro_dy <- group_by(pro_all, district, year, indicator) %>% 
            summarize(apartments = sum_NA(apartments)) %>% 
        ungroup()
    
#plot    
    p1104 <- ggplot(data = pro_dy, 
                aes(x = as.factor(year), y = apartments, fill = indicator)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        labs(x = "", y = "apartments", fill = "") +       
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_i) + 
        facet_wrap(~ district, ncol = 4) +
        neutral + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    ggsave(paste0(pro_res, "1104_projects_by-district-year.pdf"), 
        plot = p1104, width = 10, height = 12) 
    
    
#-------------------------------------------------------------------
#indicators by district, year, and owner
#-------------------------------------------------------------------
      
#by district, year, owner
    pro_dyw <- group_by(pro_all, district, year, owner, indicator) %>% 
            summarize(apartments = sum_NA(apartments)) %>% 
        ungroup()
        
#plot    
    p1105 <- function(x){
        ggplot(data = filter(pro_dyw, owner == x), 
            aes(x = as.factor(year), y = apartments, fill = indicator)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
        labs(x = "", y = "apartments", fill = "") +       
        scale_y_continuous(breaks = pretty_breaks()) +        
        scale_fill_manual(values = col_i) + 
        facet_wrap(~ district, ncol = 4) +
        ggtitle(as.character(x)) +          
        neutral + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))}
      
    pdf(paste0(pro_res, "1105_projects_by-district-year.pdf"), 
        width = 10, height = 11)    
        
        lapply(uni_w, p1105) 
        
    dev.off()    
    
    
#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#export data   
    pro_ex_data <- arrange(pro_all, district, year, owner, status, indicator)
      
#export
    write_csv(pro_ex_data, paste0(pro_exp, "projects_future.csv"))    
    
    
    
    
    
    