#-------------------------------------------------------------------
#Model outputs
#
#
#
#rok/bad, December 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, parameters, functions
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


#-------------------------------------------------------------------
#data import: past
#-------------------------------------------------------------------
 
#population
    #in contrast to rate calculations: population at the end of the year 
    pop_past <- read_csv(pop_od) %>% 
        rename(year = StichtagDatJahr, age = AlterVCd, pop = AnzBestWir) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(district = factor(distr, uni_d),
               sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
               origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o)) %>% 
        select(district, year, age, sex, origin, pop) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup() %>% 
        mutate(scenario = uni_c[1])

#births    
    bir_past <- read_csv(bir_od) %>% 
        rename(year = EreignisDatJahr, bir = AnzGebuWir) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(district = factor(distr, uni_d),
            sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o)) %>% 
        select(district, year, sex, origin, bir) %>%     
        group_by(district, year, sex, origin) %>% 
            summarize(bir = sum(bir)) %>% 
        ungroup() %>% 
        mutate(scenario = uni_c[1])
    
#deaths
    dea_past <- read_csv(dea_od) %>%
        rename(year = EreignisDatJahr, age = AlterVCd, dea = AnzSterWir) %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s)) %>%    
        group_by(year, age, sex) %>% 
            summarize(dea = sum(dea)) %>% 
        ungroup() %>% 
        mutate(scenario = uni_c[1])
    
#immigration
    imm_past <- read_csv(imm_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, imm = AnzZuzuWir) %>%    
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, sex, origin, imm) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(imm = sum(imm)) %>% 
        ungroup() %>% 
        mutate(scenario = uni_c[1])     
    
#emigration
    emi_past <- read_csv(emi_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, emi = AnzWezuWir) %>%    
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, sex, origin, emi) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(emi = sum(emi)) %>% 
        ungroup() %>% 
        mutate(scenario = uni_c[1])    
        
#relocation
    rel_past <- read_csv(rel_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, rel = AnzUmzuWir) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        rename(distr_after = distr) %>% 
        left_join(look_dis, by = c("QuarBisherCd" = "QuarCd")) %>% 
        rename(distr_before = distr) %>%     
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district_before = factor(distr_before, uni_d),
            district_after = factor(distr_after, uni_d)) %>% 
        select(district_before, district_after, year, age, sex, origin, rel) %>%      
        group_by(district_before, district_after, year, age, sex, origin) %>% 
            summarize(rel = sum(rel)) %>% 
        ungroup() %>% 
        mutate(scenario = uni_c[1])

#naturalization
    nat_past <- read_csv(nat_od) %>% 
        filter((HerkunftBisherCd == 2) & (HerkunftCd == 1)) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, nat = AnzEinbWir) %>%          
        left_join(look_dis, by = "QuarCd") %>%       
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, sex, nat) %>% 
        arrange(district, year, age, sex)
    
#previous published population scenarios
    #age: only age groups available
    #therefore, first evaluations without age
    
    look_dis_temp <- look_dis %>% 
        mutate(Quar_num = as.numeric(QuarCd),
               QuarSort = if_else(Quar_num < 20, 10, Quar_num)) %>% 
        select(QuarSort, distr) %>% 
        distinct()
    
    sce <- read_csv(sce_od) %>% 
        rename(year = StichtagDatJahr, pop = AnzBestWir) %>% 
        filter(year >= szen_begin) %>% 
        left_join(look_dis_temp, by = "QuarSort") %>% 
        mutate(sex = factor(if_else(SexSort == 1, uni_s[1], uni_s[2]), uni_s), 
            origin = factor(if_else(HeimatRegionSort == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d),
            scenario = case_when(VersionArtSort == 1 ~ uni_c[2],
                VersionArtSort== 2 ~ uni_c[3],
                TRUE ~ uni_c[4])) %>% 
        select(district, year, sex, origin, scenario, pop) %>%
        group_by(district, year, sex, origin, scenario) %>%
            summarize(pop = sum(pop)) %>%
        ungroup()


    
#-------------------------------------------------------------------
#data import: future (lower, middle, upper scenario)  
#-------------------------------------------------------------------
 
#population               
    pop_lower <- read_csv(paste0(data_path, "5_Outputs/lower/population_future.csv")) %>% 
        mutate(scenario = uni_c[2])
    
    pop_middle <- read_csv(paste0(data_path, "5_Outputs/middle/population_future.csv")) %>% 
        mutate(scenario = uni_c[3])
        
    pop_upper <- read_csv(paste0(data_path, "5_Outputs/upper/population_future.csv")) %>% 
        mutate(scenario = uni_c[4])
    
#births    
    bir_lower <- read_csv(paste0(data_path, "5_Outputs/lower/births_future.csv")) %>% 
        mutate(scenario = uni_c[2])
    
    bir_middle <- read_csv(paste0(data_path, "5_Outputs/middle/births_future.csv")) %>% 
        mutate(scenario = uni_c[3])
        
    bir_upper <- read_csv(paste0(data_path, "5_Outputs/upper/births_future.csv")) %>% 
        mutate(scenario = uni_c[4])
        
#demographic processes
    dem_lower <- read_csv(paste0(data_path, "5_Outputs/lower/demographic-processes_future.csv")) %>% 
        mutate(scenario = uni_c[2])    
    
    dem_middle <- read_csv(paste0(data_path, "5_Outputs/middle/demographic-processes_future.csv")) %>% 
        mutate(scenario = uni_c[3])
        
    dem_upper <- read_csv(paste0(data_path, "5_Outputs/upper/demographic-processes_future.csv")) %>% 
        mutate(scenario = uni_c[4])
            
    dem_future <- dem_lower %>% 
        bind_rows(dem_middle) %>% 
        bind_rows(dem_upper)    
    
    
    
#-------------------------------------------------------------------
#population
#-------------------------------------------------------------------
     
#past and future     
    pop <- pop_past %>% 
        bind_rows(pop_lower) %>%     
        bind_rows(pop_middle) %>% 
        bind_rows(pop_upper)
    
#yc
    pop_yc <- pop %>% 
        group_by(year, scenario) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup()
    
    sszplot(pop_yc, aes_x = "year", aes_y = "pop", aes_col = "scenario",
            labs_y = "population",
            scale_y = c(0, NA), 
            geom = "line",
            name = "1500_pop_yc")        
    

#-------------------------------------------------------------------
#population: new and previous scenarios
#-------------------------------------------------------------------

#preparation
    new_prev <- factor(c("new", "previous"))
    
    pop_yc_new <- pop_yc %>% 
        mutate(cat = new_prev[1])
    
    pop_yc_prev <- sce %>% 
        group_by(year, scenario) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup() %>% 
        mutate(cat = new_prev[2])

    pop_yc_new_prev <- pop_yc_new %>% 
        bind_rows(pop_yc_prev)
    
    sszplot(pop_yc_new_prev, aes_x = "year", aes_y = "pop", 
            aes_col = "scenario", aes_ltyp = "cat",
            labs_y = "population",
            scale_y = c(0, NA), 
            geom = "line",
            name = "1501_pop_yc_new_prev")       
    
   
#-------------------------------------------------------------------
#birth
#-------------------------------------------------------------------
    
#past and future    
    bir <- bir_past %>% 
        bind_rows(bir_lower) %>%     
        bind_rows(bir_middle) %>% 
        bind_rows(bir_upper) %>% 
        mutate(origin = factor(origin, levels = uni_o))
    
#yc
    bir_yc <- bir %>% 
        group_by(year, scenario) %>% 
            summarize(bir = sum(bir)) %>% 
        ungroup()
    
    sszplot(bir_yc, aes_x = "year", aes_y = "bir", aes_col = "scenario",
            labs_y = "births per year",
            scale_y = c(0, NA), 
            geom = "line",
            name = "1510_bir_yc")        
    
#yoc
    bir_yoc <- bir %>% 
        group_by(year, origin, scenario) %>% 
            summarize(bir = sum(bir)) %>% 
        ungroup()    
    
    sszplot(bir_yoc, aes_x = "year", aes_y = "bir", aes_col = "scenario",
            labs_y = "births per year",
            grid = c(".", "origin"),
            scale_y = c(0, NA), 
            geom = "line",
            name = "1511_bir_yoc")        
        
    
#-------------------------------------------------------------------
#death
#-------------------------------------------------------------------
        
#past and future  
    dea <- dem_future %>% 
        select(year, age, sex, scenario, dea) %>% 
        group_by(year, age, sex, scenario) %>% 
            summarize(dea = sum(dea)) %>% 
        ungroup() %>% 
        bind_rows(dea_past) %>% 
        left_join(look_a3, by = "age") %>% 
        mutate(sex = factor(sex, levels = uni_s))
    
#yc
    dea_yc <- dea %>% 
        group_by(year, scenario) %>% 
            summarize(dea = sum(dea)) %>% 
        ungroup()
    
    sszplot(dea_yc, aes_x = "year", aes_y = "dea", aes_col = "scenario",
            labs_y = "deaths per year",
            scale_y = c(0, NA), 
            geom = "line",
            name = "1520_dea_yc")        
    
#yac
    dea_yac <- dea %>% 
        group_by(year, age_3, scenario) %>% 
            summarize(dea = sum(dea)) %>% 
        ungroup()   
    
    sszplot(dea_yac, aes_x = "year", aes_y = "dea", aes_col = "scenario",
            labs_y = "deaths per year",
            wrap = "age_3", ncol = 5,
            scale_y = c(0, NA), 
            geom = "line",
            width = 12, height = 6,
            name = "1521_dea_yac")  
    
#ys
    dea_ys <- dea %>% 
        filter(scenario %in% uni_c[c(1, 3)]) %>% 
        group_by(year, sex) %>% 
            summarize(dea = sum(dea)) %>% 
        ungroup()
    
    sszplot(dea_ys, aes_x = "year", aes_y = "dea", aes_col = "sex",
            labs_y = "deaths per year",
            scale_y = c(0, NA), 
            geom = "line",
            name = "1522_dea_ys")      
    
#yas
    dea_yas <- dea %>% 
        filter(scenario %in% uni_c[c(1, 3)]) %>%       
        group_by(year, age_3, sex) %>% 
            summarize(dea = sum(dea)) %>% 
        ungroup()   
    
    sszplot(dea_yas, aes_x = "year", aes_y = "dea", aes_col = "sex",
            labs_y = "deaths per year",
            wrap = "age_3", ncol = 5,
            scale_y = c(0, NA), 
            geom = "line",
            i_x = szen_begin,
            width = 12, height = 6,
            name = "1523_dea_yas")      
 
       
#-------------------------------------------------------------------
#immigration
#-------------------------------------------------------------------
   
#past and future  
    imm <- dem_future %>% 
        select(district, year, age, sex, origin, scenario, imm) %>% 
        bind_rows(imm_past) %>% 
        left_join(look_a3, by = "age") %>% 
        mutate(origin = factor(origin, levels = uni_o))
    
#yc
    imm_yc <- imm %>% 
        group_by(year, scenario) %>% 
            summarize(imm = sum(imm)) %>% 
        ungroup()
    
    sszplot(imm_yc, aes_x = "year", aes_y = "imm", aes_col = "scenario",
            labs_y = "immigration per year",
            scale_y = c(0, NA), 
            geom = "line",
            name = "1530_imm_yc")        
        
#ya
    imm_ya <- imm %>% 
        filter(scenario %in% uni_c[c(1, 3)]) %>%       
        group_by(year, age_3) %>% 
            summarize(imm = sum(imm)) %>% 
        ungroup()       
    
    sszplot(imm_ya, aes_x = "year", aes_y = "imm",
            labs_y = "immigration per year",
            wrap = "age_3", ncol = 5,
            scale_y = c(0, NA), 
            geom = "line",
            i_x = szen_begin,
            width = 12, height = 6,
            name = "1531_imm_ya")        
    
#yao
    imm_yao <- imm %>% 
        filter(scenario %in% uni_c[c(1, 3)]) %>%       
        group_by(year, age_3, origin) %>% 
            summarize(imm = sum(imm)) %>% 
        ungroup()       
    
    sszplot(imm_yao, aes_x = "year", aes_y = "imm", aes_col = "origin",
            labs_y = "immigration per year",
            wrap = "age_3", ncol = 5,
            scale_y = c(0, NA), 
            geom = "line",
            i_x = szen_begin,
            width = 12, height = 6,
            name = "1532_imm_yao")        
        
       
#-------------------------------------------------------------------
#emigration
#-------------------------------------------------------------------
   
#past and future  
    emi <- dem_future %>% 
        select(district, year, age, sex, origin, scenario, emi) %>% 
        bind_rows(emi_past) %>% 
        left_join(look_a3, by = "age") %>% 
        mutate(origin = factor(origin, levels = uni_o))
    
#yc
    emi_yc <- emi %>% 
        group_by(year, scenario) %>% 
            summarize(emi = sum(emi)) %>% 
        ungroup()
    
    sszplot(emi_yc, aes_x = "year", aes_y = "emi", aes_col = "scenario",
            labs_y = "emigration per year",
            scale_y = c(0, NA), 
            geom = "line",
            name = "1540_emi_yc")        
        
#ya
    emi_ya <- emi %>% 
        filter(scenario %in% uni_c[c(1, 3)]) %>%       
        group_by(year, age_3) %>% 
            summarize(emi = sum(emi)) %>% 
        ungroup()       
    
    sszplot(emi_ya, aes_x = "year", aes_y = "emi",
            labs_y = "emigration per year",
            wrap = "age_3", ncol = 5,
            scale_y = c(0, NA), 
            geom = "line",
            i_x = szen_begin,
            width = 12, height = 6,
            name = "1541_emi_ya")        
    
#yao
    emi_yao <- emi %>% 
        filter(scenario %in% uni_c[c(1, 3)]) %>%       
        group_by(year, age_3, origin) %>% 
            summarize(emi = sum(emi)) %>% 
        ungroup()       
    
    sszplot(emi_yao, aes_x = "year", aes_y = "emi", aes_col = "origin",
            labs_y = "emigration per year",
            wrap = "age_3", ncol = 5,
            scale_y = c(0, NA), 
            geom = "line",
            i_x = szen_begin,
            width = 12, height = 6,
            name = "1542_emi_yao")          
  
      
#-------------------------------------------------------------------
#net migration
#-------------------------------------------------------------------
 
#calculate net migration     
    net <- as_tibble(expand_grid(
        district = uni_d, 
        year = date_start:szen_end,
        age = age_min:age_max,
        sex = uni_s,
        origin = uni_o,
        scenario = uni_c)) %>% 
        left_join(look_a3, by = "age") %>%       
        left_join(imm, by = c("district", "year", "age", 
                              "sex", "origin", "scenario", "age_3")) %>% 
        left_join(emi, by = c("district", "year", "age", 
                              "sex", "origin", "scenario", "age_3")) %>%      
        replace_na(list(imm = 0, emi = 0))  %>% 
        mutate(net = if_else( 
          ((year < szen_begin) & (scenario != uni_c[1])) | 
          ((year >= szen_begin) & (scenario == uni_c[1])), NA_real_, imm - emi))
    
#yc
    net_yc <- net %>% 
        group_by(year, scenario) %>% 
            summarize(net = sum(net)) %>% 
        ungroup()
    
    sszplot(net_yc, aes_x = "year", aes_y = "net", aes_col = "scenario",
            labs_y = "net migration per year",
            scale_y = c(0, NA),
            i_y = 0,
            geom = "line",
            name = "1550_net_yc") 
    
    
    
    
    

         