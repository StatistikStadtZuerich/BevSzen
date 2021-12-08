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
        ungroup()
    
#deaths
    dea_past <- read_csv(dea_od) %>%
        rename(year = EreignisDatJahr, age = AlterVCd, dea = AnzSterWir) %>% 
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s)) %>%    
        group_by(year, age, sex) %>% 
            summarize(dea = sum(dea)) %>% 
        ungroup()
    
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
        ungroup()      
    
    
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
        ungroup()      
        
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
        ungroup() 

#naturalization
    nat_past <- read_csv(nat_od) %>% 
        filter((HerkunftBisherCd == 2) & (HerkunftCd == 1)) %>% 
        rename(year = EreignisDatJahr, age = AlterVCd, nat = AnzEinbWir) %>%          
        left_join(look_dis, by = "QuarCd") %>%       
        mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, sex, nat) %>% 
        arrange(district, year, age, sex)

#processes (past)
    
    
    
    
#-------------------------------------------------------------------
#data import: future
#-------------------------------------------------------------------
 
#population (lower, middle, upper scenario)                
    pop_lower <- read_csv(paste0(data_path, "5_Outputs/lower/population_future.csv")) %>% 
        mutate(scenario = uni_c[2])
    
    pop_middle <- read_csv(paste0(data_path, "5_Outputs/middle/population_future.csv")) %>% 
        mutate(scenario = uni_c[3])
        
    pop_upper <- read_csv(paste0(data_path, "5_Outputs/upper/population_future.csv")) %>% 
        mutate(scenario = uni_c[4])
     
    
#-------------------------------------------------------------------
#data: past and future
#-------------------------------------------------------------------
     
#population    
    pop <- pop_past %>% 
        bind_rows(pop_lower) %>%     
        bind_rows(pop_middle) %>% 
        bind_rows(pop_upper)
    
#yc
    pop_yc <- pop %>% 
        group_by(year, scenario) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup() %>% 
        filter(year <= 2040)
    
    sszplot(pop_yc, aes_x = "year", aes_y = "pop", aes_col = "scenario",
            labs_y = "population",
            scale_y = c(0, NA), 
            geom = c("line", "point"),
            name = "1500_pop_yc")        
    
    
    pop_yc %>% 
        filter((scenario == "middle") & (year >= 2040))
    
  
     
    temp <- pop %>% 
        filter(year == 2050) %>% 
        group_by(year, scenario, age) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup() %>% 
        pivot_wider(names_from = scenario, values_from = pop)
    as.data.frame(temp)
    
    
    
    

