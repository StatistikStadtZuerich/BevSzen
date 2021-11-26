#-------------------------------------------------------------------
#Demography and Housing Model
#
#
#
#rok/bad, November 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#general functions already available?
    if (!exists("para")) {
      
        #working directory
            library(here)
            setwd(paste0(here(), "/2020phase2/"))
        
        #general (e.g. packages, colors)
            source("1_Code/0000_General/0000_general_phase2.r")
            
    }

    
#-------------------------------------------------------------------
#import, data preparation
#-------------------------------------------------------------------

#population: import
    
    #in contrast to rate calculations: population at the end of the year 
    #WHY: the rates will be applied here to the population of the previous year
  
    pop_import <- read_csv(pop_od) %>% 
        rename(year = StichtagDatJahr, age = AlterVCd, pop = AnzBestWir) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(district = factor(distr, uni_d),
               sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
               origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o)) %>% 
        select(district, year, age, sex, origin, pop) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup() 
    
#population: all cases
    pop <- as_tibble(expand_grid(
            district = uni_d, 
            year = date_start:date_end,
            age = age_min:age_max,
            sex = uni_s,
            origin = uni_o)) %>%    
        left_join(pop_import, by = c("district", "year", "age", "sex", "origin")) %>% 
        replace_na(list(pop = 0))           
    
#birth: fertility rate
    fer <- read_csv(paste0(exp_path, "/birth_fertility_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
               sex = uni_s[2],
               origin = factor(origin, levels = uni_o)) %>% 
        select(district, year, age, sex, origin, fer)
    
#birth: origin change    
    cha <- read_csv(paste0(exp_path, "/birth_origin-change_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
            origin = factor(origin, levels = uni_o))
               
#birth: proportion male 
    pro_male <- read_csv(paste0(exp_path, "/birth_sex-ratio_future.csv"))
       
#death: mortality rate
    mor <- read_csv(paste0(exp_path, "/mortality_future.csv")) %>% 
        mutate(sex = factor(sex, levels = uni_s))
           
#immigration*: immigration* rate
    ims_rate <- read_csv(paste0(exp_path, "/immigration-star_rate-dy_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d))      
             
#immigration*: sex and origin proportion 
    ims_prop_so <- read_csv(paste0(exp_path, "/immigration-star_prop-so-dy_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
            sex = factor(sex, levels = uni_s),                 
            origin = factor(origin, levels = uni_o))     
    
#immigration*: age proportion
    ims_prop_a <- read_csv(paste0(exp_path, "/immigration-star_prop-a-dyso_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
            sex = factor(sex, levels = uni_s),                 
            origin = factor(origin, levels = uni_o))         
    
#emigration*: emigration* rate
    ems_rate <- read_csv(paste0(exp_path, "/emigration-star_rate-dy_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d))        
             
#emigration*: sex and origin proportion 
    ems_prop_so <- read_csv(paste0(exp_path, "/emigration-star_prop-so-dy_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
            sex = factor(sex, levels = uni_s),                 
            origin = factor(origin, levels = uni_o))     
    
#emigration*: age proportion
    ems_prop_a <- read_csv(paste0(exp_path, "/emigration-star_prop-a-dyso_future.csv")) %>%     
        mutate(district = factor(district, levels = uni_d),
            sex = factor(sex, levels = uni_s),                 
            origin = factor(origin, levels = uni_o))       
    
#relocation, immigration
    rei <- read_csv(paste0(exp_path, "/relocation_immigration_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
            origin = factor(origin, levels = uni_o))         
       
#relocation, emigration
    ree <- read_csv(paste0(exp_path, "/relocation_emigration_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
            origin = factor(origin, levels = uni_o))           
       
#naturalization
    nat <- read_csv(paste0(exp_path, "/naturalization_future.csv")) %>% 
        mutate(district = factor(district, levels = uni_d),
            sex = factor(sex, levels = uni_s),                 
            origin = uni_o[2]) %>% 
        rename(rate_nat = rate_nat_dyas) %>% 
        select(district, year, age, sex, origin, rate_nat)

#housing model
    hou <- read_csv(paste0(exp_path, "/housing-model_population_d.csv")) %>% 
        mutate(district = factor(district, levels = uni_d)) %>% 
        rename(pop_limit = pop)
    
   
#-------------------------------------------------------------------
#Loop over years
#-------------------------------------------------------------------
    
#last year of data, 
    pop_last <- filter(pop, year == date_end) 
    
#years in the future
    future <- szen_begin:szen_end
    
#loop over years
    for (iyear in future){
      
        #iyear <- 2021
      
        #population at the begin of the year
            if(iyear == min(future)){popu <- select(pop_last, -year)}
      
        #births (fertility rate * women in the population)
            bir_do <- fer %>% 
                filter(year == iyear) %>% 
                left_join(popu, by = c("district", "age", "sex", "origin")) %>% 
                replace_na(list(fer = 0, pop = 0)) %>%               
                mutate(birth = pop * fer / 100) %>% 
                group_by(district, year, origin) %>% 
                    summarize(birth = sum(birth)) %>% 
                ungroup()
            
                #sum(bir_do$birth)

        #births: origin changes (from mother to baby)
            bir_do_new <- cha %>% 
                filter(year == iyear) %>% 
                select(-year) %>% 
                right_join(bir_do, by = c("district", "origin")) %>% 
                mutate(change = birth * cha / 100, 
                       keep = birth - change) %>% 
                select(district, origin, year, change, keep) %>% 
                pivot_longer(cols = c("change", "keep"), 
                             names_to = "category", values_to = "birth") %>%
                mutate(new_origin = case_when(category == "keep" ~ origin,
                                              origin == uni_o[1] ~ uni_o[2],
                                              TRUE ~ uni_o[1])) %>% 
                select(district, year, new_origin, birth) %>% 
                rename(origin = new_origin) %>% 
                group_by(district, year, origin) %>% 
                    summarize(birth = sum(birth)) %>% 
                ungroup()
            
        #births: with variable 'sex'
            
            #why age -1? still perspective at the begin of the year
            #the newborn babies are one year younger than the population at age zero
            #ageing of the population is executed at the very end of the code
            
            bir <- bir_do_new %>%            
                left_join(filter(pro_male, year == iyear), by = "year") %>% 
                mutate(male = birth * pro_male / 100,
                       female = birth - male) %>% 
                select(-c(birth, pro_male)) %>% 
                pivot_longer(cols = uni_s, 
                             names_to = "sex", values_to = "birth") %>% 
                mutate(age = -1, 
                       sex = factor(sex, levels = uni_s)) %>% 
                select(district, year, age, sex, origin, birth) %>% 
                arrange(district, sex, origin)
            
                #sum(bir_do$birth)   
                #sum(bir_do_new$birth)             
                #sum(bir$birth)                       
                       
        #deaths (mortality rate * population)
            dea <- mor %>% 
                filter(year == iyear) %>% 
                right_join(popu, by = c("age", "sex")) %>%  
                replace_na(list(mor = 0, pop = 0)) %>%               
                mutate(death = pop * mor / 100) %>% 
                select(district, year, age, sex, origin, death)

                #sum(dea$death)                                           
  
        #immigration*
            ims <- popu %>% 
                group_by(district) %>% 
                    summarize(pop = sum(pop)) %>% 
                ungroup() %>% 
                left_join(filter(ims_rate, year == iyear), by = "district") %>% 
                mutate(ims_d = pop * rate / 100) %>% 
                select(-c(pop, rate)) %>%               
                #until here: immigration* by district (31 rows)
                left_join(ims_prop_so, by = c("district", "year")) %>% 
                mutate(ims_dso = ims_d * prop / 100) %>% 
                select(-c(ims_d, prop)) %>% 
                #until here: immigration* by district, sex, origin (31*2*2 = 124 rows)              
                left_join(ims_prop_a, by = c("district", "year", "sex", "origin")) %>% 
                mutate(ims = ims_dso * prop / 100) %>%             
                select(district, year, age, sex, origin, ims)            
                #result: immigration* by district, age, sex, origin (31*121*2*2 = 15004 rows)             
            

            # sum(ims$ims)
            

  
        #emigration*
            ems <- popu %>% 
                group_by(district) %>% 
                    summarize(pop = sum(pop)) %>% 
                ungroup() %>% 
                left_join(filter(ems_rate, year == iyear), by = "district") %>% 
                mutate(ems_d = pop * rate / 100) %>% 
                select(-c(pop, rate)) %>%               
                #until here: emigration* by district (31 rows)
                left_join(ems_prop_so, by = c("district", "year")) %>% 
                mutate(ems_dso = ems_d * prop / 100) %>% 
                select(-c(ems_d, prop)) %>% 
                #until here: emigration* by district, sex, origin (31*2*2 = 124 rows)              
                left_join(ems_prop_a, by = c("district", "year", "sex", "origin")) %>% 
                mutate(ems = ems_dso * prop / 100) %>%             
                select(district, year, age, sex, origin, ems)            
                #result: emigration* by district, age, sex, origin (31*121*2*2 = 15004 rows)             
            

            # sum(ems$ems)           
            
            
        #combine the demographic processes
            
            dem <- as_tibble(expand_grid(
                        district = uni_d, 
                        year = iyear,
                        age = (-1):age_max,
                        sex = uni_s,
                        origin = uni_o)) %>% 
                left_join(popu, by = c("district", "age", "sex", "origin")) %>% 
                left_join(bir, by = c("district", "year", "age", "sex", "origin")) %>%                        
                left_join(dea, by = c("district", "year", "age", "sex", "origin")) %>%                      
                left_join(ims, by = c("district", "year", "age", "sex", "origin")) %>%                      
                left_join(ems, by = c("district", "year", "age", "sex", "origin")) %>% 
                replace_na(list(pop = 0, birth = 0, death = 0, ims = 0, ems = 0)) %>% 
                #not more than the entire population can die...
                mutate(death_real = pmin(death, pop),
                       pop_bir_dea = pop + birth - death_real)
            
            # sum(dem$pop)
            # sum(bir$birth) - sum(dea$death)
            # sum(dem$pop_bir_dea)
            # sum(dem$pop) + sum(bir$birth) - sum(dea$death)

            
        #balance (on district level)
            #if not enough space: decrease immigration, increase emigration
            #if space left: increase immigration, decrease emigration
            
            bal <- dem %>% 
                group_by(district, year) %>% 
                    summarize(
                        pop_bir_dea = sum(pop_bir_dea),
                        ims = sum(ims),
                        ems = sum(ems)) %>% 
                ungroup() %>% 
                #theoretical population:
                #pop + birth - death + immigration* - emigration*
                mutate(pop_theo = pop_bir_dea + ims - ems) %>%               
                left_join(hou, by = c("district", "year")) %>% 
                mutate(differ = pop_limit - pop_theo, 
                       corr_ims = if_else(differ < 0, 
                                          less_ims / 100 * differ,
                                          more_ims / 100 * differ),
                       corr_ems = if_else(differ < 0, 
                                          (1 - less_ims / 100) * differ,
                                          (1 - more_ims / 100) * differ))
            
            
   
                       
           
                       # corr_ims = (ims + differ_ims) / ims)
            
 
                # sum(bal$differ)
            
                       
                       
                       
            
            
            
            
            
            
            
            
            
            

            
            

            
 

            
                 
#end of loop over years      
    }     
    
    
  
    
    
    
                        
                    

    
