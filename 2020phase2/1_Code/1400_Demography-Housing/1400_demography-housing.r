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
        
        #general functions (without dependence on parameters)
            source("1_Code/0000_General/0002_general_without-parameters.r")
            
        #parameters (depend on scenario)
            i_scen <- "middle"
            for(i_para in 1:nrow(para)){
                assign(para$parameter[i_para], para[[i_scen]][i_para],
                       envir = .GlobalEnv)}

        #general functions (with dependence on parameters)
            source(paste0(code_path, "/0000_General/0003_general_with-parameters.r"))
    
    }

#start time
    t0 <- Sys.time() 
    
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
   
     
#TEST------------------------------------------    
    future <- szen_begin:2035
#TEST------------------------------------------     
    
    
    
#outputs
    out_bir <- NULL
    out_dem <- NULL
    out_pop <- NULL
    out_nat <- NULL
   
    
    
     
#TEST------------------------------------------
    test_ims <- NULL
    test_ems <- NULL
    test_dem <- NULL 
    test_bal <- NULL
    test_check <- NULL   
    test_dem_factor <- NULL
    t0test <- Sys.time() 
        
#TEST------------------------------------------    
    
    
#loop over years
    for (iyear in future){
      
        #iyear <- 2021
      
        #population at the begin of the year = population at the end of the previous year
            if(iyear == min(future)){popu <- select(pop_last, -year)} else {
                popu <- select(pop_end_year, -year)}
      
        #births (fertility rate * women in the population)
            bir_do <- fer %>% 
                filter(year == iyear) %>% 
                left_join(popu, by = c("district", "age", "sex", "origin")) %>% 
                replace_na(list(fer = 0, pop = 0)) %>%               
                mutate(bir = pop * fer / 100) %>% 
                group_by(district, origin) %>% 
                    summarize(bir = sum(bir)) %>% 
                ungroup()
            
                #sum(bir_do$bir)

        #births: origin changes (from mother to baby)
            bir_do_new <- cha %>% 
                filter(year == iyear) %>% 
                select(-year) %>% 
                right_join(bir_do, by = c("district", "origin")) %>% 
                mutate(change = bir * cha / 100, 
                       keep = bir - change) %>% 
                select(district, origin, change, keep) %>% 
                pivot_longer(cols = c("change", "keep"), 
                             names_to = "category", values_to = "bir") %>%
                mutate(new_origin = case_when(category == "keep" ~ origin,
                                              origin == uni_o[1] ~ uni_o[2],
                                              TRUE ~ uni_o[1])) %>% 
                select(district, new_origin, bir) %>% 
                rename(origin = new_origin) %>% 
                group_by(district, origin) %>% 
                    summarize(bir = sum(bir)) %>% 
                ungroup()
            
        #births: with variable 'sex'
            
            #why age -1? still perspective at the begin of the year
            #the newborn babies are one year younger than the population at age zero
            #ageing of the population is executed at the very end of the code
            
            pro_male_value <- filter(pro_male, year == iyear)
            
            bir <- bir_do_new %>%  
                mutate(pro_male = pro_male_value$pro_male,
                       male = bir * pro_male / 100,
                       female = bir - male) %>% 
                select(-c(bir, pro_male)) %>% 
                pivot_longer(cols = uni_s, 
                             names_to = "sex", values_to = "bir") %>% 
                mutate(age = -1, 
                       sex = factor(sex, levels = uni_s)) %>% 
                select(district, age, sex, origin, bir) %>% 
                arrange(district, sex, origin)
            
                #sum(bir_do$bir)   
                #sum(bir_do_new$bir)             
                #sum(bir$bir)                       
                       
        #deaths (mortality rate * population)
            dea <- mor %>% 
                filter(year == iyear) %>% 
                right_join(popu, by = c("age", "sex")) %>%  
                replace_na(list(mor = 0, pop = 0)) %>%               
                mutate(dea = pop * mor / 100) %>% 
                select(district, age, sex, origin, dea)

                #sum(dea$dea)                                           
  
        #immigration*
            ims <- popu %>% 
                group_by(district) %>% 
                    summarize(pop = sum(pop)) %>% 
                ungroup() %>% 
                left_join(filter(ims_rate, year == iyear), by = "district") %>% 
                mutate(ims_d = pop * rate / 100) %>%
                #here: keep the year in order to join the right year below
                select(-c(pop, rate)) %>%               
                #until here: immigration* by district (31 rows)
                left_join(ims_prop_so, by = c("district", "year")) %>% 
                mutate(ims_dso = ims_d * prop / 100) %>% 
                select(-c(ims_d, prop)) %>% 
                #until here: immigration* by district, sex, origin (31*2*2 = 124 rows)              
                left_join(ims_prop_a, by = c("district", "year", "sex", "origin")) %>% 
                mutate(ims = ims_dso * prop / 100) %>%             
                select(district, age, sex, origin, ims)            
                #result: immigration* by district, age, sex, origin (31*121*2*2 = 15004 rows)             
            

            # sum(ims$ims)
            
            #TEST------------------------------------------
            test1 <- ims_prop_so %>% 
                filter(district == "Wollishofen")
          
            ggplot(test1) +
                geom_line(aes(x = year, y = prop)) +
                facet_grid(sex ~ origin)
          
            ggplot(ims_prop_so) +
                geom_line(aes(x = year, y = prop, color = sex, linetype = origin)) +
                facet_wrap(~ district, ncol = 4)
            

            test2 <- ims %>% 
                group_by(district, origin) %>% 
                    summarize(ims = sum(ims)) %>% 
                ungroup() %>% 
                mutate(year = iyear)
              
            test_ims <- bind_rows(test_ims, test2)
            #TEST------------------------------------------            
            
  
            
        #emigration*
            ems <- popu %>% 
                group_by(district) %>% 
                    summarize(pop = sum(pop)) %>% 
                ungroup() %>% 
                left_join(filter(ems_rate, year == iyear), by = "district") %>% 
                mutate(ems_d = pop * rate / 100) %>% 
                #here: keep the year in order to join the right year below              
                select(-c(pop, rate)) %>%               
                #until here: emigration* by district (31 rows)
                left_join(ems_prop_so, by = c("district", "year")) %>% 
                mutate(ems_dso = ems_d * prop / 100) %>% 
                select(-c(ems_d, prop)) %>% 
                #until here: emigration* by district, sex, origin (31*2*2 = 124 rows)              
                left_join(ems_prop_a, by = c("district", "year", "sex", "origin")) %>% 
                mutate(ems = ems_dso * prop / 100) %>%             
                select(district, age, sex, origin, ems)            
                #result: emigration* by district, age, sex, origin (31*121*2*2 = 15004 rows)             
            
            
            #TEST------------------------------------------   
            test3 <- ems_prop_so %>% 
                filter(district == "Wollishofen")
          
            ggplot(test3) +
                geom_line(aes(x = year, y = prop)) +
                facet_grid(sex ~ origin)
          
            ggplot(ems_prop_so) +
                geom_line(aes(x = year, y = prop, color = sex, linetype = origin)) +
                facet_wrap(~ district, ncol = 4)            

            test4 <- ems %>% 
                group_by(district, origin) %>% 
                    summarize(ems = sum(ems)) %>% 
                ungroup() %>% 
                mutate(year = iyear)
              
            test_ems <- bind_rows(test_ems, test4)
            #TEST------------------------------------------            
            
            # sum(ems$ems)           
            
            
        #combine the demographic processes
            
            dem <- as_tibble(expand_grid(
                        district = uni_d, 
                        age = (-1):age_max,
                        sex = uni_s,
                        origin = uni_o)) %>% 
                left_join(popu, by = c("district", "age", "sex", "origin")) %>% 
                left_join(bir, by = c("district", "age", "sex", "origin")) %>%                        
                left_join(dea, by = c("district", "age", "sex", "origin")) %>%                      
                left_join(ims, by = c("district", "age", "sex", "origin")) %>%                      
                left_join(ems, by = c("district", "age", "sex", "origin")) %>% 
                replace_na(list(pop = 0, bir = 0, dea = 0, ims = 0, ems = 0)) %>% 
                #not more than the entire population can die...
                #therefore, calculate effective deaths
                mutate(dea_eff = pmin(dea, pop),
                       pop_bir_dea = pop + bir - dea_eff)
            
            # sum(dem$pop)
            # sum(bir$bir) - sum(dea$dea)
            # sum(dem$pop_bir_dea)
            # sum(dem$pop) + sum(bir$bir) - sum(dea$dea)

            # dem %>%
            #     select(pop, bir, dea, dea_eff, ims, ems, pop_bir_dea) %>% 
            #     summarize_all(list(sum))
            
            #TEST------------------------------------------            
            test5 <- dem %>% 
                group_by(district, origin) %>% 
                    summarize_at(c("pop", "bir", "dea", "dea_eff", 
                                   "ims", "ems", "pop_bir_dea"), 
                                 sum, na.rm = TRUE) %>% 
                ungroup() %>% 
                mutate(year = iyear)           
            
            test_dem <- bind_rows(test_dem, test5)            
            #TEST------------------------------------------               
            
            
        #balance (on district level)
            #if not enough space: decrease immigration, increase emigration
            #if space left: increase immigration, decrease emigration
            
            #TEST------------------------------------------               
                ggplot(filter(hou, district == "Wollishofen")) +
                    geom_line(aes(x = year, y = pop_limit)) +
                    geom_vline(xintercept = szen_begin, color = "red") +
                    expand_limits(y = 0)
            #TEST------------------------------------------           
                
            
            hou_year <- hou %>% 
                filter(year == iyear) %>% 
                select(-year)
            
            bal <- dem %>% 
                group_by(district) %>% 
                    summarize(
                        pop_bir_dea = sum(pop_bir_dea),
                        ims = sum(ims),
                        ems = sum(ems)) %>% 
                ungroup() %>% 
                #theoretical population:
                #pop + birth - death + immigration* - emigration*
                mutate(pop_theo = pop_bir_dea + ims - ems) %>%               
                left_join(hou_year, by = "district") %>% 
              
                #tests if correction (ims3, ems3 are right)
                # mutate(ims = if_else(district == "Kreis 1", 5, ims)) %>%
                # mutate(ems = if_else(district == "Leimbach", 50, ems)) %>%
              
                mutate(differ = pop_limit - pop_theo, 
                       differ_ims = if_else(differ < 0, 
                                          less_ims / 100 * differ,
                                          more_ims / 100 * differ),
                       differ_ems = if_else(differ < 0, 
                                          (1 - less_ims / 100) * differ,
                                          (1 - more_ims / 100) * differ),
                       new_ims = ims + differ_ims,
                       new_ems = ems - differ_ems,
                       #immigration* and emigration* cannot be negative
                       new_ims2 = pmax(0, new_ims),
                       new_ems2 = pmax(0, new_ems),
                       #case: not enough space
                           #if the decrease of immigration* is not enough to reach
                           #the pop limit (since immigration* is already decreased to zero), 
                           #then emigration* is increased
                       new_ems3 = if_else(new_ims < 0, new_ems2 + abs(new_ims),
                                          new_ems2),
                       #case: too much space
                           #if the decrease of emigration* is not enough to reach
                           #the pop limit (since emigration* is already decreased to zero), 
                           #then immigration* is increased
                       new_ims3 = if_else(new_ems < 0, new_ims2 + abs(new_ems),
                                          new_ims2),           
                       factor_ims = new_ims3 / ims,
                       factor_ems = new_ems3 / ems,
                       check = pop_bir_dea + new_ims3 - new_ems3 - pop_limit)
            
                # sum(bal$differ)
                # sum(abs(bal$check))            
                                   
            #check: enough immigration* and/or emigration* for proper correction?
                check <- bal %>% 
                    filter((new_ims < 0) | (new_ems < 0))
            
                
            #TEST------------------------------------------            
            bal_year <- bal %>% 
                mutate(year = iyear) 
            check_year <- check %>% 
                mutate(year = iyear)               
                
            test_bal <- bind_rows(test_bal, bal_year)
            test_check <- bind_rows(test_check, check_year)                
            #TEST------------------------------------------            
                            
                
        #apply the correction factors to the immigration* and emigration* by 
        #district, age, sex, origin (same factor by district)   
            dem_factor <- bal %>% 
                select(district, factor_ims, factor_ems) %>% 
                right_join(dem, by = "district") %>% 
                mutate(ims_eff = ims * factor_ims, 
                       ems_eff = ems * factor_ems,
                       pop_temp = pop_bir_dea + ims_eff - ems_eff,
                       pop_end_year = pmax(0, pop_temp)) %>% 
                select(district, age, sex, origin, 
                       bir, dea_eff, ims, ems, ims_eff, ems_eff, pop_bir_dea, pop_temp, pop_end_year) %>% 
                rename(ims_initial = ims, ems_initial = ems)
            
            #TEST------------------------------------------ 
            dem_factor_year <- dem_factor %>% 
                mutate(year = iyear)             
            
            test_dem_factor <- bind_rows(test_dem_factor, dem_factor_year)
            #TEST------------------------------------------            
                        
         #sum(dem_factor$pop_end_year)
            
         #with naturalization (only on end-year-population)
            
            #the naturalization rate of all new born babies of zero is correct
            #WHY? the origin changes at birth has already been calculated in the model
            
            pop_nat_temp <- nat %>%    
                filter(year == iyear) %>%  
                select(-year) %>% 
                right_join(select(dem_factor, district, age, sex, 
                                  origin, pop_end_year), 
                           by = c("district", "age", "sex", "origin")) %>% 
                replace_na(list(rate_nat = 0))  %>% 
                arrange(district, age, sex, origin) %>% 
                mutate(Swiss = if_else(origin == uni_o[1], pop_end_year,
                                       pop_end_year * rate_nat / 100),
                       foreign = if_else(origin == uni_o[1], 0,
                                       pop_end_year * (1 - rate_nat / 100))) 
            
            #WHY split the pipe here? to calculate the future amount of naturalizations
            #count, not only rates
            
            pop_nat <- pop_nat_temp %>% 
                select(-c(origin, pop_end_year)) %>% 
                pivot_longer(cols = uni_o, 
                             names_to = "origin", values_to = "pop_end_year") %>% 
                mutate(origin = factor(origin, levels = uni_o)) %>% 
                select(district, age, sex, origin, pop_end_year) %>% 
                group_by(district, age, sex, origin) %>% 
                    summarize(pop_end_year = sum(pop_end_year)) %>% 
                ungroup()
            
            nat_count <- pop_nat_temp %>% 
                filter((origin == uni_o[2]) & (age >= 0)) %>% 
                group_by(district, age, sex) %>% 
                    summarize(nat = sum(Swiss)) %>% 
                ungroup()
            
          #determine immigration and relocation from immigration*  
              rei_year <- rei %>% 
                  filter(year == iyear) %>% 
                  select(-year)
            
              imm <- dem_factor %>% 
                  filter(age >= 0) %>% 
                  select(district, age, sex, origin, ims_eff) %>% 
                  left_join(rei_year, by = c("district", "age", "origin")) %>%
                  mutate(rel = ims_eff * prop_rel_dyao / 100, 
                         imm = ims_eff - rel) %>% 
                  select(district, age, sex, origin, imm, rel)
                  
                  # sum(imm$imm)
                  # sum(imm$rel)
              
          #determine emigration and relocation from emigration* 
              ree_year <- ree %>% 
                  filter(year == iyear) %>% 
                  select(-year)              
              
              emi <- dem_factor %>% 
                  filter(age >= 0) %>% 
                  select(district, age, sex, origin, ems_eff) %>% 
                  left_join(ree_year, by = c("district", "age", "origin")) %>% 
                  mutate(rel = ems_eff * prop_rel_dyao / 100, 
                         emi = ems_eff - rel) %>% 
                  select(district, age, sex, origin, emi, rel)
                  
                  # sum(emi$emi)
                  # sum(emi$rel)
                    
          #age plus 1, and with variable 'year'
              pop_end_year <- pop_nat %>% 
                  mutate(age = age + 1,
                      year = iyear) %>% 
                  select(district, year, age, sex, origin, pop_end_year) %>% 
                  rename(pop = pop_end_year) %>% 
                  filter(age <= age_max)
              tail(pop_end_year)
              
          #outputs: birth (separate, since no 'age' variable), with variable 'year'
              out_bir <- bir %>% 
                  mutate(year = iyear) %>% 
                  select(district, year, sex, origin, bir) %>% 
                  bind_rows(out_bir)

          #outputs: demographic processes, with variable 'year'
              out_dem <- dem_factor %>% 
                  select(district, age, sex, origin, 
                         dea_eff, ims_eff, ems_eff) %>% 
                  filter(age >= 0) %>% 
                  rename(dea = dea_eff, ims = ims_eff, ems = ems_eff) %>% 
                  left_join(imm, by = c("district", "age", "sex", "origin")) %>% 
                  rename(rei = rel) %>% 
                  left_join(emi, by = c("district", "age", "sex", "origin")) %>% 
                  rename(ree = rel) %>%                 
                  mutate(year = iyear) %>%                  
                  select(district, year, age, sex, origin, 
                         dea, ims, imm, rei, ems, emi, ree) %>%                  
                  bind_rows(out_dem)
              
          #outputs: end of year population
              out_pop <- pop_end_year %>% 
                  bind_rows(out_pop)
              
          #outputs: naturalization (separate, since no 'origin' variable), with variable 'year'
              out_nat <- nat_count %>% 
                  mutate(year = iyear) %>% 
                  select(district, year, age, sex, nat) %>% 
                  bind_rows(out_nat)              
              
#end of loop over years      
    }     
    
#TEST------------------------------------------ 
     Sys.time() -  t0test    
#TEST------------------------------------------     
   
     
    
#TEST------------------------------------------       

#ims, ems, mig
    test_ims %>% 
        left_join(test_ems, by = c("year", "district", "origin")) %>% 
        mutate(mig = ims - ems) %>% 
        pivot_longer(cols = c("ims", "ems", "mig")) %>% 
        filter(district == "Wollishofen") %>% 
        ggplot() +
            geom_line(aes(x = year, y = value, color = origin)) + 
            facet_wrap(~name, ncol = 3) + 
        neutral
    
#dem: pop (result after correction with housing data) 
    test_dem %>% 
        filter(district == "Wollishofen") %>% 
        select(year, origin, pop) %>% 
        ggplot() +
            geom_line(aes(x = year, y = pop, color = origin)) +
            expand_limits(y = 0) +
            neutral
    
#dem: bir, dea, dea_eff, nat      
    test_dem %>% 
        filter(district == "Wollishofen") %>% 
        mutate(nat = bir - dea_eff) %>% 
        select(year, origin, bir, dea, dea_eff, nat) %>% 
        pivot_longer(cols = c("bir", "dea", "dea_eff", "nat")) %>%       
        ggplot() +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_line(aes(x = year, y = value, color = origin)) + 
            facet_wrap(~name, ncol = 4) +    
            expand_limits(y = 0) +
            neutral
    
#dem: bir, dea, dea_eff, ims, ems, mig      
    test_dem %>% 
        filter(district == "Wollishofen") %>% 
        mutate(mig = ims - ems) %>% 
        select(year, origin, bir, dea, dea_eff, ims, ems, mig) %>% 
        pivot_longer(cols = c("bir", "dea", "dea_eff", "ims", "ems", "mig")) %>%       
        ggplot() +
            geom_line(aes(x = year, y = value, color = origin)) + 
            facet_wrap(~name, ncol = 6) +    
            expand_limits(y = 0) +
            neutral

#dem: difference bir - dea + ims - ems      
    test_dem %>% 
        filter(district == "Wollishofen") %>% 
        mutate(bal = bir - dea_eff + ims - ems) %>% 
        ggplot() +
            geom_line(aes(x = year, y = bal, color = origin)) + 
            expand_limits(y = 0) +
            neutral
        
#balance: ims, ems, new_ims, new_ems, new_ims3, new_ems3
    test_bal %>% 
        filter(district == "Wollishofen") %>% 
        select(year, ims, ems, new_ims, new_ems, new_ims3, new_ems3) %>% 
        pivot_longer(cols = c("ims", "ems", "new_ims", "new_ems", "new_ims3", "new_ems3")) %>% 
        mutate(mig_cat = if_else(name %in% c("ims", "new_ims", "new_ims3"), "immigration", "emigration"),
               number = case_when(name %in% c("ims", "ems") ~ "mig1",
                                  name %in% c("new_ims", "new_ems") ~ "mig2",  
                                  TRUE  ~ "mig3")) %>% 
        ggplot() +
            geom_line(aes(x = year, y = value, color = number)) + 
            facet_wrap(~mig_cat, ncol = 2) +    
            expand_limits(y = 0) +
            neutral
        
#plot factor    
    test_bal %>% 
        filter(district == "Wollishofen") %>% 
        select(year, factor_ims, factor_ems) %>%    
        pivot_longer(cols = c("factor_ims", "factor_ems")) %>% 
        ggplot() +
            geom_line(aes(x = year, y = value, color = name)) + 
            expand_limits(y = 0) +      
            geom_hline(yintercept = 1, linetype = "dashed") +
            neutral             
 
#processes (after factor per district was applied to ims and ems)  
    factor_app <- test_dem_factor %>%    
        filter(district == "Wollishofen") %>%   
        select(year, origin, ims_initial, ems_initial, ims_eff, ems_eff) %>% 
        group_by(year, origin) %>% 
            summarize_at(c("ims_initial", "ems_initial", "ims_eff", "ems_eff"), 
                         sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate(ims_diff = ims_initial - ims_eff,
               ems_diff = ems_initial - ems_eff,
               mig_initial = ims_initial - ems_initial,
               mig_eff = ims_eff - ems_eff, 
               mig_diff = mig_initial - mig_eff) %>% 
        pivot_longer(cols = c("ims_initial", "ems_initial", "mig_initial",
                              "ims_eff", "ems_eff", "mig_eff",
                              "ims_diff", "ems_diff", "mig_diff")) %>% 
        mutate(process = factor(case_when(name %in% c("ims_initial", "ims_eff", "ims_diff") ~ "immigration",
                                   name %in% c("ems_initial", "ems_eff", "ems_diff") ~ "emigration",
                                   TRUE ~ "migration balance"), 
                                levels = c("immigration", "emigration", "migration balance")),
               ord = factor(case_when(name %in% c("ims_initial", "ems_initial", "mig_initial") ~ "initial",
                                   name %in% c("ims_eff", "ems_eff", "mig_eff") ~ "final",
                                   TRUE ~ "correction"),
                            levels = c("initial", "final", "correction")))
    
        
        
    factor_app %>% 
        ggplot() +
            geom_hline(yintercept = 0, color = "grey80") +  
            geom_line(aes(x = year, y = value, color = ord)) + 
            facet_grid(origin ~ process) + 
            expand_limits(y = 0) + 
            neutral
  
    factor_app %>% 
        filter(process == "migration balance") %>% 
        filter(ord != "correction") %>% 
        ggplot() +
            geom_hline(yintercept = 0, color = "grey80") +  
            geom_line(aes(x = year, y = value, color = ord)) + 
            facet_grid(process ~ origin) + 
            expand_limits(y = 0) + 
            neutral
    
    
#population    
    pop_app <- test_dem_factor %>%    
        filter(district == "Wollishofen") %>%   
        select(year, origin, ims_eff, ems_eff, pop_bir_dea, pop_temp, pop_end_year) %>% 
        group_by(year, origin) %>% 
            summarize_at(c("ims_eff", "ems_eff", "pop_bir_dea", "pop_temp", "pop_end_year"), 
                         sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate(diff_pop = pop_temp - pop_end_year,
               mig_eff = ims_eff - ems_eff,
               diff_check = pop_end_year - pop_bir_dea,
               check = mig_eff - diff_check)
     
    sum(abs(pop_app$diff_pop))
    #pmax(0, x): not a problem
    
    sum(abs(pop_app$check))
    #is ok
    
    pop_app %>% 
        ggplot() +
            geom_line(aes(x = year, y = pop_end_year, color = origin)) + 
            expand_limits(y = 0) + 
            neutral 
    
    pop_temp <- pop_app %>% 
        select(year, origin, ims_eff, ems_eff, mig_eff, pop_bir_dea, pop_end_year, diff_check) %>% 
        pivot_longer(cols = c("ims_eff", "ems_eff", "mig_eff", "pop_bir_dea", "pop_end_year", "diff_check"))  
    
    pop_temp %>%     
        ggplot() +
            geom_hline(yintercept = 0, color = "grey80") +        
            geom_line(aes(x = year, y = value, color = origin)) + 
            facet_wrap(~name, ncol = 3) +
            expand_limits(y = 0) + 
            neutral        
    
    pop_temp %>%     
        ggplot() +
            geom_hline(yintercept = 0, color = "grey80") +        
            geom_line(aes(x = year, y = value, color = origin)) + 
            facet_wrap(~name, ncol = 3, scales = "free_y") +
            expand_limits(y = 0) + 
            neutral        
    
    
#the 'Wollishofen problem' is at birth/death
    
    temp_bir <- out_bir %>% 
        filter(district == "Wollishofen") %>%       
        group_by(year, origin) %>% 
            summarize(bir = sum(bir)) %>% 
        ungroup()           
    
    temp_processes <- out_dem %>%    
        filter(district == "Wollishofen") %>%   
        select(year, origin, dea, ims, ems) %>% 
        group_by(year, origin) %>% 
            summarize_at(c("dea", "ims", "ems"), 
                         sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        left_join(temp_bir, by = c("year", "origin")) %>% 
        mutate(nat = bir - dea, 
               mig = ims - ems,
               bal = nat + mig)
    
    temp_processes %>% 
        pivot_longer(cols = c("bir", "dea",
                              "ims", "ems", 
                              "nat", "mig", "bal")) %>%   
        ggplot() +
            geom_hline(yintercept = 0, color = "grey80") +        
            geom_line(aes(x = year, y = value, color = origin)) + 
            facet_wrap(~name, ncol = 4) +
            expand_limits(y = 0) + 
            neutral  

    
    temp_processes %>%
        pivot_longer(cols = c("bir", "dea",
                              "ims", "ems", 
                              "nat", "mig", "bal")) %>%     
        filter(name %in% c("bir", "dea", "nat", "mig", "bal")) %>%     
        ggplot() +
            geom_hline(yintercept = 0, color = "grey80") +        
            geom_line(aes(x = year, y = value, color = origin)) + 
            facet_wrap(~name, ncol = 5) +
            expand_limits(y = 0) + 
            neutral     
    
    
    
#test output: processes
    temp_processes %>% 
    arrange(origin, year) %>%       
    write_csv(paste0(out_path, "/test_processes.csv"))     
    
#test output: processes
    test_dem %>% 
    filter(district == "Wollishofen") %>% 
    arrange(origin, year) %>% 
    write_csv(paste0(out_path, "/test_pop.csv"))     
    
        
    
    
    
#TEST------------------------------------------       
    
  
    
       
    
    
    
    
#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#births
    out_bir %>% 
    arrange(district, year, sex, origin) %>% 
    write_csv(paste0(out_path, "/births_future.csv")) 

#demographic processes
    out_dem %>% 
    arrange(district, year, age, sex, origin) %>% 
    write_csv(paste0(out_path, "/demographic-processes_future.csv")) 
      
#population (end of year)
    out_pop %>% 
    arrange(district, year, age, sex, origin) %>% 
    write_csv(paste0(out_path, "/population_future.csv")) 
      
#naturalization
    out_nat %>% 
    arrange(district, year, age, sex) %>% 
    write_csv(paste0(out_path, "/naturalization_future.csv"))     
    
#log info    
    cat_log(paste0("demography and housing: ", 
        capture.output(Sys.time() - t0)))



                        
                    

    
