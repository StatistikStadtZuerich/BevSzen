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
                mutate(bir = pop * fer / 100) %>% 
                group_by(district, year, origin) %>% 
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
                select(district, origin, year, change, keep) %>% 
                pivot_longer(cols = c("change", "keep"), 
                             names_to = "category", values_to = "bir") %>%
                mutate(new_origin = case_when(category == "keep" ~ origin,
                                              origin == uni_o[1] ~ uni_o[2],
                                              TRUE ~ uni_o[1])) %>% 
                select(district, year, new_origin, bir) %>% 
                rename(origin = new_origin) %>% 
                group_by(district, year, origin) %>% 
                    summarize(bir = sum(bir)) %>% 
                ungroup()
            
        #births: with variable 'sex'
            
            #why age -1? still perspective at the begin of the year
            #the newborn babies are one year younger than the population at age zero
            #ageing of the population is executed at the very end of the code
            
            bir <- bir_do_new %>%            
                left_join(filter(pro_male, year == iyear), by = "year") %>% 
                mutate(male = bir * pro_male / 100,
                       female = bir - male) %>% 
                select(-c(bir, pro_male)) %>% 
                pivot_longer(cols = uni_s, 
                             names_to = "sex", values_to = "bir") %>% 
                mutate(age = -1, 
                       sex = factor(sex, levels = uni_s)) %>% 
                select(district, year, age, sex, origin, bir) %>% 
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
                select(district, year, age, sex, origin, dea)

                #sum(dea$dea)                                           
  
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
                replace_na(list(pop = 0, bir = 0, dea = 0, ims = 0, ems = 0)) %>% 
                #not more than the entire population can die...
                #therefore, calcualate effective deaths
                mutate(dea_eff = pmin(dea, pop),
                       pop_bir_dea = pop + bir - dea_eff)
            
            # sum(dem$pop)
            # sum(bir$bir) - sum(dea$dea)
            # sum(dem$pop_bir_dea)
            # sum(dem$pop) + sum(bir$bir) - sum(dea$dea)

            
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
            
        #apply the correction factors to the immigration* and emigration* by 
        #district, age, sex, origin (same factor by district)   
            dem_factor <- bal %>% 
                select(district, year, factor_ims, factor_ems) %>% 
                right_join(dem, by = c("district", "year")) %>% 
                mutate(ims_eff = ims * factor_ims, 
                       ems_eff = ems * factor_ems,
                       pop_end_year = pmax(0, pop_bir_dea + ims_eff - ems_eff)) %>% 
                select(district, year, age, sex, origin, 
                       bir, dea_eff, ims_eff, pop_end_year)
            
         #sum(dem_factor$pop_end_year)
            
         #with naturalization (only on end-year-population)
            
            #the naturalization rate of all new born babies of zero is correct
            #WHY? the origin changes at birth has already been calculated in the model
            
            pop_nat <- nat %>%    
                filter(year == iyear) %>%  
                right_join(select(dem_factor, district, year, age, sex, 
                                  origin, pop_end_year), 
                           by = c("district", "year", "age", "sex", "origin")) %>% 
                replace_na(list(rate_nat = 0))  %>% 
                arrange(district, year, age, sex, origin) %>% 
                mutate(Swiss = if_else(origin == uni_o[1], pop_end_year,
                                       pop_end_year * rate_nat / 100),
                       foreign = if_else(origin == uni_o[1], 0,
                                       pop_end_year * (1 - rate_nat / 100))) %>% 
                select(-c(origin, pop_end_year)) %>% 
                pivot_longer(cols = uni_o, 
                             names_to = "origin", values_to = "pop_end_year") %>% 
                mutate(origin = factor(origin, levels = uni_o)) %>% 
                select(district, year, age, sex, origin, pop_end_year)
            
          #determine immigration from immigration*       
              # dem_factor
            

            
   


         
                # #relocation, immigration
                #     rei <- read_csv(paste0(exp_path, "/relocation_immigration_future.csv")) %>% 
                #         mutate(district = factor(district, levels = uni_d),
                #             origin = factor(origin, levels = uni_o))         
                # 
                #     





          #determine emigration from emigration*       
  
    
    
    
           
              # #relocation, emigration
              #     ree <- read_csv(paste0(exp_path, "/relocation_emigration_future.csv")) %>% 
              #         mutate(district = factor(district, levels = uni_d),
              #             origin = factor(origin, levels = uni_o))           
              #                    
              #             
              #             
            
            

            
dem_factor
            
            

            
 

            
                 
#end of loop over years      
    }     
    
    
  
    
    
    
                        
                    

    
