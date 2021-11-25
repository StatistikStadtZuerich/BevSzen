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

#population
    
    #in contrast to rate calculations: population at the end of the year 
    #WHY: the rates will be applied here to the population of the previous year
  
    pop <- read_csv(pop_od) %>% 
        rename(year = StichtagDatJahr, age = AlterVCd, pop = AnzBestWir) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(district = factor(distr, uni_d),
               sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
               origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o)) %>% 
        select(district, year, age, sex, origin, pop) %>%       
        group_by(district, year, age, sex, origin) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup()   
    
#birth: fertility rate
    fer <- read_csv(paste0(exp_path, "/birth_fertility_future.csv")) %>% 
        mutate(sex = uni_s[2],
               origin = factor(origin, levels = uni_o))
    
#birth: origin change    
    cha <- read_csv(paste0(exp_path, "/birth_origin-change_future.csv")) %>% 
        mutate(origin = factor(origin, levels = uni_o))
               
#birth: proportion male 
    pro_male <- read_csv(paste0(exp_path, "/birth_sex-ratio_future.csv"))
       
#death: mortality rate
    mor <- read_csv(paste0(exp_path, "/mortality_future.csv"))
           
#immigration*: immigration* rate
    ims_rate <- read_csv(paste0(exp_path, "/immigration-star_rate-dy_future.csv"))
             
#immigration*: sex and origin proportion 
    ims_prop_so <- read_csv(paste0(exp_path, "/immigration-star_prop-so-dy_future.csv"))    
    
#immigration*: age proportion
    ims_prop_a <- read_csv(paste0(exp_path, "/immigration-star_prop-a-dyso_future.csv"))    
    
#emigration*: emigration* rate
    ems_rate <- read_csv(paste0(exp_path, "/emigration-star_rate-dy_future.csv"))
             
#emigration*: sex and origin proportion 
    ems_prop_so <- read_csv(paste0(exp_path, "/emigration-star_prop-so-dy_future.csv"))    
    
#emigration*: age proportion
    ems_prop_a <- read_csv(paste0(exp_path, "/emigration-star_prop-a-dyso_future.csv"))    
    
#relocation, immigration
    rei <- read_csv(paste0(exp_path, "/relocation_immigration_future.csv"))    
       
#relocation, emigration
    ree <- read_csv(paste0(exp_path, "/relocation_emigration_future.csv"))    
       
#naturalization
    nat <- read_csv(paste0(exp_path, "/naturalization_future.csv"))       

#housing model
    hou <- read_csv(paste0(exp_path, "/housing-model_population_d.csv"))       
    
   
#-------------------------------------------------------------------
#Loop over years
#-------------------------------------------------------------------
    
#last year of data
    pop_last <- filter(pop, year == date_end)
     
#years in the future
    future <- szen_begin:szen_end
    
#loop over years
    for (iyear in future){
      
        #iyear <- 2021
      
        #population at the begin of the year
            if(iyear == min(future)){popu <- select(pop_last, -year)}
      
        #births (fertility rate * women in the population)
            bir <- fer %>% 
                filter(year == iyear) %>% 
                left_join(popu, by = c("district", "age", "sex", "origin")) %>% 
                replace_na(list(fer = 0, pop = 0)) %>%               
                mutate(birth = pop * fer / 100) %>% 
                group_by(district, year, origin) %>% 
                    summarize(birth = sum(birth)) %>% 
                ungroup()
            
                #sum(bir$birth)

        #births: origin changes (from mother to baby) and sex (of the baby)
            bir_so <- cha %>% 
                filter(year == iyear) %>% 
                select(-year) %>% 
                right_join(bir, by = c("district", "origin")) %>% 
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
                ungroup() %>% 
                left_join(filter(pro_male, year == iyear), by = "year") %>% 
                mutate(male = birth * pro_male / 100,
                       female = birth - male) %>% 
                select(-c(birth, pro_male)) %>% 
                pivot_longer(cols = uni_s, 
                             names_to = "sex", values_to = "birth") %>% 
                mutate(age = 0, 
                       sex = factor(sex, levels = uni_s)) %>% 
                select(district, year, age, sex, origin, birth) %>% 
                arrange(district, sex, origin)
            
                #sum(bir$birth)            
                #sum(bir_so$birth)                       
                       
                                   
  
            
                 
#end of loop over years      
    }     
    
    
  
    
    
    
                        
                    

    
