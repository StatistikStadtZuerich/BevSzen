#-------------------------------------------------------------------
#Housing Model
#
#
#
#rok/bad, October 2021
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

#projects (apartments, dyw)
    pro_dat <- read_csv(paste0(exp_path, "/projects_future.csv"))
    
#allocation (persons per apartment, dyw)
    aca_dat <- read_csv(paste0(exp_path, "/allocation_future.csv"))     
      
#capacity/reserves (m2, dyw)  
    car_dat <- read_csv(paste0(exp_path, "/usage_area.csv"))    
    
#living space (m2 per person, dyw)   
    spa_dat <- read_csv(paste0(exp_path, "/living-space_future.csv"))    
 
#ownership (% cooperative housing)
    own_dat <- read_csv(paste0(exp_path, "/ownership_past_future.csv")) %>% 
        unique()
    
    tail(own_dat)
    
#population   
    #why population not from the housing open data  file?
    #there only people in apartments (and not in care centers etc)
    #the population number in the housing open data is below the total 
    #amount of people in Zurich
    
    pop <- read_csv(pop_od) %>%   
        rename(year = StichtagDatJahr, pop = AnzBestWir) %>%  
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(district = factor(distr, uni_d)) %>%      
        select(district, year, pop) %>%    
        group_by(district, year) %>% 
            summarize(pop = sum(pop),
                      .groups = "drop")          
          

#-------------------------------------------------------------------
#projects and allocation (from apartments to people; future)
#-------------------------------------------------------------------

#calculate amount of people 
    pro_aca <- left_join(pro_dat, aca_dat, 
            by = c("district", "year", "owner")) %>% 
        mutate(people = apartments * aca_dyw) %>% 
        select(district, year, owner, indicator, people) %>% 
        pivot_wider(names_from = indicator, values_from = people)


#-------------------------------------------------------------------
#capacity/reserves and living space (from m2 to people; future)
#-------------------------------------------------------------------

#combine: calculate amount of people    
    #units: ha * 10,000 m2/ha / (m2/person) = person    
    
    car_spa <- left_join(car_dat, spa_dat, 
            by = c("district", "year", "owner")) %>% 
        mutate(car = usage_ha * 10000 / spa_dyw) %>% 
        select(district, year, owner, car)

 
#-------------------------------------------------------------------
#population by ownership (past)
#-------------------------------------------------------------------

#left join on ownership (since shorter data set) 
    pop_w <- left_join(own_dat, pop, by = c("district", "year")) %>% 
        mutate(cooperative = pop * prop / 100, 
               other = pop * (1 - prop / 100)) %>% 
        select(-c(prop, pop)) %>% 
        pivot_longer(c(cooperative, other), 
            names_to = "owner_text", values_to = "pop") %>% 
        mutate(owner = factor(if_else(owner_text == "cooperative", 
            uni_w[1], uni_w[2]), levels = uni_w)) %>% 
        select(district, year, owner, pop)
    
#last year of data
    pop_last <- filter(pop_w, year == date_end)
    
 
#-------------------------------------------------------------------
#combine: capacity/reserves and ownership prediction
#-------------------------------------------------------------------
   
#proportion cooperative housing according to capacity/reserves
    
  
#capacity/reserves contains only people due to additional (!) usage of reserves
#therefore, also the current population values are used    
    
    
    car_spa
    
    tail(own_dat) 
    
    
pop_last
      
    
    
    
    
    
      
#-------------------------------------------------------------------
#combine: projects and capacity
#-------------------------------------------------------------------

#combine
    pro_car <- as_tibble(expand_grid(
            district = uni_d,
            year = date_end:szen_end,
            owner = uni_w)) %>% 
        left_join(pop_last, by = c("district", "year", "owner")) %>% 
        left_join(pro_aca, by = c("district", "year", "owner")) %>%       
        left_join(car_spa, by = c("district", "year", "owner")) %>% 
        select(district, owner, year, pop, new, removed, car) %>% 
        arrange(district, owner, year)
    
    
    
    test <- filter(pro_car, (district == "Affoltern") & (owner == "cooperative housing")) %>% 
        select(-c(district, owner))
    
    



      
