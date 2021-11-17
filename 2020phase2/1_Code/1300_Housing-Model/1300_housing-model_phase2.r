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

#left join on ownership (since this data set begins later) 
    pop_w <- left_join(own_dat, pop, by = c("district", "year")) %>% 
        filter(year <= date_end) %>% 
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
   
#proportion of cooperative housing according to capacity/reserves
#capacity/reserves contains only people due to additional (!) yearly (!) usage of reserves
#therefore, add the cumulative values to the past population    

    pop_total <- car_spa %>% 
        arrange(district, owner, year) %>%       
        group_by(district, owner) %>% 
            mutate(cumulative = cumsum(car)) %>% 
        ungroup() %>% 
        left_join(select(pop_last, district, owner, pop), 
                  by = c("district", "owner")) %>% 
        mutate(total = pop + cumulative) %>% 
        select(district, year, owner, total) %>% 
        rename(pop = total)
    
#with past (for plot)
    #why? for plotting
    #why a plot? to check if capacity/reserves population values are...  
    #...meaningful in comparison to the past
    
    pop_with_past <-  bind_rows(pop_w, pop_total) %>% 
        rename(distr = district) %>% 
        mutate(district = factor(distr, uni_d)) %>% 
        select(district, year, owner, pop)
   
#plot   
    sszplot(pop_with_past,
            aes_x = "year", aes_y = "pop", aes_col = "owner",
            labs_y = "people",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA), 
            i_x = c(NA, date_end),               
            name = "1300_people_capacity-reserves",
            width = 12, height = 14)          
    
#proportion cooperative housing (according to capacity/reserves vs. district trends)
    prop_coop <- pop_with_past %>% 
        mutate(simple = if_else(owner == uni_w[1], "cooperative", "private")) %>% 
        select(-owner) %>% 
        pivot_wider(names_from = simple, values_from = pop) %>% 
        mutate(prop_car = cooperative / (cooperative + private) * 100) %>% 
        select(district, year, prop_car) %>% 
        left_join(own_dat, by = c("district", "year")) %>% 
        rename(prop_trend = prop)
    
#plot preparation
    prop_coop_plot <- prop_coop %>% 
        pivot_longer(cols = c("prop_car", "prop_trend"), 
                     names_prefix = "prop_", 
                     names_to = "category", values_to = "prop") %>% 
        mutate(cat = if_else(category == "car", 
                             "capacity/reserves", "district trends"))
        
#plot   
    sszplot(prop_coop_plot,
            aes_x = "year", aes_y = "prop", aes_col = "cat",
            labs_y = "proportion of cooperative housing (in %)",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA), 
            i_x = c(NA, date_end),               
            name = "1301_proportion-cooperative-housing_data-sources",
            width = 12, height = 14)      
 
#new proportion of cooperative housing; apply the parameter (% from capacity/reserves)    
    new_prop <- prop_coop %>% 
        mutate(prop = prop_car * car_coop/100 + prop_trend * (1 - car_coop / 100)) %>% 
        filter(year >= szen_begin) %>% 
        select(district, year, prop)
    
#apply the new proportion
    new_pop_car <- pop_total %>% 
        group_by(district, year) %>% 
        summarize(pop = sum(pop)) %>% 
        left_join(new_prop, by = c("district", "year")) %>% 
        mutate(pop_cooperative = pop * prop / 100,
               pop_private = pop * (1 - prop / 100)) %>% 
        select(-c(pop, prop)) %>% 
        pivot_longer(cols = c("pop_cooperative", "pop_private"), 
                     names_prefix = "pop_", 
                     names_to = "category", values_to = "car") %>% 
        mutate(owner = if_else(category == "cooperative", uni_w[1], uni_w[2])) %>% 
        select(district, year, owner, car)
    
    
 
#-------------------------------------------------------------------
#combine: projects and capacity (with new proportion of cooperative housing)
#-------------------------------------------------------------------

#combine
    pro_car <- as_tibble(expand_grid(
            district = uni_d,
            year = date_end:szen_end,
            owner = uni_w)) %>% 
        left_join(pop_last, by = c("district", "year", "owner")) %>% 
        left_join(pro_aca, by = c("district", "year", "owner")) %>%       
        left_join(new_pop_car, by = c("district", "year", "owner")) %>% 
        replace_na(list(new = 0, removed = 0)) %>% 
        select(district, owner, year, pop, new, removed, car) %>% 
        arrange(district, owner, year)
    
    tail(pro_car)
    
    x <- filter(pro_car, (district == "Affoltern") & (owner == "cooperative housing")) %>% 
        select(-c(district, owner))
    
    x$new[2] <- 1000
     i <- 2
    
    # for (i in 2:nrow(x)){
      
    for (i in 2:10){   
        #new colums
            x$project <- NA
            x$corr <- NA
      
        #project list (cannot be negative)
            x$project[i] <- max(0, x$pop[i-1] + x$new[i] - x$removed[i])
    
        #select the larger value of projects and capacity/reserves
            #if projects larger: known projects will be realized
            #if capacity/reserves larger: not all future buildings in the project list
            x$pop[i] <- max(x$project[i], x$car[i])
            
        #correction of the reserves (if too much used due to the project list)
            x$corr[i] <- max(0, x$project[i] - x$car[i])    
            
        #correction (proportional to the usage)
            usage <- diff(x$car[i:nrow(x)])
            proportion <- usage/sum(usage)
            index <- (i+1):nrow(x) 
            x$car[index] <- x$car[index] - x$corr[i] * proportion

    }

    
    
as.data.frame(x)
      
