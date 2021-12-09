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

#start time
    t0 <- Sys.time() 
    
#-------------------------------------------------------------------
#import, data preparation
#-------------------------------------------------------------------

#projects (apartments, dyw)
    pro_dat <- read_csv(paste0(exp_path, "/projects_future.csv"))
    
#allocation (persons per apartment, dyw)
    aca_dat <- read_csv(paste0(exp_path, "/allocation_future.csv"))     
      
#capacity/reserves (m2, dyw)  
    car_dat <- read_csv(paste0(exp_path, "/usage_area.csv")) 
    
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    y <- car_dat %>% 
        filter((district == "Albisrieden") & (owner == "cooperative housing"))
    plot(y$year, y$usage_ha)
    plot(y$year, cumsum(y$usage_ha))
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
    
#living space (m2 per person, dyw)   
    spa_dat <- read_csv(paste0(exp_path, "/living-space_future.csv"))    
 
#ownership (% cooperative housing)
    own_dat <- read_csv(paste0(exp_path, "/ownership_past_future.csv"))
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

#if additional information is available on new projects (e.g. amount of people)
    #this could be incorporated here
    

#-------------------------------------------------------------------
#capacity/reserves and living space (from m2 to people; future)
#-------------------------------------------------------------------

#combine: calculate amount of people    
    #units: ha * 10,000 m2/ha / (m2/person) = person    
    
    car_spa <- left_join(car_dat, spa_dat, 
            by = c("district", "year", "owner")) %>% 
        mutate(car = usage_ha * 10000 / spa_dyw) %>% 
        select(district, year, owner, car)
    tail(car_spa)
    
    
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    z <- car_spa %>% 
        filter((district == "Albisrieden") & (owner == "cooperative housing"))
    plot(z$year, z$car)
    plot(z$year, cumsum(z$car))
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
        
    
    
 
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
    
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    u <- pop_total %>% 
        filter((district == "Albisrieden") & (owner == "cooperative housing"))
    plot(u$year, u$pop)
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
            
    
    
    
#with past (for plot)
    #why? for plotting
    #why a plot? to check if capacity/reserves population values are...  
    #...meaningful in comparison to the past
    
    pop_with_past <-  bind_rows(pop_w, pop_total) %>% 
        rename(distr = district) %>% 
        mutate(district = factor(distr, uni_d)) %>% 
        select(district, year, owner, pop)
    
    
    
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    v <- pop_with_past %>% 
        filter((district == "Albisrieden") & (owner == "cooperative housing"))
    plot(v$year, v$pop, type = "o", ylim = c(0, max(v$pop) * 1.1))
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
            
        
   
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
                             "capacity/reserves", "district trends"),
               district = factor(district, levels = uni_d))
        
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

    
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    w <- new_pop_car %>% 
        filter((district == "Albisrieden") & (owner == "cooperative housing"))
    plot(w$year, w$car, type = "o")
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
            
            
    
 
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
    
    
#function: consider projects and reserves

    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
      x <- filter(pro_car, (district == "Albisrieden") & (owner == "cooperative housing"))
      # plot(x$year, x$car, type = "o")
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
      
      
      #consider projects and reserves
          project_reserves <- function(x,...){

          #new colums
                x$car[1] <- x$pop[1]
                x$usage <- c(0, diff(x$car))
                x$pro <- x$new - x$removed
                x$maxi <- pmax(x$usage, x$pro)
                
                
            
                x$project <- NA
                x$corr <- NA
        
          #loop from second year (i.e. first year in the future) to the last prediction year
                # for (i in 2:nrow(x)){
                
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
                as.data.frame(x)
                
                plot(x$year, x$pop, type = "o")
                plot(x$year, x$car, type = "o")
          
                
                
                i <- 4
                
                
              
                for (i in 2:nrow(x)){
      #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo    
                  
          
                    #use the information from the project list (the result can not be negative)
                        x$project[i] <- max(0, x$pop[i-1] + x$new[i] - x$removed[i])
                
                    #select the larger value of projects and capacity/reserves
                        #if projects larger: known projects will be realized
                        #if capacity/reserves larger: not all future buildings in the project list
                        x$pop[i] <- max(x$project[i], x$car[i])
                        
                    #correction of the reserves (if too much used due to the project list)
                        x$corr[i] <- max(0, x$project[i] - x$car[i])  
                        
                        
                        
                        
                        
                        
                    # #correction (proportional to the capacity)
                    #     #WHY not proportional to the usage?
                    #     #if the future capacity is also calculated based on ownership trends,
                    #     #then the usage could be negative
                    #     #a down-correction proportional to a negative usage does not make sense
                    #     
                    #     if(i < nrow(x)){
                    #         index <- (i+1):nrow(x)
                    #         x$subtract <- 0
                    #         x$subtract[index] <- x$car[index]/sum(x$car[index])*x$corr[i]
                    #         x$car[index] <- pmax(0, x$car[index] - x$subtract[index]) #no negative population limit values
                    #     }    
                    #     
                        
                        
                    # #correction (proportional to the usage)
                    #     if(i < nrow(x)){
                    #         index1 <- i:nrow(x)
                    #         index2 <- (i+1):nrow(x)
                    #         x$usage <- 0
                    #         x$mult <- 0
                    #         x$usage[index2] <- diff(x$car[index1])
                    #         sum_usage <- sum(x$usage)
                    #         if(sum_usage > 0){x$mult[index2] <- x$usage[index2] / sum_usage} #correct only, if reserves left
                    #         x$dif <- x$corr[i] * x$mult
                    #         x$correction <- pmin(x$usage, x$dif) #the correction per year cannot be larger than the usage per year
                    #         x$car <- pmax(0, x$car - x$correction) #better be safe: no negative population limit values
                    #     }

                #end of the loop over years          
                }                        
                            
         
                #output
                    y <- select(x, district, year, owner, pop) %>%
                        filter(year >= szen_begin)
                    print(y)
  
        #end of the function
        }      

    # project_reserves(x)          
          
          
#consider projects and reserves (apply the function)
    splitted <- pro_car %>% 
        group_split(district, owner) 
    pro_res_all <- bind_rows(lapply(splitted, project_reserves))
    
  
#-------------------------------------------------------------------
#apply the parameter of empty apartments
#-------------------------------------------------------------------

#prediction: back from people to apartments to apply the parameter
    # pro_res <- left_join(pro_res_all, aca_dat, 
    #                         by = c("district", "year", "owner")) %>% 
    #     mutate(apartments = pop / aca_dyw, 
    #            empty_prop = if_else(owner == uni_w[1], empty_coop, empty_private), 
    #            empty_apart = apartments * empty_prop / 100, 
    #            diff_people = empty_apart * aca_dyw, 
    #            pop_new = pop - diff_people)
    
   
#parameter can be applied directly to the population
    pro_res <- pro_res_all %>% 
        mutate(empty_prop = if_else(owner == uni_w[1], empty_coop, empty_private), 
               pop_new = pop * (100 - empty_prop) / 100) %>% 
        select(district, year, owner, pop_new) %>% 
        rename(pop = pop_new)
    

#-------------------------------------------------------------------
#plots
#-------------------------------------------------------------------
    
#with the past    
    pop_fut_past <- bind_rows(pop_w, pro_res) %>%    
        rename(distr = district) %>%  
        mutate(district = factor(distr, uni_d)) %>% 
        select(district, year, owner, pop)
    
#plot   
    sszplot(pop_fut_past,
            aes_x = "year", aes_y = "pop", aes_col = "owner",
            labs_y = "people",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA), 
            i_x = c(NA, date_end),               
            name = "1302_population_district-owner",
            width = 12, height = 14)     
    
#by district (without owner)  
    pop_d <- pop_fut_past %>% 
        group_by(district, year) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup()
    
#plot   
    sszplot(pop_d,
            aes_x = "year", aes_y = "pop", 
            labs_y = "people",            
            wrap = "district", ncol = 4,
            scale_y = c(0, NA), 
            i_x = c(NA, date_end),               
            name = "1303_population_district",
            width = 12, height = 14)      
    
#all (entire city)  
    pop_all <- pop_fut_past %>% 
        group_by(year) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup()
        
    sszplot(pop_all,
            aes_x = "year", aes_y = "pop", 
            labs_y = "people",            
            scale_y = c(0, NA), 
            i_x = c(NA, date_end),               
            name = "1304_population",
            width = 7, height = 5)      
        
    
#-------------------------------------------------------------------
#export the results
#-------------------------------------------------------------------

#per district  
    ex_data_d <- arrange(pop_d, district, year)
    write_csv(ex_data_d, paste0(exp_path, "/housing-model_population_d.csv"))       
   

#entire city (to compare with past publications) 
    ex_data_all <- arrange(pop_all, year)
    write_csv(ex_data_all, paste0(exp_path, "/housing-model_population_all.csv"))       
    
#log info    
    cat_log(paste0("housing model: ", 
        capture.output(Sys.time() - t0)))    
    
        
    
    
    
    
    
