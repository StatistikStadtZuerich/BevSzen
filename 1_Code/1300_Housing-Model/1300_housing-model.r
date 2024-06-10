# header ------------------------------------------------------------------
# housing model

# paths, general ----------------------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# import, data preparation ------------------------------------------------

# projects (apartments, dyw)
pro_dat <- read_csv(paste0(exp_path, "/projects_future.csv"), lazy = FALSE)

# allocation (persons per apartment, dyw)
aca_dat <- read_csv(paste0(exp_path, "/allocation_future.csv"), lazy = FALSE)

# car_dat
car_dat <- read_csv(paste0(exp_path, "/usage_area.csv"), lazy = FALSE)

# living space (m2 per person, dyw)
spa_dat <- read_csv(paste0(exp_path, "/living-space_future.csv"), lazy = FALSE)

# ownership (proportion of people living in cooperative housing, past)
own_dat <- read_csv(spa_od) %>%
  rename(year = StichtagDatJahr, apartments = AnzWhgStat, people = AnzBestWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    owner = fact_if(EigentuemerSSZPubl3Cd_noDM, uni_w),
    district = factor(distr, uni_d)
  ) %>%
  select(year, district, owner, people) %>%
  group_by(district, year, owner) %>%
  summarize(
    people = sum_NA(people),
    .groups = "drop") %>%
  group_by(district, year) %>%
  mutate(
    prop = round(people / sum_NA(people) * 100, round_prop),
    .groups = "drop"
  ) %>%
  filter(owner == uni_w[1]) %>%
  select(district, year, prop)

# population
# why population not from the housing open data file?
# this file only contains people in apartments (and not in care centers etc)
# the population number in the housing open data is below the total
# amount of people in Zurich

# dy (past years)
pop <- read_csv(pop_od, lazy = FALSE) %>%
  rename(year = StichtagDatJahr, pop = AnzBestWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(district = factor(distr, uni_d)) %>%
  select(district, year, pop) %>%
  group_by(district, year) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )

# projects and allocation (from apartments to people; future) -------------

# calculate amount of people from 
# - projects 'pro' (num of apartments)
# - allocation 'aca' (number of pop per apartment)
pro_aca <- left_join(pro_dat, aca_dat,
  by = c("district", "year", "owner")
) %>%
  mutate(people = apartments * aca_dyw) %>%
  select(district, year, owner, indicator, people) %>%
  pivot_wider(names_from = indicator, values_from = people)

# capacity/reserves and living space (from m2 to people; future) ----------

# combine: calculate amount of people
# units: ha * 10,000 m2/ha / (m2/person) = person

# calculate amount of people from ...
# - capacity 'car' (area)
# - living space 'spa' (area consumption)
car_spa <- left_join(car_dat, spa_dat,
  by = c("district", "year", "owner")
) %>%
  mutate(car = usage_ha * 10000 / spa_dyw) %>%
  select(district, year, owner, car)

# population by ownership (past) ------------------------------------------

# join pop on ownership (since this data set begins later)
# OUT: population of past year per ownership > this ownership information is still necessary
pop_w <- left_join(own_dat, pop, by = c("district", "year")) %>%
  filter(year <= date_end) %>%
  mutate(
    cooperative = pop * prop / 100,
    other = pop * (1 - prop / 100)
  ) %>%
  select(-c(prop, pop)) %>%
  pivot_longer(c(cooperative, other),
    names_to = "owner_text", values_to = "pop"
  ) %>%
  mutate(owner = fact_if(owner_text, uni_w, "cooperative")) %>%
  select(district, year, owner, pop)

# last year of data 
pop_last <- pop_w %>%
  ungroup() %>%
  filter(year == date_end)

# combine: capacity/reserves and ownership prediction ---------------------

# proportion of cooperative housing according to capacity/reserves
# capacity/reserves contains only people due to additional (!) yearly (!) usage of reserves
# therefore, add the cumulative values to the past population
pop_total <- car_spa %>%
  arrange(district, owner, year) %>%
  group_by(district, owner) %>%
  mutate(cumulative = cumsum(car)) %>%
  ungroup() %>%
  left_join(select(pop_last, district, owner, pop),
    by = c("district", "owner")
  ) %>%
  mutate(total = pop + cumulative) %>%
  select(district, year, owner, total) %>%
  rename(pop = total)

# row-binding past_pop and pop_total (which is today's pop + cumulative car_spa)
# Why doing so? 
# For plotting reasons, to for plotting to check if capacity/reserves population 
# values are meaningful in comparison to the past
pop_with_past <- bind_rows(pop_w, pop_total) %>%
  rename(distr = district) %>%
  mutate(district = factor(distr, uni_d)) %>%
  select(district, year, owner, pop)


# factorize and subset - form final output tibble
new_pop_car <- pop_total %>%
  mutate(owner = if_else(owner == "cooperative housing", uni_w[1], uni_w[2])) %>%
  select(district, year, owner, pop) %>%
  mutate(car = pop) %>%
  select(-pop)

# combine: projects and capacity (with new prop of cooperative) -----------

# Combining all prepared data in one tibble
# dyo with pop (current year), new, removed and car (from capacity)
pro_car <- as_tibble(expand_grid(
  district = uni_d,
  year = date_end:scen_end,
  owner = uni_w
)) %>%
  left_join(pop_last, by = c("district", "year", "owner")) %>%
  left_join(pro_aca, by = c("district", "year", "owner")) %>%
  left_join(new_pop_car, by = c("district", "year", "owner")) %>%
  replace_na(list(new = 0, removed = 0)) %>%
  select(district, owner, year, pop, new, removed, car) %>%
  arrange(district, owner, year)

# function: consider projects and reserves --------------------------------

# consider projects and reserves
project_reserves <- function(x, ...) {

  # maximum usage of the capacity, plus population
  car_max <- max_NA(x$car)

  # new columns
  x$project <- NA
  x$corr <- NA
  x$car[1] <- x$pop[1]
  x$usage <- c(0, diff(x$car))

  # loop from second year (i.e. first year in the future) to the last prediction year
  for (i in 2:nrow(x)) {

    # use the information from the project list (the result can not be negative)
    x$project[i] <- max(0, x$pop[i - 1] + x$new[i] - x$removed[i])

    # trust the projects or reserves?
    # if reserves larger, then select reserves
    # why? not all future buildings in the project list 
    # if projects larger: apply the parameter 'car_trust'
    # for car_trust = 100 the lower reserves-value is used
    # for car_trust = 0 the higher project-value is used    
  
    if (x$car[i] > x$project[i]) {
      x$pop[i] <- x$car[i]
    } else {
      x$pop[i] <- x$project[i] - (x$project[i] - x$car[i]) * (car_trust/100)
    }

    # correction of the reserves (if too much used due to the project list)
    x$corr[i] <- max(0, x$pop[i] - x$car[i])

    # correction (proportional to the reserve usage)

    if (i < nrow(x)) {
      index <- (i + 1):nrow(x)
      x$subtract <- 0

      # correction proportional to usage
      if (sum(x$usage[index]) > 0) {
        x$subtract[index] <- x$usage[index] / sum(x$usage[index]) * x$corr[i]
      } else {
        x$subtract[index] <- 0
      }

      # subtract (usage per year, and cumulative usage plus population)
      x$usage[index] <- pmax(0, x$usage[index] - x$subtract[index])

      # new reserves value based on corrected usage
      x$car[i+1] <- pmax(0, x$pop[i] + x$usage[i+1])

    }

    # end of the loop over years
  }

  # output
  # if the population exceeds the maximum possible population according the reserves calculation
  # trust the reserves or the calculated population?

  y <- x %>%
    mutate(
      upper_limit_car = pmin(pop, car_max),
      diff_to_limit = pop - upper_limit_car,
      to_subtract = car_max_trust / 100 * diff_to_limit,
      pop_new = pop - to_subtract
    ) %>%
    select(district, year, owner, pop_new) %>%
    rename(pop = pop_new) %>%
    filter(year >= scen_begin)
  print(y)

}

# consider projects and reserves (apply the function)
pro_res_all <- pro_car %>%
  group_split(district, owner) %>%
  map(project_reserves) %>%
  bind_rows()

# check: car vs. car/projects
car_pop_y <- new_pop_car |> 
  left_join(pro_res_all, by = c("district", "year", "owner")) |>
  group_by(year) |>
  summarize(car = sum(car),
            pop = sum(pop), .groups = "drop") |>
  mutate(diff = pop - car)

tail(car_pop_y)

ggplot(car_pop_y) +
  geom_line(aes(x = year, y = diff))






# apply the parameter of empty apartments ---------------------------------

# parameter can be applied directly to the population
pro_res <- pro_res_all %>%
  mutate(
    empty_prop = if_else(owner == uni_w[1], empty_coop, empty_private),
    pop_new = pop * (100 - empty_prop) / 100
  ) %>%
  select(district, year, owner, pop_new) %>%
  rename(pop = pop_new)

# plots -------------------------------------------------------------------

# with the past
pop_fut_past <- bind_rows(pop_w, pro_res) %>%
  mutate(district = factor(district, uni_d)) %>%
  select(district, year, owner, pop)

# plot 1302

# by district (without owner)
pop_d <- pop_fut_past %>%
  group_by(district, year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

# plot 1303

# all (entire city)
pop_all <- pop_fut_past %>%
  group_by(year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

# plot 1304

# plots 1305, 1306, 1307, 1308, 1309: compare projects and reserves




# export the results ------------------------------------------------------

# per district and ownership
write_csv(pop_fut_past %>% arrange(district, year, owner),paste0(exp_path, "/housing_model_population_dw.csv"))

# per district
ex_data_d <- arrange(pop_d, district, year)
write_csv(ex_data_d, paste0(exp_path, "/housing-model_population_d.csv"))


# entire city (to compare with past publications)
ex_data_all <- arrange(pop_all, year)
write_csv(ex_data_all, paste0(exp_path, "/housing-model_population_all.csv"))

# log info
cat_log(paste0(
  "housing model: ",
  capture.output(Sys.time() - t0)
))