# header ------------------------------------------------------------------
# housing model

# paths, general ----------------------------------------------------------

# general functions already available?
if (!exists("para")) {

  # working directory
  library(here)
  setwd(paste0(here(), "/2020phase2/"))

  # general functions (without dependence on parameters)
  source("1_Code/0000_General/0002_general_without-parameters.r")

  # parameters (depend on scenario)
  i_scen <- "middle"
  for (i_para in 1:nrow(para)) {
    assign(para$parameter[i_para], para[[i_scen]][i_para],
      envir = .GlobalEnv
    )
  }

  # general functions (with dependence on parameters)
  source(paste0(code_path, "/0000_General/0003_general_with-parameters.r"))
}

# start time
t0 <- Sys.time()



# import, data preparation ------------------------------------------------

# projects (apartments, dyw)
pro_dat <- read_csv(paste0(exp_path, "/projects_future.csv"), lazy = FALSE) %>% 
    mutate(apartments = if_else(district == "Wollishofen", 0, apartments))
# ------------------------------------------------------------------------------------------test

# allocation (persons per apartment, dyw)
aca_dat <- read_csv(paste0(exp_path, "/allocation_future.csv"), lazy = FALSE)

# capacity/reserves (m2, dyw)
car_dat <- read_csv(paste0(exp_path, "/usage_area.csv"), lazy = FALSE) %>% 
  mutate(usage_ha = if_else(district == "Wollishofen", usage_ha * 3, usage_ha))
# ------------------------------------------------------------------------------------------test

# living space (m2 per person, dyw)
spa_dat <- read_csv(paste0(exp_path, "/living-space_future.csv"), lazy = FALSE)

# ownership (% cooperative housing)
own_dat <- read_csv(paste0(exp_path, "/ownership_past_future.csv"), lazy = FALSE)


# population
# why population not from the housing open data file?
# there only people in apartments (and not in care centers etc)
# the population number in the housing open data is below the total
# amount of people in Zurich

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

# calculate amount of people
pro_aca <- left_join(pro_dat, aca_dat,
  by = c("district", "year", "owner")
) %>%
  mutate(people = apartments * aca_dyw) %>%
  select(district, year, owner, indicator, people) %>%
  pivot_wider(names_from = indicator, values_from = people)

# if additional information is available on new projects (e.g. amount of people)
# this could be incorporated here


# capacity/reserves and living space (from m2 to people; future) ----------

# combine: calculate amount of people
# units: ha * 10,000 m2/ha / (m2/person) = person

car_spa <- left_join(car_dat, spa_dat,
  by = c("district", "year", "owner")
) %>%
  mutate(car = usage_ha * 10000 / spa_dyw) %>%
  select(district, year, owner, car)


# population by ownership (past) ------------------------------------------

# join pop on ownership (since this data set begins later)
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
  mutate(owner = factor(if_else(owner_text == "cooperative",
    uni_w[1], uni_w[2]
  ), levels = uni_w)) %>%
  select(district, year, owner, pop)

# last year of data
pop_last <- filter(pop_w, year == date_end)



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


# with past (for plot)
# why? for plotting
# why a plot? to check if capacity/reserves population values are...
# ...meaningful in comparison to the past

pop_with_past <- bind_rows(pop_w, pop_total) %>%
  rename(distr = district) %>%
  mutate(district = factor(distr, uni_d)) %>%
  select(district, year, owner, pop)

# plot
sszplot(pop_with_past,
  aes_x = "year", aes_y = "pop", aes_col = "owner",
  labs_y = "people",
  wrap = "district", ncol = 4,
  scale_y = c(0, NA),
  i_x = c(NA, date_end),
  name = "1300_people_capacity-reserves",
  width = 12, height = 14
)


# proportion cooperative housing (according to capacity/reserves vs. district trends)
prop_coop <- pop_with_past %>%
  mutate(simple = if_else(owner == uni_w[1], "cooperative", "private")) %>%
  select(-owner) %>%
  pivot_wider(names_from = simple, values_from = pop) %>%
  mutate(prop_car = cooperative / (cooperative + private) * 100) %>%
  select(district, year, prop_car) %>%
  left_join(own_dat, by = c("district", "year")) %>%
  rename(prop_trend = prop)

# plot preparation
prop_coop_plot <- prop_coop %>%
  pivot_longer(
    cols = c("prop_car", "prop_trend"),
    names_prefix = "prop_",
    names_to = "category", values_to = "prop"
  ) %>%
  mutate(
    cat = if_else(category == "car",
      "capacity/reserves", "district trends"
    ),
    district = factor(district, levels = uni_d)
  )

# plot
sszplot(prop_coop_plot,
  aes_x = "year", aes_y = "prop", aes_col = "cat",
  labs_y = "proportion of cooperative housing (in %)",
  wrap = "district", ncol = 4,
  scale_y = c(0, NA),
  i_x = c(NA, date_end),
  name = "1301_proportion-cooperative-housing_data-sources",
  width = 12, height = 14
)

# new proportion of cooperative housing; apply the parameter (% from capacity/reserves)
new_prop <- prop_coop %>%
  mutate(prop = prop_car * car_coop / 100 + prop_trend * (1 - car_coop / 100)) %>%
  filter(year >= scen_begin) %>%
  select(district, year, prop)

# apply the new proportion
new_pop_car <- pop_total %>%
  group_by(district, year) %>%
  summarize(pop = sum(pop)) %>%
  left_join(new_prop, by = c("district", "year")) %>%
  mutate(
    pop_cooperative = pop * prop / 100,
    pop_private = pop * (1 - prop / 100)
  ) %>%
  select(-c(pop, prop)) %>%
  pivot_longer(
    cols = c("pop_cooperative", "pop_private"),
    names_prefix = "pop_",
    names_to = "category", values_to = "car"
  ) %>%
  mutate(owner = if_else(category == "cooperative", uni_w[1], uni_w[2])) %>%
  select(district, year, owner, car)

# combine: projects and capacity (with new prop of cooperative) -----------

# combine
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

  # new colums
  x$project <- NA
  x$corr <- NA

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

    # correction (proportional to the capacity)
    # WHY not proportional to the usage?
    # if the future capacity is also calculated based on ownership trends,
    # then the usage could be negative
    # a down-correction proportional to a negative usage does not make sense

    if (i < nrow(x)) {
      index <- (i + 1):nrow(x)
      x$subtract <- 0

      # if sum of population limit greater than zero: proportional correction,
      # else uniform correction
      if (sum(x$car[index]) > 0) {
        x$subtract[index] <- x$car[index] / sum(x$car[index]) * x$corr[i]
      }
      else {
        x$subtract[index] <- rep(1 / length(index), length(index)) * x$corr[i]
      }

      # no negative population limit values
      x$car[index] <- pmax(0, x$car[index] - x$subtract[index])
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



# Check
x <- filter(pro_car, (district == "Wollishofen") & (owner == "cooperative housing"))
plot(x$year, x$car, type = "o")
project_reserves(x)


# consider projects and reserves (apply the function)
pro_res_all <- pro_car %>%
  group_split(district, owner) %>%
  map(project_reserves) %>%
  bind_rows()

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

# plot
sszplot(pop_fut_past,
  aes_x = "year", aes_y = "pop", aes_col = "owner",
  labs_y = "people",
  wrap = "district", ncol = 4,
  scale_y = c(0, NA),
  i_x = c(NA, date_end),
  name = "1302_population_district-owner",
  width = 12, height = 14
)

# by district (without owner)
pop_d <- pop_fut_past %>%
  group_by(district, year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

# plot
sszplot(pop_d,
  aes_x = "year", aes_y = "pop",
  labs_y = "people",
  wrap = "district", ncol = 4,
  scale_y = c(0, NA),
  i_x = c(NA, date_end),
  name = "1303_population_district",
  width = 12, height = 14
)

# all (entire city)
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
  width = 7, height = 5
)


# compare projects and reserves -------------------------------------------

# projects and reserves (cumulative amount of people)
comp_pro_car <- pro_car %>% 
  replace_na(list(pop = 0, car = 0)) %>% 
  mutate(car_cum = if_else(year == date_end, pop, car),
         pro_y = if_else(year == date_end, pop, new - removed)) %>% 
  group_by(district, owner) %>% 
      arrange(year) %>% 
      mutate(pro_cum = cumsum(pro_y)) %>% 
  ungroup() %>% 
  arrange(district, owner, year) %>% 
  select(district, owner, year, pro_cum, car_cum)

#with final amount of people
cat_level <- c("projects", "reserves", "final")

pro_res_all %>%
  left_join(comp_pro_car, by = c("district", "year", "owner")) %>%
  pivot_longer(c(pro_cum, car_cum, pop), names_to = "category", values_to = "people") %>%
  mutate(
    district = factor(district, levels = uni_d),
    cat = factor(case_when(
      category == "pro_cum" ~ cat_level[1],
      category == "car_cum" ~ cat_level[2],
      TRUE ~ cat_level[3]
    ), levels = cat_level)
  ) %>%
  select(district, year, owner, cat, people) %>%
  # plot
  sszplot(
    aes_x = "year", aes_y = "people", aes_col = "cat",
    labs_y = "people",
    wrap = "district", ncol = 4, gridscale = "free_y",
    scale_y = c(0, NA),
    name = "1305_projects_reserves_final",
    width = 12, height = 14,
    multi = uni_w
  )

# total per district and owner
# WHY not with the object car_spa?
# it is easier to calculate totals with the original objects
# and closer to 'raw' data, i.e. ownership trends not considered

pro_res_level <- c("projects", "reserves")

pro_dw <- pro_aca %>% 
  group_by(district, owner) %>% 
    summarize(new = sum_NA(new), 
              removed = sum_NA(removed),
              .groups = "drop") %>% 
  mutate(pro = new - removed) %>% 
  select(district, owner, pro)

pro_car_dw <- car_spa %>% 
  group_by(district, owner) %>% 
    summarize(car = sum_NA(car), 
              .groups = "drop") %>% 
  left_join(pro_dw, by = c("district", "owner")) %>% 
  pivot_longer(c(car, pro), names_to = "category", values_to = "people") %>% 
  mutate(district = factor(district, levels = uni_d),
         owner = factor(owner, levels = uni_w),
         cat = factor(if_else(category == "pro", 
                              pro_res_level[1], pro_res_level[2]), 
                      levels = pro_res_level)) %>% 
  select(district, owner, cat, people)

# WHY not piped to the plot?
# data of pro_car_dw will be used for a plot by district only (without owner)

# plot: dw (people)
sszplot(pro_car_dw,
  aes_x = "district", aes_y = "people", aes_fill = "cat",
  geom = "col",
  labs_x = "", labs_y = "people",
  wrap = "owner", ncol = 2,  
  scale_x = rev(uni_d),
  name = "1306_projects_reserves_dw_people",
  width = 8, height = 8
)

# plot: dw (proportion)
# WHY with aes_fill?
# is needed in the plot function
pro_car_dw %>% 
  group_by(district, owner) %>% 
  mutate(prop = people / sum_NA(people) * 100) %>% 
  filter(cat == pro_res_level[1]) %>% 
  sszplot(
    aes_x = "district", aes_y = "prop",
    fix_col = 1, 
    geom = "col",
    labs_x = "", labs_y = "proportion in % (projects on reserves)",
    wrap = "owner", ncol = 2,      
    scale_x = rev(uni_d),
    name = "1307_projects_reserves_dw_prop",
    width = 8, height = 6
  )

# plot: d (people)
pro_car_dw %>% 
  group_by(district, cat) %>% 
  summarize(people = sum_NA(people),
            .groups = "drop") %>% 
  sszplot(
    aes_x = "district", aes_y = "people", aes_fill = "cat",
    geom = "col",
    labs_x = "", labs_y = "people",
    scale_x = rev(uni_d),
    name = "1308_projects_reserves_d_people",
    width = 8, height = 8
  )

# plot: d (proportion)
# WHY with aes_fill?
# is needed in the plot function
pro_car_dw %>% 
  group_by(district, cat) %>% 
  summarize(people = sum_NA(people),
            .groups = "drop") %>% 
  group_by(district) %>% 
  mutate(prop = people / sum_NA(people) * 100) %>% 
  filter(cat == pro_res_level[1]) %>% 
  sszplot(
    aes_x = "district", aes_y = "prop",
    fix_col = 1, 
    geom = "col",
    labs_x = "", labs_y = "proportion in % (projects on reserves)",
    scale_x = rev(uni_d),
    name = "1309_projects_reserves_d_prop",
    width = 8, height = 8
  )




# export the results ------------------------------------------------------

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
