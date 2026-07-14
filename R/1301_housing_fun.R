#' housing model function
#'
#' @param params parameter vector
#'
#' @returns list with housing data
#'      pro_aca: projects and allocation
#'      car_spa: reserves and living space
#'      pop_with_past: population based on reserves (dyw)
#'      pop_fut_past: population based on projects and reserves (dyw)
#'      pop_d: past and future population (dy)
#'      pop_all: past and future population (y)
#'      pro_car: projects, reserve usage, starting population (dyw)
#'      pro_res_all: future population after (dyw)
#'
#' @export
#'
#' @examples housing_fun(params = params)
#'

housing_fun <- function(params = params){
  
  
  # start time
  t0 <- Sys.time()
  
  # parameter ---------------------------------------------------------------
  
  # paths
  spa_od <- params$spa_od
  pop_od <- params$pop_od
  exp_path_scen <- params$exp_path_scen
  log_file <- params$log_file
  
  # time
  date_end <- params$date_end
  scen_end <- params$scen_end
  scen_begin <- params$scen_begin
  
  # project module parameters
  car_trust <- params$car_trust
  car_max_trust <- params$car_max_trust
  empty_coop <- params$empty_coop
  empty_private <- params$empty_private
  
  # text and lookup
  uni_d <- params$uni_d
  uni_w <- params$uni_w
  look_dis <- params$look_dis

  # options  
  round_prop <- params$round_prop
  

  
  # import, data preparation ------------------------------------------------

  # projects (apartments, dyw)
  pro_dat <- read_csv(file.path(exp_path_scen, "projects_future.csv"), 
                      lazy = FALSE)

  # allocation (persons per apartment, dyw)
  aca_dat <- read_csv(file.path(exp_path_scen, "allocation_future.csv"), 
                      lazy = FALSE)

  # reserve usage (ha per year, dyw) 
  car_dat <- read_csv(file.path(exp_path_scen, "usage_area.csv"), 
                      lazy = FALSE)

  # living space (m2 per person, dyw)
  spa_dat <- read_csv(file.path(exp_path_scen, "living-space_future.csv"), 
                      lazy = FALSE)
  
  
  
  # ownership (proportion of people living in cooperative housing, past)
  own_dat <- read_csv(spa_od) |>
    rename(year = StichtagDatJahr, apartments = AnzWhgStat, people = AnzBestWir) |> 
    left_join(look_dis, by = "QuarCd") |>
    mutate(
      owner = fact_if(EigentuemerSSZPubl3Cd_noDM, uni_w),
      district = factor(distr, uni_d)
    ) |>
    select(year, district, owner, people) |>
    group_by(district, year, owner) |>
    summarize(
      people = sum_NA(people),
      .groups = "drop") |>
    group_by(district, year) |>
    mutate(prop = round(people / sum_NA(people) * 100, round_prop)) |>
    ungroup() |> 
    filter(owner == uni_w[1]) |>
    select(district, year, prop)
  
  
  # population
  # why population not from the housing open data file?
  # this file only contains people in apartments (and not in care centers etc)
  # the population number in the housing open data is below the total
  # amount of people in Zurich

  # dy (past years)
  pop <- read_csv(pop_od, lazy = FALSE) |>
    rename(year = StichtagDatJahr, pop = AnzBestWir) |>
    left_join(look_dis, by = "QuarCd") |>
    mutate(district = factor(distr, uni_d)) |>
    select(district, year, pop) |>
    group_by(district, year) |>
    summarize(
      pop = sum(pop),
      .groups = "drop"
    )

  # projects and allocation (from apartments to people; future) -------------

  # calculate amount of people from
  # - projects 'pro' (num of apartments)
  # - allocation 'aca' (number of pop per apartment)
  pro_aca <- pro_dat |> 
    left_join(aca_dat,
                       by = c("district", "year", "owner")) |>
    mutate(people = apartments * aca_dyw) |>
    select(district, year, owner, indicator, people) |>
    pivot_wider(names_from = indicator, values_from = people)

  
  # capacity/reserves and living space (from m2 to people; future) ----------

  # combine: calculate amount of people
  # units: ha * 10,000 m2/ha / (m2/person) = person

  # calculate amount of people from ...
  # - capacity 'car' (area)
  # - living space 'spa' (area consumption)
  car_spa <- car_dat |> 
    left_join(spa_dat, by = c("district", "year", "owner")
  ) |>
    mutate(car = usage_ha * 10000 / spa_dyw) |>
    select(district, year, owner, car)


  
  # population by ownership (past) ------------------------------------------

  # join pop on ownership (since this data set begins later)
  # OUT: population of past year per ownership > this ownership information is still necessary
  pop_w <- own_dat |> 
    left_join(pop, by = c("district", "year")) |>
    filter(year <= date_end) |>
    mutate(
      cooperative = pop * prop / 100,
      other = pop * (1 - prop / 100)
    ) |>
    select(-c(prop, pop)) |>
    pivot_longer(c(cooperative, other),
                 names_to = "owner_text", values_to = "pop"
    ) |>
    mutate(owner = fact_if(owner_text, uni_w, "cooperative")) |>
    select(district, year, owner, pop)
  
  

  # last year of data
  pop_last <- pop_w |>
    ungroup() |>
    filter(year == date_end)

  # combine: capacity/reserves and ownership prediction ---------------------

  # proportion of cooperative housing according to capacity/reserves
  # capacity/reserves contains only people due to additional (!) yearly (!) usage of reserves
  # therefore, add the cumulative values to the past population
  pop_total <- car_spa |>
    arrange(district, owner, year) |>
    group_by(district, owner) |>
    mutate(cumulative = cumsum(car)) |>
    ungroup() |>
    left_join(select(pop_last, district, owner, pop),
              by = c("district", "owner")
    ) |>
    mutate(total = pop + cumulative) |>
    select(district, year, owner, total) |>
    rename(pop = total)
  
  
  # row-binding past_pop and pop_total (which is today's pop + cumulative car_spa)
  # Why doing so?
  # For plotting reasons, to for plotting to check if capacity/reserves population
  # values are meaningful in comparison to the past
  pop_with_past <- pop_w |> 
    bind_rows(pop_total) |>
    rename(distr = district) |>
    mutate(district = factor(distr, uni_d)) |>
    select(district, year, owner, pop)

  # factorize and subset - form final output tibble
  new_pop_car <- pop_total |>
    mutate(owner = if_else(owner == "cooperative housing", uni_w[1], uni_w[2])) |>
    select(district, year, owner, pop) |>
    mutate(car = pop) |>
    select(-pop)

  # combine: projects and capacity (with new prop of cooperative) -----------

  # Combining all prepared data in one tibble
  # dyo with pop (current year), new, removed and car (from capacity)
  pro_car <- as_tibble(expand_grid(
    district = uni_d,
    year = date_end:scen_end,
    owner = uni_w)) |>
    left_join(pop_last, by = c("district", "year", "owner")) |>
    left_join(pro_aca, by = c("district", "year", "owner")) |>
    left_join(new_pop_car, by = c("district", "year", "owner")) |>
    replace_na(list(new = 0, removed = 0)) |>
    select(district, owner, year, pop, new, removed, car) |>
    arrange(district, owner, year)

  # function: consider projects and reserves --------------------------------
  
  # consider projects and reserves (apply the function)
  pro_res_all <- pro_car |>
    group_split(district, owner) |>
    map(~ project_reserves(.x, params)) |>
    bind_rows()
  
  
  # check: car vs. car/projects
  car_pop_y <- new_pop_car |>
    left_join(pro_res_all, by = c("district", "year", "owner")) |>
    group_by(year) |>
    summarize(car = sum(car),
              pop = sum(pop), .groups = "drop") |>
    mutate(diff = pop - car)
  

  # ggplot(car_pop_y) +
  #   geom_line(aes(x = year, y = diff))


  
  # apply the parameter of empty apartments ---------------------------------

  # parameter can be applied directly to the population
  pro_res <- pro_res_all |>
    mutate(
      empty_prop = if_else(owner == uni_w[1], empty_coop, empty_private),
      pop_new = pop * (100 - empty_prop) / 100
    ) |>
    select(district, year, owner, pop_new) |>
    rename(pop = pop_new)

  
  
  
  # plots -------------------------------------------------------------------

  # with the past
  pop_fut_past <- bind_rows(pop_w, pro_res) |>
    mutate(district = factor(district, uni_d)) |>
    select(district, year, owner, pop)

  # by district (without owner)
  pop_d <- pop_fut_past |>
    group_by(district, year) |>
    summarize(pop = sum(pop), .groups = "drop")

  # all (entire city)
  pop_all <- pop_fut_past |>
    group_by(year) |>
    summarize(pop = sum(pop), .groups = "drop")
  
  

  
  # export the results ------------------------------------------------------

  # per district and ownership
  hm_pop_dyw <- pop_fut_past |> 
    arrange(district, year, owner)
  
  write_csv(hm_pop_dyw, file.path(exp_path_scen, "housing_model_population_dw.csv"))
  

  # per district
  hm_pop_dy <- pop_d |> 
    arrange(district, year)
  
  write_csv(hm_pop_dy, file.path(exp_path_scen, "housing-model_population_d.csv"))

  
  # entire city (to compare with past publications)
  ex_data_all <- pop_all |> 
    arrange(year)
 
  write_csv(ex_data_all, file.path(exp_path_scen, "housing-model_population_all.csv"))
  
  # log info
  cat_log(
    text = paste0("housing model: ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )  
  
  # outputs -----------------------------------------------------------------
  
  out <- list()
  out$pro_aca <- pro_aca
  out$car_spa <- car_spa
  out$pop_with_past <- pop_with_past
  out$pop_fut_past <- pop_fut_past
  out$pop_d <- pop_d
  out$pop_all <- pop_all
  out$pro_car <- pro_car
  out$pro_res_all <- pro_res_all  
  return(out)  
  
}




