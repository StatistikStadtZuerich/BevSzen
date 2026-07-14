#' capacity and reserves: data preparation function
#'
#' @param params parameter vector
#'
#' @returns list with capacity and reserve data
#'      car_dat: capacity and reserves data after import
#'      car_dyepw: aggregate the capacity and reserves data by district, year, residence, plot, owner
#'      usage_prop: proportion of usage
#'      check_total_usage: total usage (ha)
#'      check: sum of usage per district and owner until year 'car_y'
#'      total: the total sum (until year 'scen_end')
#'
#' @export
#'
#' @examples capacity_reserve_fun(params = params)
#'


capacity_reserves_fun <- function(params = params){

  # start time
  t0 <- Sys.time()

  # parameter ---------------------------------------------------------------

  # paths
  car_path <- params$car_path
  car_path_rev <- params$car_path_rev
  exp_path_scen <- params$exp_path_scen
  log_file <- params$log_file

  # time
  date_start <- params$date_start
  date_end <- params$date_end
  scen_begin <- params$scen_begin
  scen_end <- params$scen_end

  # capacity and reserves parameters
  car_rev <- params$car_rev
  car_sc <- params$car_sc
  car_resi <- params$car_resi
  car_plot <- params$car_plot
  car_uti_input <- params$car_uti_input
  car_uti <- params$car_uti
  car_pp <- params$car_pp
  car_y <- params$car_y
  car_lambda <- params$car_lambda

  # text and lookup
  uni_d <- params$uni_d
  uni_w <- params$uni_w
  uni_e <- params$uni_e
  uni_p <- params$uni_p
  look_car <- params$look_car
  look_reg <- params$look_reg
  car_initial <- params$car_initial
  car_category <- params$car_category

  # options
  round_area <- params$round_area



  # import and data preparation ---------------------------------------------

  # which file: standard or revision data?
  car_file <- if (car_rev == 1) car_path else car_path_rev

  # capacity and reserves: data
  car_dat <- read_csv(car_file) |>
    rename(year = PublJahr) |>
    pivot_longer(
      cols = all_of(car_initial),
      names_to = "initial",
      values_to = "area"
    ) |>
    left_join(look_car, by = "initial") |>
    mutate(distnum = as.numeric(QuarCd)) |>
    left_join(look_reg, by = "distnum") |>
    mutate(
      cat = factor(category, car_category),
      residence = factor(
        case_when(
          WohnanteilCd == 1 ~ uni_e[1],
          WohnanteilCd == 2 ~ uni_e[2],
          TRUE ~ uni_e[3]
        )
      ),
      plot = fact_if(ArealCd, uni_p),
      owner = fact_if(EigentumGrundstkCd, uni_w),
      ha = area / 10000
    ) |>
    select(year, residence, plot, district, owner, cat, ha)

  # aggregate (district, year, residence, plot, owner)
  car_dyepw <- car_dat |>
    group_by(district, year, residence, plot, owner, cat) |>
    summarize(ha = sum(ha), .groups = "drop")


  # combine information (with the parameters) -------------------------------

  # current data (and no area values below zero)
  curr <- mutate(car_dyepw, area = pmax(0, ha)) |>
    filter(year == max(year)) |>
    select(-c(year, ha))

  # residence portion
  residence <- mutate(curr, resi = case_when(
    residence == uni_e[1] ~ "min",
    residence == uni_e[2] ~ "real", TRUE ~ "max"
  )) |>
    select(-residence) |>
    pivot_wider(names_from = resi, values_from = area) |>
    mutate(
      car_resid = car_resi,
      area = if_else(car_resid >= 0, real + (max - real) * car_resid / 100,
                     real + (real - min) * car_resid / 100
      )
    ) |>
    select(district, plot, owner, cat, area)


  # plot construction
  pcon <- residence |>
    mutate(pcon = if_else(plot == uni_p[1], "with", "without")) |>
    select(-plot) |>
    pivot_wider(names_from = pcon, values_from = area) |>
    mutate(area = without + (with - without) * car_plot / 100) |>
    select(district, owner, cat, area)

  # conversion from total to living area (e.g. elevators, stairs)
  living <- pcon |>
    mutate(living = area / ((100 + car_sc) / 100)) |>
    select(-area)

  # degree of utilization, proportion of usage
  usage_prop <- living |>
    mutate(area = if_else(cat %in% car_category[c(1, 4)],
                                              living / car_uti_input * car_uti, living
  )) |>
    select(-living) |>
    pivot_wider(names_from = cat, values_from = area) |>
    # new reserves, usage proportion
    mutate(
      reserve_new = capacity - buildings,
      usage_prop = pmax(0, pmin(
        100,
        if_else(reserve_new == 0, 0, usage / reserve_new * 100) + car_pp
      ))
    )

  # check: reserves and usage in the entire city (city area approx. 8800 ha)
  entire_city <- usage_prop |>
    summarize(
      reserve_new = sum(reserve_new),
      usage = sum(usage)
    ) |>
    mutate(prop = usage / reserve_new * 100)


  # usage per year ----------------------------------------------------------

  # exp-distribution over years
  exp_y <- tibble(year = scen_begin:scen_end) |>
    mutate(
      delta = year - date_end,
      exp_y = exp(car_lambda * delta)
    )

  # expand_grid(
  #   year = scen_begin:scen_end,
  #   car_lambda = seq(-0.06, -0.02, by = 0.005)) |>
  #   mutate(
  #     delta = year - date_end,
  #     exp_y = exp(car_lambda * delta)
  #   ) |> 
  # ggplot() +
  #   geom_line(aes(x = year, y = exp_y, color = as.factor(car_lambda)))

  
  # used reserves until a certain year (parameter: car_y)
  exp_y_sum <- filter(exp_y, year <= car_y) |>
    summarize(exp_y_sum = sum(exp_y))

  # usage: proportion of the reserves per year
  usage_y_prop <- as_tibble(expand_grid(
    district = uni_d,
    year = scen_begin:scen_end,
    owner = uni_w,
    exp_y_sum = exp_y_sum$exp_y_sum
  )) |>
    left_join(select(usage_prop, district, owner, reserve_new, usage_prop),
              by = c("district", "owner")
    ) |>
    left_join(select(exp_y, year, exp_y), by = "year") |>
    mutate(usage_y_prop = usage_prop * exp_y / exp_y_sum)



  # check: usage (ha) per year
  check_usage_y <- usage_y_prop |>
    mutate(usage_new = reserve_new * usage_y_prop / 100) |>
    group_by(year) |>
    summarize(usage_sum = sum(usage_new))

  # check: total usage (ha)
  check_total_usage <- sum(check_usage_y$usage_sum)

  # check: sum until year 'car_y'
  # check if sum of the proportions per year equals the total proportion
  check <- filter(usage_y_prop, year <= car_y) |>
    group_by(district, owner) |>
    summarize(
      usage_prop = max(usage_prop),
      usage_prop_y_sum = sum(usage_y_prop),
      .groups = "drop"
    )

  # the total sum (until year 'scen_end')
  # the total should not exceed 100 percent of the reserves

  total <- usage_y_prop |>
    group_by(district, owner) |>
    summarize(total = sum(usage_y_prop), .groups = "drop") |>
    filter(total > 100) |>
    mutate(diff = total - 100) |>
    left_join(usage_y_prop, by = c("district", "owner")) |>
    group_by(district, owner) |>
    arrange(district, owner, desc(year)) |>
    mutate(
      usage_cumu = cumsum(usage_y_prop),
      usage_corr = if_else(usage_cumu < diff, 0, usage_y_prop)
    ) |>
    ungroup() |>
    select(district, owner, year, usage_corr) |>
    right_join(usage_y_prop, by = c("district", "owner", "year")) |>
    arrange(district, owner, year) |>
    mutate(
      usage_y_prop_corr = if_else(is.na(usage_corr), usage_y_prop, usage_corr),
      usage_area = reserve_new * usage_y_prop_corr / 100
    ) |>
    select(district, year, owner, reserve_new, usage_y_prop_corr, usage_area) |>
    rename(reserve = reserve_new, usage_prop = usage_y_prop_corr)

  # check total
  check_total <- total |>
    group_by(year) |>
    summarize(usage_area = sum(usage_area), .groups = "drop")

  # sum(check_total$usage_area)


  # export the results ------------------------------------------------------

  # usage (area in ha)
  car_ex_data <- mutate(total, usage_ha = round(usage_area, round_area)) |>
    select(district, year, owner, usage_ha) |>
    arrange(district, year, owner)

  # export
  write_csv(car_ex_data, file.path(exp_path_scen, "usage_area.csv"))

  # log info
  cat_log(
    text = paste0("capacity and reserves: ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )


  # outputs -----------------------------------------------------------------

  out <- list()
  out$car_dat <- car_dat
  out$car_dyepw <- car_dyepw
  out$usage_prop <- usage_prop
  out$check_total_usage <- check_total_usage
  out$check <- check
  out$total <- total
  return(out)


}



