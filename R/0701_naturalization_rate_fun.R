#' naturalization rate: future values
#'
#' @param params parameter vector
#'
#' @returns
#' @export
#'
#' @examples
#' 
#' 
#' @returns list with naturalization data and rates
#'      nat_denat: naturalization and denaturalization
#'      nat_pop: naturalization and population (to calculate rates for different Levels)
#'      nat_pop_das_smooth: naturalization and population (both smoothed)
#'      nat_rate_das: naturalization rate by district, age, sex
#'      nat_ya: naturalization and population (both smoothed) by year, age 
#'      nat_rate_ya: naturalization rate by year, age
#'      rate_ya_pred: predicted naturalization rate by year, age
#'      rate_ya_past_pred: past and predicted naturalization rate by year, age
#'      nat_a: naturalization and population (both smoothed) by age (for trend factor calculation)
#'      nat_rate_a: naturalization rate by age
#'      tf_ya: trend factor by age
#'      rate_dyas_future: prediction of the naturalization rate by district, year, age, sex
#'      
#' @export 
#'
#' @examples naturalization_rate_fun(params = params)


naturalization_rate_fun <- function(params = params){

  # start time
  t0 <- Sys.time()  
  

  # parameter ---------------------------------------------------------------

  # paths 
  nat_od <- params$nat_od
  pop_od <- params$pop_od
  exp_path_scen <- params$exp_path_scen
  log_file <- params$log_file
  
  # time  
  date_start <- params$date_start
  date_end <- params$date_end
  scen_begin <- params$scen_begin
  scen_end <- params$scen_end
  
  # naturalization parameters  
  nat_base_begin <- params$nat_base_begin
  nat_base_end <- params$nat_base_end
  nat_pop_span_das <- params$nat_pop_span_das
  nat_nat_span_das <- params$nat_nat_span_das
  nat_pop_thres <- params$nat_pop_thres
  nat_pop_value <- params$nat_pop_value
  nat_rate_span_das <- params$nat_rate_span_das
  nat_pop_span_ya <- params$nat_pop_span_ya
  nat_nat_span_ya <- params$nat_nat_span_ya
  nat_rate_span_ya <- params$nat_rate_span_ya
  nat_prop_trend <- params$nat_prop_trend
  nat_thres_percent <- params$nat_thres_percent
  nat_window_thres <- params$nat_window_thres
  nat_lower_thres <- params$nat_lower_thres
  nat_pop_span_a <- params$nat_pop_span_a
  nat_nat_span_a <- params$nat_nat_span_a
  nat_rate_span_a <- params$nat_rate_span_a
  nat_factor_thres <- params$nat_factor_thres
  nat_factor_value <- params$nat_factor_value
  nat_rate_span_dyas <- params$nat_rate_span_dyas
  
  # age
  age_min <- params$age_min
  age_max <- params$age_max  
  
  # text and lookup  
  uni_d <- params$uni_d
  uni_s <- params$uni_s   
  uni_o <- params$uni_o  
  look_dis <- params$look_dis  
  
  # options  
  round_rate <- params$round_rate  


  # import and data preparation ---------------------------------------------

  # naturalization and denaturalization
  nat_denat <- read_csv(nat_od) |> 
    rename(year = EreignisDatJahr, age = AlterVCd, nat = AnzEinbWir) |>
    left_join(look_dis, by = "QuarCd") |>
    mutate(
      sex = fact_if(SexCd, uni_s),
      origin_prev = fact_if(HerkunftBisherCd, uni_o),
      origin = fact_if(HerkunftCd, uni_o),
      district = factor(distr, uni_d)
    ) |>
    select(district, year, age, sex, origin_prev, origin, nat)

  # naturalization
  nat <- nat_denat |> 
    filter((origin_prev == uni_o[2]) & (origin == uni_o[1])) |>
    group_by(district, year, age, sex) |>
    summarize(
      nat = sum(nat),
      .groups = "drop"
    )

  # population (foreign population only)
  # year: begin of year population

  pop <- read_csv(pop_od) |>
    rename(age = AlterVCd, pop = AnzBestWir) |>
    filter(HerkunftCd == 2) |>
    left_join(look_dis, by = "QuarCd") |>
    mutate(
      year = StichtagDatJahr + 1,
      sex = fact_if(SexCd, uni_s),
      district = factor(distr, uni_d)
    ) |>
    select(district, year, age, sex, pop) |>
    group_by(district, year, age, sex) |>
    summarize(
      pop = sum(pop),
      .groups = "drop"
    )


  # naturalization and population -------------------------------------------

  # nat and pop
  nat_pop <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):date_end,
    age = age_min:age_max,
    sex = uni_s
  )) |>
    left_join(pop, by = c("district", "year", "age", "sex")) |>
    left_join(nat, by = c("district", "year", "age", "sex")) |>
    replace_na(list(pop = 0, nat = 0))


  # base years --------------------------------------------------------------

  # base years, aggregate (mean over base years)
  nat_pop_das <- nat_pop |> 
    filter((year >= nat_base_begin) & (year <= nat_base_end)) |>
    group_by(district, age, sex) |>
    summarize(
      nat = mean(nat),
      pop = mean(pop),
      .groups = "drop"
    )

  # das: smoothing (nat and pop) --------------------------------------------

  # smoothing (direction: age, result should not be below zero)
  nat_pop_das_smooth <- nat_pop_das |> 
    group_by(district, sex) |>
    arrange(age) |>
    mutate(
      pop_a = pmax(0, predict(loess(pop ~ age, span = 0.1, 
                                    degree = 1, na.action = na.aggregate))),
      nat_a = pmax(0, predict(loess(nat ~ age, span = nat_nat_span_das, 
                                    degree = 1, na.action = na.aggregate)))
    ) |>
    ungroup()



  # das: naturalization rate ------------------------------------------------

  # rate, threshold, smooth
  nat_rate_das <- nat_pop_das_smooth |> 
    mutate(rate_das = if_else(pop_a == 0, NA_real_, 
                              round(nat_a / pop_a * 100, round_rate)),
                         rate_thres = if_else(pop_a < nat_pop_thres, 
                                              nat_pop_value, rate_das)
  ) |>
    group_by(district, sex) |>
    arrange(age) |>
    mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
                                               span = nat_rate_span_das, 
                                               degree = 1, 
                                               na.action = na.aggregate
    )))) |>
    ungroup()
  
  
  # ya: smooth processes (preparation for trend by ya) ----------------------

  # WHY trend for ya?
  # shown in plots by a -> age is relevant
  # since naturalization is conditional to duration of residence
  # moreover different regulations for families, children
  # therefore, age is an essential variable
  # district and sex not that different (look at plots)
  # same regulations in entire city, and not differentiate by sex

  # smooth processes (base years only)
  nat_ya <- nat_pop |> 
    filter((year >= nat_base_begin) & (year <= nat_base_end)) |>
    group_by(year, age) |>
    summarize(
      pop = sum(pop),
      nat = sum(nat),
      .groups = "drop"
    ) |>
    group_by(year) |>
    arrange(age) |>
    mutate(
      pop_a = pmax(0, predict(loess(pop ~ age,
                                    span = nat_pop_span_ya,
                                    degree = 1,
                                    na.action = na.aggregate
      ))),
      nat_a = pmax(0, predict(loess(nat ~ age,
                                    span = nat_nat_span_ya,
                                    degree = 1,
                                    na.action = na.aggregate
      )))
    ) |>
    ungroup()


  
  # ya: rate ----------------------------------------------------------------

  # rate
  nat_rate_ya <- nat_ya |> 
    mutate(rate_ya = if_else(pop_a == 0, NA_real_, 
                             round(nat_a / pop_a * 100, round_rate)),
           rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_ya)
  ) |>
    group_by(year) |>
    arrange(age) |>
    mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
                                               span = nat_rate_span_ya, 
                                               degree = 1, 
                                               na.action = na.aggregate
    )))) |>
    ungroup()

  
  # constrained regression --------------------------------------------------

  # prediction
  rate_ya_pred <- con_reg(
    data = nat_rate_ya, x = "year", y = "rate_smooth",
    group_cols = "age",
    window = nat_window_thres, base_t0 = nat_base_begin,
    scen_t0 = scen_begin, scen_t1 = scen_end,
    prop_trend = nat_prop_trend, thres_percent = nat_thres_percent,
    lower_thres = nat_lower_thres
  )

  # past and prediction
  rate_ya_past_pred <- as_tibble(expand_grid(
    year = nat_base_begin:scen_end,
    age = age_min:age_max
  )) |>
    left_join(select(nat_rate_ya, year, age, rate_smooth),
              by = c("year", "age")
    ) |>
    left_join(select(rate_ya_pred, year, age, pred_roll),
              by = c("year", "age")
    ) |>
    mutate(rate_ya = if_else(year < scen_begin, rate_smooth, pred_roll))




  # smooth processes (preparation for the trend factor calculation) ---------

  # smooth processes (base years only)
  nat_a <- nat_pop |> 
    filter((year >= nat_base_begin) & (year <= nat_base_end)) |>
    group_by(age) |>
    summarize(
      pop = mean(pop),
      nat = mean(nat),
      .groups = "drop"
    ) |>
    arrange(age) |>
    mutate(
      pop_a = pmax(0, predict(loess(pop ~ age,
                                    span = nat_pop_span_a,
                                    degree = 1,
                                    na.action = na.aggregate
      ))),
      nat_a = pmax(0, predict(loess(nat ~ age,
                                    span = nat_pop_span_a,
                                    degree = 1,
                                    na.action = na.aggregate
      ))))


  
  # rate --------------------------------------------------------------------

  nat_rate_a <- nat_a |> 
    mutate(rate_a = if_else(pop_a == 0, NA_real_, 
                            round(nat_a / pop_a * 100, round_rate)),
           rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_a)
  ) |>
    arrange(age) |>
    mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
                                               span = nat_rate_span_a, 
                                               degree = 1, 
                                               na.action = na.aggregate
    ))))

  
  
  # trend factor ('rate ya' to 'rate a') ------------------------------------

  # preparation
  prep_rate_ya <- rate_ya_past_pred |> 
    filter(year >= scen_begin) |>
    select(year, age, rate_ya)

  prep_rate_a <- nat_rate_a |> 
    select(age, rate_smooth) |>
    rename(rate_a = rate_smooth)


  # trend factor (has to be at least 0, otherwise 'denaturalization')
  tf_ya <- prep_rate_ya |> 
    left_join(prep_rate_a, by = "age") |>
    mutate(tf_ya = pmax(0, if_else(rate_a <= nat_factor_thres, 
                                   nat_factor_value, rate_ya / rate_a)))


  # future rate (dyas) ------------------------------------------------------

  rate_dyas_future <- as_tibble(expand_grid(
    district = uni_d,
    year = scen_begin:scen_end,
    age = age_min:age_max,
    sex = uni_s
  )) |>
    left_join(select(nat_rate_das, district, age, sex, rate_smooth),
              by = c("district", "age", "sex")
    ) |>
    rename(rate_das = rate_smooth) |>
    left_join(select(tf_ya, year, age, tf_ya), by = c("year", "age")) |>
    mutate(rate_dyas = round(rate_das * tf_ya, round_rate)) |>
    group_by(district, year, sex) |>
    arrange(age) |>
    mutate(rate_smoothed = pmax(0, predict(loess(rate_dyas ~ age,
                                                 span = nat_rate_span_dyas, 
                                                 degree = 1, 
                                                 na.action = na.aggregate
    )))) |>
    ungroup()


  # export the results ------------------------------------------------------

  # rate: prediction
  nat_ex_data <- rate_dyas_future |> 
    mutate(rate_nat_dyas = round(rate_smoothed, round_rate)) |>
    select(district, year, age, sex, rate_nat_dyas) |>
    arrange(district, year, age, sex)

  # export
  write_csv(nat_ex_data, file.path(exp_path_scen, "naturalization_future.csv"))  

  # log info
  cat_log(
    text = paste0("naturalization rate: ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )
  
  # outputs -----------------------------------------------------------------
  
  out <- list()
  out$nat_denat <- nat_denat
  out$nat_pop <- nat_pop
  out$nat_pop_das_smooth <- nat_pop_das_smooth
  out$nat_rate_das <- nat_rate_das
  out$nat_ya <- nat_ya
  out$nat_rate_ya <- nat_rate_ya
  out$rate_ya_pred <- rate_ya_pred
  out$rate_ya_past_pred <- rate_ya_past_pred
  out$nat_a <- nat_a
  out$nat_rate_a <- nat_rate_a
  out$tf_ya <- tf_ya
  out$rate_dyas_future <- rate_dyas_future  
  return(out)

}

  
