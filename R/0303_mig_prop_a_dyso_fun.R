#' migration: age proportion (by district, year, sex, origin)
#'
#' @param process "immigration" or "emigration"
#' @param params parameter vector
#'
#' @returns list with rates for different variables
#'          mis_dyso: migration star by district, year, sex, origin
#'          mis_dyaso: by district, year, age, sex, origin 
#'          mis_smooth: migration star smoothed over years (by district, year, age, sex, origin)
#'          mis_age_prop_smooth: age proportion (by district, year, age, sex, origin)
#'          mis_a_past_pred: age proportion (by district, year, age, sex, origin; past and future)
#'          prop_fit: smoothing proportion by age with LOESS 
#'          prop_pred: prediction with regression
#'          ex_mig_prop_a_dyso: migration star, distribution of age by district, year, sex, origin (future)
#'         
#' @export
#'
#' @examples mig_prop_a_dyso_fun(process = "immigration", params = params)



mig_prop_a_dyso_fun <- function(process, params = params) {
  
  # start time
  t0 <- Sys.time()
  
  # parameter: immigration --------------------------------------------------
  
  if (process == "immigration"){    
    
    # paths
    mig_path = params$imm_od
    ex_path = file.path(params$exp_path_scen, 
                        "immigration-star_prop-a-dyso_future.csv")  
    
    # migration names 
    mig_vari = "AnzZuzuWir"
    mig_district = "QuarCd"
    mig_name = "immigration"
    mig_number = "03" 
  
    # migration parameters    
    mis_span_y = params$ims_span_y
    mis_span_a = params$ims_span_a
    mis_age_base_begin = params$ims_age_base_begin
    mis_age_base_end = params$ims_age_base_end    
    mis_age_window_thres = params$ims_age_window_thres
    mis_age_prop_trend = params$ims_age_prop_trend  
    mis_age_thres_percent = params$ims_age_thres_percent
    mis_age_lower_thres = params$ims_age_lower_thres       
    
  }    
  
  # parameter: emigration ---------------------------------------------------
  
  if (process == "emigration"){  

    # paths  
    mig_path = params$emi_od  
    ex_path = file.path(params$exp_path_scen, 
                        "/emigration-star_prop-a-dyso_future.csv")

    # migration names 
    mig_vari = "AnzWezuWir"
    mig_district = "QuarBisherCd"
    mig_name = "emigration"
    mig_number = "04"
    
    # migration parameters    
    mis_span_y = params$ems_span_y
    mis_span_a = params$ems_span_a
    mis_age_base_begin = params$ems_age_base_begin
    mis_age_base_end = params$ems_age_base_end    
    mis_age_window_thres = params$ems_age_window_thres
    mis_age_prop_trend = params$ems_age_prop_trend  
    mis_age_thres_percent = params$ems_age_thres_percent
    mis_age_lower_thres = params$ems_age_lower_thres       
  
  
  }  

  
  # parameter: immigration and emigration ----------------------------------- 
  
  # paths 
  rel_od <- params$rel_od
  pop_od <- params$pop_od  
  log_file <- params$log_file
  
  # time
  date_start <- params$date_start
  date_end <- params$date_end
  scen_begin <- params$scen_begin
  scen_end <- params$scen_end    
  
  # age  
  mis_age_min = params$age_min
  mis_age_max = params$age_max    
  
  # text and lookup  
  uni_d <- params$uni_d
  uni_s <- params$uni_s   
  uni_o <- params$uni_o  
  look_dis <- params$look_dis  
  
  # options  
  round_prop <- params$round_prop    
  


  # import and data preparation ---------------------------------------------
  
  # migration (immigration or emigration)
  mig <- read_csv(mig_path) |> 
    rename(year = EreignisDatJahr, age = AlterVCd, mig = all_of(mig_vari)) |> 
    left_join(look_dis, by = "QuarCd") |> 
    mutate(
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
      district = factor(distr, uni_d)
    ) |> 
    select(district, year, age, sex, origin, mig) |> 
    group_by(district, year, age, sex, origin) |> 
    summarize(
      mig = sum(mig),
      .groups = "drop"
    )
  
  # relocation
  # only migration to a certain district
  # WHY? this is needed to calculate immigration* (i.e. migration to a certain district)
  
  rel <- read_csv(rel_od) |> 
    rename(year = EreignisDatJahr, age = AlterVCd, dis = mig_district, rel = AnzUmzuWir) |> 
    left_join(look_dis, c("dis" = "QuarCd")) |> 
    mutate(
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
      district = factor(distr, uni_d)
    ) |> 
    select(district, year, age, sex, origin, rel) |> 
    group_by(district, year, age, sex, origin) |> 
    summarize(
      rel = sum(rel),
      .groups = "drop"
    )
  
  # migration* (i.e. migration to a certain district, 'migration star' = mis)
  mis <- bind_rows(
    rename(mig, mis = mig),
    rename(rel, mis = rel)
  ) |> 
    group_by(district, year, age, sex, origin) |> 
    summarize(
      mis = sum(mis),
      .groups = "drop"
    )
  

  # migration*: per district, year, sex, origin -----------------------------

  # WHY? to see where the denominator of the age proportion is low
  
  # possible cases: dyso
  cas_dyso <- as_tibble(expand_grid(
    district = uni_d,
    year = date_start:date_end,
    sex = uni_s,
    origin = uni_o
  ))
  
  # migration*: dyso
  mis_dyso <- group_by(mis, district, year, sex, origin) |> 
    summarize(
      mis_dyso = sum(mis),
      .groups = "drop"
    ) |> 
    right_join(cas_dyso, by = c("district", "year", "sex", "origin")) |> 
    replace_na(list(mis_dyso = 0))

  
  # age proportion: per district, year, sex, origin -------------------------
  
  # migration*: dyaso (already summarized before)
  mis_dyaso <- as_tibble(expand_grid(
    district = uni_d,
    year = date_start:date_end,
    age = mis_age_min:mis_age_max,
    sex = uni_s,
    origin = uni_o
  )) |> 
    left_join(mis, by = c("district", "year", "age", "sex", "origin")) |> 
    rename(mis_dyaso = mis) |> 
    replace_na(list(mis_dyaso = 0)) |> 
    left_join(mis_dyso, by = c("district", "year", "sex", "origin")) |> 
    arrange(district, year, sex, origin, age) |> 
    mutate(mis_prop_a = if_else(mis_dyso == 0, NA_real_, 
                                round(mis_dyaso / mis_dyso * 100, round_prop)))
  
  
  # smoothing migration* with LOESS over years (by district, age, sex) -------
  
  mis_smooth <- mis_dyaso |> 
    arrange(district, age, sex, origin, year) |> 
    group_by(district, age, sex, origin) |> 
    mutate(mis_smooth = pmax(0, predict(
      loess(mis_dyaso ~ year, span = mis_span_y, degree = 1, 
            na.action = na.aggregate)))) |> 
    ungroup()
  
  # age proportion (after smoothing migration* over years) ------------------
  
  # preparation
  mis_smooth_prep <- mis_smooth |>  
    select(district, year, age, sex, origin, mis_smooth) |> 
    rename(mis_dyaso = mis_smooth)
  
  # age proportion
  mis_age_prop_smooth <- group_by(mis_smooth_prep, district, year, sex, origin) |> 
    mutate(mis_dyso = sum_NA(mis_dyaso), 
           prop_a_smooth = if_else(mis_dyso == 0, NA_real_, 
                                   round(mis_dyaso / mis_dyso * 100, 
                                         round_prop))) |> 
    ungroup() |>  
    select(district, year, age, sex, origin, prop_a_smooth) |> 
    arrange(district, year, sex, origin, age)           

  
  # smoothing proportion by age with LOESS (by district, year, sex,  --------
  
  prop_fit <- arrange(mis_age_prop_smooth, district, year, sex, origin, age) |> 
    group_by(district, year, sex, origin) |> 
    mutate(prop_fit = pmax(0, predict(
      loess(prop_a_smooth ~ age, span = mis_span_a, degree = 1, 
            na.action = na.aggregate)))) |> 
    ungroup()
  
  
  # constrained regression --------------------------------------------------
  
  # years: base period
  years_base <- mis_age_base_begin:mis_age_base_end
  
  # data for base period
  prop_base <- filter(prop_fit, year %in% years_base)
  
  # prediction (duration: approx. 30 seconds)
  # Why no upper threshold?
  # proportions will be standardized to 100 percent anyways
  
  prop_pred <- con_reg(
    data = prop_base, x = "year", y = "prop_fit",
    group_cols = c("district", "age", "sex", "origin"),
    window = mis_age_window_thres, base_t0 = min(years_base),
    scen_t0 = max(years_base) + 1, scen_t1 = scen_end,
    prop_trend = mis_age_prop_trend, thres_percent = mis_age_thres_percent,
    lower_thres = mis_age_lower_thres, upper_thres = NA
  )
  
  # limit prediction period
  prop_pred_begin <- filter(prop_pred, year >= scen_begin)
  
  # standardize the sum of the proportions to 100 percent
  prop_stand <- group_by(prop_pred_begin, district, year, sex, origin) |> 
    summarize(
      pred_roll_sum = sum_NA(pred_roll),
      .groups = "drop"
    ) |> 
    right_join(prop_pred_begin, by = c("district", "year", "sex", "origin")) |> 
    mutate(pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, round(pred_roll / pred_roll_sum * 100, round_prop)))
  
  # past and prediction
  mis_a_past_pred <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):scen_end,
    age = mis_age_min:mis_age_max,
    sex = uni_s,
    origin = uni_o
  )) |> 
    left_join(select(mis_dyaso, district, year, age, sex, origin, mis_prop_a),
              by = c("district", "year", "age", "sex", "origin")
    ) |> 
    left_join(select(prop_stand, district, year, age, sex, origin, pred_roll_stand),
              by = c("district", "year", "age", "sex", "origin")
    ) |> 
    mutate(prop_a = if_else(year <= mis_age_base_end, mis_prop_a, pred_roll_stand))
  

  # export ------------------------------------------------------------------

  # export preparation
  ex_mig_prop_a_dyso <- mutate(mis_a_past_pred,
                               prop = round(prop_a, round_prop)
  ) |> 
    filter(year >= scen_begin) |> 
    select(district, year, age, sex, origin, prop) |> 
    arrange(district, year, sex, origin, age)
  
  # export the data
  write_csv(ex_mig_prop_a_dyso, ex_path)
  
  # log info  
  cat_log(
    text = paste0(process, "(mig prop, a_dyso): ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )    
  
  # output ------------------------------------------------------------------

  out <- list()
  out$ex_mig_prop_a_dyso <- ex_mig_prop_a_dyso
  out$mis_dyso <- mis_dyso
  out$mis_dyaso <- mis_dyaso
  out$mis_smooth <- mis_smooth
  out$mis_age_prop_smooth <- mis_age_prop_smooth
  out$prop_fit <- prop_fit
  out$prop_pred <- prop_pred
  out$mis_a_past_pred <- mis_a_past_pred    
  return(out)
  
  
}






