#' migration: proportion of sex and origin, by distict and year
#'
#' @param process "immigration" or "emigration"
#' @param params parameter vector
#'
#' @returns list with rates for different variables
#'          ex_mig_prop_dy: migration star, distribution of groups (sex and origin) by district and year (future)
#'          mis_dyso: migration star, distribution of groups (sex and origin) by district and year (past)
#'          mis_so_past_pred: migration star, distribution of groups (sex and origin) by district and year (past and future)
#'          
#' @export
#'
#' @examples mig_prop_so_dy_fun(process = "immigration", params = params)
#' 
#' 
#' 
mig_prop_so_dy_fun <- function(process, params = params) {
  
# start time
t0 <- Sys.time()  
  
  # parameter: immigration -------------------------------------------------- 
  
  if (process == "immigration"){  
    
    # paths
    mig_path = params$imm_od  
    ex_path = file.path(params$exp_path_scen, "immigration-star_prop-so-dy_future.csv")    
    
    # migration names 
    mig_vari = "AnzZuzuWir"
    mig_district = "QuarCd"
    mig_name = "immigration"
    mig_number = "03"  
    
    # migration parameters    
    mis_so_base_begin = params$ims_so_base_begin
    mis_so_base_end = params$ims_so_base_end
    mis_so_window_thres = params$ims_so_window_thres
    mis_so_prop_trend = params$ims_so_prop_trend
    mis_so_thres_percent = params$ims_so_thres_percent
    mis_so_lower_thres = params$ims_so_lower_thres
    
  } 
  
  
  
  # parameter: emigration --------------------------------------------------- 
  
  if (process == "emigration"){    
    
    # paths
    mig_path = params$emi_od  
    ex_path = file.path(params$exp_path_scen, "emigration-star_prop-so-dy_future.csv")    
    
    # migration names 
    mig_vari = "AnzWezuWir"
    mig_district = "QuarBisherCd"
    mig_name = "emigration"
    mig_number = "04"  
    
    # migration parameters    
    mis_so_base_begin = params$ems_so_base_begin
    mis_so_base_end = params$ems_so_base_end
    mis_so_window_thres = params$ems_so_window_thres
    mis_so_prop_trend = params$ems_so_prop_trend
    mis_so_thres_percent = params$ems_so_thres_percent
    mis_so_lower_thres = params$ems_so_lower_thres
    
  }    
  
  
  # parameter: immigration and emigration -----------------------------------  
  
  # paths 
  rel_od <- params$rel_od
  log_file <- params$log_file
  
  # text and lookup  
  uni_d <- params$uni_d
  uni_s <- params$uni_s   
  uni_o <- params$uni_o  
  look_dis <- params$look_dis  
  
  # time
  date_start <- params$date_start
  date_end <- params$date_end
  scen_begin <- params$scen_begin
  scen_end <- params$scen_end  
  
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
  
  # migration: distribution of sex and origin -------------------------------
  
  # mis and pop: aggregate
  mis_dy <- group_by(mis, district, year) |> 
    summarize(
      mis = sum(mis),
      .groups = "drop"
    )
  
  # possible cases
  cas_dyso <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):date_end,
    sex = uni_s,
    origin = uni_o
  ))
  
  # distribution of sex and origin per year and district
  # comment: a proportion not a rate
  
  mis_dyso <- group_by(mis, district, year, sex, origin) |> 
    summarize(
      mis_dyso = sum(mis),
      .groups = "drop"
    ) |> 
    left_join(rename(mis_dy, mis_dy = mis),
              by = c("district", "year")
    ) |> 
    right_join(cas_dyso, by = c("district", "year", "sex", "origin")) |> 
    replace_na(list(mis_dyso = 0, mis_dy = 0)) |> 
    mutate(mis_prop_dyso = if_else(mis_dy == 0, NA_real_, 
                                   round(mis_dyso / mis_dy * 100, round_prop)))
  
  
  # distribution of sex and origin: prediction ------------------------------
  
  # base years
  mis_so_base <- filter(
    mis_dyso,
    (year >= mis_so_base_begin) & (year <= mis_so_base_end)
  )
  
  # prediction: constrained regression
  
  # Why no upper threshold?
  # proportions per district/year will be standardized to 100 percent anyways
  
  mis_so_pred <- con_reg(
    data = mis_so_base, x = "year", y = "mis_prop_dyso",
    group_cols = c("district", "sex", "origin"),
    window = mis_so_window_thres, base_t0 = mis_so_base_begin,
    scen_t0 = scen_begin, scen_t1 = scen_end,
    prop_trend = mis_so_prop_trend, thres_percent = mis_so_thres_percent,
    lower_thres = mis_so_lower_thres, upper_thres = NA
  )
  
  # standardize proportions per district and year to 100 percent
  mis_so_pred_stand <- group_by(mis_so_pred, district, year) |> 
    mutate(pred_roll_sum = sum_NA(pred_roll),
           pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, 
                                     round(pred_roll / pred_roll_sum * 100, 
                                           round_prop))) |>  
    ungroup()
  
  # past and prediction
  mis_so_past_pred <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):scen_end,
    sex = uni_s,
    origin = uni_o
  )) |> 
    left_join(select(mis_dyso, district, year, sex, origin, mis_prop_dyso),
              by = c("district", "year", "sex", "origin")
    ) |> 
    left_join(mis_so_pred_stand, by = c("district", "year", "sex", "origin")) |> 
    mutate(prop_all = if_else(year <= mis_so_base_end, 
                              mis_prop_dyso, pred_roll_stand))
  

  # export ------------------------------------------------------------------

  # export preparation
  ex_mig_prop_dy <- mutate(mis_so_past_pred,
                           prop = round(prop_all, round_prop)
  ) |> 
    filter(year >= scen_begin) |> 
    select(district, year, sex, origin, prop) |> 
    arrange(district, year, sex, origin)
  
  # export the data
  write_csv(ex_mig_prop_dy, ex_path)
  
  # log info  
  cat_log(
    text = paste0(process, "(mig prop, so_dy): ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )    
  

  # output ------------------------------------------------------------------

  out <- list()
  out$ex_mig_prop_dy <- ex_mig_prop_dy
  out$mis_dyso <- mis_dyso
  out$mis_so_past_pred <- mis_so_past_pred
  return(out)  
  
}
