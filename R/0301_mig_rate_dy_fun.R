#' migration rate per district and year
#'
#' @param process "immigration" or "emigration"
#' @param params parameter vector
#'
#' @returns list with migration rates
#'          ex_mig_rate_dy: migration rate star by district and year (future)
#'          mis_rate_dy: migration rate star by district and year (past) 
#'          mis_past_pred: migration rate star by district and year (past and future) 
#'      
#'         
#' @export
#'
#' @examples mig_rate_dy_fun(process = "immigration", params = params)


mig_rate_dy_fun <- function(process, params = params) {

# start time
t0 <- Sys.time()

    
  # parameter: immigration --------------------------------------------------
  
  if (process == "immigration"){
    
    # paths
    mig_path = params$imm_od
    ex_path = file.path(params$exp_path_scen, "immigration-star_rate-dy_future.csv")  
    
    # migration names 
    mig_vari = "AnzZuzuWir"
    mig_district = "QuarCd"
    mig_name = "immigration"
    mig_number = "03"
    
    # migration parameters    
    mis_base_begin = params$ims_base_begin
    mis_base_end = params$ims_base_end
    mis_rate_window_thres = params$ims_rate_window_thres
    mis_rate_prop_trend = params$ims_rate_prop_trend
    mis_rate_thres_percent = params$ims_rate_thres_percent
    mis_rate_lower_thres = params$ims_rate_lower_thres
    
  }  
  
  
  # parameter: emigration ---------------------------------------------------
  
  if (process == "emigration"){  
    
    # paths  
    mig_path = params$emi_od  
    ex_path = file.path(params$exp_path_scen, "/emigration-star_rate-dy_future.csv")
    
    # migration names   
    mig_vari = "AnzWezuWir"
    mig_district = "QuarBisherCd"
    mig_name = "emigration"
    mig_number = "04"
    
    # migration parameters      
    mis_base_begin = params$ems_base_begin
    mis_base_end = params$ems_base_end
    mis_rate_window_thres = params$ems_rate_window_thres
    mis_rate_prop_trend = params$ems_rate_prop_trend
    mis_rate_thres_percent = params$ems_rate_thres_percent
    mis_rate_lower_thres = params$ems_rate_lower_thres
    look_dis <- params$look_dis
    
  }      
  

  # parameter: both (immigration and emigration) ----------------------------

  # paths 
  rel_od <- params$rel_od
  pop_od <- params$pop_od
  log_file <- params$log_file  
  
  # time
  date_start <- params$date_start
  date_end <- params$date_end
  scen_begin <- params$scen_begin
  scen_end <- params$scen_end  
  
  # text and lookup  
  uni_d <- params$uni_d
  uni_s <- params$uni_s   
  uni_o <- params$uni_o  
  look_dis <- params$look_dis  
  
  # options  
  round_rate <- params$round_rate  
  
  
  
  
  # import and data preparation ---------------------------------------------
  
  # migration (immigration or emigration)
  mig <- read_csv(mig_path) |> 
    rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) |> 
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
  
  # population
  # year: begin of year population (therefore, StichtagDatJahr + 1)
  
  pop <- read_csv(pop_od) |> 
    rename(age = AlterVCd, pop = AnzBestWir) |> 
    left_join(look_dis, by = "QuarCd") |> 
    mutate(
      year = StichtagDatJahr + 1,
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
      district = factor(distr, uni_d)
    ) |> 
    select(district, year, age, sex, origin, pop) |> 
    group_by(district, year, age, sex, origin) |> 
    summarize(
      pop = sum(pop),
      .groups = "drop"
    )
  
  
  # migration* rate (per district and year) ---------------------------------
  
  # mis and pop: aggregate
  mis_dy <- group_by(mis, district, year) |> 
    summarize(
      mis = sum(mis),
      .groups = "drop"
    )
  
  pop_dy <- group_by(pop, district, year) |> 
    summarize(
      pop = sum(pop),
      .groups = "drop"
    )
  
  # migration* rate (based on all possible cases)
  mis_rate_dy <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):date_end
  )) |> 
    left_join(pop_dy, by = c("district", "year")) |> 
    left_join(mis_dy, by = c("district", "year")) |> 
    replace_na(list(pop = 0, ims = 0)) |> 
    mutate(mis_rate_dy = if_else(pop == 0, NA_real_, round(mis / pop * 100, round_rate)))
  
  
  # prediction: future migration* rate (per year and district) --------------
  
  # base years
  mis_base <- filter(
    mis_rate_dy,
    (year >= mis_base_begin) & (year <= mis_base_end)
  )
  
  # prediction: constrained regression
  mis_pred <- con_reg(
    data = mis_base, x = "year", y = "mis_rate_dy",
    group_cols = "district",
    window = mis_rate_window_thres, base_t0 = mis_base_begin,
    scen_t0 = scen_begin, scen_t1 = scen_end,
    prop_trend = mis_rate_prop_trend, thres_percent = mis_rate_thres_percent,
    lower_thres = mis_rate_lower_thres, upper_thres = NA
  )
  
  # past and prediction
  mis_past_pred <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):scen_end
  )) |> 
    left_join(select(mis_rate_dy, district, year, mis_rate_dy),
              by = c("district", "year")
    ) |> 
    left_join(mis_pred, by = c("district", "year")) |> 
    mutate(rate_all = if_else(year <= mis_base_end, mis_rate_dy, pred_roll))
  

  # export ------------------------------------------------------------------

  # export preparation
  ex_mig_rate_dy <- mutate(mis_past_pred,
                           rate = round(rate_all, round_rate)
  ) |> 
    filter(year >= scen_begin) |> 
    select(district, year, rate) |> 
    arrange(district, year)
  
  # export the data
  write_csv(ex_mig_rate_dy, ex_path)
  
  # log info  
  cat_log(
    text = paste0(process, "(mig rate, dy): ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )  
  

  # output ------------------------------------------------------------------

  out <- list()
  out$ex_mig_rate_dy <- ex_mig_rate_dy
  out$mis_rate_dy <- mis_rate_dy
  out$mis_past_pred <- mis_past_pred
  return(out)
  
}
