#' relocation proportion of migration-star (immigration-star or emigration-star)
#'
#' @param process "relocation-immigration" or "relocation-emigration"
#' @param params parameter vector
#'
#' @returns list with migration rates
#'          mis_rel: past data of relocation and migration-star (by district, year, age, sex, origin)
#'          rem_dyao: past data of relocation and migration-star (by district, year, age, origin)
#'          dyao_smooth: past data of smoothed relocation and smoothed migration-star (by district, year, age, origin)
#'          prop_dyao_smooth: past proportion of relocation (smoothed) on migration-star (smoothed) by district, year, age, origin
#'          dao_smooth: past data of smoothed relocation and smoothed migration-star (by district, age, origin)
#'          prop_dao_smooth: past proportion of relocation (smoothed) on migration-star (smoothed) by district, age, origin
#'          prop_agg: past proportion of the relocation Proportion with both aggregation levels (dyao and dao)
#'          prop_agg_smooth: proportion of the relocation Proportion after smoothing (after using both aggregation levels dyao and dao)
#'          pred_smooth: prediction of the relocation proportion on migration-star (both values: before and after smoothing)
#'          rel_ex_data: finale prediction of the proportion of relocation on migration-star (by district, year, age, origin)
#'          rem_base_begin: begin of base period
#'          rem_base_end: end of base period
#'          sources: different aggregation Levels (dyao, dao)
#'          
#'          
#' @export
#'
#' @examples relocation_fun(process = "relocation-immigration", params = params)



relocation_fun <- function(process, params = params) {

  # start time
  t0 <- Sys.time()    
  
  # parameter: relocation-immigration ---------------------------------------

  if (process == "relocation-immigration"){
 
    # paths  
    mig_path = params$imm_od
    ex_path = file.path(params$exp_path_scen, 
                        "/relocation_immigration_future.csv")    
    
    # relocation names   
    mig_vari = "AnzZuzuWir"
    mig_district = "QuarCd"
    mig_name = "immigration"
    rem_number = "05"
    
    # relocation parameters      
    rem_base_begin = params$rei_base_begin
    rem_base_end = params$rei_base_end
    rem_age_max = params$rei_age_max
    rem_mis_span_dyao = params$rei_ims_span_dyao
    rem_rel_span_dyao = params$rei_rel_span_dyao
    rem_mis_span_dao = params$rei_ims_span_dao
    rem_rel_span_dao = params$rei_rel_span_dao
    rem_mis_thres_y = params$rei_ims_thres_y
    rem_prop_span = params$rei_prop_span
    rem_window_thres = params$rei_window_thres
    rem_prop_trend = params$rei_prop_trend
    rem_thres_percent = params$rei_thres_percent
    rem_lower_thres = params$rei_lower_thres
    rem_upper_thres = params$rei_upper_thres
    rem_pred_span = params$rei_pred_span   
    
  }
  


# parameter: relocation-emigration ----------------------------------------

  if (process == "relocation-emigration"){    
    
    # paths  
    mig_path = params$emi_od
    ex_path = file.path(params$exp_path_scen,
                        "/relocation_emigration_future.csv")

    # relocation names   
    mig_vari = "AnzWezuWir"
    mig_district = "QuarBisherCd"
    mig_name = "emigration"
    rem_number = "06" 
    
    # relocation parameters      
    rem_base_begin = params$ree_base_begin
    rem_base_end = params$ree_base_end
    rem_age_max = params$ree_age_max
    rem_mis_span_dyao = params$ree_ems_span_dyao
    rem_rel_span_dyao = params$ree_rel_span_dyao
    rem_mis_span_dao = params$ree_ems_span_dao
    rem_rel_span_dao = params$ree_rel_span_dao
    rem_mis_thres_y = params$ree_ems_thres_y
    rem_prop_span = params$ree_prop_span
    rem_window_thres = params$ree_window_thres
    rem_prop_trend = params$ree_prop_trend
    rem_thres_percent = params$ree_thres_percent
    rem_lower_thres = params$ree_lower_thres
    rem_upper_thres = params$ree_upper_thres
    rem_pred_span = params$ree_pred_span

  }    
    

# parameter: both (relocation-immigration and relocation-emigration) ------

  # paths 
  rel_od <- params$rel_od
  log_file <- params$log_file

  # time
  date_start <- params$date_start
  date_end <- params$date_end
  scen_begin <- params$scen_begin
  scen_end <- params$scen_end  
  
  # age
  age_min <- params$age_min
  age_max <- params$age_max  
  
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
  # WHY? this is needed to calculate migration* (i.e. migration from/to a certain district)

  rel <- read_csv(rel_od) |>
    rename(year = EreignisDatJahr, age = AlterVCd, 
           dis = all_of(mig_district), rel = AnzUmzuWir) |>
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

  # migration* (immigration* or emigration*) and relocation (based on all possible cases)
  mis_rel <- as_tibble(expand_grid(
    district = uni_d,
    year = date_start:date_end,
    age = age_min:age_max,
    sex = uni_s,
    origin = uni_o
  )) |>
    left_join(mis, by = c("district", "year", "age", "sex", "origin")) |>
    left_join(rel, by = c("district", "year", "age", "sex", "origin")) |>
    replace_na(list(mis = 0, rel = 0))

  

  # aggregate over sex ------------------------------------------------------

  # This plot is the starting point (before smoothing, age corrections)

  # aggregate over sex
  rem_dyao <- mis_rel |> 
    group_by(district, year, age, origin) |>
    summarize(
      mis = sum(mis),
      rel = sum(rel),
      .groups = "drop"
    )


  # maximum age, base years -------------------------------------------------

  # WHY mean and not sum over high age? result is mean age
  # is more appropriate e.g. in plots
  # and can be explained (e.g. the mean amount of relocations from people as of age x)

  # aggregate
  dyao_amax_ybase <- rem_dyao |> 
    filter((year >= rem_base_begin) & (year <= rem_base_end)) |>
    mutate(age_new = pmin(rem_age_max, age)) |>
    group_by(district, year, age_new, origin) |>
    summarize(
      mis = mean(mis),
      rel = mean(rel),
      .groups = "drop"
    ) |>
    rename(age = age_new)
  
  

  # dyao: smoothing ---------------------------------------------------------

  # smoothing (direction: age, result should not be below zero)
  dyao_smooth <- dyao_amax_ybase |> 
    group_by(district, year, origin) |>
    arrange(age) |>
    mutate(
      mis_a = pmax(0, predict(loess(mis ~ age, span = rem_mis_span_dyao, 
                                    degree = 1, na.action = na.aggregate))),
      rel_a = pmax(0, predict(loess(rel ~ age, span = rem_rel_span_dyao, 
                                    degree = 1, na.action = na.aggregate)))
    ) |>
    ungroup()

  
  # dyao: proportion after smoothing ----------------------------------------

  # proportion (within the range from 0 to 100 percent)
  prop_dyao_smooth <- dyao_smooth |> 
    mutate(, rel_prop_dyao = pmax(0, pmin(100,
    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop))
  )))


  # dao (without y) ---------------------------------------------------------

  # aggregate
  dao <- dyao_amax_ybase |> 
    group_by(district, age, origin) |>
    summarize(
      mis = mean(mis),
      rel = mean(rel),
      .groups = "drop"
    )
  


  # smoothing (direction: age)
  dao_smooth <- dao |> 
    group_by(, district, origin) |>
    arrange(age) |>
    mutate(
      mis_a = pmax(0, predict(loess(mis ~ age,
                                    span = rem_mis_span_dao,
                                    degree = 1,
                                    na.action = na.aggregate
      ))),
      rel_a = pmax(0, predict(loess(rel ~ age,
                                    span = rem_rel_span_dao,
                                    degree = 1,
                                    na.action = na.aggregate
      )))
    ) |>
    ungroup()


  # proportion after smoothing
  prop_dao_smooth <- dao_smooth |> 
    mutate(rel_prop_dao = pmax(0, pmin(100,
    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop))
  )))

  
  # proportion from different aggregation levels ----------------------------

  # proportion
  temp_dyao <- prop_dyao_smooth |> 
    select(district, year, age, origin, mis_a, rel_prop_dyao) |>
    rename(mis = mis_a)
  
  temp_dao <- prop_dao_smooth |> 
    select(district, age, origin, rel_prop_dao)

  sources <- c("dyao", "dao")

  prop_agg <- temp_dyao |> 
    left_join(temp_dao, by = c("district", "age", "origin")) |>
    mutate(
      rel_prop = if_else(mis >= rem_mis_thres_y, 
                         rel_prop_dyao, rel_prop_dao),
      rel_prop_source = factor(if_else(mis >= rem_mis_thres_y, 
                                       sources[1], sources[2]), 
                               levels = sources)
    )



  # smoothing (after different levels of aggregation were brought to --------

  # smoothing (direction: age)
  prop_agg_smooth <- prop_agg |> 
    select(district, year, age, origin, rel_prop) |>
    group_by(district, year, origin) |>
    arrange(age) |>
    mutate(prop_smooth = pmax(0, pmin(100, predict(loess(rel_prop ~ age,
                                                         span = rem_prop_span,
                                                         degree = 1, 
                                                         na.action = na.aggregate
    ))))) |>
    ungroup()
  


  # constrained regression --------------------------------------------------

  # prediction (duration: approx. 20 seconds)

  # Why no upper threshold?
  # proportions will be standardized to 100 percent anyway

  prop_pred <-
    con_reg(
      data = prop_agg_smooth,
      x = "year",
      y = "prop_smooth",
      group_cols = c("district", "age", "origin"),
      window = rem_window_thres,
      base_t0 = rem_base_begin,
      scen_t0 = scen_begin,
      scen_t1 = scen_end,
      prop_trend = rem_prop_trend,
      thres_percent = rem_thres_percent,
      lower_thres = rem_lower_thres,
      upper_thres = rem_upper_thres
    )

  
  # limit prediction period
  prop_pred_begin <- prop_pred |> 
    filter(year >= scen_begin)

  
  
  # expand to ages beyond age threshold -------------------------------------

  # age at age threshold
  prop_age_thres <- prop_pred_begin |> 
    filter(age == rem_age_max) |>
    rename(pred_age_thres = pred_roll) |>
    select(district, year, origin, pred_age_thres)
  

  # at ages beyond age threshold: adopt proportion
  prop_age <- as_tibble(expand_grid(
    district = uni_d,
    year = scen_begin:scen_end,
    age = age_min:age_max,
    origin = uni_o
  )) |>
    left_join(select(prop_pred_begin, district, year, age, origin, pred_roll),
              by = c("district", "year", "age", "origin")
    ) |>
    left_join(prop_age_thres, by = c("district", "year", "origin")) |>
    mutate(pred_all_age = if_else(age > rem_age_max, pred_age_thres, pred_roll))

  # smoothing (direction: age)
  pred_smooth <- group_by(prop_age, district, year, origin) |>
    arrange(age) |>
    mutate(pred_smooth = pmax(0, pmin(100, predict(loess(pred_all_age ~ age,
                                                         span = rem_pred_span, 
                                                         degree = 1, 
                                                         na.action = na.aggregate
    ))))) |>
    ungroup()
  
  
  
  
  # export the results ------------------------------------------------------

  # proportion: prediction
  rel_ex_data <- pred_smooth |> 
    mutate(prop_rel_dyao = round(pred_smooth, round_prop)) |>
    select(district, year, age, origin, prop_rel_dyao) |>
    arrange(district, year, age, origin)
  
  # export
  write_csv(rel_ex_data, ex_path)

  # log info  
  cat_log(
    text = paste0(process, ": ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )
  

  # output ------------------------------------------------------------------

  out <- list()
  out$mis_rel <- mis_rel
  out$rem_dyao <- rem_dyao
  out$dyao_smooth <- dyao_smooth  
  out$prop_dyao_smooth <- prop_dyao_smooth  
  out$dao_smooth <- dao_smooth  
  out$prop_dao_smooth <- prop_dao_smooth  
  out$prop_agg <- prop_agg  
  out$prop_agg_smooth <- prop_agg_smooth  
  out$pred_smooth <- pred_smooth  
  out$rel_ex_data <- rel_ex_data  
  out$rem_base_end <- rem_base_end  
  out$rem_base_begin <- rem_base_begin    
  out$sources <- sources     
  return(out) 
  

  
  
  
    
}
    
  

