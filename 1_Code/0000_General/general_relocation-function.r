#' relocation proportion of migration* (immigration* or emigration*)
#' 
#' variables: WHY only migration path and variable name?  
#' these are the only variables changed (when evaluation immigration or emigration), 
#' relocation and population variables remain the same
#'
#' @param mig_path 
#' @param mig_vari 
#' @param mig_district 
#' @param mig_name 
#' @param rem_number 
#' @param ex_path 
#' @param rem_base_begin 
#' @param rem_base_end 
#' @param rem_age_max 
#' @param rem_mis_span_dyao 
#' @param rem_rel_span_dyao 
#' @param rem_mis_span_dao 
#' @param rem_rel_span_dao 
#' @param rem_mis_thres_y 
#' @param rem_prop_span 
#' @param rem_window_thres 
#' @param rem_prop_trend 
#' @param rem_thres_percent 
#' @param rem_lower_thres 
#' @param rem_upper_thres 
#' @param rem_pred_span 
#'
#' @return
#' @export
#'
#' @examples
rel_prop <- function(mig_path, mig_vari, mig_district,
                     mig_name, rem_number, ex_path,
                     rem_base_begin, rem_base_end, rem_age_max,
                     rem_mis_span_dyao, rem_rel_span_dyao,
                     rem_mis_span_dao, rem_rel_span_dao,
                     rem_mis_thres_y,
                     rem_prop_span,
                     rem_window_thres, rem_prop_trend, rem_thres_percent,
                     rem_lower_thres, rem_upper_thres,
                     rem_pred_span) {

# import and data preparation ---------------------------------------------

  # migration (immigration or emigration)
  mig <- read_csv(mig_path) %>%
    rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>%
    left_join(look_dis, by = "QuarCd") %>%
    mutate(
      sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
      origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
      district = factor(distr, uni_d)
    ) %>%
    select(district, year, age, sex, origin, mig) %>%
    group_by(district, year, age, sex, origin) %>%
    summarize(
      mig = sum(mig),
      .groups = "drop"
    )

  # relocation
  # only migration to a certain district
  # WHY? this is needed to calculate immigration* (i.e. migration to a certain district)

  rel <- read_csv(rel_od) %>%
    rename(year = EreignisDatJahr, age = AlterVCd, dis = mig_district, rel = AnzUmzuWir) %>%
    left_join(look_dis, c("dis" = "QuarCd")) %>%
    mutate(
      sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
      origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
      district = factor(distr, uni_d)
    ) %>%
    select(district, year, age, sex, origin, rel) %>%
    group_by(district, year, age, sex, origin) %>%
    summarize(
      rel = sum(rel),
      .groups = "drop"
    )

  # migration* (i.e. migration to a certain district, 'migration star' = mis)
  mis <- bind_rows(
    rename(mig, mis = mig),
    rename(rel, mis = rel)
  ) %>%
    group_by(district, year, age, sex, origin) %>%
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
  )) %>%
    left_join(mis, by = c("district", "year", "age", "sex", "origin")) %>%
    left_join(rel, by = c("district", "year", "age", "sex", "origin")) %>%
    replace_na(list(mis = 0, rel = 0))

# plots  0500/0600: check differences by sex (sum over years)

# plots  0511/0611: differences by origin (sum over years and sex)

# plots 0502/0602, 0503/0603: which base years?

# aggregate over sex ------------------------------------------------------

  # This plot is the starting point (before smoothing, age corrections)

  # aggregate over sex
  rem_dyao <- group_by(mis_rel, district, year, age, origin) %>%
    summarize(
      mis = sum(mis),
      rel = sum(rel),
      .groups = "drop"
    )

  # plots 0511/0611, 0512/0612

# maximum age, base years -------------------------------------------------

  # WHY mean and not sum over high age? result is mean age
  # is more appropriate e.g. in plots
  # and can be explained (e.g. the mean amount of relocations from people as of age x)

  # aggregate
  dyao_amax_ybase <- filter(rem_dyao, (year >= rem_base_begin) & (year <= rem_base_end)) %>%
    mutate(age_new = pmin(rem_age_max, age)) %>%
    group_by(district, year, age_new, origin) %>%
    summarize(
      mis = mean(mis),
      rel = mean(rel),
      .groups = "drop"
    ) %>%
    rename(age = age_new)

# dyao: smoothing ---------------------------------------------------------

  # smoothing (direction: age, result should not be below zero)
  dyao_smooth <- group_by(dyao_amax_ybase, district, year, origin) %>%
    arrange(age) %>%
    mutate(
      mis_a = pmax(0, predict(loess(mis ~ age, span = rem_mis_span_dyao, degree = 1, na.action = na.aggregate))),
      rel_a = pmax(0, predict(loess(rel ~ age, span = rem_rel_span_dyao, degree = 1, na.action = na.aggregate)))
    ) %>%
    ungroup()

  # plots 0506/0606, 0507/0607

# dyao: proportion after smoothing ----------------------------------------

  # proportion (within the range from 0 to 100 percent)
  prop_dyao_smooth <- mutate(dyao_smooth, rel_prop_dyao = pmax(0, pmin(
    100,
    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop))
  )))

  # plots 0508/0608, 0509/0609

# dao (without y) ---------------------------------------------------------

  # aggregate
  dao <- group_by(dyao_amax_ybase, district, age, origin) %>%
    summarize(
      mis = mean(mis),
      rel = mean(rel),
      .groups = "drop"
    )

  # smoothing (direction: age)
  dao_smooth <- group_by(dao, district, origin) %>%
    arrange(age) %>%
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
    ) %>%
    ungroup()

  # plots 0510/0610

  # proportion after smoothing
  prop_dao_smooth <- mutate(dao_smooth, rel_prop_dao = pmax(0, pmin(
    100,
    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop))
  )))

  # plots 0511/0611

# proportion from different aggregation levels ----------------------------

  # proportion
  temp_dyao <- select(prop_dyao_smooth, district, year, age, origin, mis_a, rel_prop_dyao) %>%
    rename(mis = mis_a)
  temp_dao <- select(prop_dao_smooth, district, age, origin, rel_prop_dao)

  sources <- c("dyao", "dao")

  prop_agg <- left_join(temp_dyao, temp_dao, by = c("district", "age", "origin")) %>%
    mutate(
      rel_prop = if_else(mis >= rem_mis_thres_y, rel_prop_dyao, rel_prop_dao),
      rel_prop_source = factor(if_else(mis >= rem_mis_thres_y, sources[1], sources[2]), levels = sources)
    )

  # plots 0512/0612, 0513/0613, 0514/0614

# smoothing (after different levels of aggregation were brought to --------

  # smoothing (direction: age)
  prop_agg_smooth <- select(prop_agg, district, year, age, origin, rel_prop) %>%
    group_by(district, year, origin) %>%
    arrange(age) %>%
    mutate(prop_smooth = pmax(0, pmin(100, predict(loess(rel_prop ~ age,
      span = rem_prop_span,
      degree = 1, na.action = na.aggregate
    ))))) %>%
    ungroup()

  # plots 0515/0615, 0516/0616

# constrained regression --------------------------------------------------

  # prediction (duration: approx. 20 seconds)

  # Why no upper threshold?
  # proportions will be standardized to 100 percent anyways

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
  prop_pred_begin <- filter(prop_pred, year >= scen_begin)

# expand to ages beyond age threshold -------------------------------------

  # age at age threshold
  prop_age_thres <- filter(prop_pred_begin, age == rem_age_max) %>%
    rename(pred_age_thres = pred_roll) %>%
    select(district, year, origin, pred_age_thres)

  # at ages beyond age threshold: adopt proportion
  prop_age <- as_tibble(expand_grid(
    district = uni_d,
    year = scen_begin:scen_end,
    age = age_min:age_max,
    origin = uni_o
  )) %>%
    left_join(select(prop_pred_begin, district, year, age, origin, pred_roll),
      by = c("district", "year", "age", "origin")
    ) %>%
    left_join(prop_age_thres, by = c("district", "year", "origin")) %>%
    mutate(pred_all_age = if_else(age > rem_age_max, pred_age_thres, pred_roll))

  # smoothing (direction: age)
  pred_smooth <- group_by(prop_age, district, year, origin) %>%
    arrange(age) %>%
    mutate(pred_smooth = pmax(0, pmin(100, predict(loess(pred_all_age ~ age,
      span = rem_pred_span, degree = 1, na.action = na.aggregate
    ))))) %>%
    ungroup()

  # plots 0517/0617

# plots 0518/0618, 0519/0619, 0520/0620: the final predictions 

# export the results ------------------------------------------------------

  # proportion: prediction
  rel_ex_data <- mutate(pred_smooth, prop_rel_dyao = round(pred_smooth, round_prop)) %>%
    select(district, year, age, origin, prop_rel_dyao) %>%
    arrange(district, year, age, origin)

  # export
  write_csv(rel_ex_data, ex_path)

  # output (to get an idea of the exported output)
  return(list(rel_ex_data = rel_ex_data,
              mis_rel = mis_rel,
              rem_dyao = rem_dyao,
              dyao_smooth = dyao_smooth,
              prop_dyao_smooth = prop_dyao_smooth,
              dao_smooth = dao_smooth,
              prop_dao_smooth = prop_dao_smooth,
              prop_agg = prop_agg,
              prop_agg_smooth = prop_agg_smooth,
              pred_smooth = pred_smooth,
              rem_base_end = rem_base_end,
              rem_base_begin = rem_base_begin,
              sources = sources))
}
