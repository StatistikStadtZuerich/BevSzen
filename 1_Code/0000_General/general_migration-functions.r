# header ------------------------------------------------------------------
# migration functions
# -rate (by district, year)
# -proportion of sex/origin (by district, year)
# -proportion of age (by district, year, sex, origin)


#' migration rate per district and year
#'
#' @param mig_path 
#' @param mig_vari 
#' @param mig_district 
#' @param mig_name 
#' @param mig_number 
#' @param ex_path 
#' @param mis_base_begin 
#' @param mis_base_end 
#' @param mis_rate_window_thres 
#' @param mis_rate_prop_trend 
#' @param mis_rate_thres_percent 
#' @param mis_rate_lower_thres 
#'
#' @return NULL
#' @export
#'
#' @examples
mig_rate_dy <- function(mig_path, mig_vari, mig_district,
                        mig_name, mig_number, ex_path,
                        mis_base_begin, mis_base_end,
                        mis_rate_window_thres, mis_rate_prop_trend,
                        mis_rate_thres_percent, mis_rate_lower_thres) {

  # variables
  # WHY only migration path and variable name?
  # that are the only variables changed (when evaluation immigration or emigration)
  # relocation and population variables remain the same

  # import and data preparation ---------------------------------------------

  # migration (immigration or emigration)
  mig <- read_csv(mig_path) %>%
    rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>%
    left_join(look_dis, by = "QuarCd") %>%
    mutate(
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
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
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
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

  # population
  # year: begin of year population (therefore, StichtagDatJahr + 1)

  pop <- read_csv(pop_od) %>%
    rename(age = AlterVCd, pop = AnzBestWir) %>%
    left_join(look_dis, by = "QuarCd") %>%
    mutate(
      year = StichtagDatJahr + 1,
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
      district = factor(distr, uni_d)
    ) %>%
    select(district, year, age, sex, origin, pop) %>%
    group_by(district, year, age, sex, origin) %>%
    summarize(
      pop = sum(pop),
      .groups = "drop"
    )


  # migration* rate (per district and year) ---------------------------------

  # mis and pop: aggregate
  mis_dy <- group_by(mis, district, year) %>%
    summarize(
      mis = sum(mis),
      .groups = "drop"
    )

  pop_dy <- group_by(pop, district, year) %>%
    summarize(
      pop = sum(pop),
      .groups = "drop"
    )

  # migration* rate (based on all possible cases)
  mis_rate_dy <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):date_end
  )) %>%
    left_join(pop_dy, by = c("district", "year")) %>%
    left_join(mis_dy, by = c("district", "year")) %>%
    replace_na(list(pop = 0, ims = 0)) %>%
    mutate(mis_rate_dy = if_else(pop == 0, NA_real_, round(mis / pop * 100, round_rate)))
  
  # plots 0300/0400

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
  )) %>%
    left_join(select(mis_rate_dy, district, year, mis_rate_dy),
      by = c("district", "year")
    ) %>%
    left_join(mis_pred, by = c("district", "year")) %>%
    mutate(rate_all = if_else(year <= mis_base_end, mis_rate_dy, pred_roll))

  # plots 0301/0401, 0302/0402

  # export preparation
  ex_mig_rate_dy <- mutate(mis_past_pred,
    rate = round(rate_all, round_rate)
  ) %>%
    filter(year >= scen_begin) %>%
    select(district, year, rate) %>%
    arrange(district, year)

  # export the data
  write_csv(ex_mig_rate_dy, ex_path)

  # output (to get an idea of the exported output)
  return(list(ex_mig_rate_dy = ex_mig_rate_dy,
              mis_rate_dy = mis_rate_dy, 
              mis_past_pred = mis_past_pred,
              mis_base_end = mis_base_end,
              mis_base_begin = mis_base_begin))
}

#' migration: proportion of sex and origin, by distict and year
#'
#' @param mig_path 
#' @param mig_vari 
#' @param mig_district 
#' @param mig_name 
#' @param mig_number 
#' @param ex_path 
#' @param mis_so_base_begin 
#' @param mis_so_base_end 
#' @param mis_so_window_thres 
#' @param mis_so_prop_trend 
#' @param mis_so_thres_percent 
#' @param mis_so_lower_thres 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
mig_prop_so_dy <- function(mig_path, mig_vari, mig_district,
                           mig_name, mig_number, ex_path,
                           mis_so_base_begin, mis_so_base_end,
                           mis_so_window_thres, mis_so_prop_trend,
                           mis_so_thres_percent, mis_so_lower_thres, ...) {

  # variables
  # WHY only migration path and variable name?
  # that are the only variables changed (when evaluation immigration or emigration)
  # relocation variables remain the same

# import and data preparation ---------------------------------------------

  # migration (immigration or emigration)
  mig <- read_csv(mig_path) %>%
    rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>%
    left_join(look_dis, by = "QuarCd") %>%
    mutate(
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
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
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
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

# migration: distribution of sex and origin -------------------------------

  # ims and pop: aggregate
  mis_dy <- group_by(mis, district, year) %>%
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

  mis_dyso <- group_by(mis, district, year, sex, origin) %>%
    summarize(
      mis_dyso = sum(mis),
      .groups = "drop"
    ) %>%
    left_join(rename(mis_dy, mis_dy = mis),
      by = c("district", "year")
    ) %>%
    right_join(cas_dyso, by = c("district", "year", "sex", "origin")) %>%
    replace_na(list(mis_dyso = 0, mis_dy = 0)) %>%
    mutate(mis_prop_dyso = if_else(mis_dy == 0, NA_real_, round(mis_dyso / mis_dy * 100, round_prop)))
  
  # plots  0303/0403

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
  mis_so_pred_stand <- group_by(mis_so_pred, district, year) %>%
    mutate(pred_roll_sum = sum_NA(pred_roll),
           pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, round(pred_roll / pred_roll_sum * 100, round_prop))) %>% 
    ungroup()

  # past and prediction
  mis_so_past_pred <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):scen_end,
    sex = uni_s,
    origin = uni_o
  )) %>%
    left_join(select(mis_dyso, district, year, sex, origin, mis_prop_dyso),
      by = c("district", "year", "sex", "origin")
    ) %>%
    left_join(mis_so_pred_stand, by = c("district", "year", "sex", "origin")) %>%
    mutate(prop_all = if_else(year <= mis_so_base_end, mis_prop_dyso, pred_roll_stand))

  # plots  0304/0404

  # export preparation
  ex_mig_prop_dy <- mutate(mis_so_past_pred,
    prop = round(prop_all, round_prop)
  ) %>%
    filter(year >= scen_begin) %>%
    select(district, year, sex, origin, prop) %>%
    arrange(district, year, sex, origin)

  # export the data
  write_csv(ex_mig_prop_dy, ex_path)
  
  # output (to get an idea of the exported output)
  return(list(ex_mig_prop_dy = ex_mig_prop_dy,
              mis_dyso = mis_dyso,
              mis_so_past_pred = mis_so_past_pred,
              mis_so_base_begin = mis_so_base_begin,
              mis_so_base_end = mis_so_base_end))
  
}

#' migration: age proportion (by district, year, sex, origin)
#'
#' @param mig_path 
#' @param mig_vari 
#' @param mig_district 
#' @param mig_name 
#' @param mig_number 
#' @param ex_path 
#' @param mis_age_min 
#' @param mis_age_max 
#' @param mis_span_y 
#' @param mis_span_a 
#' @param mis_age_window_years 
#' @param mis_age_base_begin 
#' @param mis_age_base_end 
#' @param mis_age_window_thres 
#' @param mis_age_prop_trend 
#' @param mis_age_thres_percent 
#' @param mis_age_lower_thres 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
mig_prop_a_dyso <- function(mig_path, mig_vari, mig_district,
                            mig_name, mig_number, ex_path,
                            mis_age_min, mis_age_max,
                            mis_span_y, mis_span_a, mis_age_window_years,
                            mis_age_base_begin, mis_age_base_end,
                            mis_age_window_thres, mis_age_prop_trend,
                            mis_age_thres_percent, mis_age_lower_thres, ...) {

  # variables
  # WHY only migration path and variable name?
  # that are the only variables changed (when evaluation immigration or emigration)
  # relocation variables remain the same


# import and data preparation ---------------------------------------------

  # migration (immigration or emigration)
  mig <- read_csv(mig_path) %>%
    rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>%
    left_join(look_dis, by = "QuarCd") %>%
    mutate(
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
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
      sex = fact_if(SexCd, uni_s),
      origin = fact_if(HerkunftCd, uni_o),
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

# immigration*: per district, year, sex, origi ----------------------------

  # WHY? to see where the denominator of the age proportion is low

  # possible cases: dyso
  cas_dyso <- as_tibble(expand_grid(
    district = uni_d,
    year = date_start:date_end,
    sex = uni_s,
    origin = uni_o
  ))

  # migration*: dyso
  mis_dyso <- group_by(mis, district, year, sex, origin) %>%
    summarize(
      mis_dyso = sum(mis),
      .groups = "drop"
    ) %>%
    right_join(cas_dyso, by = c("district", "year", "sex", "origin")) %>%
    replace_na(list(mis_dyso = 0))

  # plots  0310/0410,  0311/0411

# age proportion: per district, year, sex, origin -------------------------

  # migration*: dyaso (already summarized before)
  mis_dyaso <- as_tibble(expand_grid(
    district = uni_d,
    year = date_start:date_end,
    age = mis_age_min:mis_age_max,
    sex = uni_s,
    origin = uni_o
  )) %>%
    left_join(mis, by = c("district", "year", "age", "sex", "origin")) %>%
    rename(mis_dyaso = mis) %>%
    replace_na(list(mis_dyaso = 0)) %>%
    left_join(mis_dyso, by = c("district", "year", "sex", "origin")) %>%
    arrange(district, year, sex, origin, age) %>%
    mutate(mis_prop_a = if_else(mis_dyso == 0, NA_real_, round(mis_dyaso / mis_dyso * 100, round_prop)))

  # plots  0312/0412,  0313/0413

# smoothing migration* with LOESS over years (by district, age, sex --------

  mis_smooth <- mis_dyaso %>%
    arrange(district, age, sex, origin, year) %>%
    group_by(district, age, sex, origin) %>%
    mutate(mis_smooth = pmax(0, predict(
      loess(mis_dyaso ~ year, span = mis_span_y, degree = 1, na.action = na.aggregate)))) %>%
    ungroup()

  # plots  0314/0414,  0315/0415

# age proportion (after smoothing migration* over years) ------------------

  # preparation
  mis_smooth_prep <- mis_smooth %>% 
    select(district, year, age, sex, origin, mis_smooth) %>%
    rename(mis_dyaso = mis_smooth)

  # age proportion
  mis_age_prop_smooth <- group_by(mis_smooth_prep, district, year, sex, origin) %>%
    mutate(mis_dyso = sum_NA(mis_dyaso), 
           prop_a_smooth = if_else(mis_dyso == 0, NA_real_, round(mis_dyaso / mis_dyso * 100, round_prop))) %>%
    ungroup() %>% 
    select(district, year, age, sex, origin, prop_a_smooth) %>%
    arrange(district, year, sex, origin, age)           

  # plots  0316/0416,  0317/0417

# smoothing proportion by age with LOESS (by district, year, sex,  --------

  prop_fit <- arrange(mis_age_prop_smooth, district, year, sex, origin, age) %>%
    group_by(district, year, sex, origin) %>%
    mutate(prop_fit = pmax(0, predict(
      loess(prop_a_smooth ~ age, span = mis_span_a, degree = 1, na.action = na.aggregate)))) %>%
    ungroup()

  # plot  0318/0418

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
  prop_stand <- group_by(prop_pred_begin, district, year, sex, origin) %>%
    summarize(
      pred_roll_sum = sum_NA(pred_roll),
      .groups = "drop"
    ) %>%
    right_join(prop_pred_begin, by = c("district", "year", "sex", "origin")) %>%
    mutate(pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, round(pred_roll / pred_roll_sum * 100, round_prop)))

  # past and prediction
  mis_a_past_pred <- as_tibble(expand_grid(
    district = uni_d,
    year = (date_start + 1):scen_end,
    age = mis_age_min:mis_age_max,
    sex = uni_s,
    origin = uni_o
  )) %>%
    left_join(select(mis_dyaso, district, year, age, sex, origin, mis_prop_a),
      by = c("district", "year", "age", "sex", "origin")
    ) %>%
    left_join(select(prop_stand, district, year, age, sex, origin, pred_roll_stand),
      by = c("district", "year", "age", "sex", "origin")
    ) %>%
    mutate(prop_a = if_else(year <= mis_age_base_end, mis_prop_a, pred_roll_stand))

  # export preparation
  ex_mig_prop_a_dyso <- mutate(mis_a_past_pred,
    prop = round(prop_a, round_prop)
  ) %>%
    filter(year >= scen_begin) %>%
    select(district, year, age, sex, origin, prop) %>%
    arrange(district, year, sex, origin, age)

  # export the data
  write_csv(ex_mig_prop_a_dyso, ex_path)

  # plots  0320/0420,  0321/0421

  # output (to get an idea of the exported output)
  return(list(ex_mig_prop_a_dyso = ex_mig_prop_a_dyso,
              mis_dyso = mis_dyso,
              mis_dyaso = mis_dyaso,
              mis_smooth = mis_smooth,
              mis_age_prop_smooth = mis_age_prop_smooth,
              prop_fit = prop_fit,
              mis_a_past_pred = mis_a_past_pred,
              mis_age_base_begin = mis_age_base_begin,
              mis_age_base_end = mis_age_base_end))

}
