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
#' @return
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

  # population
  # year: begin of year population (therefore, StichtagDatJahr + 1)

  pop <- read_csv(pop_od) %>%
    rename(age = AlterVCd, pop = AnzBestWir) %>%
    left_join(look_dis, by = "QuarCd") %>%
    mutate(
      year = StichtagDatJahr + 1,
      sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
      origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
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

  # years of the past (for plot)
  year_past <- (date_start + 1):date_end
  year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

  # plot
  sszplot(mis_rate_dy,
    aes_x = "year", aes_y = "mis_rate_dy",
    i_x = year_past_5,
    wrap = "district", ncol = 4,
    labs_y = paste0(mig_name, "* rate (in % per year)"),
    name = paste0(mig_number, "00_", mig_name, "-star-rate_by-district-year"),
    width = 12, height = 14
  )


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

  # plot

  # levels
  time_lev <- c("past", "future")

  # plot data
  plot_dat_mis_pred <- select(mis_past_pred, district, year, rate_all) %>%
    mutate(time = factor(if_else(year <= mis_base_end,
      time_lev[1], time_lev[2]
    ), levels = time_lev))

  sszplot(plot_dat_mis_pred,
    aes_x = "year", aes_y = "rate_all", aes_ltyp = "time",
    i_x = c(mis_base_begin, mis_base_end),
    wrap = "district", ncol = 4,
    labs_y = paste0(mig_name, "* rate (in % per year)"),
    name = paste0(mig_number, "01_", mig_name, "-star-rate_by-district-year_predicition"),
    width = 12, height = 14
  )

  # plot (more detailed: regression and limits)
  sszplot(mis_past_pred,
    aes_x = "year", aes_y = "mis_rate_dy",
    geom = "point",
    i_x = c(mis_base_begin, mis_base_end),
    wrap = "district", ncol = 4,
    labs_y = paste0(mig_name, "* rate (in % per year)"),
    name = paste0(mig_number, "02_", mig_name, "-star-rate_by-district-year_predicition-details"),
    width = 12, height = 14,
    quotes = c(
      quote(geom_line(aes(x = year, y = pred), linetype = 2)),
      quote(geom_line(aes(x = year, y = pred_mean), linetype = 3)),
      quote(geom_line(aes(x = year, y = pred_roll)))
    )
  )

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
  return(list(ex_mig_rate_dy))
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
#' @return
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

  # years of the past (for plot)
  year_past <- (date_start + 1):date_end
  year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

  # plot
  sszplot(mis_dyso,
    aes_x = "year", aes_y = "mis_prop_dyso", aes_col = "sex", aes_ltyp = "origin",
    i_x = year_past_5,
    wrap = "district", ncol = 4,
    labs_y = paste0("proportion in % (per district and year, in ", mig_name, "*)"),
    name = paste0(mig_number, "03_", mig_name, "_proportion-sex-origin_by-district-year"),
    width = 12, height = 14
  )


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

  # plot
  sszplot(mis_so_past_pred,
    aes_x = "year", aes_y = "prop_all", aes_col = "sex", aes_ltyp = "origin",
    i_x = c(mis_so_base_begin, mis_so_base_end),
    wrap = "district", ncol = 4,
    labs_y = paste0("proportion in % (per district and year, in ", mig_name, "*)"),
    name = paste0(mig_number, "04_", mig_name, "_proportion-sex-origin_by-district-year_prediction"),
    width = 12, height = 14
  )

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
  return(list(ex_mig_prop_dy))
  
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
#' @return
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

  # years of the past (for plot)
  year_past <- date_start:date_end
  year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

  # plot
  sszplot(mis_dyso,
    aes_x = "year", aes_y = "mis_dyso", aes_col = "sex", aes_ltyp = "origin",
    i_x = year_past_5,
    wrap = "district", ncol = 4,
    labs_y = paste0(mig_name, "* per year"),
    name = paste0(mig_number, "10_", mig_name, "_star_per-district-year-sex-origin"),
    width = 12, height = 14
  )

  # plot
  sszplot(mis_dyso,
    aes_x = "year", aes_y = "mis_dyso", aes_col = "sex", aes_ltyp = "origin",
    i_x = year_past_5,
    wrap = "district", ncol = 4, gridscale = "free",
    labs_y = paste0(mig_name, "* per year"),
    name = paste0(mig_number, "11_", mig_name, "_star_per-district-year-sex-origin_free-scales"),
    width = 12, height = 14
  )

  

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

  # plot: focus age distribution
  # years (subjectively selected)
  # WHY with rev? To have the last year in the plot
  years_plot <- rev(seq(date_end, date_start, by = -8))

  sszplot(filter(mis_dyaso, year %in% years_plot),
    aes_x = "age", aes_y = "mis_prop_a", aes_col = "year",
    grid = c("sex", "origin"),
    labs_y = "proportion in %", labs_col = "year",
    name = paste0(mig_number, "12_", mig_name, "_star_age-proportion_per-district-year-sex-origin_focus-age"),
    width = 11, height = 8,
    multi = uni_d
  )


  # plot: focus years
  # age (subjectively selected)
  age_plot <- seq(0, 60, by = 20)

  sszplot(filter(mis_dyaso, age %in% age_plot),
    aes_x = "year", aes_y = "mis_prop_a", aes_col = "age",
    grid = c("sex", "origin"),
    labs_y = "proportion in %", labs_col = "age",
    name = paste0(mig_number, "13_", mig_name, "_star_age-proportion_per-district-year-sex-origin_focus-years"),
    width = 11, height = 8,
    multi = uni_d
  )


# smoothing migration* with LOESS over years (by district, age, sex --------

  mis_smooth <- mis_dyaso %>%
    arrange(district, age, sex, origin, year) %>%
    group_by(district, age, sex, origin) %>%
    mutate(mis_smooth = pmax(0, predict(
      loess(mis_dyaso ~ year, span = mis_span_y, degree = 1, na.action = na.aggregate)))) %>%
    ungroup()

  # plot preparation

  fit_lev <- c("initial", "smoothed")  
  mis_smooth_plot <- mis_smooth %>% 
    pivot_longer(c(mis_dyaso, mis_smooth), names_to = "category", values_to = "mis") %>% 
    mutate(cat = factor(if_else(category == "mis_dyaso",
      fit_lev[1], fit_lev[2]
    ), levels = fit_lev)) %>%
    select(district, year, age, sex, origin, cat, mis)


  # plot: focus age distribution
  sszplot(filter(mis_smooth_plot, year %in% year_past_5),
    aes_x = "age", aes_y = "mis", aes_col = "cat",
    grid = c("as.factor(year)", "origin*sex"),
    labs_y = paste0(mig_name, "* per year"),
    name = paste0(mig_number, "14_", mig_name, "_star_smoothed-over-year_focus-age"),
    width = 11, height = 8,
    multi = uni_d
  )

  # plot: focus years
  # age (subjectively selected)
  age_plot_smooth <- seq(0, 60, by = 20)

  sszplot(filter(mis_smooth_plot, age %in% age_plot_smooth),
    aes_x = "year", aes_y = "mis", aes_col = "cat",
    grid = c("as.factor(age)", "origin*sex"),
    labs_y = paste0(mig_name, "* per year"),
    name = paste0(mig_number, "15_", mig_name, "_star_smoothed-over-year_focus-years"),
    width = 11, height = 8,
    multi = uni_d
  )



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

  # plot: focus age distribution
  sszplot(filter(mis_age_prop_smooth, year %in% year_past_5),
    aes_x = "age", aes_y = "prop_a_smooth", aes_col = "year",
    grid = c("sex", "origin"),
    labs_y = "proportion in %",
    name = paste0(mig_number, "16_", mig_name, "_star_age-proportion_after-smoothing_focus-age"),
    width = 11, height = 8,
    multi = uni_d
  )

  # plot: focus years
  # age (subjectively selected)
  age_plot_smooth_prop <- seq(0, 60, by = 20)

  sszplot(filter(mis_age_prop_smooth, age %in% age_plot_smooth_prop),
    aes_x = "year", aes_y = "prop_a_smooth", aes_col = "age",
    grid = c("sex", "origin"),
    labs_y = "proportion in %", labs_col = "age",
    name = paste0(mig_number, "17_", mig_name, "_star_age-proportion_after-smoothing_focus-years"),
    width = 11, height = 8,
    multi = uni_d
  )


# smoothing proportion by age with LOESS (by district, year, sex,  --------

  prop_fit <- arrange(mis_age_prop_smooth, district, year, sex, origin, age) %>%
    group_by(district, year, sex, origin) %>%
    mutate(prop_fit = pmax(0, predict(
      loess(prop_a_smooth ~ age, span = mis_span_a, degree = 1, na.action = na.aggregate)))) %>%
    ungroup()

  
  # plot preparation
  fit_lev <- c("initial", "smoothed")

  mis_fit_plot <- prop_fit %>% 
    pivot_longer(c(prop_a_smooth, prop_fit), names_to = "category", values_to = "prop") %>% 
    mutate(cat = factor(if_else(category == "prop_a_smooth",
      fit_lev[1], fit_lev[2]
    ), levels = fit_lev)) %>%
    select(district, year, age, sex, origin, cat, prop)    
    
  # plot: focus age distribution

  sszplot(filter(mis_fit_plot, year %in% year_past_5),
    aes_x = "age", aes_y = "prop", aes_col = "cat",
    grid = c("as.factor(year)", "origin*sex"),
    labs_y = "proportion in %",
    name = paste0(mig_number, "18_", mig_name, "_star_proportion_smoothed_focus-age"),
    width = 11, height = 8,
    multi = uni_d
  )


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

  sszplot(mis_a_past_pred,
    aes_x = "age", aes_y = "prop_a", aes_col = "year",
    grid = c("origin", "sex"),
    labs_y = "proportion in %",
    name = paste0(mig_number, "19_", mig_name, "_star_age-proportion_by-district-year-sex-origin_past-future"),
    width = 11, height = 8,
    multi = uni_d
  )
  
  # plot levels
  time_lev <- c("past", "future")

  # plot data
  plot_a_past_pred <- mutate(mis_a_past_pred,
    time = factor(
      if_else(year <= mis_age_base_end, time_lev[1], time_lev[2]),
      levels = time_lev
    )
  )

  # plot: focus age distribution
  # WHY this plot: it is recommendable to look precisely at the age plots over years
  # therefore, a plot that focuses on certain years

  # years
  year_plot <- seq(date_start + 1, scen_end, by = 8)

  sszplot(filter(plot_a_past_pred, year %in% year_plot),
    aes_x = "age", aes_y = "prop_a", aes_col = "year",
    grid = c("sex", "origin"),
    labs_y = "proportion in %", labs_col = "year",
    fix_size = 1,
    name = paste0(mig_number, "20_", mig_name, "_star_age-proportion_by-district-year-sex-origin_past-future_focus-age"),
    width = 11, height = 8,
    multi = uni_d
  )

  # plot: focus years

  # age (subjectively selected)
  age_plot_pred <- seq(0, 60, by = 20)

  sszplot(filter(plot_a_past_pred, age %in% age_plot_pred),
    aes_x = "year", aes_y = "prop_a", aes_col = "age",
    grid = c("sex", "origin"),
    labs_y = "proportion in %", labs_col = "age",
    name = paste0(mig_number, "21_", mig_name, "_star_age-proportion_by-district-year-sex-origin_past-future_focus-years"),
    width = 11, height = 8,
    multi = uni_d
  )

  # output (to get an idea of the exported output)
  return(list(ex_mig_prop_a_dyso))

}




