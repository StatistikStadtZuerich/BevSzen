#-------------------------------------------------------------------
# function: relocation proportion of migration*
# (immigration* or emigration*)
#
#
#
# rok/bad, August 2021
#-------------------------------------------------------------------



# proportion of relocation based on migration*

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


  # variables
  # WHY only migration path and variable name?
  # that are the only variables changed (when evaluation immigration or emigration)
  # relocation and population variables remain the same



  #-------------------------------------------------------------------
  # import and data preparation
  #-------------------------------------------------------------------

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


  #-------------------------------------------------------------------
  # check differences by sex (sum over years)
  #-------------------------------------------------------------------

  # WHY? Check if minor differences between sex? Because omitted in precition
  # theoretically this should be evaluated with all interactions (i.e. over years)
  # however, there are not enough data points available

  # proportion by daso
  rel_prop_daso <- group_by(mis_rel, district, age, sex, origin) %>%
    summarize(
      mis = sum(mis),
      rel = sum(rel),
      .groups = "drop"
    ) %>%
    mutate(rel_prop_daso = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)))

  # plot: focus sex
  sszplot(rel_prop_daso,
    aes_x = "age", aes_y = "rel_prop_daso", aes_col = "sex",
    wrap = "district", ncol = 4,
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "00_proportion-daso_focus-sex"),
    width = 12, height = 14,
    multi = uni_o
  )

  #-------------------------------------------------------------------
  # differences by origin (sum over years and sex)
  #-------------------------------------------------------------------

  # WHY?
  # first: to emphasize the need of origin in the model
  # second: and the plots can be used to get an idea the value of an age threshold

  # proportion by dao
  rel_prop_dao <- group_by(mis_rel, district, age, origin) %>%
    summarize(
      mis = sum(mis),
      rel = sum(rel),
      .groups = "drop"
    ) %>%
    mutate(rel_prop_dao = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)))

  # plot
  sszplot(rel_prop_dao,
    aes_x = "age", aes_y = "rel_prop_dao", aes_col = "origin",
    wrap = "district", ncol = 4,
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "01_proportion-dao_focus-origin"),
    width = 11, height = 10
  )

  #-------------------------------------------------------------------
  # Which base years?
  #-------------------------------------------------------------------

  # year and origin

  # WHY? simplest plot to determine the base years

  # proportion by yo
  rel_prop_yo <- group_by(mis_rel, year, origin) %>%
    summarize(
      mis = sum(mis),
      rel = sum(rel),
      .groups = "drop"
    ) %>%
    mutate(rel_prop_yo = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)))

  # plot
  sszplot(rel_prop_yo,
    aes_x = "year", aes_y = "rel_prop_yo", aes_col = "origin",
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "02_proportion-yo"),
    width = 8, height = 6,
    quotes = quote(expand_limits(y = 0))
  )

  # year, age, origin

  # proportion by yao
  rel_prop_yao <- group_by(mis_rel, year, age, origin) %>%
    summarize(
      mis = sum(mis),
      rel = sum(rel),
      .groups = "drop"
    ) %>%
    rename(age_num = age) %>%
    mutate(
      rel_prop_yao = if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop)),
      age = as.factor(age_num)
    )

  # age (subjectively selected)
  age_plot <- seq(0, 80, by = 10)

  # plot
  sszplot(rel_prop_yao %>% filter(age_num %in% age_plot),
    aes_x = "year", aes_y = "rel_prop_yao", aes_col = "origin",
    grid = c("age", "."),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "03_proportion-yao"),
    width = 11, height = 14
  )

  #-------------------------------------------------------------------
  # aggregate over sex
  #-------------------------------------------------------------------

  # This plot is the starting point (before smoothing, age corrections)

  # aggregate over sex
  rem_dyao <- group_by(mis_rel, district, year, age, origin) %>%
    summarize(
      mis = sum(mis),
      rel = sum(rel),
      .groups = "drop"
    )

  # plot preparation
  process_lev <- c("relocation", paste0(mig_name, "*"))

  process_plot <- gather(rem_dyao, `rel`, `mis`, key = category, value = count) %>%
    mutate(cat = factor(if_else(category == "rel",
      process_lev[1], process_lev[2]
    ), levels = process_lev)) %>%
    select(district, year, age, origin, cat, count)


  # plot: focus age

  # years (subjectively, but last year in the plot)
  year_plot <- seq(date_end, date_start, by = -8)

  sszplot(process_plot %>% filter(year %in% year_plot),
    aes_x = "age", aes_y = "count", aes_col = "cat",
    grid = c("year", "origin"),
    labs_y = "quantity per year",
    name = paste0(rem_number, "04_processes_dyao_focus-age"),
    width = 11, height = 14,
    multi = uni_d
  )

  # plot: focus years

  # age (subjectively selected)
  age_plot <- seq(0, 80, by = 20)

  # with age as factor
  process_plot_age <- rename(process_plot, age_num = age) %>%
    mutate(age = as.factor(age_num))

  sszplot(process_plot_age %>% filter(age_num %in% age_plot),
    aes_x = "year", aes_y = "count", aes_col = "cat",
    grid = c("age", "origin"),
    labs_y = "quantity per year",
    name = paste0(rem_number, "05_processes_dyao_focus-years"),
    width = 12, height = 8,
    multi = uni_d
  )

  #-------------------------------------------------------------------
  # maximum age, base years
  #-------------------------------------------------------------------

  # WHY mean and not sum over high age? result is mean age
  # is more appropriate e.g. in plots
  # and can be explaned (e.g. the mean amount of relocations from people as of age x)

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


  #-------------------------------------------------------------------
  # dyao: smoothing
  #-------------------------------------------------------------------

  # smoothing (direction: age, result should not be below zero)
  dyao_smooth <- group_by(dyao_amax_ybase, district, year, origin) %>%
    arrange(age) %>%
    mutate(
      mis_a = pmax(0, predict(loess(mis ~ age, span = rem_mis_span_dyao, degree = 1, na.action = na.aggregate))),
      rel_a = pmax(0, predict(loess(rel ~ age, span = rem_rel_span_dyao, degree = 1, na.action = na.aggregate)))
    ) %>%
    ungroup()

  # plot preparation
  smooth_lev <- c("initial", "smoothed")

  temp_initial <- gather(dyao_smooth, `rel`, `mis`, key = category, value = count) %>%
    mutate(
      cat = factor(if_else(category == "rel",
        process_lev[1], process_lev[2]
      ), levels = process_lev),
      smooth = factor(smooth_lev[1], levels = smooth_lev)
    ) %>%
    select(district, year, age, origin, cat, smooth, count)

  temp_smooth <- gather(dyao_smooth, `rel_a`, `mis_a`, key = category, value = count) %>%
    mutate(
      cat = factor(if_else(category == "rel_a",
        process_lev[1], process_lev[2]
      ), levels = process_lev),
      smooth = factor(smooth_lev[2], levels = smooth_lev)
    ) %>%
    select(district, year, age, origin, cat, smooth, count)

  smooth_plot <- bind_rows(temp_initial, temp_smooth)


  # plot: focus age

  # base years (subjectively, but last year in the plot)
  year_plot <- seq(rem_base_end, rem_base_begin, by = -7)

  # plot
  sszplot(smooth_plot %>% filter(year %in% year_plot),
    aes_x = "age", aes_y = "count", aes_col = "cat", aes_ltyp = "smooth", aes_alpha = "smooth",
    grid = c("origin", "year"),
    labs_y = "quantity per year",
    name = paste0(rem_number, "06_processes_dyao_smooth_focus-age"),
    quotes = quote(scale_alpha_manual(values = c(0.3, 1))),
    width = 15, height = 8,
    multi = uni_d
  )

  # plot: focus years

  # age (subjectively selected)
  age_plot <- seq(0, 60, by = 20)

  # plot
  sszplot(smooth_plot %>% filter(age %in% age_plot),
    aes_x = "year", aes_y = "count", aes_col = "cat", aes_ltyp = "smooth", aes_alpha = "smooth",
    grid = c("origin", "age"),
    labs_y = "quantity per year",
    name = paste0(rem_number, "07_processes_dyao_smooth_focus-years"),
    quotes = quote(scale_alpha_manual(values = c(0.3, 1))),
    width = 15, height = 8,
    multi = uni_d
  )

  #-------------------------------------------------------------------
  # dyao: proportion after smoothing
  #-------------------------------------------------------------------

  # proportion (within the range from 0 to 100 percent)
  prop_dyao_smooth <- mutate(dyao_smooth, rel_prop_dyao = pmax(0, pmin(
    100,
    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop))
  )))


  # plot: focus age

  # base years (subjectively, but last year in the plot)
  year_plot <- seq(rem_base_end, rem_base_begin, by = -3)

  # plot
  sszplot(prop_dyao_smooth,
    aes_x = "age", aes_y = "rel_prop_dyao", aes_col = "origin",
    wrap = "district", ncol = 4,
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "08_proportion_dyao_smooth_focus-age"),
    width = 11, height = 10,
    multi = year_plot, multif = "filter(year == x)"
  )

  # plot: focus year

  # age (subjectively selected)
  age_plot <- seq(0, 70, by = 10)

  # plot
  sszplot(prop_dyao_smooth,
    aes_x = "year", aes_y = "rel_prop_dyao", aes_col = "origin",
    wrap = "district", ncol = 4,
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "09_proportion_dyao_smooth_focus-years"),
    title = "paste0('age: ', as.character(x))",
    width = 11, height = 10,
    multi = age_plot, multif = "filter(age == x)"
  )

  #-------------------------------------------------------------------
  # dao (without y)
  #-------------------------------------------------------------------

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

  # plot preparation
  temp_initial <- gather(dao_smooth, `rel`, `mis`, key = category, value = count) %>%
    mutate(
      cat = factor(if_else(category == "rel",
        process_lev[1], process_lev[2]
      ), levels = process_lev),
      smooth = factor(smooth_lev[1], levels = smooth_lev)
    ) %>%
    select(district, age, origin, cat, smooth, count)

  temp_smooth <- gather(dao_smooth, `rel_a`, `mis_a`, key = category, value = count) %>%
    mutate(
      cat = factor(if_else(category == "rel_a",
        process_lev[1], process_lev[2]
      ), levels = process_lev),
      smooth = factor(smooth_lev[2], levels = smooth_lev)
    ) %>%
    select(district, age, origin, cat, smooth, count)

  dao_smooth_plot <- bind_rows(temp_initial, temp_smooth)


  # plot
  sszplot(dao_smooth_plot,
    aes_x = "age", aes_y = "count", aes_col = "cat", aes_ltyp = "smooth", aes_alpha = "smooth",
    grid = c("origin", "."),
    labs_y = "quantity per year",
    name = paste0(rem_number, "10_processes_dao_smooth"),
    quotes = quote(scale_alpha_manual(values = c(0.3, 1))),
    width = 15, height = 10,
    multi = uni_d
  )

  # proportion after smoothing
  prop_dao_smooth <- mutate(dao_smooth, rel_prop_dao = pmax(0, pmin(
    100,
    if_else(mis_a == 0, NA_real_, round(rel_a / mis_a * 100, round_prop))
  )))

  # plot
  sszplot(prop_dao_smooth,
    aes_x = "age", aes_y = "rel_prop_dao", aes_col = "origin",
    wrap = "district", ncol = 4,
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "11_proportion_dao_smooth"),
    width = 11, height = 10
  )

  #-------------------------------------------------------------------
  # proportion from different aggregation levels
  #-------------------------------------------------------------------


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

  # plot: entire curves (different aggregations levels)
  entire_curves <- select(prop_agg, district, year, age, origin, rel_prop_dyao, rel_prop_dao) %>%
    rename(dyao = rel_prop_dyao, dao = rel_prop_dao) %>%
    gather(`dyao`, `dao`, key = category, value = count) %>%
    mutate(cat = factor(category, levels = sources))

  # base years (subjectively, but last year in the plot)
  year_plot <- seq(rem_base_end, rem_base_begin, by = -3)

  sszplot(entire_curves %>% filter(year %in% year_plot),
    aes_x = "age", aes_y = "count", aes_col = "cat",
    grid = c("year", "origin"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "12_proportion_different-aggregation-levels"),
    width = 12, height = 8,
    multi = uni_d
  )

  # plot: focus age

  # base years (subjectively, but last year in the plot)
  year_plot <- seq(rem_base_end, rem_base_begin, by = -3)

  sszplot(prop_agg %>% filter(year %in% year_plot),
    aes_x = "age", aes_y = "rel_prop", aes_col = "rel_prop_source",
    quotes_top = quote(geom_line(aes(x = age, y = rel_prop), color = "black")),
    geom = "point",
    grid = c("year", "origin"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "13_one-proportion-from-different-aggregation-levels_focus-age"),
    width = 12, height = 8,
    multi = uni_d
  )

  # plot: focus years

  # age (subjectively selected)
  age_plot <- seq(0, 60, by = 20)

  sszplot(prop_agg %>% filter(age %in% age_plot),
    aes_x = "year", aes_y = "rel_prop", aes_col = "rel_prop_source",
    quotes_top = quote(geom_line(aes(x = year, y = rel_prop), color = "black")),
    geom = "point",
    grid = c("age", "origin"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "14_one-proportion-from-different-aggregation-levels_focus-years"),
    width = 12, height = 8,
    multi = uni_d
  )

  #-------------------------------------------------------------------
  # smoothing (after different levels of aggregation were brought together
  #-------------------------------------------------------------------

  # smoothing (direction: age)
  prop_agg_smooth <- select(prop_agg, district, year, age, origin, rel_prop) %>%
    group_by(district, year, origin) %>%
    arrange(age) %>%
    mutate(prop_smooth = pmax(0, pmin(100, predict(loess(rel_prop ~ age,
      span = rem_prop_span,
      degree = 1, na.action = na.aggregate
    ))))) %>%
    ungroup()

  # plot preparation
  plot_agg_smooth <- gather(prop_agg_smooth, `rel_prop`, `prop_smooth`, key = category, value = count) %>%
    mutate(cat = factor(if_else(category == "rel_prop",
      smooth_lev[1], smooth_lev[2]
    ), levels = smooth_lev)) %>%
    select(district, year, age, origin, cat, count)

  # plot: focus age

  # base years (subjectively, but last year in the plot)
  year_plot <- seq(rem_base_end, rem_base_begin, by = -3)

  # plot
  sszplot(plot_agg_smooth %>% filter(year %in% year_plot),
    aes_x = "age", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
    grid = c("year", "origin"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "15_one-proportion_smoothing_focus-age"),
    quotes = quote(scale_alpha_manual(values = c(0.3, 1))),
    width = 15, height = 8,
    multi = uni_d
  )

  # plot: focus years

  # age (subjectively selected)
  age_plot <- seq(0, 60, by = 20)

  # plot
  sszplot(plot_agg_smooth %>% filter(age %in% age_plot),
    aes_x = "year", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
    grid = c("origin", "age"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "16_one-proportion_smoothing_focus-years"),
    quotes = quote(scale_alpha_manual(values = c(0.3, 1))),
    width = 15, height = 8,
    multi = uni_d
  )

  #-------------------------------------------------------------------
  # Constrained regression
  #-------------------------------------------------------------------

  # prediction (duration: approx. 20 seconds)

  # Why no upper threshold?
  # proportions will be standardized to 100 percent anyways


  # system.time(
  prop_pred <-
    con_reg(
      data = prop_agg_smooth,
      x = "year",
      y = "prop_smooth",
      group_cols = c("district", "age", "origin"),
      window = rem_window_thres,
      base_t0 = rem_base_begin,
      szen_t0 = szen_begin,
      szen_t1 = szen_end,
      prop_trend = rem_prop_trend,
      thres_percent = rem_thres_percent,
      lower_thres = rem_lower_thres,
      upper_thres = rem_upper_thres
    )
  # )


  # limit prediction period
  prop_pred_begin <- filter(prop_pred, year >= szen_begin)

  #-------------------------------------------------------------------
  # expand to ages beyond age threshold
  #-------------------------------------------------------------------

  # age at age threshold
  prop_age_thres <- filter(prop_pred_begin, age == rem_age_max) %>%
    rename(pred_age_thres = pred_roll) %>%
    select(district, year, origin, pred_age_thres)

  # at ages beyond age threshold: adopt proportion
  prop_age <- as_tibble(expand_grid(
    district = uni_d,
    year = szen_begin:szen_end,
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


  # plot preparation
  plot_pred_smooth <- select(pred_smooth, district, year, age, origin, pred_all_age, pred_smooth) %>%
    gather(pred_smooth, `pred_all_age`, `pred_smooth`, key = category, value = count) %>%
    mutate(cat = factor(if_else(category == "pred_all_age",
      smooth_lev[1], smooth_lev[2]
    ), levels = smooth_lev)) %>%
    select(district, year, age, origin, cat, count)

  # plot: focus age

  # base years (subjectively, but last year in the plot)
  year_plot <- seq(szen_begin, szen_end, by = 7)

  # plot
  sszplot(plot_pred_smooth %>% filter(year %in% year_plot),
    aes_x = "age", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
    grid = c("origin", "year"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "17_prediction_dyao_smooth"),
    quotes = quote(scale_alpha_manual(values = c(0.3, 1))),
    width = 15, height = 8,
    multi = uni_d
  )

  #-------------------------------------------------------------------
  # plot the final predictions
  #-------------------------------------------------------------------

  # past rate (before smoothing)
  rem_dyao_not_smoothed <- mutate(rem_dyao,
    rel_prop_dyao = pmax(0, pmin(100, if_else(mis == 0, NA_real_, round(rel / mis * 100, round_prop))))
  ) %>%
    select(district, year, age, origin, rel_prop_dyao)

  # past and prediction
  rem_past_pred <- as_tibble(expand_grid(
    district = uni_d,
    year = date_start:szen_end,
    age = age_min:age_max,
    origin = uni_o
  )) %>%
    left_join(rem_dyao_not_smoothed, by = c("district", "year", "age", "origin")) %>%
    left_join(select(pred_smooth, district, year, age, origin, pred_smooth),
      by = c("district", "year", "age", "origin")
    ) %>%
    mutate(prop = if_else(year < szen_begin, rel_prop_dyao, pred_smooth))

  # colors

  # plot: focus age distribution
  sszplot(rem_past_pred,
    aes_x = "age", aes_y = "prop", aes_col = "year",
    grid = c(".", "origin"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "18_proportion_dyao_past-future"),
    width = 14, height = 7,
    multi = uni_d
  )

  # plot levels
  time_lev <- c("past", "future")

  # plot data
  plot_a_past_pred <- mutate(rem_past_pred,
    time = factor(if_else(year < szen_begin,
      time_lev[1], time_lev[2]
    ), levels = time_lev)
  )

  # plot: focus age distribution
  # test: x <- uni_d[3]
  # WHY this plot: it is recommendable to look precisely at the age plots over years
  # therefore, a plot that focuses on certain years

  # years
  year_temp <- date_start:szen_end
  year_plot <- seq(min(year_temp), max(year_temp), by = 8)

  sszplot(plot_a_past_pred %>% filter(year %in% year_plot),
    aes_x = "age", aes_y = "prop", aes_col = "year", aes_alpha = "time",
    grid = c(".", "origin"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"),
    name = paste0(rem_number, "19_proportion_dyao_past-future_focus-age"),
    quotes = quote(scale_alpha_manual(values = c(0.4, 1))),
    width = 14, height = 7,
    multi = uni_d
  )


  # plot: focus years

  # age (subjectively selected)
  age_plot_pred <- seq(0, 60, by = 20)

  sszplot(plot_a_past_pred %>% filter(age %in% age_plot_pred),
    aes_x = "year", aes_y = "prop", aes_col = "age",
    i_x = c(rem_base_begin, rem_base_end),
    grid = c(".", "origin"),
    labs_y = paste0("proportion in % (relocation on ", mig_name, "*)"), labs_col = "age",
    name = paste0(rem_number, "20_proportion_dyao_past-future_focus-years"),
    quotes = quote(scale_alpha_manual(values = c(0.4, 1))),
    width = 14, height = 6,
    multi = uni_d
  )

  #-------------------------------------------------------------------
  # export the results
  #-------------------------------------------------------------------

  # proportion: prediction
  rel_ex_data <- mutate(pred_smooth, prop_rel_dyao = round(pred_smooth, round_prop)) %>%
    select(district, year, age, origin, prop_rel_dyao) %>%
    arrange(district, year, age, origin)

  # export
  write_csv(rel_ex_data, ex_path)

  # output (to get an idea of the exported output)
  return(list(rel_ex_data))
}
