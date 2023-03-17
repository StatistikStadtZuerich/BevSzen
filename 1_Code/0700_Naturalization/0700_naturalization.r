# header ------------------------------------------------------------------
# naturalization


# paths, general ----------------------------------------------------------
source(paste0(here::here(),"/1_code/0000_general/general_utils.R"))
util_gf()

# start time
t0 <- Sys.time()



# import and data preparation ---------------------------------------------

# naturalization and denaturalization
nat_denat <- read_csv(nat_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, nat = AnzEinbWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    origin_prev = factor(if_else(HerkunftBisherCd == 1, uni_o[1], uni_o[2]), uni_o),
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, origin_prev, origin, nat)

# naturalization
nat <- filter(nat_denat, (origin_prev == uni_o[2]) & (origin == uni_o[1])) %>%
  group_by(district, year, age, sex) %>%
  summarize(
    nat = sum(nat),
    .groups = "drop"
  )

# population (foreign population only)
# year: begin of year population

pop <- read_csv(pop_od) %>%
  rename(age = AlterVCd, pop = AnzBestWir) %>%
  filter(HerkunftCd == 2) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    year = StichtagDatJahr + 1,
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, pop) %>%
  group_by(district, year, age, sex) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )



# # naturalization vs. denaturalization -------------------------------------
# 
# # process levels
# processes <- c("nat", "denat")
# 
# # processes
# proc <- filter(nat_denat, origin_prev != origin) %>%
#   mutate(proc = factor(if_else(origin_prev == uni_o[2],
#     processes[1], processes[2]
#   ), levels = processes)) %>%
#   group_by(year, proc) %>%
#   summarize(
#     count = sum(nat),
#     .groups = "drop"
#   ) %>%
#   spread(key = proc, value = count) %>%
#   replace_na(list(nat = 0, denat = 0)) %>%
#   mutate(
#     total = nat + denat,
#     prop_nat = if_else(total == 0, NA_real_, round(nat / total * 100, round_prop))
#   )
# 
# 
# # plot
# sszplot(proc,
#   aes_x = "year", aes_y = "prop_nat",
#   labs_y = "proportion in % (naturalization on citizenship change)",
#   name = "0700_naturalization-on-citizenship-change",
#   width = 8, height = 6
# )



# naturalization and population -------------------------------------------

# nat and pop
nat_pop <- as_tibble(expand_grid(
  district = uni_d,
  year = (date_start + 1):date_end,
  age = age_min:age_max,
  sex = uni_s
)) %>%
  left_join(pop, by = c("district", "year", "age", "sex")) %>%
  left_join(nat, by = c("district", "year", "age", "sex")) %>%
  replace_na(list(pop = 0, nat = 0))



# # naturalization rate: which variables? -----------------------------------
# 
# # rate by as
# nat_as <- group_by(nat_pop, age, sex) %>%
#   summarize(
#     pop = sum(pop),
#     nat = sum(nat),
#     .groups = "drop"
#   ) %>%
#   mutate(nat_as = if_else(pop == 0, NA_real_, round(nat / pop * 100, round_rate)))
# 
# sszplot(nat_as,
#   aes_x = "age", aes_y = "nat_as", aes_col = "sex",
#   labs_y = "naturalization rate (in % per year)",
#   scale_y = c(0, NA),
#   name = "0701_rate_as",
#   width = 8, height = 6
# )
# 
# # rate by ys
# nat_ys <- group_by(nat_pop, year, sex) %>%
#   summarize(
#     pop = sum(pop),
#     nat = sum(nat),
#     .groups = "drop"
#   ) %>%
#   mutate(nat_ys = if_else(pop == 0, NA_real_, round(nat / pop * 100, round_rate)))
# 
# sszplot(nat_ys,
#   aes_x = "year", aes_y = "nat_ys", aes_col = "sex",
#   labs_y = "naturalization rate (in % per year)",
#   scale_y = c(0, NA),
#   name = "0702_rate_ys",
#   width = 8, height = 6
# )
# 
# # rate by yas
# nat_yas <- group_by(nat_pop, year, age, sex) %>%
#   summarize(
#     pop = sum(pop),
#     nat = sum(nat),
#     .groups = "drop"
#   ) %>%
#   mutate(nat_yas = if_else(pop == 0, NA_real_, round(nat / pop * 100, round_rate)))
# 
# 
# # years (subjectively, but last year in the plot)
# year_plot <- seq(date_end, (date_start + 1), by = -4)
# 
# sszplot(filter(nat_yas, year %in% year_plot),
#   aes_x = "age", aes_y = "nat_yas", aes_col = "sex",
#   labs_y = "naturalization rate (in % per year)",
#   wrap = "year", ncol = 3,
#   name = "0703_rate_yas_focus-age",
#   width = 10, height = 10
# )
# 
# 
# # age (subjectively selected)
# age_plot <- seq(0, 70, by = 10)
# 
# sszplot(filter(nat_yas, age %in% age_plot),
#   aes_x = "year", aes_y = "nat_yas", aes_col = "sex",
#   labs_y = "naturalization rate (in % per year)",
#   wrap = "age", ncol = 2,
#   name = "0704_rate_yas_focus-years",
#   width = 8, height = 10
# )
# 
# # rate by das
# nat_das <- group_by(nat_pop, district, age, sex) %>%
#   summarize(
#     pop = sum(pop),
#     nat = sum(nat),
#     .groups = "drop"
#   ) %>%
#   mutate(nat_das = if_else(pop == 0, NA_real_, round(nat / pop * 100, round_rate)))
# 
# sszplot(nat_das,
#   aes_x = "age", aes_y = "nat_das", aes_col = "sex",
#   labs_y = "naturalization rate (in % per year)",
#   scale_y = c(0, 25),
#   wrap = "district", ncol = 4,
#   name = "0705_rate_das",
#   width = 12, height = 14
# )
# 
# # rate by dys
# nat_dys <- group_by(nat_pop, district, year, sex) %>%
#   summarize(
#     pop = sum(pop),
#     nat = sum(nat),
#     .groups = "drop"
#   ) %>%
#   mutate(nat_dys = if_else(pop == 0, NA_real_, round(nat / pop * 100, round_rate)))
# 
# sszplot(nat_dys,
#   aes_x = "year", aes_y = "nat_dys", aes_col = "sex",
#   labs_y = "naturalization rate (in % per year)",
#   wrap = "district", ncol = 4,
#   name = "0706_rate_dys",
#   width = 12, height = 14
# )



# base years --------------------------------------------------------------

# base years, aggregate (mean over base years)
nat_pop_das <- filter(
  nat_pop,
  (year >= nat_base_begin) & (year <= nat_base_end)
) %>%
  group_by(district, age, sex) %>%
  summarize(
    nat = mean(nat),
    pop = mean(pop),
    .groups = "drop"
  )



# das: smoothing (nat and pop) --------------------------------------------

# smoothing (direction: age, result should not be below zero)
nat_pop_das_smooth <- group_by(nat_pop_das, district, sex) %>%
  arrange(age) %>%
  mutate(
    pop_a = pmax(0, predict(loess(pop ~ age, span = 0.1, degree = 1, na.action = na.aggregate))),
    nat_a = pmax(0, predict(loess(nat ~ age, span = nat_nat_span_das, degree = 1, na.action = na.aggregate)))
  ) %>%
  ungroup()

# # plot preparation
# process_lev <- c("naturalization", "population")
# smooth_lev <- c("initial", "smoothed")
# 
# temp_initial <- gather(nat_pop_das_smooth, `nat`, `pop`, key = category, value = count) %>%
#   mutate(
#     cat = factor(if_else(category == "nat",
#       process_lev[1], process_lev[2]
#     ), levels = process_lev),
#     smooth = factor(smooth_lev[1], levels = smooth_lev)
#   ) %>%
#   select(district, age, sex, cat, smooth, count)
# 
# temp_smooth <- gather(nat_pop_das_smooth, `nat_a`, `pop_a`, key = category, value = count) %>%
#   mutate(
#     cat = factor(if_else(category == "nat_a",
#       process_lev[1], process_lev[2]
#     ), levels = process_lev),
#     smooth = factor(smooth_lev[2], levels = smooth_lev)
#   ) %>%
#   select(district, age, sex, cat, smooth, count)
# 
# smooth_plot <- bind_rows(temp_initial, temp_smooth)
# 
# 
# # plot
# sszplot(smooth_plot,
#   aes_x = "age", aes_y = "count", aes_col = "cat", aes_ltyp = "smooth", aes_alpha = "smooth",
#   labs_y = "quantity per year (naturalization), quantity (population)",
#   grid = c("cat", "sex"), gridscale = "free_y",
#   name = "0707_processes_das_smooth",
#   width = 15, height = 8,
#   multi = uni_d,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )



# das: naturalization rate ------------------------------------------------

# rate, threshold, smooth
nat_rate_das <- mutate(nat_pop_das_smooth,
  rate_das = if_else(pop_a == 0, NA_real_, round(nat_a / pop_a * 100, round_rate)),
  rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_das)
) %>%
  group_by(district, sex) %>%
  arrange(age) %>%
  mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
    span = nat_rate_span_das, degree = 1, na.action = na.aggregate
  )))) %>%
  ungroup()

# # plot preparation
# rate_plot <- gather(nat_rate_das, `rate_thres`, `rate_smooth`, key = category, value = count) %>%
#   mutate(cat = factor(if_else(category == "rate_thres",
#     smooth_lev[1], smooth_lev[2]
#   ), levels = smooth_lev)) %>%
#   select(district, age, sex, cat, count)
# 
# # plot
# sszplot(rate_plot,
#   aes_x = "age", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
#   labs_y = "naturalization rate (in % per year)",
#   grid = c(".", "sex"),
#   name = "0708_rate_das",
#   width = 12, height = 5,
#   multi = uni_d,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )



# ya: smooth processes (preparation for trend by ya) ----------------------

# WHY trend for ya?
# shown in plots by a -> age is relevant
# since naturalization is conditional to duration of residence
# moreover different regulations for families, children
# therefore, age is an essential variable
# district and sex not that different (look at plots)
# same regulations in entire city, and not differentiate by sex


# smooth processes (base years only)
nat_ya <-
  filter(nat_pop, (year >= nat_base_begin) &
    (year <= nat_base_end)) %>%
  group_by(year, age) %>%
  summarize(
    pop = sum(pop),
    nat = sum(nat),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  arrange(age) %>%
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
  ) %>%
  ungroup()


# # plot preparation
# temp_initial_ya <- gather(nat_ya, `nat`, `pop`, key = category, value = count) %>%
#   mutate(
#     cat = factor(if_else(category == "nat",
#       process_lev[1], process_lev[2]
#     ), levels = process_lev),
#     smooth = factor(smooth_lev[1], levels = smooth_lev)
#   ) %>%
#   select(year, age, cat, smooth, count)
# 
# temp_smooth_ya <- gather(nat_ya, `nat_a`, `pop_a`, key = category, value = count) %>%
#   mutate(
#     cat = factor(if_else(category == "nat_a",
#       process_lev[1], process_lev[2]
#     ), levels = process_lev),
#     smooth = factor(smooth_lev[2], levels = smooth_lev)
#   ) %>%
#   select(year, age, cat, smooth, count)
# 
# smooth_plot_ya <- bind_rows(temp_initial_ya, temp_smooth_ya)
# 
# 
# # plots
# 
# # years (subjectively, but last year in the plot)
# year_plot <- seq(nat_base_end, nat_base_begin, by = -4)
# 
# sszplot(filter(smooth_plot_ya, year %in% year_plot),
#   aes_x = "age", aes_y = "count", aes_col = "cat", aes_ltyp = "smooth", aes_alpha = "smooth",
#   labs_y = "quantity per year (naturalization), quantity (population)",
#   grid = c("cat", "year"), gridscale = "free_y",
#   name = "0709_processes_ya_smooth_focus-age",
#   width = 15, height = 8,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )
# 
# # age (subjectively selected)
# age_plot <- seq(0, 100, by = 20)
# 
# sszplot(filter(smooth_plot_ya, age %in% age_plot),
#   aes_x = "year", aes_y = "count", aes_col = "cat", aes_ltyp = "smooth", aes_alpha = "smooth",
#   labs_y = "quantity per year (naturalization), quantity (population)",
#   grid = c("cat", "age"), gridscale = "free_y",
#   name = "0710_processes_ya_smooth_focus-years",
#   width = 15, height = 8,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )



# ya: rate ----------------------------------------------------------------

# rate
nat_rate_ya <- mutate(nat_ya,
  rate_ya = if_else(pop_a == 0, NA_real_, round(nat_a / pop_a * 100, round_rate)),
  rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_ya)
) %>%
  group_by(year) %>%
  arrange(age) %>%
  mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
    span = nat_rate_span_ya, degree = 1, na.action = na.aggregate
  )))) %>%
  ungroup()

# # plot preparation
# rate_ya_plot <- gather(nat_rate_ya, `rate_thres`, `rate_smooth`, key = category, value = count) %>%
#   mutate(cat = factor(if_else(category == "rate_thres",
#     smooth_lev[1], smooth_lev[2]
#   ), levels = smooth_lev)) %>%
#   select(year, age, cat, count)
# 
# # plots
# 
# # years (subjectively, but last year in the plot)
# year_plot <- seq(nat_base_end, nat_base_begin, by = -2)
# 
# sszplot(filter(rate_ya_plot, year %in% year_plot),
#   aes_x = "age", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
#   labs_y = "naturalization rate (in % per year)",
#   wrap = "year", ncol = 3,
#   name = "0711_rate_ya_focus-age",
#   width = 15, height = 8,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )
# 
# # age (subjectively selected)
# age_plot <- seq(0, 100, by = 20)
# 
# sszplot(filter(rate_ya_plot, age %in% age_plot),
#   aes_x = "year", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
#   labs_y = "naturalization rate (in % per year)",
#   grid = c(".", "age"),
#   name = "0712_rate_ya_focus-years",
#   width = 15, height = 6,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )




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
)) %>%
  left_join(select(nat_rate_ya, year, age, rate_smooth),
    by = c("year", "age")
  ) %>%
  left_join(select(rate_ya_pred, year, age, pred_roll),
    by = c("year", "age")
  ) %>%
  mutate(rate_ya = if_else(year < scen_begin, rate_smooth, pred_roll))


# # plots
# 
# # years (subjectively, but last year in the plot)
# year_plot <- seq(nat_base_begin, scen_end, by = 10)
# 
# sszplot(rate_ya_past_pred,
#   aes_x = "age", aes_y = "rate_ya", aes_col = "year",
#   labs_y = "naturalization rate (in % per year)",
#   name = "0713_rate_ya_past-future_focus-age",
#   width = 10, height = 7
# )
# 
# # age (subjectively selected)
# age_plot <- seq(0, 100, by = 10)
# 
# sszplot(filter(rate_ya_past_pred, age %in% age_plot),
#   aes_x = "year", aes_y = "rate_ya",
#   labs_y = "naturalization rate (in % per year)",
#   wrap = "age", ncol = 5,
#   name = "0714_rate_ya_past-future_focus-years",
#   width = 10, height = 7
# )




# smooth processes (preparation for the trend factor calculation) ---------

# smooth processes (base years only)
nat_a <- filter(nat_pop, (year >= nat_base_begin) & (year <= nat_base_end)) %>%
  group_by(age) %>%
  summarize(
    pop = mean(pop),
    nat = mean(nat),
    .groups = "drop"
  ) %>%
  arrange(age) %>%
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
    )))
  )


# # plot preparation
# temp_initial_a <- gather(nat_a, `nat`, `pop`, key = category, value = count) %>%
#   mutate(
#     cat = factor(if_else(category == "nat",
#       process_lev[1], process_lev[2]
#     ), levels = process_lev),
#     smooth = factor(smooth_lev[1], levels = smooth_lev)
#   ) %>%
#   select(age, cat, smooth, count)
# 
# temp_smooth_a <- gather(nat_a, `nat_a`, `pop_a`, key = category, value = count) %>%
#   mutate(
#     cat = factor(if_else(category == "nat_a",
#       process_lev[1], process_lev[2]
#     ), levels = process_lev),
#     smooth = factor(smooth_lev[2], levels = smooth_lev)
#   ) %>%
#   select(age, cat, smooth, count)
# 
# smooth_plot_a <- bind_rows(temp_initial_a, temp_smooth_a)
# 
# 
# # plots
# sszplot(smooth_plot_a,
#   aes_x = "age", aes_y = "count", aes_col = "cat", aes_ltyp = "smooth", aes_alpha = "smooth",
#   labs_y = "quantity per year (naturalization), quantity (population)",
#   grid = c("cat", "."), gridscale = "free_y",
#   name = "0715_processes_a_smooth",
#   width = 10, height = 8,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )



# rate --------------------------------------------------------------------

# rate
nat_rate_a <- mutate(nat_a,
  rate_a = if_else(pop_a == 0, NA_real_, round(nat_a / pop_a * 100, round_rate)),
  rate_thres = if_else(pop_a < nat_pop_thres, nat_pop_value, rate_a)
) %>%
  arrange(age) %>%
  mutate(rate_smooth = pmax(0, predict(loess(rate_thres ~ age,
    span = nat_rate_span_a, degree = 1, na.action = na.aggregate
  ))))

# # plot preparation
# rate_a_plot <- gather(nat_rate_a, `rate_thres`, `rate_smooth`, key = category, value = count) %>%
#   mutate(cat = factor(if_else(category == "rate_thres",
#     smooth_lev[1], smooth_lev[2]
#   ), levels = smooth_lev)) %>%
#   select(age, cat, count)
# 
# # plots
# sszplot(rate_a_plot,
#   aes_x = "age", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
#   labs_y = "naturalization rate (in % per year)",
#   name = "0716_rate_a",
#   width = 12, height = 7,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )



# trend factor ('rate ya' to 'rate a') ------------------------------------

# preparation
prep_rate_ya <- filter(rate_ya_past_pred, year >= scen_begin) %>%
  select(year, age, rate_ya)

prep_rate_a <- select(nat_rate_a, age, rate_smooth) %>%
  rename(rate_a = rate_smooth)


# trend factor (has to be at least 0, otherwise 'denaturalization')
tf_ya <- left_join(prep_rate_ya, prep_rate_a, by = "age") %>%
  mutate(tf_ya = pmax(0, if_else(rate_a <= nat_factor_thres, nat_factor_value, rate_ya / rate_a)))

# sszplot(tf_ya,
#   aes_x = "age", aes_y = "tf_ya", aes_col = "year",
#   i_y = 1,
#   labs_y = "trend factor",
#   scale_y = c(0, 2),
#   name = "0717_trend-factor_focus-age",
#   width = 10, height = 8
# )
# 
# # age (subjectively selected)
# age_plot <- seq(0, 100, by = 10)
# 
# sszplot(filter(tf_ya, age %in% age_plot),
#   aes_x = "year", aes_y = "tf_ya",
#   i_y = 1,
#   labs_y = "trend factor",
#   wrap = "age", ncol = 4,
#   name = "0718_trend-factor_focus-years",
#   width = 10, height = 8
# )


# future rate (dyas) ------------------------------------------------------

# future rate
rate_dyas_future <- as_tibble(expand_grid(
  district = uni_d,
  year = scen_begin:scen_end,
  age = age_min:age_max,
  sex = uni_s
)) %>%
  left_join(select(nat_rate_das, district, age, sex, rate_smooth),
    by = c("district", "age", "sex")
  ) %>%
  rename(rate_das = rate_smooth) %>%
  left_join(select(tf_ya, year, age, tf_ya), by = c("year", "age")) %>%
  mutate(rate_dyas = round(rate_das * tf_ya, round_rate)) %>%
  group_by(district, year, sex) %>%
  arrange(age) %>%
  mutate(rate_smoothed = pmax(0, predict(loess(rate_dyas ~ age,
    span = nat_rate_span_dyas, degree = 1, na.action = na.aggregate
  )))) %>%
  ungroup()

# # plot preparation
# plot_pred_smooth <- select(rate_dyas_future, district, year, age, sex, rate_dyas, rate_smoothed) %>%
#   gather(`rate_dyas`, `rate_smoothed`, key = category, value = count) %>%
#   mutate(cat = factor(if_else(category == "rate_dyas",
#     smooth_lev[1], smooth_lev[2]
#   ), levels = smooth_lev)) %>%
#   select(district, year, age, sex, cat, count)
# 
# # plot
# 
# # base years (subjectively)
# year_plot <- seq(scen_begin, scen_end, by = 7)
# 
# # plot
# sszplot(filter(plot_pred_smooth, year %in% year_plot),
#   aes_x = "age", aes_y = "count", aes_ltyp = "cat", aes_alpha = "cat",
#   labs_y = "naturalization rate (in % per year)",
#   grid = c("sex", "year"),
#   name = "0719_prediction_dyao_smooth",
#   width = 15, height = 8,
#   multi = uni_d,
#   quotes = quote(scale_alpha_manual(values = c(0.3, 1)))
# )



# # past and rate (dyas) ----------------------------------------------------
# 
# # past
# rate_dyas_past <- group_by(nat_pop, district, year, age, sex) %>%
#   summarize(
#     pop = sum(pop),
#     nat = sum(nat),
#     .groups = "drop"
#   ) %>%
#   mutate(rate_past = pmin(100, if_else(pop == 0, NA_real_, round(nat / pop * 100, round_rate))))
# 
# 
# # past and future
# rate_dyas_past_future <- as_tibble(expand_grid(
#   district = uni_d,
#   year = (date_start + 1):scen_end,
#   age = age_min:age_max,
#   sex = uni_s
# )) %>%
#   left_join(select(rate_dyas_past, district, year, age, sex, rate_past),
#     by = c("district", "year", "age", "sex")
#   ) %>%
#   left_join(select(rate_dyas_future, district, year, age, sex, rate_smoothed),
#     by = c("district", "year", "age", "sex")
#   ) %>%
#   replace_na(list(rate_past = 0, rate_smoothed = 0)) %>%
#   mutate(rate_dyas = if_else(year < scen_begin, rate_past, rate_smoothed))
# 
# # plot: focus age distribution
# sszplot(rate_dyas_past_future,
#   aes_x = "age", aes_y = "rate_dyas", aes_col = "year",
#   labs_y = "naturalization rate (in % per year)",
#   grid = c(".", "sex"),
#   name = "0720_rate_dyao_past-future",
#   width = 14, height = 7,
#   multi = uni_d
# )
# 
# # plot levels
# time_lev <- c("past", "future")
# 
# # plot data
# plot_a_past_pred <- mutate(rate_dyas_past_future,
#   time = factor(if_else(year < scen_begin,
#     time_lev[1], time_lev[2]
#   ), levels = time_lev)
# )
# 
# # plot: focus age distribution
# # WHY this plot: it is recommendable to look precisely at the age plots over years
# # therefore, a plot that focuses on certain years
# 
# # years
# year_plot <- seq(date_start + 1, scen_end, by = 7)
# 
# sszplot(filter(plot_a_past_pred, year %in% year_plot),
#   aes_x = "age", aes_y = "rate_dyas", aes_col = "year",
#   labs_y = "naturalization rate (in % per year)", labs_col = "year",
#   grid = c(".", "sex"),
#   name = "0721_rate_dyao_past-future_focus-age",
#   width = 14, height = 7,
#   multi = uni_d
# )
# 
# # plot: focus years
# 
# # age (subjectively selected)
# age_plot_pred <- seq(0, 60, by = 20)
# 
# sszplot(filter(plot_a_past_pred, age %in% age_plot_pred),
#   aes_x = "year", aes_y = "rate_dyas", aes_col = "age",
#   labs_y = "naturalization rate (in % per year)", labs_col = "age",
#   grid = c(".", "sex"),
#   name = "0722_rate_dyao_past-future_focus-years",
#   width = 14, height = 6,
#   multi = uni_d
# )


# export the results ------------------------------------------------------

# rate: prediction
nat_ex_data <- mutate(rate_dyas_future, rate_nat_dyas = round(rate_smoothed, round_rate)) %>%
  select(district, year, age, sex, rate_nat_dyas) %>%
  arrange(district, year, age, sex)

# export
write_csv(nat_ex_data, paste0(exp_path, "/naturalization_future.csv"))

# log info
cat_log(paste0(
  "naturalization rate: ",
  capture.output(Sys.time() - t0)
))
