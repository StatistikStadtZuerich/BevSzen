#' fertility rate: future values
#'
#' @param params parameter vector
#'
#' @returns list with fertility rates
#'          fer_ya: fertility rate by year, age
#'          fer_yao: fertility rate by year, age, origin
#'          fer_dyao: fertility rate by district, year, age, origin
#'          fer_tail: corrected fertility rates with tail corrections 
#'          fer_fit: smoothed the corrected fertility rate  
#'          fer_pred: prediction of the fertility rate  
#'          fer_ex: future fertility rates (for export)
#'          fer_ex_past: past fertility rates (for export)
#' 
#' @export 
#'
#' @examples fertility_rate_fun(params = params)

fertility_rate_fun <- function(params = params){

# start time
t0 <- Sys.time()


# parameter ---------------------------------------------------------------

# paths
bir_od <- params$bir_od
pop_od <- params$pop_od
exp_path_scen <- params$exp_path_scen
log_file <- params$log_file

# time
date_start <- params$date_start
date_end <- params$date_end
scen_begin <- params$scen_begin
scen_end <- params$scen_end


# birth parameters
bir_age_begin <- params$bir_age_begin
bir_age_end <- params$bir_age_end
bir_base_begin <- params$bir_base_begin
bir_base_end <- params$bir_base_end
bir_thres_value <- params$bir_thres_value
bir_thres_const <- params$bir_thres_const
bir_thres_overall <- params$bir_thres_overall
bir_thres_origin <- params$bir_thres_origin
bir_fer_span <- params$bir_fer_span
bir_window_thres <- params$bir_window_thres
bir_prop_trend <- params$bir_prop_trend
bir_thres_percent <- params$bir_thres_percent
bir_lower_thres <- params$bir_lower_thres
bir_upper_thres <- params$bir_upper_thres
bir_mult_begin <- params$bir_mult_begin
bir_mult_end <- params$bir_mult_end
bir_mult_exp <- params$bir_mult_exp
bir_fer_span_pred <- params$bir_fer_span_pred


# text and lookup
uni_d <- params$uni_d
uni_o <- params$uni_o
look_dis <- params$look_dis
look_a1 <- params$look_a1
look_a2 <- params$look_a2

# options
round_rate <- params$round_rate



# import and data preparation ---------------------------------------------

# birth
# age: only births of women at 'fertile age'
# origin: here origin of mother

bir <- read_csv(bir_od) |> 
  rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) |>
  filter((age >= bir_age_begin) & (age <= bir_age_end)) |>
  left_join(look_dis, by = "QuarCd") |>
  mutate(
    origin = fact_if(HerkunftMutterCd, uni_o),
    district = factor(distr, uni_d)
  ) |>
  select(district, year, age, origin, bir) |>
  group_by(district, year, age, origin) |>
  summarize(
    bir = sum(bir),
    .groups = "drop"
  )

# population
# year: begin of year population (therefore: StichtagDatJahr + 1)
# age: only women at 'fertile age'

pop <- read_csv(pop_od) |>
  rename(age = AlterVCd, pop = AnzBestWir) |>
  filter((SexCd == 2) & (age >= bir_age_begin) & (age <= bir_age_end)) |>
  left_join(look_dis, by = "QuarCd") |>
  mutate(
    year = StichtagDatJahr + 1,
    origin = fact_if(HerkunftCd, uni_o),
    district = factor(distr, uni_d)
  ) |>
  select(district, year, age, origin, pop) |>
  group_by(district, year, age, origin) |>
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )

# fertility ---------------------------------------------------------------

# values for all possible cases

# WHY with age categories? to calculate TFR by age category

cas <- as_tibble(expand_grid(
  district = uni_d,
  year = (date_start + 1):date_end,
  age = bir_age_begin:bir_age_end,
  origin = uni_o
)) |>
  left_join(pop, by = c("district", "year", "age", "origin")) |>
  left_join(bir, by = c("district", "year", "age", "origin")) |>
  replace_na(list(pop = 0, bir = 0)) |>
  left_join(look_a1, by = "age") |>
  left_join(look_a2, by = "age")

# fertility by year, age
fer_ya <- group_by(cas, year, age) |>
  summarize(
    pop = sum(pop),
    bir = sum(bir),
    .groups = "drop"
  ) |>
  mutate(fer_ya = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) |>
  select(year, age, fer_ya)

# fertility by year, age, origin
fer_yao <- group_by(cas, year, age, origin) |>
  summarize(
    pop = sum(pop),
    bir = sum(bir),
    .groups = "drop"
  ) |>
  mutate(fer_yao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) |>
  select(year, age, origin, fer_yao)

# fertility by district, year, age, origin
# is already aggregated by district, year, age, origin
fer_dyao <- mutate(cas, fer_dyao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate)))



# fertility: replace values in tails, base period only --------------------

# cumulative sums vs. thresholds
# lowcum: cumulative sum from the lower tail of the age distribution
# upcum: cumulative sum from the upper tail of the age distribution

fer_tail <- filter(fer_dyao, year >= bir_base_begin) |>
  left_join(fer_yao, by = c("year", "age", "origin")) |>
  left_join(fer_ya, by = c("year", "age")) |>
  arrange(district, year, origin, age) |>
  group_by(district, year, origin) |>
  mutate(
    low_cum = cumsum(pop),
    up_cum = rev(cumsum(rev(pop)))
  ) |>
  ungroup() |>
  mutate(
    min_cum = pmin(low_cum, up_cum),
    fer = if_else(min_cum < bir_thres_const, bir_thres_value,
      if_else(min_cum < bir_thres_overall, fer_ya,
        if_else(min_cum < bir_thres_origin, fer_yao, fer_dyao)
      )
    )
  )



# smooth the corrected fertility rate -------------------------------------

# smoothing with loess
fer_fit <- arrange(fer_tail, district, year, origin, age) |>
  group_by(district, year, origin) |>
  mutate(fer_fit = pmax(0, predict(
    loess(fer ~ age, span = bir_fer_span, degree = 1, na.action = na.aggregate)
  ))) |>
  ungroup()



# prediction --------------------------------------------------------------

# constrained regression
# (proportion of linear model and mean, within bandwidth)

fer_pred_temp <- con_reg(
  data = fer_fit, x = "year", y = "fer_fit",
  group_cols = c("district", "age", "origin"),
  window = bir_window_thres, base_t0 = bir_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = bir_prop_trend, thres_percent = bir_thres_percent,
  lower_thres = bir_lower_thres, upper_thres = bir_upper_thres
) |>
  # with the input data (WHY? to assess the regression)
  left_join(select(fer_fit, district, year, age, origin, fer_fit),
    by = c("district", "year", "age", "origin")
  ) |>
  # output generation, one rate variable for both future and past
  left_join(select(fer_dyao, district, year, age, origin, fer_dyao),
    by = c("district", "year", "age", "origin")
  ) |>
  mutate(fer_all_temp = if_else(year <= bir_base_end, fer_dyao, pred_roll))


# fertility multiplier ----------------------------------------------------

# multiplier by year
fer_mult_dat <- tibble(year = c(scen_begin, scen_end),
                       mult = c(bir_mult_begin, bir_mult_end)) |> 
  mutate(year_new = (year - scen_begin + 1)^bir_mult_exp)

fer_mult <- tibble(year = scen_begin:scen_end) |> 
  mutate(year_new = (year - scen_begin + 1)^bir_mult_exp) |> 
  add_predictions(lm(mult ~ year_new, data = fer_mult_dat), var = "mult")

# ggplot(fer_mult) + 
#   geom_point(aes(x = year, y = mult))



# fertility rates with multiplier
fer_pred <- fer_pred_temp |> 
  left_join(fer_mult, by = "year") |> 
  mutate(fer_all = fer_all_temp * mult)


# smooth the future fertility rates ---------------------------------------

# smoothed (only the prediction years)
pred_fit <- filter(fer_pred, year >= scen_begin) |>
  arrange(district, year, origin, age) |>
  group_by(district, year, origin) |>
  mutate(pred_fit = pmax(0, predict(
    loess(fer_all ~ age, span = bir_fer_span_pred, degree = 1, na.action = na.aggregate)
  ))) |>
  ungroup()



# export fertility rates --------------------------------------------------

# prepare the export data
fer_ex <- mutate(pred_fit, fer = round(pred_fit, round_rate)) |>
  filter(year >= scen_begin) |>
  select(district, year, age, origin, fer) |>
  arrange(district, year, age, origin)

# export
write_csv(fer_ex, file.path(exp_path_scen, "birth_fertility_future.csv"))


# TFR (total fertility rate) ----------------------------------------------

# fertility rate of past and future
fer_ex_past <- rename(fer_dyao, fer = fer_dyao) |>
  select(district, year, age, origin, fer) |>
  arrange(district, year, age, origin)

# export the data of the past (for model evaluation later on)
write_csv(fer_ex, file.path(exp_path_scen, "birth_fertility_past.csv"))



# log info
cat_log(
  text = paste0("fertility rate: ", capture.output(Sys.time() - t0)),
  log_file = log_file
)


# outputs -----------------------------------------------------------------

out <- list()
out$fer_ya <- fer_ya
out$fer_yao <- fer_yao
out$fer_dyao <- fer_dyao
out$fer_tail <- fer_tail
out$fer_fit <- fer_fit
out$fer_pred <- fer_pred
out$pred_fit <- pred_fit
out$fer_ex <- fer_ex
out$fer_ex_past <- fer_ex_past

return(out)

}


