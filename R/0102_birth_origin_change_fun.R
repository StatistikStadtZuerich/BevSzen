#' Birth, origin changes (child other origin as mother), future values
#'
#' @param params parameter vector
#'
#' @returns list with future birth origin changes 
#'          cha: past origin change
#'          cha_pred: predicted origin change
#'          
#' @export
#'
#' @examples
birth_origin_change_fun <- function(params = params){
  
# start time
t0 <- Sys.time()


# parameter ---------------------------------------------------------------

# paths
bir_od <- params$bir_od
exp_path_scen <- params$exp_path_scen
log_file <- params$log_file

# time
scen_begin <- params$scen_begin
scen_end <- params$scen_end


# birth parameters
bir_age_begin <- params$bir_age_begin
bir_age_end <- params$bir_age_end
bir_cha_base_begin <- params$bir_cha_base_begin
bir_cha_base_end <- params$bir_cha_base_end
bir_cha_window_thres <- params$bir_cha_window_thres
bir_cha_prop_trend <- params$bir_cha_prop_trend
bir_cha_thres_percent <- params$bir_cha_thres_percent
bir_cha_lower_thres <- params$bir_cha_lower_thres
bir_cha_upper_thres <- params$bir_cha_upper_thres


# text and lookup
uni_d <- params$uni_d
uni_o <- params$uni_o
look_dis <- params$look_dis

# options
round_rate <- params$round_rate



# import and data preparation ---------------------------------------------

# birth
# age: only births of women at 'fertile age'
# originb: origin of baby
# origin: origin of mother

bir <- read_csv(bir_od) |>
  rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) |>
  filter((age >= bir_age_begin) & (age <= bir_age_end)) |>
  left_join(look_dis, by = "QuarCd") |>
  mutate(
    originb = fact_if(HerkunftCd, uni_o),
    origin = fact_if(HerkunftMutterCd, uni_o),
    district = factor(distr, uni_d)
  ) |>
  select(district, year, age, originb, origin, bir) |>
  group_by(district, year, age, originb, origin) |>
  summarize(
    bir = sum(bir),
    .groups = "drop"
  )

# origin change: plots ----------------------------------------------------

# WHY plots?
# find out the relevant processes/variables

# origin change: data preparation
cha <- bir |> 
  pivot_wider(names_from = "originb", values_from = "bir") |>
  replace_na(list(Swiss = 0, foreign = 0)) |>
  mutate(
    change = if_else(origin == "Swiss", foreign, Swiss),
    nochange = if_else(origin == "Swiss", Swiss, foreign),
    total = change + nochange
  )

# plots 0170, 0171, 0172

# change by district, year, origin
cha_dyo <- group_by(cha, district, year, origin) |>
  summarize(
    change = sum(change),
    total = sum(total),
    .groups = "drop"
  ) |>
  mutate(cha_dyo = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))

# plots 0173, 0174

# origin change (cha): model ----------------------------------------------

# base years
cha_base <- filter(cha_dyo, (year >= bir_cha_base_begin) & (year <= bir_cha_base_end))

# constrained regression
# (proportion of linear model and mean, within bandwidth)

cha_pred <- con_reg(
  data = cha_base, x = "year", y = "cha_dyo",
  group_cols = c("district", "origin"),
  window = bir_cha_window_thres, base_t0 = bir_cha_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = bir_cha_prop_trend, thres_percent = bir_cha_thres_percent,
  lower_thres = bir_cha_lower_thres, upper_thres = bir_cha_upper_thres
) |>
  # with the input data (WHY? to assess the regression)
  full_join(select(cha_dyo, district, year, origin, cha_dyo),
    by = c("district", "year", "origin")
  ) |>
  # output generation, one rate variable for both future and past
  mutate(cha_all = if_else(year <= bir_cha_base_end, cha_dyo, pred_roll)) |>
  arrange(district, year, origin)

# plot 0175

# export the results ------------------------------------------------------

# prepare the export data
cha_ex <- mutate(cha_pred, cha = round(cha_all, round_rate)) |>
  filter(year >= scen_begin) |>
  select(district, year, origin, cha) |>
  arrange(district, year, origin)

# export
write_csv(cha_ex, file.path(exp_path_scen, "birth_origin-change_future.csv"))

# log info
cat_log(
  text = paste0("origin change at birth: ", capture.output(Sys.time() - t0)),
  log_file = log_file
)

# outputs -----------------------------------------------------------------

out <- list()
out$cha <- cha
out$cha_dyo <- cha_dyo
out$cha_pred <- cha_pred
out$cha_ex <- cha_ex

return(out)

}




