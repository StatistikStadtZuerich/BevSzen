#' living space (m2 per person) prediction
#'
#' @param params parameter vector
#'
#' @returns list with living space data
#'      spa_dat: living area and people per year, district, ownership (past data)
#'      spa_yw: living space by year, owner (past data)
#'      spa_dyw: living space by year, district, ownership (past data)
#'      spa_yw_past_pred: living space by year, ownership (past and prediction)
#'      spa_dyw_past_pred: living space by district, year, ownership (past and prediction)
#'      spa_yw_past_pred_52: living space in district 52 (Escher Wyss) by year, ownership (past and prediction)
#'      spa_comb: living space by district, year, ownership (past and prediction)
#'
#' @export
#'
#' @examples living_space_fun(params = params)
#'


living_space_fun <- function(params = params){

  # start time
  t0 <- Sys.time()

  # parameter ---------------------------------------------------------------

  # paths
  spa_od <- params$spa_od
  exp_path_scen <- params$exp_path_scen
  log_file <- params$log_file

  # time
  scen_begin <- params$scen_begin
  scen_end <- params$scen_end
  spa_base_begin <- params$spa_base_begin
  spa_base_end <- params$spa_base_end
  spa_base_begin_52p <- params$spa_base_begin_52p

  # living space parameters
  spa_apart <- params$spa_apart
  spa_prop_trend <- params$spa_prop_trend
  spa_thres_percent <- params$spa_thres_percent
  spa_window_thres <- params$spa_window_thres
  spa_prop_trend_52p <- params$spa_prop_trend_52p

  # text and lookup
  uni_d <- params$uni_d
  uni_w <- params$uni_w
  look_dis <- params$look_dis

  # options
  round_area <- params$round_area


  # import, data preparation ------------------------------------------------

  # living space: data
  spa_dat <- read_csv(spa_od) |>
    rename(year = StichtagDatJahr, area = Wohnflaeche,
           apartments = AnzWhgStat, people = AnzBestWir) |>
    left_join(params$look_dis, by = "QuarCd") |>
    mutate(
      owner = fact_if(EigentuemerSSZPubl3Cd_noDM, uni_w),
      district = factor(distr, uni_d)
    ) |>
    group_by(year, district, owner) |>
    summarize(
      area = sum_NA(area),
      apartments = sum_NA(apartments),
      people = sum_NA(people),
      .groups = "drop") |>
    select(year, district, owner, area, apartments, people)


  # living space by year and owner ------------------------------------------

  # livings space (yw)
  spa_yw <- spa_dat |>
    group_by(year, owner) |>
    summarize(
      area = sum_NA(area),
      people = sum_NA(people),
      .groups = "drop") |>
    mutate(spa_yw = round(area / people, round_area))


  # living space (area, apartments, people, by dyw) -------------------------

  # livings space (dyw)
  spa_dyw <- spa_dat |>
    group_by(district, year, owner) |>
    summarize(
      area = sum_NA(area),
      area_ha = area / 10000,
      apartments = sum_NA(apartments),
      people = sum_NA(people),
      .groups = "drop"
    ) |>
    mutate(spa_dyw = round(area / people, round_area))


  # prediction: entire city, by owner ---------------------------------------

  # base years
  spa_yw_base <- spa_yw |>
    filter((year >= spa_base_begin) & (year <= spa_base_end)) |>
    select(year, owner, spa_yw)

  # prediction (no living space below zero)
  spa_yw_pred <- con_reg(
    data = spa_yw_base, x = "year", y = "spa_yw",
    group_cols = "owner",
    window = spa_window_thres, base_t0 = spa_base_begin,
    scen_t0 = scen_begin, scen_t1 = scen_end,
    prop_trend = spa_prop_trend, thres_percent = spa_thres_percent,
    lower_thres = 0
  )


  # past and prediction
  spa_yw_past_pred <- as_tibble(expand_grid(
    year = (min(spa_yw$year)):scen_end,
    owner = uni_w)) |>
    left_join(select(spa_yw, year, owner, spa_yw), by = c("year", "owner")) |>
    left_join(select(spa_yw_pred, year, owner, pred_roll),
              by = c("year", "owner")
    ) |>
    mutate(spa_yw_all = if_else(year < scen_begin, spa_yw, pred_roll))


  # prediction: by district and owner ---------------------------------------

  # base years
  spa_dyw_base <- spa_dyw |>
    filter((year >= spa_base_begin) & (year <= spa_base_end)) |>
    select(district, year, owner, spa_dyw)

  # prediction (no living space below zero)
  spa_dyw_pred <- con_reg(
    data = spa_dyw_base, x = "year", y = "spa_dyw",
    group_cols = c("district", "owner"),
    window = spa_window_thres, base_t0 = spa_base_begin,
    scen_t0 = scen_begin, scen_t1 = scen_end,
    prop_trend = spa_prop_trend, thres_percent = spa_thres_percent,
    lower_thres = 0
  )


  # past and prediction
  spa_dyw_past_pred <- as_tibble(expand_grid(
    district = uni_d,
    year = (min(spa_dyw$year)):scen_end,
    owner = uni_w)) |>
    left_join(spa_dyw, by = c("district", "year", "owner")) |>
    left_join(select(spa_dyw_pred, district, year, owner, pred_roll),
              by = c("district", "year", "owner")
    ) |>
    mutate(spa_dyw_all = if_else(year < scen_begin, spa_dyw, pred_roll))


  # prediction: Escher Wyss, by owner ---------------------------------------

  # WHY a different approach for the Escher Wyss district, private housing?
  # the living space over time differs substantially from the other districts
  # (increase until 2013, then decrease)
  # reason: few apartments in 2008 then very intense construction activity
  # numbers: less than 1500 apartments in 2008
  # more than 3100 apartments in 2017
  # after construction some apartments were finished, yet no one lived there yet
  # for a certain moment the empty apartments increase the living space (based on buildings)
  # due to the high ratio of new buildings (compared to existing buildings) the living space was increased
  # this will not happen again in future (due to the higher amount of buildings)

  # WHY with owner?
  # con_reg function needs groups (so far)
  #
  # Escher Wyss (district number: 52)
  spa_dyw_52 <- spa_dyw |>
    filter(district == "Escher Wyss") |>
    select(year, owner, spa_dyw)

  # base years
  spa_dyw_base_52 <- spa_dyw_52 |>
    filter((year >= spa_base_begin_52p) & (year <= spa_base_end))

  # prediction (no living space below zero)
  spa_dyw_pred_52 <- con_reg(
    data = spa_dyw_base_52, x = "year", y = "spa_dyw",
    group_cols = "owner",
    window = spa_window_thres, base_t0 = spa_base_begin_52p,
    scen_t0 = scen_begin, scen_t1 = scen_end,
    prop_trend = spa_prop_trend_52p, thres_percent = spa_thres_percent,
    lower_thres = 0
  )


  # past and prediction
  spa_yw_past_pred_52 <- as_tibble(expand_grid(
    year = (min(spa_yw$year)):scen_end,
    owner = uni_w)) |>
    left_join(select(spa_dyw_52, year, owner, spa_dyw), by = c("year", "owner")) |>
    left_join(select(spa_dyw_pred_52, year, owner, pred_roll),
              by = c("year", "owner")
    ) |>
    mutate(
      spa_dyw_all = if_else(year < scen_begin, spa_dyw, pred_roll),
      district = uni_d[uni_d == "Escher Wyss"]
    )

  # combine the different data sets -----------------------------------------

  # district, year, owner
  spa_prep_dyw <- spa_dyw_past_pred |>
    select(district, year, owner, spa_dyw_all) |>
    rename(spa_dyw = spa_dyw_all)

  # year, owner
  spa_prep_yw <- spa_yw_past_pred |>
    select(year, owner, spa_yw_all) |>
    rename(spa_yw = spa_yw_all)

  # Escher Wyss (only future: values of the past already contained in 'spa_prep_dyo')
  spa_prep_52p <- spa_yw_past_pred_52 |>
    select(district, year, owner, spa_dyw_all) |>
    filter((owner == uni_w[2]) & (year >= scen_begin)) |>
    rename(spa_52p = spa_dyw_all)

  # apartment threshold (mean over base years)

  # WHY with expand_grid?
  # correct calculation if no apartments (by owner) in a certain district and year

  spa_apart_thres <- as_tibble(expand_grid(
    district = uni_d,
    year = spa_base_begin:spa_base_end,
    owner = uni_w)) |>
    left_join(select(spa_dyw, district, year, owner, apartments),
              by = c("district", "year", "owner")
    ) |>
    replace_na(list(apartments = 0)) |>
    group_by(district, owner) |>
    summarize(apart_thres = mean(apartments), .groups = "drop")


  # combine the data sets
  spa_comb <- spa_prep_dyw |>
    left_join(spa_prep_yw, by = c("year", "owner")) |>
    left_join(spa_prep_52p, by = c("district", "year", "owner")) |>
    left_join(spa_apart_thres, by = c("district", "owner")) |>
    mutate(spa = if_else(year < scen_begin, spa_dyw,
                         if_else((district == "Escher Wyss") & (owner == uni_w[2]), spa_52p,
                                 if_else(apart_thres < spa_apart, spa_yw, spa_dyw)
                         )
    ))


  # export the results ------------------------------------------------------

  # export data
  spa_ex_data <- spa_comb |>
    mutate(spa_dyw = round(spa, round_area)) |>
    select(district, year, owner, spa_dyw) |>
    filter(year >= scen_begin) |>
    arrange(district, year, owner)

  # export
  write_csv(spa_ex_data, file.path(exp_path_scen, "living-space_future.csv"))

  # log info
  cat_log(
    text = paste0("living space: ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )


  # outputs -----------------------------------------------------------------

  out <- list()
  out$spa_dat <- spa_dat
  out$spa_yw <- spa_yw
  out$spa_dyw <- spa_dyw
  out$spa_yw_past_pred <- spa_yw_past_pred
  out$spa_dyw_past_pred <- spa_dyw_past_pred
  out$spa_yw_past_pred_52 <- spa_yw_past_pred_52
  out$spa_comb <- spa_comb
  return(out)

}



