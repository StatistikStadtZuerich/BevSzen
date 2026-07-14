#' project prediction
#'
#' @param params parameter vector
#'
#' @returns list with allocation data
#'      pro_all: all project categories
#'      delay: results of the delay function 
#'      (e.g. how many percent of the projects are delayed three years)
#'      pro_delay: projects after the delay function is applied
#'      
#'
#' @export
#'
#' @examples projects_fun(params = params)
#'

projects_fun <- function(params = params){


  # start time
  t0 <- Sys.time()

  # parameter ---------------------------------------------------------------

  # paths
  pro_path <- params$pro_path  
  exp_path_scen <- params$exp_path_scen
  log_file <- params$log_file

  # time
  pro_begin <- params$pro_begin
  pro_end <- params$pro_end

  # project module parameters
  pro_lambda_begin <- params$pro_lambda_begin
  pro_lambda_end <- params$pro_lambda_end
  pro_transfo <- params$pro_transfo
  pro_max_delay <- params$pro_max_delay
  pro_not_scheduled_ip <- params$pro_not_scheduled_ip
  pro_not_scheduled_other <- params$pro_not_scheduled_other
  pro_not_submitted <- params$pro_not_submitted
  pro_not_approved <- params$pro_not_approved
  pro_not_started <- params$pro_not_started
  pro_not_completed <- params$pro_not_completed
  pro_not_onhold <- params$pro_not_onhold

  # text and lookup
  uni_d <- params$uni_d
  uni_w <- params$uni_w
  uni_i <- params$uni_i
  uni_t <- params$uni_t  
  look_reg <- params$look_reg
  look_not <- params$look_not
  look_pro <- params$look_pro  
  

  # import, data preparation ------------------------------------------------

  # projects: data
  pro_dat <- read_csv(pro_path) |> 
    rename(year = StichtagDatJahr) |>
    left_join(look_pro, by = c("StatusCd" = "code")) |>
    mutate(distnum = as.numeric(QuarCd)) |>
    left_join(look_reg, by = "distnum") |>
    mutate(owner = fact_if(EigentumCd, uni_w)) |>
    select(district, year, owner, status, WhgNeu, WhgAbbruch) |>
    pivot_longer(
      cols = c("WhgNeu", "WhgAbbruch"), names_to = "ind",
      values_to = "apartments"
    ) |>
    mutate(indicator = fact_if(ind, uni_i, "WhgNeu")) |>
    select(district, year, owner, status, indicator, apartments)

  
  # with all possible cases
  pro_all <- as_tibble(expand_grid(
    district = uni_d,
    year = min(pro_dat$year):max(pro_dat$year),
    owner = uni_w,
    status = uni_t,
    indicator = uni_i
  )) |>
    left_join(pro_dat, by = c("district", "year", "owner", "status", "indicator")) |>
    replace_na(list(apartments = 0))


  # not all projects realized -----------------------------------------------

  # not realized
  pro_not <- left_join(pro_all, look_not,
                       by = "status"
  ) |>
    mutate(realized = apartments * (100 - not_realized) / 100)


  
  # delayed -----------------------------------------------------------------

  # lambda: non-linear model
  # WHY lambda positive?
  # to allow transformation before the regression

  data_points <- tibble(
    year = c(pro_begin, pro_end),
    lambda = c(pro_lambda_begin, pro_lambda_end)^(1 / pro_transfo)
  )

  lm_ <- lm(lambda ~ year, data = data_points)

  lambda_y <- tibble(year = pro_begin:pro_end) |>
    add_predictions(lm_) |>
    mutate(lambda = pred^pro_transfo) |>
    select(year, lambda)

  # plot(lambda_y$year, lambda_y$lambda)

  # delay in years (1 to 'pro_max_delay')
  # WHY plus 1? since 'no delay' is also possible, one additional category

  delay <- as_tibble(expand_grid(
    year = pro_begin:pro_end,
    delta = 1:(pro_max_delay + 1)
  )) |>
    left_join(lambda_y, by = "year") |>
    mutate(
      y = exp(-lambda * delta),
      delay = delta - 1,
      delayText = if_else(delay == 1, paste0(delay, " year"),
                          paste0(delay, " years")
      )
    ) |>
    group_by(year) |>
    mutate(ynorm = y / sum(y) * 100) |>
    ungroup()
  

# projects and delay
pro_delay <- as_tibble(expand_grid(
  district = uni_d,
  year = pro_begin:pro_end,
  owner = uni_w,
  # status = pro_category,
  status = uni_t,
  indicator = uni_i,
  delay = as.double(0:pro_max_delay)
)) |>
  left_join(select(pro_not, c(district, year, owner, status, indicator, realized)),
            by = c("district", "year", "owner", "status", "indicator")
  ) |>
  left_join(select(delay, year, delay, ynorm),
            by = c("year", "delay")
  ) |>
  mutate(
    year_new = year + delay,
    realized_new = realized * ynorm / 100
  ) |>
  group_by(district, year_new, owner, status, indicator) |>
  summarize(
    apartments = sum(realized_new),
    .groups = "drop"
  ) |>
  rename(year = year_new)

  # control
  # sum(pro_not$realized)
  # sum(pro_delay$apartments)


  # summarize (status not needed anymore) -----------------------------------

  pro_sum <- pro_delay |>
    group_by(district, year, owner, indicator) |>
    summarize(apartments = sum(apartments),
              .groups = "drop")


  # export the results (projects: new/removed, by category) -----------------

  # export data
  pro_ex_data <- pro_sum |> 
    arrange(district, year, owner, indicator)
  
  # export
  write_csv(pro_ex_data, file.path(exp_path_scen, "projects_future.csv"))
  
  # log info
  cat_log(
    text = paste0("projects: ", capture.output(Sys.time() - t0)),
    log_file = log_file
  )
  
  # outputs -----------------------------------------------------------------
  
  out <- list()
  out$pro_all <- pro_all
  out$delay <- delay  
  out$pro_delay <- pro_delay
  return(out)  


}

