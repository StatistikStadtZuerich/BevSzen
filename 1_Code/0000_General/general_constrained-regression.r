#' constrained regression
#' 
#' proportion of linear model and mean within bandwidth
#'
#' @param data 
#' @param ... 
#'
#' @return tibble with prediction data
#' @export
#'
#' @examples
con_reg <- function(data, ...) {

  # variables
  vars <- list(...)
  x <- vars$x
  y <- vars$y
  group_cols <- vars$group_cols
  window <- vars$window
  base_t0 <- vars$base_t0
  scen_t0 <- vars$scen_t0
  scen_t1 <- vars$scen_t1
  prop_trend <- vars$prop_trend
  thres_percent <- vars$thres_percent
  lower_thres <- vars$lower_thres
  upper_thres <- vars$upper_thres

  # mean
  pred_mean <- group_by(data, !!!syms(group_cols)) %>%
    summarize(pred_mean = mean(!!sym(y))) %>%
    ungroup()

  # linear models
  pred_mod <- group_by(data, !!!syms(group_cols)) %>%
    do(model = lm(!!sym(y) ~ !!sym(x), data = .)) %>%
    ungroup()

  # extend time span (WHY? we need enough values for the moving average window)
  half_window <- ceiling(window / 2)
  t0 <- min(base_t0, scen_t0 - half_window)
  t1 <- scen_t1 + half_window

  # prediction
  predi <- expand(data, !!sym(x) := t0:t1, !!!syms(group_cols)) %>%
    # with models
    left_join(pred_mod, by = group_cols) %>%
    # prediction: linear model
    group_by(!!!syms(group_cols)) %>%
    do(modelr::add_predictions(., first(.$model))) %>%
    mutate(model = NULL) %>%
    ungroup() %>%
    # prediction: mean
    left_join(pred_mean, by = group_cols) %>%
    # proportion of linear trend vs. mean
    mutate(pred_prop = pred_mean + (pred - pred_mean) * prop_trend / 100) %>%
    # bandwidth: lower limit
    # if provided: also compare with lower threshold (e.g. no negative rates, or at least 0 %)
    {
      if (invalid(lower_thres)) {
        mutate(., lower_limit = pred_mean * (1 - thres_percent / 100))
      } else {
        mutate(., lower_limit = pmax(lower_thres, pred_mean * (1 - thres_percent / 100)))
      }
    } %>%
    # bandwidth: upper limit
    # if provided: also compare with upper threshold (e.g. not more than 100 %)
    {
      if (invalid(upper_thres)) {
        mutate(., upper_limit = pred_mean * (1 + thres_percent / 100))
      } else {
        mutate(., upper_limit = pmin(upper_thres, pred_mean * (1 + thres_percent / 100)))
      }
    } %>%
    # prediction within limits
    mutate(pred_limit = pmin(upper_limit, pmax(lower_limit, pred_prop))) %>%
    # smooth potential edge (due to bandwidth cut off) in x direction
    group_by(!!!syms(group_cols)) %>%
    mutate(pred_roll = rollmean(pred_limit, k = window, na.pad = TRUE)) %>%
    ungroup() %>%
    # restrict to prediction period (without additional x-values needed for smoothing)
    filter((!!sym(x) >= base_t0) & (!!sym(x) <= scen_t1))

  # output
  return(predi)
}
