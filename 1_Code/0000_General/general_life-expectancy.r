#' life expectancy
#' 
#' life expectancy based on mortality rate (based on Yusuf et al. 2014)
#' 
#' method differences (rate vs. deaths/births/population)  
#' -age capped (if only below a age threshold): rate aggregation does not make sense  
#' -population Px: not average population over a year, here the rate is based on population at the end of the previous year  
#' -qx not with births (probability to die between age x and age x+1): at age zero: with births  
#' -Lx_ (person-years lived) at maximum age: cannot be calculated as dx / mx, since dx (deaths) are not available  
#' 
#' based on Yusuf F., Martins J. M., Swanson D. A., 2014. Methods of Demographic Analysis.  
#' Springer Dordrecht Heidelberg New York London. DOI 10.1007/978-94-007-6784-3
#'
#' @param data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
life_exp <- function(data, ...) {

  # variables
  vars <- list(...)
  mor <- vars$mor # mortality rate in percent per year
  group_cols <- vars$group_cols
  age <- vars$age
  age_max <- vars$age_max
  qx_NA <- vars$qx_NA # value for qx if qx is NA (because Px is NA)
  age_at <- vars$age_at # life expectancy at certain age (e.g. at birth)
  radix <- vars$radix

  le <- data %>%
    # from percent to rate
    mutate(mx = !!sym(mor) / 100) %>%
    # age limit
    filter(!!sym(age) <= age_max) %>%
    # qx: probability to die between age x and age x+1, Yusuf et al. (2014), eq 7.4, 7.5, 7.6
    # the last two age-values: qx should bei 1
    # why? otherwise after the lag, some people 'survive' the last age
    mutate(
      qx1 = pmin(1, if_else(!!sym(age) %in% c(age_max, age_max - 1), 1, 2 * mx / (2 + mx), 1)),
      # if there is no one at a certain age in the population (e.g. no 96 year old men),
      # then qx1 is NA. However, a value is need to multiply the subsequent survival probabilities
      qx = if_else(is.na(qx1), qx_NA, qx1),
      # survival
      px_ = 1 - qx
    ) %>%
    # cumulative product
    arrange(!!!syms(group_cols), !!sym(age)) %>%
    group_by(!!!syms(group_cols)) %>%
    mutate(
      mult = cumprod(px_),
      multlag = lag(mult, default = 1)
    ) %>%
    ungroup() %>%
    mutate(
      radi = radix,
      lx = radi * multlag,
      # expected deaths dx_, Yusuf et al. (2014), eq 7.8
      dx_ = lx * qx,
      # number of survivors lxp1, Yusuf et al. (2014), eq 7.9
      lxp1 = lx - dx_,
      # person-years lived, Yusuf et al. (2014), eq 7.12, 7.14
      # at age zero: it is more likely to die soon after birth than later in the first life year
      Lx_ = if_else(age == 0, 0.3 * lx + 0.7 * lxp1, 0.5 * lx + 0.5 * lxp1)
    ) %>%
    # life expectancy at certain age (e.g. birth)
    group_by(!!!syms(group_cols)) %>%
    summarize(
      life_years = sum(Lx_[!!sym(age) >= age_at]),
      # start_pop: min is only used to avoid an additional mutate statement
      start_pop = min(lx[!!sym(age) == age_at])
    ) %>%
    ungroup() %>%
    mutate(life_exp = if_else(start_pop == 0, NA_real_, age_at + life_years / start_pop)) %>%
    select(-c(life_years, start_pop))

  # output
  return(le)
}
