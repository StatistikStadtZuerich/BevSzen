#' combine projects and reserves
#'
#' @param x population, project, usage data (dyw)
#' @param params parameter vector
#'
#' @returns
#' @export
#'
#' @examples
project_reserves <- function(x, params) {

  # parameter
  scen_begin <- params$scen_begin
  car_trust <- params$car_trust
  car_max_trust <- params$car_max_trust

  # maximum usage of the capacity, plus population
  car_max <- max_NA(x$car)

  # new columns
  x$project <- NA
  x$corr <- NA
  x$car[1] <- x$pop[1]
  x$usage <- c(0, diff(x$car))

  # loop from second year (i.e. first year in the future) to the last prediction year
  for (i in 2:nrow(x)) {

    # use the information from the project list (the result can not be negative)
    x$project[i] <- max(0, x$pop[i - 1] + x$new[i] - x$removed[i])

    # trust the projects or reserves?
    # if reserves larger, then select reserves
    # why? not all future buildings in the project list
    # if projects larger: apply the parameter 'car_trust'
    # for car_trust = 100 the lower reserves-value is used
    # for car_trust = 0 the higher project-value is used

    if (x$car[i] > x$project[i]) {
      x$pop[i] <- x$car[i]
    } else {
      x$pop[i] <- x$project[i] - (x$project[i] - x$car[i]) * (car_trust/100)
    }

    # correction of the reserves (if too much used due to the project list)
    x$corr[i] <- max(0, x$pop[i] - x$car[i])

    # correction (proportional to the reserve usage)

    if (i < nrow(x)) {
      index <- (i + 1):nrow(x)
      x$subtract <- 0

      # correction proportional to usage
      if (sum(x$usage[index]) > 0) {
        x$subtract[index] <- x$usage[index] / sum(x$usage[index]) * x$corr[i]
      } else {
        x$subtract[index] <- 0
      }

      # subtract (usage per year, and cumulative usage plus population)
      x$usage[index] <- pmax(0, x$usage[index] - x$subtract[index])

      # new reserves value based on corrected usage
      x$car[i+1] <- pmax(0, x$pop[i] + x$usage[i+1])

    }

    # end of the loop over years
  }

  # output
  # if the population exceeds the maximum possible population according the reserves calculation
  # trust the reserves or the calculated population?

  y <- x |>
    mutate(
      upper_limit_car = pmin(pop, car_max),
      diff_to_limit = pop - upper_limit_car,
      to_subtract = car_max_trust / 100 * diff_to_limit,
      pop_new = pop - to_subtract
    ) |>
    select(district, year, owner, pop_new) |>
    rename(pop = pop_new) |>
    filter(year >= scen_begin)
  # print(y)
  y

}





