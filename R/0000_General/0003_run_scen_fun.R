#' running separate modules of the scenario
#'
#' This function runs parts of or the whole population model by calling respective functions and creates data and plot outputs.
#'
#' @param scenarios character vector. Valid values: "lower", "middle", "upper"
#' @param modules character vector. Valid values:
#' all
#' dem (demography-modules)
#' bir (birth)
#' dea (death)
#' ims (immigration*)
#' ems (emigration*)
#' rei (relocation-immigration)
#' ree (relocation-emigration)
#' nat (naturalization)
#' hom (housing-modules)
#' car (capacity-reserves)
#' spa (living space)
#' aca (allocation)
#' pro (projects)
#' hou (housing model)
#' deh (demography and housing model)
#' out (model outputs)
#' how (housing without output: hom, hou, deh)
#' alw (all without output: since 'out' needs lower/middle/upper scenario)
#' @param vars_init_once list of variables that do not depend on the scenario
#' @param keep_log boolean. Log is kept if parameter is TRUE. Default TRUE
#'
#' @return no return value
#' @export
#'
#' @examples run_scen(scenarios = c("lower", "middle", "upper"), modules = c("all"))


run_scen <- function(scenarios, modules, vars_init_once, keep_log = TRUE) {


  # different scenarios
  for (i_scen in scenarios) {
    # i_scen <- "middle"


    # init scenario (dependent on scenario)
    params <- init_scen(i_scen = i_scen, vars_init_once = vars_init_once)


    # start with a new log file (delete the previous file)
    if (file.exists(params$log_file) && (i_scen == scenarios[1]) && (keep_log == FALSE)) {file.remove(params$log_file)}


    # scenario in the log file
    t0 <- Sys.time()
    cat_log(
      text = paste0(" ------ scenario ", i_scen, " ------"),
      log_file = params$log_file
    )


    # birth
    if (modules %in% c("all", "alw", "dem", "bir")) {
      fertility_rate_fun(params)
      birth_origin_change_fun(params)
      birth_sex_ratio_fun(params)
    }

    # death
    if (modules %in% c("all", "alw", "dem", "dea")) {
      mortality_rate_fun(params)
    }

    # immigration*
    if (modules %in% c("all", "alw", "dem", "ims")) {
      mig_rate_dy_fun(process = "immigration", params = params)
      mig_prop_so_dy_fun(process = "immigration", params = params)
      mig_prop_a_dyso_fun(process = "immigration", params = params)
    }

    # emigration*
    if (modules %in% c("all", "alw", "dem", "ems")) {
      mig_rate_dy_fun(process = "emigration", params = params)
      mig_prop_so_dy_fun(process = "emigration", params = params)
      mig_prop_a_dyso_fun(process = "emigration", params = params)
    }

    # relocation-immigration
    if (modules %in% c("all", "alw", "dem", "rei")) {
      relocation_fun(process = "relocation-immigration", params = params)
    }

    # relocation-emigration
    if (modules %in% c("all", "alw", "dem", "ree")) {
      relocation_fun(process = "relocation-emigration", params = params)
    }

    # naturalization
    if (modules %in% c("all", "alw", "dem", "nat")) {
      naturalization_rate_fun(params = params)
    }

    # capacity, reserves
    if (modules %in% c("all", "alw", "how", "hom", "car")) {
      capacity_reserves_fun(params = params)
    }

    # living space
    if (modules %in% c("all", "alw", "how", "hom", "spa")) {
      living_space_fun(params = params)
    }

    # allocation
    if (modules %in% c("all", "alw", "how", "hom", "aca")) {
      allocation_fun(params = params)
    }

    # projects
    if (modules %in% c("all", "alw", "how", "hom", "pro")) {
      projects_fun(params = params)
    }

    # housing model
    if (modules %in% c("all", "alw", "how", "hom", "hou")) {
      housing_fun(params = params)
    }

    # demography and housing model
    if (modules %in% c("all", "alw", "how", "deh")) {
      deh <- demography_housing_fun(params = params)
    }

  }



  # model outputs: only to be run after all 3 scenarios are computed
  # for (i_scen in scenarios) {
  #   if (modules %in% c("all", "out")) {
  #     # # scenario in the log file
  #     # cat_log(paste0("------ scenario ", i_scen, " ------"))
  #     # params <- init_scen(i_scen = i_scen, vars_init_once = vars_init_once)
  #     #
  #     # source(paste0(code_path, "1500_Model/1501_model_outputs.r"))
  #   }
  #
  # }



}









