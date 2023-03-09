
# run_scen ----------------------------------------------------------------



#' model control
#' 
#' This function runs parts of or the whole population model by calling respective functions and creates data and plot outputs.
#'
#' @param scenarios Character vector. Valid values: "lower", "middle", "upper"
#' @param modules Character vector. Valid values:
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
#' own (ownership)  
#' hou (housing model)  
#' deh (demography and housing model)  
#' out (model outputs)  
#' how (housing without output: hom, hou, deh)  
#' alw (all without output: since 'out' needs lower/middle/upper scenario)  
#'
#' @return no return value
#' @export
#'
#' @examples run_scen(scenarios = c("lower", "middle", "upper"), modules = c("all"))
run_scen <- function(scenarios, modules) {
  
  # general functions
  util_gf()
  
  # start with a new log file (delete the previous file)
  if (file.exists(log_file))
    file.remove(log_file)
  
  # different scenarios
  for (i_scen in scenarios) {
    # i_scen <- "middle"
    
    # scenario in the log file
    cat_log(paste0("------ scenario ", i_scen, " ------"))
    
    # scenario specific: assign values to parameters to global environment
    util_gf(i_scen)
    
    # birth
    if (modules %in% c("all", "alw", "dem", "bir")) {
      source(paste0(code_path, "0100_Birth/0100_birth-fertility.r"))
      source(paste0(code_path, "0100_Birth/0110_birth-origin.r"))
      source(paste0(code_path, "0100_Birth/0120_birth-sex-ratio.r"))
    }
    
    # death
    if (modules %in% c("all", "alw", "dem", "dea")) {
      source(paste0(code_path, "0200_Death/0200_death.r"))
    }
    
    # immigration*
    if (modules %in% c("all", "alw", "dem", "ims")) {
      source(paste0(code_path, "0300_Immigration/0300_immigration.r"))
    }
    
    # emigration*
    if (modules %in% c("all", "alw", "dem", "ems")) {
      source(paste0(code_path, "0400_Emigration/0400_emigration.r"))
    }
    
    # relocation-immigration
    if (modules %in% c("all", "alw", "dem", "rei")) {
      source(paste0(code_path, "0500_Relocation-Immigration/0500_relocation-immigration.r"))
    }
    
    # relocation-emigration
    if (modules %in% c("all", "alw", "dem", "ree")) {
      source(paste0(code_path, "0600_Relocation-Emigration/0600_relocation-emigration.r"))
    }
    
    # naturalization
    if (modules %in% c("all", "alw", "dem", "nat")) {
      source(paste0(code_path, "0700_Naturalization/0700_naturalization.r"))
    }
    
    # capacity, reserves
    if (modules %in% c("all", "alw", "how", "hom", "car")) {
      source(paste0(code_path, "0800_Capacity-Reserves/0800_capacity-reserves.r"))
    }
    
    # living space
    if (modules %in% c("all", "alw", "how", "hom", "spa")) {
      source(paste0(code_path, "0900_Living-Space/0900_living-space.r"))
    }
    
    # allocation
    if (modules %in% c("all", "alw", "how", "hom", "aca")) {
      source(paste0(code_path, "1000_Allocation/1000_allocation.r"))
    }
    
    # projects
    if (modules %in% c("all", "alw", "how", "hom", "pro")) {
      source(paste0(code_path, "1100_Projects/1100_projects.r"))
    }
    
    # ownership
    if (modules %in% c("all", "alw", "how", "hom", "own")) {
      source(paste0(code_path, "1200_Ownership/1200_ownership.r"))
    }
    
    # housing model
    if (modules %in% c("all", "alw", "how", "hom", "hou")) {
      source(paste0(code_path, "1300_Housing-Model/1300_housing-model.r"))
    }
    
    # demography and housing model
    if (modules %in% c("all", "alw", "how", "deh")) {
      source(paste0(code_path, "1400_Demography-Housing/1400_demography-housing.r"))
    }
    
    # end: different scenarios
  }
  
  # model outputs: only to be run after all 3 scenarios are computed
  for (i_scen in scenarios) {
    if (modules %in% c("all", "out")) {
      # scenario in the log file
      cat_log(paste0("------ scenario ", i_scen, " ------"))
      
      # assign values to parameters
      # to global environment
      # WHY? will be used in functions outside this function
      for (i_para in 1:nrow(para)) {
        assign(para$parameter[i_para], para[[i_scen]][i_para],
               envir = .GlobalEnv
        )
      }
      
      # same with the scenario name
      assign("i_scen", i_scen, envir = .GlobalEnv)
      
      # general functions (with dependence on parameters)
      source(paste0(code_path, "0000_General/0003_general_with-parameters.r"))
      source(paste0(code_path, "1500_Model/1501_model_outputs.r"))
    }
  }
  
  # end of scenario function
}



# util_gf -----------------------------------------------------------------



#' load general functions and parameters
#'
#' @param i_scen 
#'
#' @return NULL
#' @export
#'
#' @examples util_gf("lower")
util_gf <- function(i_scen = "middle"){
  # general functions already available?
  if (!exists("para", envir = .GlobalEnv)) {
    
    # general functions (without dependence on parameters)
    source(paste0(here::here(), "/1_Code/0000_General/0002_general_without-parameters.r"))
  }
  
  # parameters (depend on scenario)
  for (i_para in 1:nrow(para)) {
    assign(para$parameter[i_para], para[[i_scen]][i_para],
           envir = .GlobalEnv
    )
  }
  
  if (!exists("i_scen", envir = .GlobalEnv))
    assign("i_scen", i_scen, envir = .GlobalEnv)
  
  # general functions (with dependence on parameters)
  source(paste0(code_path, "0000_General/0003_general_with-parameters.r"))
}


#' define name for plots
#' 
#' depending upon parameter setting, name is either returned or not. sszplot function will plot a respective
#' PDF file in this case.
#' If not, an empty string is returned. No PDF is generated; this is used when creating a quarto file with all plots.
#'
#' @param name 
#'
#' @return String
#' @export
#'
#' @examples plot_name("my_plot")
plot_name <- function(name){
  ifelse(isTRUE(params$pdf_output), "0200_mortality_by-year-age-sex-region_focus-age-sex", "")
}
