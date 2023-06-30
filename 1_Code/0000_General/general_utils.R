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
#' own (ownership)  
#' hou (housing model)  
#' deh (demography and housing model)  
#' out (model outputs)  
#' how (housing without output: hom, hou, deh)  
#' alw (all without output: since 'out' needs lower/middle/upper scenario)
#' @param keep_log boolean. Log is kept if parameter is TRUE. Default TRUE
#'
#' @return no return value
#' @export
#'
#' @examples run_scen(scenarios = c("lower", "middle", "upper"), modules = c("all"))
run_scen <- function(scenarios, modules, keep_log = TRUE) {
  
  # different scenarios
  for (i_scen in scenarios) {
    # i_scen <- "middle"
    # scenario specific: assign values to parameters to global environment
    init(i_scen)
    
    # start with a new log file (delete the previous file)
    if (file.exists(log_file) && i_scen == scenarios[1] && keep_log == FALSE)
      file.remove(log_file)
    
    # scenario in the log file
    # start time
    t0 <- Sys.time()
    cat_log(" ------ scenario ", i_scen, " ------")
    
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
    
    cat_log(paste0("total runtime scenario ", i_scen, ": ", capture.output(Sys.time() - t0)))   
    # end: different scenarios
  }
  
  # model outputs: only to be run after all 3 scenarios are computed
  for (i_scen in scenarios) {
    if (modules %in% c("all", "out")) {
      # scenario in the log file
      cat_log(paste0("------ scenario ", i_scen, " ------"))
      init(i_scen)
      
      source(paste0(code_path, "1500_Model/1501_model_outputs.r"))
    }
  }
}


#' define name for plots
#' 
#' depending upon parameter setting, name is either returned or not. sszplot function will plot a respective
#' PDF file in this case.
#' If not, an empty string is returned. No PDF is generated; this is used when creating a quarto file with all plots.
#'
#' @param name string for name of plot output file
#'
#' @return string
#' @export
#'
#' @examples plot_name("my_plot")
plot_name <- function(name){
  ifelse(isTRUE(params$pdf_output), name, "")
}



#' render complete book
#' 
#' run the necessary parts of all scenarios, then render book
#' (this is necessary because last chapter needs input from all scenarios)
#' however, usually it is not necessary to create books for each scenario
#' but only for the middle one.
#'
#' @param cache_refresh should cache of quarto rendering be completely dropped? Defaults to TRUE
#'
#' @return no visible return value
#' @export
#'
#' @examples render_book()
render_book <- function(cache_refresh = TRUE){
    run_scen(scenarios = c("lower", "middle", "upper"), modules = "all", keep_log = TRUE)
    
    quarto_render(input = "./Plots",
                  execute_params = list(scen = "middle",
                                        output_dir = paste(book_path)),
                  cache_refresh = cache_refresh,
                  as_job = FALSE)
}


#' calculate sum after removing NA values
#'
#' @param x vector of values
#'
#' @return sum of x after removing NA's
#' @export
#'
#' @examples sum_NA(c(1,2,3,NA))
sum_NA <- function(x) {
  sum(x, na.rm = TRUE)
}

#' calculate max value after removing NA values
#'
#' @param x vector of values
#'
#' @return max of vector x after removing NA's
#' @export
#'
#' @examples max_NA(c(1,2,3,NA))
max_NA <- function(x) {
  max(x, na.rm = TRUE)
}

#' cat to log file
#' 
#' appends input to a log file
#'
#' @param ... text to be added to log
#'
#' @return no return value
#' @export
#'
#' @examples
cat_log <- function(...) {
  sub_path <- regmatches(
    log_file,
    regexpr(".*[\\/]", log_file)
  )
  dir_ex_create(sub_path)
  
  cat(paste(Sys.time(), ":", ...) ,
      file = log_file, sep = "\n", append = TRUE
  )
}



#' check and create file directories
#' 
#' @description checks if a certain directory is existing; if not, 
#'
#' @param path 
#'
#' @return message of success
#' @export
#'
#' @examples dir_ex_create("results")
dir_ex_create <- function(path){
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE
    )
    paste(path, "created successfully")
  } else
    paste(path, "already exists")
}
