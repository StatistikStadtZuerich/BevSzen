
#' load general functions and parameters
#'
#' @param i_scen 
#'
#' @return NULL
#' @export NULL
#'
#' @examples util_gf("lower")
util_gf <- function(i_scen = "middle"){
  # general functions already available?
  if (!exists("para", envir = .GlobalEnv)) {
    
    # general functions (without dependence on parameters)
    source("1_Code/0000_General/0002_general_without-parameters.r")
    
    # parameters (depend on scenario)
    for (i_para in 1:nrow(para)) {
      assign(para$parameter[i_para], para[[i_scen]][i_para],
             envir = .GlobalEnv
      )
    }
    
    if (!exists("i_scen", envir = .GlobalEnv))
      assign("i_scen", i_scen, envir = .GlobalEnv)
    
    # general functions (with dependence on parameters)
    source(paste0(code_path, "/0000_General/0003_general_with-parameters.r"))
  }
}
