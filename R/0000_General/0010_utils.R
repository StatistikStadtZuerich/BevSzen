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
render_book <- function(book_path, cache_refresh = TRUE){
    run_scen(scenarios = c("lower", "middle", "upper"), modules = "all", keep_log = TRUE)
    
    quarto_render(input = "./Quarto",
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
#' @param text text to be added to log
#' @param log_file path of log file
#'
#' @return no return value
#' @export
#'
#' @examples
cat_log <- function(text, log_file) {
  sub_path <- regmatches(
    log_file,
    regexpr(".*[\\/]", log_file)
  )
  dir_ex_create(sub_path)
  
  cat(paste(Sys.time(), ":", text) ,
      file = log_file, sep = "\n", append = TRUE
  )
}



#' check and create file directories
#' 
#' @description checks if a certain directory is existing; if not, creates it
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



#' shortcut function to create a one dimensional factor
#' 
#' @description This is a shortcut and replaces code like fact_if(SexCd, uni_s).
#'
#' @param cd Which value should be checked for 1
#' @param uni factor on which to base the new factor
#'
#' @return factor with 1 label and all original levels
#' @export
#'
#' @examples fact_if(SexCd, uni_s)
fact_if <- function(cd, uni, equals = 1, lev1 = 1, lev2 = 2) {
  factor(if_else(cd == equals, uni[lev1], uni[lev2]), uni)
}
