## test for equalness of two tibbles
## idea: compare csv's from original code run vs. current scenario run
## to make sure any changes in code do not affect output
## Files in consideration are in 2_Data/4_Rates and 2_Data/5_Outputs
## Prerequisite: original outputs to be stored in data_freeze/2_Data


# preliminary work --------------------------------------------------------

# load necessary variables first
# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()


# functions ---------------------------------------------------------------

# function to obtain list of files
# searches in subpaths of 2_Data
# if freeze is set to TRUE, searches in subpaths of data_freeze/2_data
# returns list of csv's as character vectors (input for comparison function)
get_filelist <- function(subpath, freeze = FALSE) {
  if (freeze)
    list.files(subpath, recursive = TRUE, full.names = TRUE)
  else
    list.files(paste0(data_path, subpath), recursive = TRUE, full.names = TRUE)
}

# comparison function:
# function accepts two character vectors with paths to csv files
# and compares them for equality
# NB: names are not checked; comparison based on position inside vector
check_eq <- function(current, freeze) {

  out <- rep("not tested", length(current))
  cnt_eq <- 0L
  
  if (length(current) != length(freeze)) {
    out <- "file lists do not have same length"
    print(out)
    return(out)
  }
  
  for (i in seq_along(current)) {
    
    print(paste("checking file", i, "out of", length(current), "files"))
    a <- read_csv(current[i], show_col_types = FALSE)
    b <- read_csv(freeze[i], show_col_types = FALSE)
    
    a <- a %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 10)))
    b <- b %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 10)))
    
    check <- all.equal(a, b, ignore_row_order = FALSE)
    
    if (!isTRUE(check)) {
      out[i] <- paste("files not equal:",
                      "first file:", current[i],
                      ";  second file:", freeze[i])
      break
    } else {
      out[i] <- paste("files equal:", current[i])
      cnt_eq <- cnt_eq + 1
    }
  }
  
  if (cnt_eq == length(current))
    print("comparison finished: all files are equal")
  else
    print("comparison interrupted: files differ; check output for details")
  
  out
}


# analysis ----------------------------------------------------------------

# compare rates
eq_rates <- check_eq(get_filelist("4_rates"),
                     get_filelist("data_freeze/2_Data/4_rates", freeze = TRUE))

# compare outputs
eq_outputs <- check_eq(get_filelist("5_outputs"),
                       get_filelist("data_freeze/2_Data/5_outputs", freeze = TRUE))
