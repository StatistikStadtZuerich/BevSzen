## test for equalness of two tibbles
## idea: compare csv's from origiinal code run vs. current scenario run
## to make sure any changes in code do not affect output
## Files in consideration are in 2_Data/4_Rates and 2_Data/5_Outputs
## Prerequisite: original outputs to be stored in data_freeze/2_Data


# preliminary work --------------------------------------------------------

# load necessary variables first
util_gf()


# functions ---------------------------------------------------------------

# function to obtain list of files
# searches in subpaths of 2_Data
# if freeze is set to TRUE, searches in subpaths of data_freeze/2_data
# returns list of csv's as character vectors (input for comparison function)
get_filelist <- function(subpath, freeze = FALSE) {
  if (freeze)
    list.files(paste0("data_freeze/", data_path, subpath), recursive = TRUE, full.names = TRUE)
  else
    list.files(paste0(data_path, subpath), recursive = TRUE, full.names = TRUE)
}

# comparison function:
# function accepts two character vectors with paths to csv files
# and compares them for equality
# NB: names are not checked; comparison based on position inside vector
check_eq <- function(current, freeze) {
  
  if (length(current) != length(freeze)) {
    out <- "file lists do not have same length"
    return(out)
  }
  
  out <- rep("not tested", length(current))
  
  for (i in seq_along(current)) {
    
    a <- read_csv(current[i])
    b <- read_csv(freeze[i])
    
    check <- all_equal(a, b, ignore_row_order = FALSE)
    if (!isTRUE(check)) {
      out[i] <- paste("files not equal:",
                      "first file:", current[i],
                      ";  second file:", freeze[i])
      break
    } else {
      out[i] <- paste("files equal:", current[i])
    }
  }
  
  out
}


# analysis ----------------------------------------------------------------

# compare rates
eq_rates <- check_eq(get_filelist("4_rates"), get_filelist("4_rates", freeze = TRUE))

# compare outputs
eq_outputs <- check_eq(get_filelist("5_outputs"), get_filelist("5_outputs", freeze = TRUE))
