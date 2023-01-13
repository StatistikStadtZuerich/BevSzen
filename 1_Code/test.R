## test for equalness of two tibbles
## idea: compare csv's from origiinal code run vs. current scenario run
## to make sure any changes in code do not affect output
## Files in consideration are in 2_Data/4_Rates and 2_Data/5_Outputs
## Prerequisite: original outputs to be stored in data_freeze/2_Data


# load necessary variables first
util_gf()

# prepare list of csv's to be compared
list_current_rates <- list.files(paste0(data_path, "4_Rates"), recursive = TRUE, full.names = TRUE)
list_freeze_rates <- list.files(paste0("data_freeze/", data_path, "4_Rates"), recursive = TRUE, full.names = TRUE)
list_current_outputs <- list.files(paste0(data_path, "5_Outputs"), recursive = TRUE, full.names = TRUE)
list_freeze_outputs <- list.files(paste0("data_freeze/", data_path, "5_Outputs"), recursive = TRUE, full.names = TRUE) 




# function accepts two character vectors with paths to csv files
# and compares them for equality
check_eq <- function(current, freeze) {
  
  if (length(current) != length(freeze)) {
    out <- "file lists do not have same length"
    return(out)
  }
  
  out <- character(length(current))
  
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


# compare rates
eq_rates <- check_eq(list_current_rates, list_freeze_rates)

# compare outputs
eq_outputs <- check_eq(list_current_outputs, list_freeze_outputs)
