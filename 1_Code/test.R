
util_gf()


list_current_rates <- list.files(paste0(data_path, "4_Rates"), recursive = TRUE, full.names = TRUE)
list_freeze_rates <- list.files(paste0("data_freeze/", data_path, "4_Rates"), recursive = TRUE, full.names = TRUE)
list_current_outputs <- list.files(paste0(data_path, "5_Outputs"), recursive = TRUE, full.names = TRUE)
list_freeze_outputs <- list.files(paste0("data_freeze/", data_path, "5_Outputs"), recursive = TRUE, full.names = TRUE) 





check_eq <- function(current, freeze) {
  
  out <- character(length(current))
  
  for (i in seq_along(current)) {
    
    a <- read_csv(current[i])
    b <- read_csv(freeze[i])
    
    check <- all_equal(a, b, ignore_row_order = FALSE)
    if (!isTRUE(check)) {
      out[i] <- paste("files not equal:",
                      "first file:", current[i],
                      "second file:", freeze[i])
      break
    } else {
      out[i] <- paste("files equal:",
                      "first file:", current[i])
    }
  }
  
  out
}



check_eq(list_current_rates, list_freeze_rates)

check_eq(list_current_outputs, list_freeze_outputs)



current <- list_current_outputs
freeze <- list_freeze_outputs
