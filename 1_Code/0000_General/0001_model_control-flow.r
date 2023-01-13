source("R/general_utils.R")

time_start <- Sys.time()

run_scen(
    scenarios = c("lower", "middle", "upper"),
    modules = c("all"))

# run_scen(
#     scenarios = c("lower", "middle", "upper"),
#     modules = c("out"))

Sys.time() - time_start



