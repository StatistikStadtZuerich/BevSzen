source("1_code/0000_general/general_init.R")
params <- init("middle")

time_start <- Sys.time()

# run_scen(
#     scenarios = c("lower", "middle", "upper"),
#     modules = c("all"))

# run_scen(
#     scenarios = c("lower", "middle", "upper"),
#     modules = c("out"))

render_book(cache_refresh = TRUE)
