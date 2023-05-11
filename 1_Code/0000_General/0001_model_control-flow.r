source("1_code/0000_general/general_utils.R")

time_start <- Sys.time()

# run_scen(
#     scenarios = c("lower", "middle", "upper"),
#     modules = c("all"))

# run_scen(
#     scenarios = c("lower", "middle", "upper"),
#     modules = c("out"))

util_gf()
render_book()

Sys.time() - time_start