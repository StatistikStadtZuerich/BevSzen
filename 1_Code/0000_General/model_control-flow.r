source("1_code/0000_general/general_init.R")
params <- init("middle_birth_upper")

time_start <- Sys.time()

run_scen(
    scenarios = c("lower", "middle", "middle_birth_lower", "middle_birth_upper", "upper"),
    modules = c("ims"))

# run_scen(
#     scenarios = c("lower", "middle", "middle_birth_lower", "middle_birth_upper", "upper"),
#     modules = c("all"))

render_book(cache_refresh = TRUE)
