source("1_code/0000_general/general_init.R")
params <- init("middle")

time_start <- Sys.time()

run_scen(scenarios = c("lower", "middle", "upper"), modules = c("pro"))
run_scen(scenarios = c("lower", "middle", "upper"), modules = c("hou"))
run_scen(scenarios = c("lower", "middle", "upper"), modules = c("deh"))
run_scen(scenarios = c("lower", "middle", "upper"), modules = c("out"))


# run_scen(
#     scenarios = c("lower", "middle", "middle_birth_lower", "middle_birth_upper", "upper"),
#     modules = c("all"))

Sys.time() - time_start



render_book(cache_refresh = TRUE)


