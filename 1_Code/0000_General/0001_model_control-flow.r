source("1_code/0000_general/general_init.R")
params <- init("middle") # inits assign directly to global env > doenst need to be

time_start <- Sys.time()

run_scen(scenarios = c("middle"), modules = c("hou"))

run_scen(scenarios = c("middle"), modules = c("car"))
run_scen(scenarios = c("middle"), modules = c("spa"))
run_scen(scenarios = c("middle"), modules = c("aca"))
run_scen(scenarios = c("middle"), modules = c("pro"))
run_scen(scenarios = c("middle"), modules = c("own"))

# run_scen(
#     scenarios = c("lower", "middle", "upper"),
#     modules = c("out"))

render_book(cache_refresh = TRUE)
