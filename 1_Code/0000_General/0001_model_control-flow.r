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


# quarto_render(input = paste0(here::here(), "/Plots/01_birth.qmd"),
# execute_params = list(scen = "middle",
# output_dir = paste0(here::here(), "/3_Results/Plots/middle")),
# cache_refresh = FALSE, as_job = FALSE)

Sys.time() - time_start




