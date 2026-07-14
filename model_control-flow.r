# source R files ----------------------------------------------------------

# root
library(rprojroot)
root <- find_root(is_rstudio_project)

# source
r_files <- list.files(path = file.path(root, "R"), pattern = "\\.R$",
  recursive = TRUE, full.names = TRUE)
invisible(lapply(r_files, source))



# init --------------------------------------------------------------------

# init once (independent of scenario)
vars_once <- init_once(i_data_path = "~/v/Szen/BevSzen/2026")

# init scenario (dependent on scenario)
params <- init_scen(i_scen = "middle", vars_init_once = vars_once)



# run scenario ------------------------------------------------------------

# duration: approx. four minutes for all modules (alw) for one scenario

time_start <- Sys.time()

run_scen(scenarios = c("lower", "middle", "upper"),
         modules = c("alw"),
         vars_init_once = vars_once)

Sys.time() - time_start



# scenarios
#' lower
#' middle
#' upper
#' middle_birth_lower  
#' middle_birth_upper
#' lower_car_rev   
#' middle_car_rev   
#' upper_car_rev   

# modules
#' all
#' alw (all without output: since 'out' needs lower/middle/upper scenario)
#' dem (demography-modules)
#' bir (birth)
#' dea (death)
#' ims (immigration*)
#' ems (emigration*)
#' rei (relocation-immigration)
#' ree (relocation-emigration)
#' nat (naturalization)
#' hom (housing-modules)
#' car (capacity-reserves)
#' spa (living space)
#' aca (allocation)
#' pro (projects)
#' hou (housing model)
#' deh (demography and housing model)
#' out (model outputs)
#' how (housing without output: hom, hou, deh)



# model outputs -----------------------------------------------------------

# execute the model outputs function
# run at the very end
# after all other modules are executed for all scenarios and birth versions and BZO versions
mod <- model_outputs_fun(params = params, BZO_compare = TRUE)




# output for DWH ----------------------------------------------------------

# export: duration approx. one minute
# file examples: see V:\Szen\BevSzen\2026\2_Daten\7_DWH

time_start <- Sys.time()

out_dwh <- outputs_dwh_fun(
      scenarios =c("lower", "middle", "upper", 
                   "middle_birth_lower", "middle_birth_upper"),
      params = params)

Sys.time() - time_start


# control plots: demography
(out_dwh$dem_control_dat |> 
  ggplot() +
  geom_line(aes(x = Jahr, y = value, color = scenario)) +
  facet_wrap(~type, ncol = 3, scales = "free_y") +
  labs(x = "", y = "count", color = "") +
  expand_limits(y = 0) +
  params$neutral) |>
  ggplotly()


# control plots: capacity and reserves
(out_dwh$car_control_dat |> 
  ggplot() +
  geom_line(aes(x = PublJahr, y = ha, color = Wohnanteil, linetype = Areal)) +
  facet_grid(Bereich~Eigentum, scales = "free_y") +
  expand_limits(y = 0) +
  params$neutral) |>
  ggplotly() |> 
  layout(
    legend = list(orientation = "h",  x = 0.5, 
                  xanchor = "center",  y = -0.2, yanchor = "top"))  


# control plots: housing
(out_dwh$hou_control_dat |> 
  group_by(Jahr, scenario, owner, type) |> 
  summarize(value = sum_NA(value), .groups = "drop") |> 
  ggplot() +
  geom_line(aes(x = Jahr, y = value, color = scenario)) +
  facet_grid(type~owner, scales = "free_y") +    
  labs(x = "", y = "value", color = "") +
  expand_limits(y = 0) +
  params$neutral) |>
  ggplotly() |> 
  layout(
    legend = list(orientation = "h",  x = 0.5, 
                  xanchor = "center",  y = -0.2, yanchor = "top"))




