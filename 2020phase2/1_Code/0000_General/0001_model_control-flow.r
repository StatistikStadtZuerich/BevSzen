# header ------------------------------------------------------------------
# model control flows



# paths, parameters, functions --------------------------------------------

# remove previous objects
rm(list = ls())

# working directory
library(here)
setwd(paste0(here(), "/2020phase2/"))

# general functions without dependence on parameters
source("1_Code/0000_General/0002_general_without-parameters.r")



# source the code files ---------------------------------------------------

# function for different scenarios and modules

# possible values

# scenarios
#   lower
#   middle
#   upper

# modules
# all
#   dem (demography-modules),
#     bir (birth)
#     dea (death)
#     ims (immigration*)
#     ems (emigration*)
#     rei (relocation-immigration)
#     ree (relocation-emigration)
#     nat (naturalization)
#   hom (housing-modules)
#     car (capacity-reserves)
#     spa (living space)
#     aca (allocation)
#     pro (projects)
#     own (ownership)
#   hou (housing model)
#   deh (demography and housing model)
#   out (model outputs)
# how (housing without output: hom, hou, deh)
# alw (all without output: since 'out' needs lower/middle/upper scenario)




run_scen <- function(scenarios, modules) {

  # start with a new log file (delete the previous file)
  file.remove(log_file)

  # different scenarios
  for (i_scen in scenarios) {
    # i_scen <- "middle"

    # scenario in the log file
    cat_log(paste0("------ scenario ", i_scen, " ------"))

    # assign values to parameters
    # to global environment
    # WHY? will be used in functions outside this function

    for (i_para in 1:nrow(para)) {
      assign(para$parameter[i_para], para[[i_scen]][i_para],
        envir = .GlobalEnv
      )
    }

    # same with the scenario name
    assign("i_scen", i_scen, envir = .GlobalEnv)

    # general functions (with dependence on parameters)
    source(paste0(code_path, "/0000_General/0003_general_with-parameters.r"))

    # birth
    if (modules %in% c("all", "alw", "dem", "bir")) {
      source(paste0(code_path, "0100_Birth/0100_birth-fertility.r"))
      source(paste0(code_path, "0100_Birth/0110_birth-origin.r"))
      source(paste0(code_path, "0100_Birth/0120_birth-sex-ratio.r"))
    }

    # death
    if (modules %in% c("all", "alw", "dem", "dea")) {
      source(paste0(code_path, "0200_Death/0200_death.r"))
    }

    # immigration*
    if (modules %in% c("all", "alw", "dem", "ims")) {
      source(paste0(code_path, "0300_Immigration/0300_immigration.r"))
    }

    # emigration*
    if (modules %in% c("all", "alw", "dem", "ems")) {
      source(paste0(code_path, "0400_Emigration/0400_emigration.r"))
    }

    # relocation-immigration
    if (modules %in% c("all", "alw", "dem", "rei")) {
      source(paste0(code_path, "0500_Relocation-Immigration/0500_relocation-immigration.r"))
    }

    # relocation-emigration
    if (modules %in% c("all", "alw", "dem", "ree")) {
      source(paste0(code_path, "0600_Relocation-Emigration/0600_relocation-emigration.r"))
    }

    # naturalization
    if (modules %in% c("all", "alw", "dem", "nat")) {
      source(paste0(code_path, "0700_Naturalization/0700_naturalization.r"))
    }

    # capacity, reserves
    if (modules %in% c("all", "alw", "how", "hom", "car")) {
      source(paste0(code_path, "0800_Capacity-Reserves/0800_capacity-reserves.r"))
    }

    # living space
    if (modules %in% c("all", "alw", "how", "hom", "spa")) {
      source(paste0(code_path, "0900_Living-Space/0900_living-space.r"))
    }

    # allocation
    if (modules %in% c("all", "alw", "how", "hom", "aca")) {
      source(paste0(code_path, "1000_Allocation/1000_allocation.r"))
    }

    # projects
    if (modules %in% c("all", "alw", "how", "hom", "pro")) {
      source(paste0(code_path, "1100_Projects/1100_projects.r"))
    }

    # ownership
    if (modules %in% c("all", "alw", "how", "hom", "own")) {
      source(paste0(code_path, "1200_Ownership/1200_ownership.r"))
    }

    # housing model
    if (modules %in% c("all", "alw", "how", "hom", "hou")) {
      source(paste0(code_path, "1300_Housing-Model/1300_housing-model.r"))
    }

    # demography and housing model
    if (modules %in% c("all", "alw", "how", "deh")) {
      source(paste0(code_path, "1400_Demography-Housing/1400_demography-housing.r"))
    }

    # model outputs
    if (modules %in% c("all", "out")) {
      source(paste0(code_path, "1500_Model/1501_model_outputs.r"))
    }

    # end: different scenarios
  }

  # end of scenario function
}


# execute the function
# run_scen(
#     scenarios = c("lower", "middle", "upper"),
#     modules = c("hou"))

run_scen(
    scenarios = c("lower", "middle", "upper"),
    modules = c("out"))





#   hom (housing-modules)
#     car (capacity-reserves)
#     spa (living space)
#     aca (allocation)
#     pro (projects)
#     own (ownership)
#   hou (housing model)
#   deh (demography and housing model)
#   out (model outputs)