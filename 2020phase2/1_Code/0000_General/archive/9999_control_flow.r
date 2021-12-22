#-------------------------------------------------------------------
# This could be the main control file
#
#
#
#bad/rok, August 2021
#-------------------------------------------------------------------

## function to source all the code files in the correct order
## function may be called for all , only demographics/living, or for the single
## modules
## possible param values: "all", "demo", "live", "birth", "death", "migration",
##                        "immigration", "emigration", "relocation",
##                        "relocation-immigration", "relocation-emigration",
##                        "naturalization", "living"
## currently, location of results and exports are determined inside
## these source files. Consider to change.

## TODO: some log

i_scen = "middle"
run_pops <- function(modules = c("all"), i_scen = "middle") {
  setwd(paste0(here::here(), "/2020phase2"))
  code <- "1_Code/"
  term <- ".r"
  
  #general functions already available?
  if (!exists("para")) {
    #general functions (without dependence on parameters)
    source("1_Code/0000_General/0000_general_without-parameters.r")
    
    #parameters (depend on scenario)
    for(i_para in 1:nrow(para)){
      assign(para$parameter[i_para], para[[i_scen]][i_para],
             envir = .GlobalEnv)}
    
    #general functions (with dependence on parameters)
    source(paste0(code_path, "/0000_General/0001_general_with-parameters.r"))
    
  }
  

  # source(paste0(code, "0000_General/0000_general", term))
    
  if (modules %in% c("all", "demo", "birth")) {
    source(paste0(code, "0100_Birth/0100_birth-fertility", term))
    source(paste0(code, "0100_Birth/0110_birth-origin", term))
    source(paste0(code, "0100_Birth/0120_birth-sex-ratio", term))
  }
  
  if (modules %in% c("all", "demo", "death"))
    source(paste0(code, "0200_Death/0200_death", term))

  if (modules %in% c("all", "demo", "migration", "immigration"))
    source(paste0(code, "0300_Immigration/0300_immigration", term))
  
  if (modules %in% c("all", "demo", "migration", "emigration"))
    source(paste0(code, "0400_Emigration/0400_emigration", term))
  
  if (modules %in% c("all", "demo", "relocation", "relocation-immigration"))
    source(paste0(code, "0500_Relocation-Immigration/0500_relocation-immigration", term))
  
  if (modules %in% c("all", "demo", "relocation", "relocation-emigration"))
    source(paste0(code, "0600_Relocation-Emigration/0600_relocation-emigration", term))
  
  if (modules %in% c("all", "demo", "naturalization"))
    source(paste0(code, "0700_Naturalization/0700_naturalization", term))
  
  if (modules %in% c("all", "live", "capacity"))
    source(paste0(code, "0800_Capacity-Reserves/0800_capacity-reserves", term))
    
  if (modules %in% c("all", "live", "living"))
    source(paste0(code, "0900_Living-Space/0900_living-space", term))

  if (modules %in% c("all", "live", "alloc"))
    source(paste0(code, "1000_Allocation/1000_allocation", term))
  
  if (modules %in% c("all"))
    source(paste0(code, "1100_Projects/1100_projects", term))
  
  if (modules %in% c("all"))
    source(paste0(code, "1200_Ownership/1200_ownership", term))
  
  if (modules %in% c("all"))
    source(paste0(code, "1300_Housing-Model/1300_housing-model", term))
}

system.time(run_pops())
