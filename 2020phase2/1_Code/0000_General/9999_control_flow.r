#-------------------------------------------------------------------
# This could be the main control file
#
#
#
#bad/rok, August 2021
#-------------------------------------------------------------------

## simply sources all the code files in the correct order
## currently, location of results and exports are determined inside
## these source files. Consider to changge.

## TODO: some log

setwd(paste0(here::here(), "/2020phase2/"))
beg <- "1_Code"
term <- "_phase2.r"
source(paste0(beg, "/0100_Birth/0100_birth-fertility", term))
source(paste0(beg, "/0100_Birth/0110_birth-origin", term))
source(paste0(beg, "/0100_Birth/0120_birth-sex-ratio", term))
source(paste0(beg, "/0200_Death/0200_death", term))
# source(paste0(beg, "/0300_Immigration/0300_immigration", term))
# source(paste0(beg, "/0400_Emigration/0400_emigration", term))
# source(paste0(beg, "/0500_Relocation-Immigration/0500_relocation-immigration", term))
# source(paste0(beg, "/0600_Relocation-Emigration/0600_relocation-emigration", term))
source(paste0(beg, "/0700_Naturalization/0700_naturalization", term))
# source(paste0(beg, "/0800_Capacity-Reserves/0800_capacity-reserves", term))