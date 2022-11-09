
# header ------------------------------------------------------------------

# calculate values for downscaling request by AfS
# 
# for the years 2021, 2037, 2040, 2045, 2050

# prep work ---------------------------------------------------------------

# general functions (without dependence on parameters)
source("1_Code/0000_General/0002_general_without-parameters.r")
# parameters (depend on scenario)
i_scen <- "middle"
for (i_para in 1:nrow(para)) {
  assign(para$parameter[i_para], para[[i_scen]][i_para],
         envir = .GlobalEnv
  )
}
# general functions (with dependence on parameters)
source(paste0(code_path, "/0000_General/0003_general_with-parameters.r"))

# exchange KaReB file
inp_path <- paste0(data_path, "1_Input/")
file.rename(paste0(inp_path, "KaReB.csv"), paste0(inp_path, "KaReB_2016.csv"))
file.rename(paste0(inp_path, "KaReB_2040.csv"), paste0(inp_path, "KaReB.csv"))

# output path creation
req_path <- paste0(data_path, "8_requests/")
if (!dir.exists(req_path)) {
  dir.create(req_path, recursive = TRUE)
}
req_path <- paste0(req_path, "pop_scen_downscaling.csv")



# load data ---------------------------------------------------------------
# library(readr)
living_space_future <- read_csv("2_Data/4_Rates/middle/living-space_future.csv")

population_future <- read_csv("2_Data/5_Outputs/middle/population_future.csv") %>%
  group_by(district, year) %>%
  summarise(pop = sum(pop))
