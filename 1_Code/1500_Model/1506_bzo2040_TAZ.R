# header ------------------------------------------------------------------

# calculate values for TAZ according request 5881
# middle scenario, KaReB according BZO2040
# for the years 2022-2040

# prep work ---------------------------------------------------------------
# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
params <- init("middle")

# exchange KaReB file
inp_path <- paste0(data_path, "1_Input/")
if (file.exists(paste0(inp_path, "KaReB_2040.csv"))) {
  file.rename(paste0(inp_path, "KaReB.csv"), paste0(inp_path, "KaReB_2016.csv"))
  file.rename(paste0(inp_path, "KaReB_2040.csv"), paste0(inp_path, "KaReB.csv"))
}

# output path creation
req_path <- paste0(data_path, "8_requests/")
if (!dir.exists(req_path)) {
  dir.create(req_path, recursive = TRUE)
}
req_path <- paste0(req_path, "pop_scen_TAZ.csv")

# run the model and build output data -------------------------------------

run_scen(
  scenarios = c("middle"),
  modules = c("all"))
source(paste0(code_path, "1500/1501_model_outputs.r"))

#(in case pop_middle is not existing, run the follwoing code lines:
# pop_middle <- read_csv(paste0(data_path, "5_Outputs/middle/population_future.csv")) %>%
#  mutate(scenario = uni_c[3])
#)

pop_middle %>%
  filter(year <= 2040) %>%
  mutate(pop = round(pop, -1)) %>%
  group_by(district, year) %>%
  summarise(pop = sum_NA(pop),
            .groups = "drop") %>%
  left_join(lookup_map, by = "district") %>% 
  arrange(QuarSort) %>% 
  select(-QuarCd, -QuarSort) %>%
  write_delim(req_path, delim = ";")


# checks ------------------------------------------------------------------
# this is all very similar to request by SSD (file 1504_output_SSD) except for the age groups.
# Therefore no new checks necessary.

# cleanup work ------------------------------------------------------------

# exchange KaReB file back to original
if (file.exists(paste0(inp_path, "KaReB_2016.csv"))) {
  file.rename(paste0(inp_path, "KaReB.csv"), paste0(inp_path, "KaReB_2040.csv"))
  file.rename(paste0(inp_path, "KaReB_2016.csv"), paste0(inp_path, "KaReB.csv"))
}
