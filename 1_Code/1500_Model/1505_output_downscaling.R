
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
read_csv("2_Data/4_Rates/middle/living-space_future.csv")
spa_dyw
spa_dyw_past_pred


read_csv("2_Data/5_Outputs/middle/population_future.csv") %>%
  group_by(district, year) %>%
  summarise(pop = sum(pop)) %>%
  left_join(pop_total, by = c("district", "year"))
pop_total


car_dat
# same as:
# usage_area <- read_csv("2_Data/4_Rates/middle/usage_area.csv")

kareb <- read_csv("2_Data/1_Input/KaReB.csv") %>%
  group_by(QuarCd, PublJahr, ArealCd, WohnanteilCd, EigentumGrundstkCd) %>%
  summarise(Inanspruchnahme = sum(Inanspruchnahme))
kareb %>% filter(QuarCd == "010") %>% print(n=Inf)





downscale <- spa_dyw_past_pred %>%
  filter(year >= scen_begin-1) %>%
  left_join(lookup_map, by = "district") %>% 
  left_join(tibble(ownerCd = labels(uni_w), owner = uni_w), by = "owner") %>%
  mutate(wohnflaeche.ksj = NA, flaeche.ina = NA, flaeche.ina.eff = NA, ratio.ina.eff = NA, bq.ksj = NA) %>%
  rename(wohnflaeche.lbj = area, wf.ksj = spa_dyw_all, anz.wohn.ksj = apartments,	anz.pers.ksj = people) %>%
  select(year, QuarCd, district, ownerCd, owner, wohnflaeche.lbj,	wohnflaeche.ksj, flaeche.ina, flaeche.ina.eff, ratio.ina.eff, bq.ksj, wf.ksj, anz.wohn.ksj, anz.pers.ksj)


downscale %>%
  left_join(pop_total, by = c("district", "year", "owner")) %>%
  mutate(anz.pers.ksj = rowSums(tibble(.$anz.pers.ksj, .$pop), na.rm = TRUE)) %>%
  mutate(wohnflaeche.ksj = wf.ksj * anz.pers.ksj) %>%
  select( -pop) %>%
  left_join(car_dat, by = c("district", "year", "owner")) %>%
  mutate(flaeche.ina.eff = usage_ha * 1e4) %>%
  select(-usage_ha) %>%
  left_join(aca_comb, by = c("district", "year", "owner")) %>%
  mutate(bq.ksj = aca) %>%
  select(-aca_dyw, -aca_yw, -aca_52p, -aca, -apart_thres) %>%
  mutate(anz.wohn.ksj = anz.pers.ksj / bq.ksj)
