
# header ------------------------------------------------------------------

# calculate values for downscaling request by AfS
#
# for the years 2021, 2037, 2040, 2045, 2050
# 
# with bzo2040: make sure to use 1504_output_SSZ.r to obtain respective KaReB file!!

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


# check some things -------------------------------------------------------

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

# check parameters used for flaeche.ina (should be 50 and 85 according to spec)
car_plot
car_uti

# load data ---------------------------------------------------------------
kareb <- read_csv("2_Data/1_Input/KaReB.csv") %>%
  group_by(QuarCd, PublJahr, ArealCd, WohnanteilCd, EigentumGrundstkCd) %>%
  summarise(Inanspruchnahme = sum(Inanspruchnahme)) %>%
  filter(PublJahr == scen_begin, ArealCd == 1, WohnanteilCd == 2) %>%
  ungroup() %>%
  mutate(EigentumGrundstkCd = as.character(EigentumGrundstkCd))



# prepare output data based on spa_dyw
# this is still according to the format of last year, transposing to long comes later
downscale <-
  spa_dyw_past_pred %>%
  filter(year >= scen_begin - 1) %>%
  left_join(lookup_map, by = "district") %>%
  left_join(tibble(ownerCd = labels(uni_w), owner = uni_w), by = "owner") %>%
  mutate(wohnflaeche.ksj = NA, flaeche.ina = NA, flaeche.ina.eff = NA, ratio.ina.eff = NA, bq.ksj = NA) %>%
  rename(wohnflaeche.lbj = area, wf.ksj = spa_dyw_all, anz.wohn.ksj = apartments, anz.pers.ksj = people) %>%
  select(
    year, QuarCd, district, ownerCd, owner, wohnflaeche.lbj, wohnflaeche.ksj, flaeche.ina, flaeche.ina.eff,
    ratio.ina.eff, bq.ksj, wf.ksj, anz.wohn.ksj, anz.pers.ksj
  ) %>%
  # add the population
  left_join(pop_total, by = c("district", "year", "owner")) %>%
  mutate(anz.pers.ksj = rowSums(tibble(.$anz.pers.ksj, .$pop), na.rm = TRUE)) %>%
  mutate(wohnflaeche.ksj = wf.ksj * anz.pers.ksj) %>%
  select(-pop) %>%
  # add bq.ksj
  left_join(aca_comb, by = c("district", "year", "owner")) %>%
  mutate(bq.ksj = aca) %>%
  select(-aca_dyw, -aca_yw, -aca_52p, -aca, -apart_thres) %>%
  mutate(anz.wohn.ksj = anz.pers.ksj / bq.ksj) %>%
  # calculate delta population and derive flaeche.ina.eff
  left_join((spa_dyw %>% filter(year == max(year)) %>% select(district, owner, people)), by = c("district", "owner")) %>%
  mutate(delta_pop = anz.pers.ksj - people) %>%
  mutate(flaeche.ina.eff = delta_pop * wf.ksj) %>%
  # add flaeche.ina from kareb and calculate ratio
  left_join(kareb %>% select(QuarCd, ownerCd = EigentumGrundstkCd, Inanspruchnahme), by = c("QuarCd", "ownerCd")) %>%
  mutate(flaeche.ina = Inanspruchnahme) %>%
  mutate(ratio.ina.eff = flaeche.ina.eff / flaeche.ina)



# transpose to long format
# 
downscale %>%
  rename(wohnflaeche = wohnflaeche.ksj,
         bq = bq.ksj,
         wfp = wf.ksj,
         anz.wohn = anz.wohn.ksj,
         anz.pers = anz.pers.ksj) %>%
  select(year, QuarCd, district, ownerCd, owner, wohnflaeche, ratio.ina.eff, bq, wfp, anz.wohn, anz.pers) %>%
  pivot_longer(cols =  c(wohnflaeche, ratio.ina.eff, bq, wfp, anz.wohn, anz.pers),
               names_to = "type")
