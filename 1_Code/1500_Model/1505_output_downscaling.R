
# header ------------------------------------------------------------------

# calculate values for downscaling request by AfS
# 
# with bzo2040: make sure to use correct KaReB file!!
# 
# data is basically prepared based on previously calculated values from 
# BevSzen (but with BZO2040). Calculations concern mainly the usage from
# KaReB, see below.

# prep work ---------------------------------------------------------------

util_gf()

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
req_path <- paste0(req_path, "pop_scen_downscaling.csv")


# run the model with bzo2040 -------------------------------------

run_scen(
  scenarios = c("middle"),
  modules = c("all"))
source(paste0(code_path, "1500/1501_model_outputs.r"))


# load data ---------------------------------------------------------------
kareb <- read_csv("2_Data/1_Input/KaReB.csv") %>%
  group_by(QuarCd, PublJahr, ArealCd, WohnanteilCd, EigentumGrundstkCd) %>%
  summarise(Inanspruchnahme = sum(Inanspruchnahme)) %>%
  # filter on current year,  plot construction, real portion
  filter(PublJahr == scen_begin, ArealCd == 1, WohnanteilCd == 2) %>%
  ungroup() %>%
  mutate(EigentumGrundstkCd = as.character(EigentumGrundstkCd))

# calculate flaeche.ina from kareb
# for this, we have to distribute the flaeche.ina exponentially
# exp_y was used for this (see in 0800); but here we need to distribute it from 1 (2047) to 0 (now)
# therefore we apply a normalization (and restrict years to scen_begin + 25)
# the data is cumulative; hence scale values do not add up to 100% but insteaad
# reach 100% after 25 years (therefore we revert "direction" by using 1 - scale)
exp_afs <- exp_y %>%
  add_row(year = date_end, delta = 0, exp_y = 1) %>%
  filter(year <= scen_begin + 25) %>%
  mutate(scale = 1 - (exp_y - min(exp_y))/(1-min(exp_y)))

flaeche.ina <- exp_afs %>%
  full_join(kareb %>% select(QuarCd, ownerCd = EigentumGrundstkCd, Inanspruchnahme),
            by = character()) %>%
  mutate(flaeche.ina = Inanspruchnahme * scale)

# prepare output data
# this is still according to the format of last year (2021), transposing to long follows further down
downscale <-
  # spa_dyw provides general structure incl. area, apartments and people for the current year
  # plus living area per person (wf.ksj) for all years
  spa_dyw_past_pred %>%
  filter(year >= scen_begin - 1) %>%
  left_join(lookup_map, by = "district") %>%
  left_join(tibble(ownerCd = labels(uni_w), owner = uni_w), by = "owner") %>%
  rename(wohnflaeche.lbj = area, wf.ksj = spa_dyw_all, anz.wohn.ksj = apartments, anz.pers.ksj = people)%>%

  # add the population for the future years and calculate living area (wohnflaeche.ksj)
  left_join(pop_total, by = c("district", "year", "owner")) %>%
  mutate(anz.pers.ksj = rowSums(tibble(.$anz.pers.ksj, .$pop), na.rm = TRUE)) %>%
  mutate(wohnflaeche.ksj = wf.ksj * anz.pers.ksj) %>%
  select(-pop) %>%
  
  # add bq.ksj from allocation and calculate number of future apartments
  left_join(aca_comb, by = c("district", "year", "owner")) %>%
  mutate(bq.ksj = aca) %>%
  select(-aca_dyw, -aca_yw, -aca_52p, -aca, -apart_thres) %>%
  mutate(anz.wohn.ksj = anz.pers.ksj / bq.ksj) %>%
  
  # calculate delta population (future years - current year) and derive flaeche.ina.eff
  #  (also add the part coming from car_sc (Proportion of staircases; for the conversion from total area to living area))
  left_join((spa_dyw %>% filter(year == max(year)) %>% select(district, owner, people)), by = c("district", "owner")) %>%
  mutate(delta_pop = anz.pers.ksj - people) %>%
  mutate(flaeche.ina.eff = delta_pop * wf.ksj * (100+car_sc)/100) %>%
  
  # add flaeche.ina and calculate ratio ina.eff/ina
  left_join(flaeche.ina, by = c("QuarCd", "ownerCd", "year")) %>%
  mutate(ratio.ina = flaeche.ina.eff / flaeche.ina) %>%
  
  # select the desired columns
  select(
    year, QuarCd, district, ownerCd, owner, wohnflaeche.lbj, wohnflaeche.ksj, flaeche.ina, flaeche.ina.eff,
    ratio.ina, bq.ksj, wf.ksj, anz.wohn.ksj, anz.pers.ksj
  )

# transpose to long format
# consolidate ksj and lbj figures into one
downscale_long <- downscale %>%
  rename(districtCd = QuarCd,
         wohnflaeche = wohnflaeche.ksj,
         bq = bq.ksj,
         wfp = wf.ksj,
         anz.wohn = anz.wohn.ksj,
         anz.pers = anz.pers.ksj) %>%
  select(year, districtCd, district, ownerCd, owner, wohnflaeche, flaeche.ina, flaeche.ina.eff, ratio.ina, bq, wfp, anz.wohn, anz.pers) %>%
  filter(year <= scen_begin + 25) %>%
  pivot_longer(cols =  c(wohnflaeche, flaeche.ina, flaeche.ina.eff, ratio.ina, bq, wfp, anz.wohn, anz.pers),
               names_to = "type")


# write output
downscale_long %>%
  write_delim(req_path, delim = ";")


# create some plots for plausibility testing ------------------------------
# show the 8 variables over time (summarised)
downscale_long %>%
  group_by(year, type) %>%
  summarise(value = sum(value),
            .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "value",
          geom = "line",
          labs_x = "year", labs_y = "value",
          angle = 90,
          gridscale = "free_y",
          name = "15C1_downscale_y",
          width = 20, height = 14,
          wrap = "type", ncol = 3,
          quotes = quote(expand_limits(y = 0)))

# show the 8 variables over time, differentiated by ownership,
# one page per district
downscale_long %>%
  group_by(year, district, owner, type) %>%
  summarise(value = sum(value)) %>%
  sszplot(aes_x = "year", aes_y = "value",  aes_col = "owner",
          geom = "line",
          labs_x = "year", labs_y = "value",
          angle = 90,
          gridscale = "free_y",
          name = "15C2_downscale_ydw",
          width = 20, height = 14,
          wrap = "type", ncol = 3,
          quotes = quote(expand_limits(y = 0)),
          multi = uni_d)


# cleanup work ------------------------------------------------------------

# exchange KaReB file back to original
file.rename(paste0(inp_path, "KaReB.csv"), paste0(inp_path, "KaReB_2040.csv"))
file.rename(paste0(inp_path, "KaReB_2016.csv"), paste0(inp_path, "KaReB.csv"))
