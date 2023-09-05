# header -------------------------------------------------------------------
# prepare data for csv output files which are later loaded into a DWH

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# prep work ---------------------------------------------------------------
params <- init("middle")

# output path creation
dwh_path <- paste0(data_path, "7_DWH/")
dir_ex_create(dwh_path)

# population data ---------------------------------------------------------

# get list of model output files as input data for DWH
files_output <- paste0(paste0(data_path, "5_Outputs/"),
                       list.files(path = paste0(data_path, "5_Outputs/"), recursive = TRUE))

# read birth data and adapt structure -------------------------------------

dwh_path <- paste0(data_path, "7_DWH/")
dir_ex_create(dwh_path)

# population data ---------------------------------------------------------

# get list of model output files as input data for DWH
files_output <- paste0(paste0(data_path, "5_Outputs/"),
                       list.files(path = paste0(data_path, "5_Outputs/"), recursive = TRUE))

# read birth data and adapt structure -------------------------------------

# past data
bir_past <- read_csv(bir_od, lazy = FALSE) %>%
  left_join(look_dis, by = "QuarCd") %>%
  rename("Jahr" = EreignisDatJahr,
         "district" = distr) %>%
  mutate(BasisSzenarienCd = basis_fact, # de facto value from past
         AlterVCd = 0) %>%
  group_by(Jahr,
           SexCd,
           AlterVCd,
           HerkunftCd,
           district,
           BasisSzenarienCd) %>%
  summarize(AnzGebuWir = sum(AnzGebuWir),
            .groups = "drop")

# scenario data
read_files <- files_output[str_detect(files_output, "births_future")]
stop_3(read_files)

births <-
  # lower scenario (VersionArtCd = 1)
  read_csv(read_files[1], lazy = FALSE) %>%
  mutate(VersionArtCd = 1) %>%
  # middle scenario
  bind_rows(read_csv(read_files[2], lazy = FALSE) %>%
              mutate(VersionArtCd = 2)) %>%
  # upper scenario
  bind_rows(read_csv(read_files[3], lazy = FALSE) %>%
              mutate(VersionArtCd = 3)) %>%
  mutate(BasisSzenarienCd = basis_scen,  # calculated scenario value
         AlterVCd = 0) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  left_join(tibble(origin = levels(uni_o),
                   HerkunftCd = as.numeric(labels(uni_o)))) %>%
  rename("Jahr" = year,
         "AnzGebuWir" = bir) %>%
  # add past data, once for each VersionArtCd (DWH necessity)
  bind_rows(bir_past %>%
              mutate(VersionArtCd = 1)) %>%
  bind_rows(bir_past %>%
              mutate(VersionArtCd = 2)) %>%
  bind_rows(bir_past %>%
              mutate(VersionArtCd = 3)) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd,
         HerkunftCd, district, AnzGebuWir)

# read population data and adapt structure --------------------------------

# past data
pop_past <- read_csv(pop_od, lazy = FALSE) %>%
  left_join(look_dis, by = "QuarCd") %>%
  rename("Jahr" = StichtagDatJahr,
         "district" = distr) %>%
  mutate(BasisSzenarienCd = basis_fact) %>%
  group_by(Jahr,
           AlterVCd,
           SexCd,
           HerkunftCd,
           district,
           BasisSzenarienCd) %>%
  summarize(AnzBestWir = sum(AnzBestWir),
            .groups = "drop") 

#scenario data
read_files <- files_output[str_detect(files_output, "population_future")]
stop_3(read_files)

pop <-
  # lower scenario
  read_csv(read_files[1], lazy = FALSE) %>%
  mutate(VersionArtCd = 1) %>%
  # middle scenario
  bind_rows(read_csv(read_files[2], lazy = FALSE) %>%
              mutate(VersionArtCd = 2)) %>%
  # upper scenario
  bind_rows(read_csv(read_files[3], lazy = FALSE) %>%
              mutate(VersionArtCd = 3)) %>%
  mutate(BasisSzenarienCd = basis_scen) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  left_join(tibble(origin = levels(uni_o),
                   HerkunftCd = as.numeric(labels(uni_o)))) %>%
  rename("Jahr" = year, 
         "AnzBestWir" = pop,
         "AlterVCd" = age) %>%
  # add past data, once for each VersionArtCd (DWH necessity)
  bind_rows(pop_past %>%
              mutate(VersionArtCd = 1)) %>%
  bind_rows(pop_past %>%
              mutate(VersionArtCd = 2)) %>%
  bind_rows(pop_past %>%
              mutate(VersionArtCd = 3)) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzBestWir)

# read naturalization data and adapt structure ----------------------------

# past data
nat_past <- read_csv(nat_od, lazy = FALSE) %>%
  filter((HerkunftBisherCd == 2) & (HerkunftCd == 1)) %>%
  left_join(look_dis, by = "QuarCd") %>%
  rename("Jahr" = EreignisDatJahr,
         "district" = distr) %>%
  mutate(BasisSzenarienCd = basis_fact) %>%
  group_by(Jahr,
           AlterVCd,
           SexCd,
           HerkunftCd,
           district,
           BasisSzenarienCd) %>%
  summarize(AnzEinbWir = sum(AnzEinbWir),
            .groups = "drop") 

# scenario data 
read_files <- files_output[str_detect(files_output, "naturalization_future")]
stop_3(read_files)

nat <-
  # lower scenario
  read_csv(read_files[1], lazy = FALSE) %>%
  mutate(VersionArtCd = 1) %>%
  # middle scenario
  bind_rows(read_csv(read_files[2], lazy = FALSE) %>%
              mutate(VersionArtCd = 2)) %>%
  # upper scenario
  bind_rows(read_csv(read_files[3], lazy = FALSE) %>%
              mutate(VersionArtCd = 3)) %>%
  mutate(BasisSzenarienCd = basis_scen, 
         HerkunftCd = 1) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  rename("Jahr" = year, 
         "AnzEinbWir" = nat,
         "AlterVCd" = age) %>%
  # add past data, once for each VersionArtCd (DWH necessity)
  bind_rows(nat_past %>%
              mutate(VersionArtCd = 1)) %>%
  bind_rows(nat_past %>%
              mutate(VersionArtCd = 2)) %>%
  bind_rows(nat_past %>%
              mutate(VersionArtCd = 3)) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzEinbWir)

# read demographic data and adapt structure -------------------------------

# past data
# death
demo_past <- read_csv(dea_od, lazy = FALSE) %>%
  # immigration
  bind_rows(
    read_csv(imm_od, lazy = FALSE)) %>%
  # emigration
  bind_rows(
    read_csv(emi_od, lazy = FALSE)) %>%
  left_join(look_dis, by = "QuarCd") %>%
  rename("Jahr" = EreignisDatJahr,
         "district" = distr) %>%
  mutate(BasisSzenarienCd = basis_fact) %>%
  group_by(Jahr,
           AlterVCd,
           SexCd,
           HerkunftCd,
           district,
           BasisSzenarienCd) %>%
  summarize(AnzSterWir = sum(AnzSterWir, na.rm = TRUE),
            AnzWezuWir = sum(AnzWezuWir, na.rm = TRUE),
            AnzZuzuWir = sum(AnzZuzuWir, na.rm = TRUE),
            .groups = "drop")

# scenario data
read_files <- files_output[str_detect(files_output, "demographic")]
stop_3(read_files)

demo <-
  # lower scenario
  read_csv(read_files[1], lazy = FALSE) %>%
  mutate(VersionArtCd = 1) %>%
  # middle scenario
  bind_rows(read_csv(read_files[2], lazy = FALSE) %>%
              mutate(VersionArtCd = 2)) %>%
  # upper scenario
  bind_rows(read_csv(read_files[3], lazy = FALSE) %>%
              mutate(VersionArtCd = 3)) %>%
  mutate(BasisSzenarienCd = basis_scen) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  left_join(tibble(origin = levels(uni_o),
                   HerkunftCd = as.numeric(labels(uni_o)))) %>%
  rename("Jahr" = year, 
         "AnzZuzuWir" = imm, 
         "AnzWezuWir" = emi, 
         "AnzSterWir" = dea,
         "AlterVCd" = age)  %>%
  # add past data, once for each VersionArtCd (DWH necessity)
  bind_rows(demo_past %>%
              mutate(VersionArtCd = 1)) %>%
  bind_rows(demo_past %>%
              mutate(VersionArtCd = 2)) %>%
  bind_rows(demo_past %>%
              mutate(VersionArtCd = 3)) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzZuzuWir, AnzWezuWir, AnzSterWir)

# control plots -----------------------------------------------------------

births %>%
  group_by(Jahr, VersionArtCd) %>%
  summarise(birth = sum_NA(AnzGebuWir), .groups = "drop") %>%
  sszplot(aes_x = "Jahr", aes_y = "birth", aes_col = "VersionArtCd",
          labs_x = "year", labs_y = "frequency")
# name = "1590_bir_yc")

pop %>%
  group_by(Jahr, VersionArtCd) %>%
  summarise(pop = sum_NA(AnzBestWir), .groups = "drop") %>%
  sszplot(aes_x = "Jahr", aes_y = "pop", aes_col = "VersionArtCd",
          labs_x = "year", labs_y = "frequency")
# name = "1591_pop_yc")

nat %>%
  group_by(Jahr, VersionArtCd) %>%
  summarise(nat = sum_NA(AnzEinbWir), .groups = "drop") %>%
  sszplot(aes_x = "Jahr", aes_y = "nat", aes_col = "VersionArtCd",
          labs_x = "year", labs_y = "frequency")
# name = "1592_nat_yc")

demo %>%
  group_by(Jahr, VersionArtCd) %>%
  summarise(imm = sum_NA(AnzZuzuWir), 
            emi = sum_NA(AnzWezuWir),
            dea = sum_NA(AnzSterWir),
            .groups = "drop") %>%
  pivot_longer(cols = c(imm, emi, dea),
               names_to = "type") %>%
  sszplot(aes_x = "Jahr", aes_y = "value", aes_col = "VersionArtCd",
          wrap = "type",
          labs_x = "year", labs_y = "frequency",
          gridscale = "free",
          width = 10, height = 5)
# name = "1593_demo_yc")

# combine population data -------------------------------------------------

# join the above tibbles, write output file
pop %>%
  full_join(demo) %>%
  full_join(nat) %>%
  full_join(births) %>%
  left_join(look_reg, by = c("district")) %>%
  mutate(QuarCd = distnum,
         PublJahr = scen_begin) %>%
  select(
    Jahr, PublJahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, HerkunftCd, QuarCd,
    AnzBestWir, AnzGebuWir, AnzZuzuWir, AnzWezuWir, AnzEinbWir, AnzSterWir
  ) %>%
  # DWH requirement: NAs as 0
  mutate(across(everything(), ~ replace_na(.x, replace = 0))) %>%
  # as.character to avoid things like 9.12e-04 etc. in csv file
  mutate(across(everything(), as.character)) %>%
  mutate(AlterVCd = case_when(
    nchar(AlterVCd) == 1 ~ paste0("00", AlterVCd),
    nchar(AlterVCd) == 2 ~ paste0("0", AlterVCd),
    nchar(AlterVCd) == 3 ~ AlterVCd
  )) %>%
  mutate(QuarCd = case_when(
    nchar(QuarCd) == 2 ~ paste0("0", QuarCd),
    nchar(QuarCd) == 3 ~ QuarCd
  )) %>%
  write_delim(paste0(dwh_path, "DM_BEV.csv"), delim = ";")

# capacity and reserves ---------------------------------------------------

# read data, bring to long format and adjust BereichCd
read_csv(car_path, lazy = FALSE) %>%
  pivot_longer(
    cols = car_initial,
    names_to = "BereichCd",
    values_to = "BruttoGeschFlaeche"
  ) %>%
  select("PublJahr", "QuarCd", "EigentumGrundstkCd", "BereichCd", "ArealCd", 
         "WohnanteilCd", "BruttoGeschFlaeche") %>%
  mutate(BereichCd = case_when(
    BereichCd == car_initial[2] ~ 1,
    BereichCd == car_initial[4] ~ 2,
    BereichCd == car_initial[1] ~ 3,
    BereichCd == car_initial[3] ~ 4
  )) %>%
  mutate(BruttoGeschFlaeche = as.integer(BruttoGeschFlaeche),
         ArealCd = as.factor(ArealCd),
         WohnanteilCd = as.factor(WohnanteilCd),
         BereichCd = as.factor(BereichCd)) %T>%
  # write data after filtering into csv, then proceed unfiltered for plot
  { write_delim(filter(., PublJahr == scen_begin),
                paste0(dwh_path, "DM_KAREB.csv"),
                delim = ";")  } %>%
  # prepare control plot
  group_by(PublJahr, WohnanteilCd, ArealCd, BereichCd) %>%
  summarize(BruttoGeschFlaeche = sum(BruttoGeschFlaeche),
            .groups = "drop") %>%
  mutate(BruttoGeschFlaeche = BruttoGeschFlaeche/10000) %>%
  sszplot(aes_x = "PublJahr", aes_y = "BruttoGeschFlaeche",
          aes_col = "WohnanteilCd", aes_ltyp = "ArealCd",
          geom = c("line", "point"),
          wrap = "BereichCd", ncol = 2, gridscale = "free",
          labs_x = "publication year", labs_y = "area (ha)",
          scale_y = c(0, NA),
          width = 10, height = 8)
#name = "1594_cap_res")

# habitation --------------------------------------------------------------

# get list of model output files as input data for DWH
files_rate <- paste0(paste0(data_path, "4_Rates/"),
                     list.files(path = paste0(data_path, "4_Rates/"), recursive = TRUE))

# consumption rate
read_files <- files_rate[str_detect(files_rate, "living-space_future")]
stop_3(read_files)

consumption <- read_csv(read_files[1]) %>%
  mutate(VersionArtCd = 1) %>%
  add_row(read_csv(read_files[2]) %>%
            mutate(VersionArtCd = 2)) %>%
  add_row(read_csv(read_files[3]) %>%
            mutate(VersionArtCd = 3)) %>%
  rename(WohnungsflProPers = spa_dyw)

# occupancy rate 
read_files <- files_rate[str_detect(files_rate, "allocation_future")]
stop_3(read_files)

occupancy <- read_csv(read_files[1]) %>%
  mutate(VersionArtCd = 1) %>%
  add_row(read_csv(read_files[2]) %>%
            mutate(VersionArtCd = 2)) %>%
  add_row(read_csv(read_files[3]) %>%
            mutate(VersionArtCd = 3)) %>%
  rename(PersProWhg = aca_dyw)

# population data (needed for calculation of area and apartments)
read_files <- files_rate[str_detect(files_rate, "housing_model_population_dw")]
stop_3(read_files)

pop_dw <- read_csv(read_files[1]) %>%
  mutate(VersionArtCd = 1) %>%
  add_row(read_csv(read_files[2]) %>%
            mutate(VersionArtCd = 2)) %>%
  add_row(read_csv(read_files[3]) %>%
            mutate(VersionArtCd = 3))

# area data
join_cond <- c("year", "VersionArtCd", "district", "owner")
area <- consumption %>%
  left_join(pop_dw, by = join_cond) %>%
  mutate(BruttoGeschFlaeche = pop * WohnungsflProPers / 10000) # in ha

# apartments data
apartments <- occupancy %>%
  left_join(pop_dw, by = join_cond) %>%
  mutate(AnzWhgStat = pop / PersProWhg)

# plausibility plots
consumption %>%
  sszplot(aes_x = "year", aes_y = "WohnungsflProPers", aes_col = "VersionArtCd", aes_ltyp = "owner",
          labs_x = "year", labs_y = "m2/person", labs_col = "Scenario",
          quotes = quote(scale_color_manual(labels = c("lower", "middle", "upper"),
                                            values = c("blue", "green", "red"))),
          wrap = "district")

occupancy %>%
  sszplot(aes_x = "year", aes_y = "PersProWhg ", aes_col = "VersionArtCd", aes_ltyp = "owner",
          labs_x = "year", labs_y = "persons/apartment",
          quotes = quote(scale_color_manual(labels = c("lower", "middle", "upper"),
                                            values = c("blue", "green", "red"))),
          wrap = "district")

area %>%
  sszplot(aes_x = "year", aes_y = "BruttoGeschFlaeche ", aes_col = "VersionArtCd", aes_ltyp = "owner",
          labs_x = "year", labs_y = "area (ha)",
          quotes = quote(scale_color_manual(labels = c("lower", "middle", "upper"),
                                            values = c("blue", "green", "red"))),
          wrap = "district")

apartments %>%
  sszplot(aes_x = "year", aes_y = "AnzWhgStat ", aes_col = "VersionArtCd", aes_ltyp = "owner",
          labs_x = "year", labs_y = "apartments",
          quotes = quote(scale_color_manual(labels = c("lower", "middle", "upper"),
                                            values = c("blue", "green", "red"))),
          wrap = "district")

# combine datasets and write output
area %>%
  left_join(apartments, by = join_cond) %>%
  left_join(look_reg, by = "district") %>%
  left_join(look_own, by = "owner") %>%
  rename(Jahr = year) %>%
  mutate(BasisSzenarienCd = if_else(Jahr < scen_begin, basis_fact, basis_scen), # calculated scenario value
         PublJahr = scen_begin,
         QuarCd = distnum)  %>%
  mutate(BruttoGeschFlaeche = round(BruttoGeschFlaeche, round_rate),
         AnzWhgStat = round(AnzWhgStat, round_rate),
         WohnungsflProPers = round(WohnungsflProPers, round_rate),
         PersProWhg = round(PersProWhg, round_rate)) %>%
  # DWH requirement: NAs as 0
  mutate(across(everything(), ~ replace_na(.x, replace = 0))) %>%
  # as.character to avoid things like 9.12e-04 etc. in csv file
  mutate(across(everything(), as.character)) %>%
  mutate(QuarCd = case_when(
    nchar(QuarCd) == 2 ~ paste0("0", QuarCd),
    nchar(QuarCd) == 3 ~ QuarCd
  )) %>%
  select(Jahr, PublJahr, VersionArtCd, BasisSzenarienCd, QuarCd, EigentumGrundstkCd,
         BruttoGeschFlaeche, AnzWhgStat, PersProWhg, WohnungsflProPers) %>%
  write_delim(paste0(dwh_path, "DM_WOHNEN.csv"), delim = ";")
