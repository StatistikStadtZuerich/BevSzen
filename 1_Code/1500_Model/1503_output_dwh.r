# header -------------------------------------------------------------------
# prepare data for csv output files which are later loaded into a DWH


# prep work ---------------------------------------------------------------
source("1_Code/0000_General/0002_general_without-parameters.r")

scen_begin <- para$middle[para$parameter == "scen_begin"]

dwh_path <- paste0(data_path, "7_DWH/")
if (!dir.exists(dwh_path)) {
  dir.create(dwh_path, recursive = TRUE)
}

# population data ---------------------------------------------------------

# get list of relevant input data files
files <- paste0(paste0(data_path, "5_Outputs/"),
                list.files(path = paste0(data_path, "5_Outputs/"), recursive = TRUE))

# read birth data and adapt structure
read_files <- files[str_detect(files, "births_future")]
births <-
  # lower scenario
  read_csv(read_files[1], lazy = FALSE) %>%
  mutate(VersionArtCd = 1) %>%
  # middle scenario
  bind_rows(read_csv(read_files[2], lazy = FALSE) %>%
    mutate(VersionArtCd = 2)) %>%
  # upper scenario
  bind_rows(read_csv(read_files[3], lazy = FALSE) %>%
    mutate(VersionArtCd = 3)) %>%
  mutate(BasisSzenarienCd = 2, 
         AlterVCd = 0) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  left_join(tibble(origin = levels(uni_o),
                   HerkunftCd = as.numeric(labels(uni_o)))) %>%
  rename("Jahr" = year,
         "AnzGebuWir" = bir) %>%
  # add past data
  bind_rows(read_csv(bir_od, lazy = FALSE) %>%
            left_join(look_dis, by = "QuarCd") %>%
            rename("Jahr" = EreignisDatJahr,
                   "district" = distr) %>%
            mutate(BasisSzenarienCd = 1,
                   AlterVCd = 0,
                   VersionArtCd = 1) %>%
            group_by(Jahr, BasisSzenarienCd, VersionArtCd, SexCd, 
                       HerkunftCd, district) %>%
            summarize(AnzGebuWir = sum(AnzGebuWir),
                        .groups = "drop")
  ) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd,
         HerkunftCd, district, AnzGebuWir)


# read population data and adapt structure
read_files <- files[str_detect(files, "population_future")]
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
  mutate(BasisSzenarienCd = 2) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  left_join(tibble(origin = levels(uni_o),
                   HerkunftCd = as.numeric(labels(uni_o)))) %>%
  rename("Jahr" = year, 
         "AnzBestWir" = pop,
         "AlterVCd" = age) %>%
  # add past data
  bind_rows(read_csv(pop_od, lazy = FALSE) %>%
              left_join(look_dis, by = "QuarCd") %>%
              rename("Jahr" = StichtagDatJahr,
                     "district" = distr) %>%
              mutate(BasisSzenarienCd = 1,
                     VersionArtCd = 1) %>%
              group_by(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
                       HerkunftCd, district) %>%
              summarize(AnzBestWir = sum(AnzBestWir),
                        .groups = "drop")
  ) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzBestWir)


# read naturalization data and adapt structure
read_files <- files[str_detect(files, "naturalization_future")]
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
  mutate(BasisSzenarienCd = 2, 
         HerkunftCd = 1) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  rename("Jahr" = year, 
         "AnzEinbWir" = nat,
         "AlterVCd" = age) %>% 
  # add past data
  bind_rows(read_csv(nat_od, lazy = FALSE) %>%
              filter((HerkunftBisherCd == 2) & (HerkunftCd == 1)) %>%
              left_join(look_dis, by = "QuarCd") %>%
              rename("Jahr" = EreignisDatJahr,
                   "district" = distr) %>%
              mutate(BasisSzenarienCd = 1,
                   VersionArtCd = 1)
) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzEinbWir)


# read demographic data and adapt structure
read_files <- files[str_detect(files, "demographic")]
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
  mutate(BasisSzenarienCd = 2) %>%
  # get codes instead of text
  left_join(tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))) %>%
  left_join(tibble(origin = levels(uni_o),
                   HerkunftCd = as.numeric(labels(uni_o)))) %>%
  rename("Jahr" = year, 
         "AnzZuzuWir" = ims, 
         "AnzWezuWir" = ems, 
         "AnzUmzuWir" = rei, 
         "AnzSterWir" = dea,
         "AlterVCd" = age)  %>%
  # add past data
  # death
  bind_rows(read_csv(dea_od, lazy = FALSE) %>%
              left_join(look_dis, by = "QuarCd") %>%
              rename("Jahr" = EreignisDatJahr,
                     "district" = distr) %>%
              mutate(BasisSzenarienCd = 1,
                     VersionArtCd = 1) %>%
              group_by(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
                       HerkunftCd, district) %>%
              summarize(AnzSterWir = sum(AnzSterWir),
                        .groups = "drop")
  ) %>%  
  # immigration
  bind_rows(read_csv(imm_od, lazy = FALSE) %>%
              left_join(look_dis, by = "QuarCd") %>%
              rename("Jahr" = EreignisDatJahr,
                   "district" = distr) %>%
              mutate(BasisSzenarienCd = 1,
                   VersionArtCd = 1) %>%
              group_by(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
                       HerkunftCd, district) %>%
              summarize(AnzZuzuWir = sum(AnzZuzuWir),
                        .groups = "drop")
            ) %>%
  # emigration
  bind_rows(read_csv(emi_od, lazy = FALSE) %>%
              left_join(look_dis, by = "QuarCd") %>%
              rename("Jahr" = EreignisDatJahr,
                     "district" = distr) %>%
              mutate(BasisSzenarienCd = 1,
                     VersionArtCd = 1) %>%
              group_by(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
                       HerkunftCd, district) %>%
              summarize(AnzWezuWir = sum(AnzWezuWir),
                        .groups = "drop")
  ) %>%
  # relocation
  bind_rows(read_csv(rel_od, lazy = FALSE) %>%
              left_join(look_dis, by = "QuarCd") %>%
              rename("Jahr" = EreignisDatJahr,
                     "district" = distr) %>%
              mutate(BasisSzenarienCd = 1,
                     VersionArtCd = 1) %>%
              group_by(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
                       HerkunftCd, district) %>%
              summarize(AnzUmzuWir = sum(AnzUmzuWir),
                        .groups = "drop")
  ) %>%
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzZuzuWir, AnzWezuWir, AnzUmzuWir, AnzSterWir)


# join the relevant data, write output file
pop %>%
  full_join(demo) %>%
  full_join(nat) %>%
  full_join(births) %>%
  left_join(look_reg, by = c("district")) %>%
  mutate(QuarCd = distnum,
         PublJahr = scen_begin) %>%
  select(
    Jahr, PublJahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, HerkunftCd, QuarCd,
    AnzBestWir, AnzGebuWir, AnzZuzuWir, AnzWezuWir, AnzUmzuWir, AnzEinbWir, AnzSterWir
  ) %>%
  write_delim(paste0(dwh_path, "DM_BEV.csv"), delim = ";")


# capacity and reserves ---------------------------------------------------

read_csv("2_Data/1_Input/KaReB.csv", lazy = FALSE) %>%
  pivot_longer(
    cols = c("Kapazitaet", "Bestand", "Reserve", "Inanspruchnahme"),
    names_to = "BereichCd",
    values_to = "BruttoGeschFlaeche"
  ) %>%
  select("PublJahr", "QuarCd", "EigentumGrundstkCd", "BereichCd", "ArealCd", 
         "WohnanteilCd", "BruttoGeschFlaeche") %>%
  mutate(BereichCd = case_when(
    BereichCd == "Bestand" ~ 1,
    BereichCd == "Inanspruchnahme" ~ 2,
    BereichCd == "Kapazitaet" ~ 3,
    BereichCd == "Reserve" ~ 4
  )) %>%
  mutate(BruttoGeschFlaeche = as.integer(BruttoGeschFlaeche)) %>%
  filter(PublJahr == scen_begin) %>%
  write_delim(paste0(dwh_path, "DM_KAREB.csv"), delim = ";")
