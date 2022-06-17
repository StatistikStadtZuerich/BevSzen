# header -------------------------------------------------------------------
# prepare data for csv output files which are later loaded into a DWH

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
births <-
  read_csv(files[str_detect(files, "births_future")][1]) %>%
  mutate(VersionArtCd = 1) %>%
  bind_rows(read_csv(files[str_detect(files, "births_future")][2]) %>%
    mutate(VersionArtCd = 2)) %>%
  bind_rows(read_csv(files[str_detect(files, "births_future")][3]) %>%
    mutate(VersionArtCd = 3)) %>%
  mutate(PublJahr = scen_begin, 
         BasisSzenarienCd = 2, 
         AlterVCd = 0) %>%
  rename("Jahr" = year,
         "QuarCd" = district, 
         "AnzGebuWir" = bir, 
         "SexCd" = sex,
         "HerkunftCd" = origin)

# read population data and adapt structure
pop <-
  read_csv(files[str_detect(files, "population_future")][1]) %>%
  mutate(VersionArtCd = 1) %>%
  bind_rows(read_csv(files[str_detect(files, "population_future")][2]) %>%
    mutate(VersionArtCd = 2)) %>%
  bind_rows(read_csv(files[str_detect(files, "population_future")][3]) %>%
    mutate(VersionArtCd = 3)) %>%
  mutate(PublJahr = scen_begin,
         BasisSzenarienCd = 2) %>%
  rename("Jahr" = year, 
         "QuarCd" = district, 
         "AnzBestWir" = pop, 
         "SexCd" = sex, 
         "HerkunftCd" = origin, 
         "AlterVCd" = age)

# read naturalization data and adapt structure
nat <-
  read_csv(files[str_detect(files, "naturalization_future")][1]) %>%
  mutate(VersionArtCd = 1) %>%
  bind_rows(read_csv(files[str_detect(files, "naturalization_future")][2]) %>%
    mutate(VersionArtCd = 2)) %>%
  bind_rows(read_csv(files[str_detect(files, "naturalization_future")][3]) %>%
    mutate(VersionArtCd = 3)) %>%
  mutate(PublJahr = scen_begin, 
         BasisSzenarienCd = 2, 
         HerkunftCd = "foreign") %>%
  rename("Jahr" = year, 
         "QuarCd" = district, 
         "AnzEinbWir" = nat, 
         "SexCd" = sex, 
         "AlterVCd" = age)

# read demographic data and adapt structure
demo <-
  read_csv(files[str_detect(files, "demographic")][1]) %>%
  mutate(VersionArtCd = 1) %>%
  bind_rows(read_csv(files[str_detect(files, "demographic")][2]) %>%
    mutate(VersionArtCd = 2)) %>%
  bind_rows(read_csv(files[str_detect(files, "demographic")][3]) %>%
    mutate(VersionArtCd = 3)) %>%
  mutate(PublJahr = scen_begin, 
         BasisSzenarienCd = 2) %>%
  rename("Jahr" = year, 
         "QuarCd" = district, 
         "AnzZuzuWir" = ims, 
         "AnzWezuWir" = ems, 
         "AnzUmzuWir" = rei, 
         "AnzSterWir" = dea, 
         "SexCd" = sex, 
         "HerkunftCd" = origin, 
         "AlterVCd" = age)

# join the relevant data, change char to codes, write output file
pop %>%
  left_join(demo, 
            by = c("Jahr", "PublJahr", "BasisSzenarienCd", "VersionArtCd", "AlterVCd",
                   "SexCd", "HerkunftCd", "QuarCd")) %>%
  left_join(nat, 
            by = c("Jahr", "PublJahr", "BasisSzenarienCd", "VersionArtCd", "AlterVCd", 
                   "SexCd", "HerkunftCd", "QuarCd")) %>%
  left_join(births, 
            by = c("Jahr", "PublJahr", "BasisSzenarienCd", "VersionArtCd", "AlterVCd",
                   "SexCd", "HerkunftCd", "QuarCd")) %>%
  select(
    Jahr, PublJahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, HerkunftCd, QuarCd,
    AnzBestWir, AnzGebuWir, AnzZuzuWir, AnzWezuWir, AnzUmzuWir, AnzEinbWir, AnzSterWir
  ) %>%
  left_join(look_reg, 
            by = c("QuarCd" = "district")) %>%
  mutate(
    SexCd = case_when(
      SexCd == "male" ~ 1,
      SexCd == "female" ~ 2
    ),
    HerkunftCd = case_when(
      HerkunftCd == "Swiss" ~ 1,
      HerkunftCd == "foreign" ~ 2
    ),
    QuarCd = distnum
  ) %>%
  select(-distnum) %>%
  write_delim(paste0(dwh_path, "DM_BEV.csv"), delim = ";")


# capacity and reserves ---------------------------------------------------

read_csv("2_Data/1_Input/KaReB.csv") %>%
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
