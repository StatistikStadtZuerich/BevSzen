#' Outputs for DWH
#'
#' @param scenarios scenarios for the DWH
#'    the same options as in the model control function
#'
#'    usually in the DWH: the three scenarios (lower, middle, upper)
#'    and the birth versions (middle_birth_lower, middle_birth_upper)
#'
#'    BZO revision scenarios not in the DWH (since only not regular analysis),
#'    and the data can be found on the V-drive (for experts)
#'
#' @param params parameter vector
#'
#' @returns list with data for control plots
#'          dem_control_dat: demography
#'          car_control_dat: capacity and reserves
#'          hou_control_dat: housing
#'          
#' @export
#'
#' @examples outputs_dwh_fun(
#'     scenarios =c("lower", "middle", "upper", "middle_birth_lower", "middle_birth_upper"), 
#'     params = params)
outputs_dwh_fun <- function(scenarios, params = params){

  # start time
  t0 <- Sys.time()



  # parameter ---------------------------------------------------------------

  # paths
  pop_od <- params$pop_od
  bir_od <- params$bir_od
  dea_od <- params$dea_od
  imm_od <- params$imm_od
  emi_od <- params$emi_od  
  nat_od <- params$nat_od  
  car_path <- params$car_path
  exp_path <- params$exp_path
  out_path <- params$out_path
  dwh_path <- params$DWH_path
  
  

  # rel_od <- params$rel_od

  # sce_od <- params$sce_od
  # out_path <- params$out_path
  # log_file <- params$log_file

  # time
  scen_begin <- params$scen_begin
  
  
  
  # date_end <- params$date_end
  # scen_begin <- params$scen_begin
  # scen_end <- params$scen_end
  # scen_end_public <- params$scen_end_public
  # uniy_scen_public <- params$uniy_scen_public

  # text and lookup
  uni_d <- params$uni_d
  uni_s <- params$uni_s
  uni_o <- params$uni_o
  uni_e <- params$uni_e
  uni_w <- params$uni_w
  uni_p <- params$uni_p
  look_dis <- params$look_dis
  look_scen_dwh <- params$look_scen_dwh
  look_reg <- params$look_reg
  look_own <- params$look_own
  car_initial <- params$car_initial
  
  
  # additional lookup files
  look_s <- tibble(sex = levels(uni_s),
                   SexCd = as.numeric(labels(uni_s)))
  
  look_o <- tibble(origin = levels(uni_o),
         HerkunftCd = as.numeric(labels(uni_o)))

  # uni_c <- params$uni_c
  # uni_cb <- params$uni_cb
  # age_5 <- params$age_5
  # age_3t <- params$age_3t
  # age_5t <- params$age_5t

  # look_a3 <- params$look_a3
  # look_a5 <- params$look_a5
  # text_c <- params$text_c

  # options
  round_rate <- params$round_rate




# birth: past data --------------------------------------------------------

# import
# why remove QuarLang? to avoid two variables with the same name
  bir_past <- read_csv(bir_od, lazy = FALSE) |>
    select(-QuarLang) |>
    left_join(look_dis, by = "QuarCd") |>
    rename("Jahr" = EreignisDatJahr,
           "district" = distr) |>
    mutate(BasisSzenarienCd = 1,
           AlterVCd = 0) |>
    group_by(Jahr,
             SexCd,
             AlterVCd,
             HerkunftCd,
             district,
             BasisSzenarienCd) |>
    summarize(AnzGebuWir = sum(AnzGebuWir),
              .groups = "drop")




# birth: scenarios --------------------------------------------------------

# Check if all files are there
birth_files <- file.path(out_path, scenarios, "births_future.csv")

bir_missing_files <- birth_files[!file.exists(birth_files)]

if (length(bir_missing_files) > 0) {
  stop(
    "these birth files are missing:\n",
    paste(bir_missing_files, collapse = "\n")
  )
}

# import files
  births_future_all <- scenarios |>
    set_names() |>
    map_dfr(
      ~ read_csv(
        file.path(out_path, .x, "births_future.csv"),
        lazy = FALSE
      ),
      .id = "scenario"
    ) |>
    left_join(look_scen_dwh, by = "scenario")

# message: all birth files imported?
  if (n_distinct(births_future_all$scenario) != length(scenarios)) {
    stop("not all birth scenarios have been imported")
  }



# birth: output for DWH ---------------------------------------------------

# past data: multiple times
# why: for every scenario and version the past data is provided as well
  levels_version <- sort(unique(births_future_all$VersionArtCd))
  
  bir_past_multiple <- map_dfr(
    levels_version,
    ~ bir_past |> 
      mutate(VersionArtCd = .x)
  ) |> 
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
           HerkunftCd, district, AnzGebuWir)    
  
  
# future and past births
births_dwh <- births_future_all |> 
    # summarize (without age of mother)
    group_by(district, year, sex, origin, VersionArtCd) |>
    summarize(bir = sum_NA(bir), .groups = "drop") |> 
    mutate(BasisSzenarienCd = 2,
           AlterVCd = 0) |> 
    rename("Jahr" = year,
         "AnzGebuWir" = bir) |> 
    # get dwh-codes (Cd) instead of text/names
    left_join(look_s, by = "sex") |> 
    left_join(look_o, by = "origin") |> 
    select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzGebuWir) |> 
    bind_rows(bir_past_multiple)
    


# population: past data ---------------------------------------------------

# import
# why remove QuarLang? to avoid two variables with the same name
pop_past <- read_csv(pop_od, lazy = FALSE) |> 
  select(-QuarLang) |>
  left_join(look_dis, by = "QuarCd") |> 
  rename("Jahr" = StichtagDatJahr,
           "district" = distr) |> 
    mutate(BasisSzenarienCd = 1) |>
    group_by(Jahr,
             AlterVCd,
             SexCd,
             HerkunftCd,
             district,
             BasisSzenarienCd) |>
    summarize(AnzBestWir = sum(AnzBestWir),
              .groups = "drop")
  
  



# population: scenarios ---------------------------------------------------

# Check if all files are there
pop_files <- file.path(out_path, scenarios, "population_future.csv")

pop_missing_files <- pop_files[!file.exists(pop_files)]

if (length(pop_missing_files) > 0) {
  stop(
    "these population files are missing:\n",
    paste(pop_missing_files, collapse = "\n")
  )
}

# import files
pop_future_all <- scenarios |>
  set_names() |>
  map_dfr(
    ~ read_csv(
      file.path(out_path, .x, "population_future.csv"),
      lazy = FALSE
    ),
    .id = "scenario"
  ) |>
  left_join(look_scen_dwh, by = "scenario")


# message: all population files imported?
if (n_distinct(pop_future_all$scenario) != length(scenarios)) {
  stop("not all population scenarios have been imported")
}



# population: output for DWH ----------------------------------------------

# past data: multiple times
# why: for every scenario and version the past data is provided as well

pop_past_multiple <- map_dfr(
  levels_version,
  ~ pop_past |> 
    mutate(VersionArtCd = .x)
  ) |> 
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzBestWir)


# future and past population
pop_dwh <- pop_future_all |> 
  mutate(BasisSzenarienCd = 2) |> 
  rename("Jahr" = year, 
         "AnzBestWir" = pop,
         "AlterVCd" = age) |>   
  left_join(look_s, by = "sex") |> 
  left_join(look_o, by = "origin") |> 
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzBestWir) |> 
  bind_rows(pop_past_multiple)



# naturalization: past data -----------------------------------------------

# import
  nat_past <- read_csv(nat_od, lazy = FALSE) |>
    select(-QuarLang) |>
    filter((HerkunftBisherCd == 2) & (HerkunftCd == 1)) |>
    left_join(look_dis, by = "QuarCd") |>
    rename("Jahr" = EreignisDatJahr,
           "district" = distr) |>
    mutate(BasisSzenarienCd = 1) |>
    group_by(Jahr,
             AlterVCd,
             SexCd,
             HerkunftCd,
             district,
             BasisSzenarienCd) |>
    summarize(AnzEinbWir = sum(AnzEinbWir),
              .groups = "drop")



# naturalization: scenarios -----------------------------------------------

# Check if all files are there
nat_files <- file.path(out_path, scenarios, "naturalization_future.csv")

nat_missing_files <- nat_files[!file.exists(nat_files)]

if (length(nat_missing_files) > 0) {
  stop(
    "these naturalzation files are missing:\n",
    paste(nat_missing_files, collapse = "\n")
  )
}

# import files
nat_future_all <- scenarios |>
  set_names() |>
  map_dfr(
    ~ read_csv(
      file.path(out_path, .x, "naturalization_future.csv"),
      lazy = FALSE
    ),
    .id = "scenario"
  ) |>
  left_join(look_scen_dwh, by = "scenario")


# message: all naturalization files imported?
if (n_distinct(nat_future_all$scenario) != length(scenarios)) {
  stop("not all naturalization scenarios have been imported")
}



# naturalization: output for DWH ------------------------------------------

# past data: multiple times
nat_past_multiple <- map_dfr(
  levels_version,
  ~ nat_past |> 
    mutate(VersionArtCd = .x)
) |> 
  select(VersionArtCd, BasisSzenarienCd, 
         district, Jahr, AlterVCd, SexCd, HerkunftCd, AnzEinbWir)


# future and past naturalization
nat_dwh <- nat_future_all |> 
  mutate(BasisSzenarienCd = 2,
         HerkunftCd = 1) |> 
  rename("Jahr" = year, 
         "AnzEinbWir" = nat,
         "AlterVCd" = age) |> 
  left_join(look_s, by = "sex") |> 
  select(Jahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, 
         HerkunftCd, district, AnzEinbWir) |> 
  bind_rows(nat_past_multiple)




# other demographic processes: past data ----------------------------------

dem_past <- read_csv(dea_od, lazy = FALSE) |> 
  bind_rows(read_csv(imm_od, lazy = FALSE)) |> 
  bind_rows(read_csv(emi_od, lazy = FALSE)) |> 
  select(-QuarLang) |>
  left_join(look_dis, by = "QuarCd") |> 
  rename("Jahr" = EreignisDatJahr,
         "district" = distr) |> 
  mutate(BasisSzenarienCd = 1) |>
  group_by(Jahr,
           AlterVCd,
           SexCd,
           HerkunftCd,
           district,
           BasisSzenarienCd) |>
  summarize(AnzSterWir = sum(AnzSterWir, na.rm = TRUE),
            AnzWezuWir = sum(AnzWezuWir, na.rm = TRUE),
            AnzZuzuWir = sum(AnzZuzuWir, na.rm = TRUE),
            .groups = "drop")



# other demographic processes: scenarios ----------------------------------

# Check if all files are there
dem_files <- file.path(out_path, scenarios, "demographic-processes_future.csv")

dem_missing_files <- dem_files[!file.exists(dem_files)]

if (length(dem_missing_files) > 0) {
  stop(
    "these demography files are missing:\n",
    paste(dem_missing_files, collapse = "\n")
  )
}

# import files
dem_future_all <- scenarios |>
  set_names() |>
  map_dfr(
    ~ read_csv(
      file.path(out_path, .x, "demographic-processes_future.csv"),
      lazy = FALSE
    ),
    .id = "scenario"
  ) |>
  left_join(look_scen_dwh, by = "scenario")

# message: all demography files imported?
if (n_distinct(dem_future_all$scenario) != length(scenarios)) {
  stop("not all demography scenarios have been imported")
}



# other demographic processes: output for dwh -----------------------------

# past data: multiple times

dem_past_multiple <- map_dfr(
  levels_version,
  ~ dem_past |> 
    mutate(VersionArtCd = .x)
) |> 
  select(VersionArtCd, BasisSzenarienCd, 
         district, Jahr, AlterVCd, SexCd, HerkunftCd, 
         AnzSterWir, AnzWezuWir, AnzZuzuWir)


# future and past
dem_dwh <- dem_future_all |> 
  mutate(BasisSzenarienCd = 2) |> 
  rename("Jahr" = year, 
         "AnzZuzuWir" = imm, 
         "AnzWezuWir" = emi, 
         "AnzSterWir" = dea,
         "AlterVCd" = age) |> 
  left_join(look_s, by = "sex") |> 
  left_join(look_o, by = "origin") |> 
  select(VersionArtCd, BasisSzenarienCd, 
         district, Jahr, AlterVCd, SexCd, HerkunftCd, 
         AnzSterWir, AnzWezuWir, AnzZuzuWir) |> 
  bind_rows(dem_past_multiple)



# data for control plots --------------------------------------------------

bir_temp <- births_dwh |> 
  group_by(Jahr, VersionArtCd) |> 
  summarise(bir = sum_NA(AnzGebuWir), .groups = "drop")
 
pop_temp <- pop_dwh |>
  group_by(Jahr, VersionArtCd) |>
  summarise(pop = sum_NA(AnzBestWir), .groups = "drop")  
 
nat_temp <- nat_dwh |>
  group_by(Jahr, VersionArtCd) |>
  summarise(nat = sum_NA(AnzEinbWir), .groups = "drop")

dem_control_dat <- dem_dwh |>
  group_by(Jahr, VersionArtCd) |>
  summarise(imm = sum_NA(AnzZuzuWir),
            emi = sum_NA(AnzWezuWir),
            dea = sum_NA(AnzSterWir),
            .groups = "drop") |> 
  left_join(bir_temp, by = c("Jahr", "VersionArtCd")) |> 
  left_join(pop_temp, by = c("Jahr", "VersionArtCd")) |> 
  left_join(nat_temp, by = c("Jahr", "VersionArtCd")) |> 
  pivot_longer(cols = c(imm, emi, dea, pop, bir, nat),
               names_to = "type") |> 
  left_join(params$look_scen_dwh, by = "VersionArtCd") |> 
  mutate(type = factor(type, levels = c("pop", "bir", "dea", 
                                        "imm", "emi", "nat")),
         scenario = factor(scenario, levels = scenarios))
  
  
  


# combine population data -------------------------------------------------

# population: all data
pop_dat_all <- expand_grid(Jahr = sort(unique(pop_dwh$Jahr)),
            VersionArtCd = sort(unique(pop_dwh$VersionArtCd)),
            AlterVCd = sort(unique(pop_dwh$AlterVCd)),
            SexCd = sort(unique(pop_dwh$SexCd)),
            HerkunftCd = sort(unique(pop_dwh$HerkunftCd)),
            district = sort(unique(pop_dwh$district))) |> 
  mutate(BasisSzenarienCd = if_else(Jahr < scen_begin, 1, 2)) |> 
  left_join(pop_dwh, by = c("Jahr", "BasisSzenarienCd", "VersionArtCd", 
                            "AlterVCd", "SexCd", "HerkunftCd", "district")) |> 
  left_join(births_dwh, by = c("Jahr", "BasisSzenarienCd", "VersionArtCd", 
                            "AlterVCd", "SexCd", "HerkunftCd", "district")) |>  
  left_join(dem_dwh, by = c("Jahr", "BasisSzenarienCd", "VersionArtCd", 
                               "AlterVCd", "SexCd", "HerkunftCd", "district")) |>  
  left_join(nat_dwh, by = c("Jahr", "BasisSzenarienCd", "VersionArtCd", 
                            "AlterVCd", "SexCd", "HerkunftCd", "district")) |> 
  left_join(look_reg, by = c("district")) |>
  rename(QuarCd = distnum) |> 
  mutate(PublJahr = scen_begin) |> 
  select(
      Jahr, PublJahr, BasisSzenarienCd, VersionArtCd, AlterVCd, SexCd, HerkunftCd, QuarCd,
      AnzBestWir, AnzGebuWir, AnzZuzuWir, AnzWezuWir, AnzEinbWir, AnzSterWir
    )


# DWH requirement: NAs as 0
# as.character to avoid things like 9.12e-04 etc. in csv file
out_dwh <- pop_dat_all |> 
  mutate(across(everything(), ~ replace_na(.x, replace = 0))) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(AlterVCd = case_when(
      nchar(AlterVCd) == 1 ~ paste0("00", AlterVCd),
      nchar(AlterVCd) == 2 ~ paste0("0", AlterVCd),
      nchar(AlterVCd) == 3 ~ AlterVCd
    )) |>
  mutate(QuarCd = case_when(
      nchar(QuarCd) == 2 ~ paste0("0", QuarCd),
      nchar(QuarCd) == 3 ~ QuarCd
    ))
  
# checks  
# sort(unique(out_dwh$AlterVCd))
# sort(unique(out_dwh$QuarCd))

# output
write_delim(out_dwh, file.path(dwh_path, "DM_BEV.csv"), delim = ";")



# capacity and reserves ---------------------------------------------------

# be careful: the required numbers from the dwh-team are not logical
# 1: Bestand
# 2: Inanspruchnahme
# 3: Kapazitaet
# 4: Reserve

# data with all publication years
car_all_years <- read_csv(car_path, lazy = FALSE) |>
  pivot_longer(
    cols = all_of(car_initial),
    names_to = "BereichCd",
    values_to = "BruttoGeschFlaeche"
  ) |> 
select("PublJahr", "QuarCd", "EigentumGrundstkCd", "BereichCd", "ArealCd",
         "WohnanteilCd", "BruttoGeschFlaeche") |>
  rename(BereichLang = BereichCd) |> 
  mutate(BereichCd = case_when(
    BereichLang == car_initial[2] ~ 1,
    BereichLang == car_initial[4] ~ 2,
    BereichLang == car_initial[1] ~ 3,
    BereichLang == car_initial[3] ~ 4)) |> 
  mutate(BruttoGeschFlaeche = as.integer(BruttoGeschFlaeche),
         ArealCd = as.factor(ArealCd),
         WohnanteilCd = as.factor(WohnanteilCd),
         BereichCd = as.factor(BereichCd)) 

# export: current year only
car_all_years |> 
  filter(PublJahr == scen_begin) |> 
  select(PublJahr, QuarCd, EigentumGrundstkCd, BereichCd, ArealCd, WohnanteilCd, BruttoGeschFlaeche) |> 
  write_delim(file.path(dwh_path, "DM_KAREB.csv"), delim = ";")

# for control plots: data with all publication years 
car_control_dat <- car_all_years |> 
  group_by(PublJahr, EigentumGrundstkCd, WohnanteilCd, ArealCd, BereichLang) |>
  summarize(BruttoGeschFlaeche = sum(BruttoGeschFlaeche),
            .groups = "drop") |> 
  mutate(ha = BruttoGeschFlaeche/10000,
         Bereich = factor(BereichLang, levels = car_initial), 
         Eigentum = if_else(EigentumGrundstkCd == 1, uni_w[1], uni_w[2]),
         Wohnanteil = case_when(
           WohnanteilCd == 1 ~ uni_e[1],
           WohnanteilCd == 2 ~ uni_e[2],
           WohnanteilCd == 3 ~ uni_e[3]),
         Areal = if_else(ArealCd == 1, uni_p[1], uni_p[2])) |> 
  select(PublJahr, Bereich, Eigentum, Wohnanteil, Areal, ha)



# living space ------------------------------------------------------------

# Check if all files are there
spa_files <- file.path(exp_path, scenarios, "living-space_future.csv")

spa_missing_files <- spa_files[!file.exists(spa_files)]

if (length(spa_missing_files) > 0) {
  stop(
    "these living space files are missing:\n",
    paste(spa_missing_files, collapse = "\n")
  )
}

# import files
spa_future_all <- scenarios |>
  set_names() |>
  map_dfr(
    ~ read_csv(
      file.path(exp_path, .x, "living-space_future.csv"),
      lazy = FALSE
    ),
    .id = "scenario"
  ) |>
  left_join(look_scen_dwh, by = "scenario") |> 
  rename(WohnungsflProPers = spa_dyw)

# message: all files imported?
if (n_distinct(spa_future_all$scenario) != length(scenarios)) {
  stop("not all living space scenarios have been imported")
}



# allocation --------------------------------------------------------------

# Check if all files are there
aca_files <- file.path(exp_path, scenarios, "allocation_future.csv")

aca_missing_files <- aca_files[!file.exists(aca_files)]

if (length(aca_missing_files) > 0) {
  stop(
    "these allocation files are missing:\n",
    paste(aca_missing_files, collapse = "\n")
  )
}

# import files
aca_future_all <- scenarios |>
  set_names() |>
  map_dfr(
    ~ read_csv(
      file.path(exp_path, .x, "allocation_future.csv"),
      lazy = FALSE
    ),
    .id = "scenario"
  ) |>
  left_join(look_scen_dwh, by = "scenario") |> 
  rename(PersProWhg = aca_dyw)


# message: all files imported?
if (n_distinct(aca_future_all$scenario) != length(scenarios)) {
  stop("not all allocation scenarios have been imported")
}



# housing, population -----------------------------------------------------

# Check if all files are there
hou_files <- file.path(exp_path, scenarios, "housing_model_population_dw.csv")

hou_missing_files <- hou_files[!file.exists(hou_files)]

if (length(hou_missing_files) > 0) {
  stop(
    "these housing files are missing:\n",
    paste(hou_missing_files, collapse = "\n")
  )
}

# import files
pop_dw <- scenarios |>
  set_names() |>
  map_dfr(
    ~ read_csv(
      file.path(exp_path, .x, "housing_model_population_dw.csv"),
      lazy = FALSE
    ),
    .id = "scenario"
  ) |>
  left_join(look_scen_dwh, by = "scenario")

# message: all files imported?
if (n_distinct(pop_dw$scenario) != length(scenarios)) {
  stop("not all housing scenarios have been imported")
}



# combine housing data ----------------------------------------------------

# area data
join_cond <- c("VersionArtCd", "scenario", "year", "district", "owner")
area <- spa_future_all |>
  left_join(pop_dw, by = join_cond) |>
  mutate(BruttoGeschFlaeche = pop * WohnungsflProPers / 10000) # in ha

# apartments data
apartments <- aca_future_all |>
  left_join(pop_dw, by = join_cond) |>
  mutate(AnzWhgStat = pop / PersProWhg)

# combine datasets and write output
comb_dat <- area |>
  select(-pop) |> 
  left_join(apartments, by =  join_cond) |>
  left_join(look_reg, by = "district") |>
  left_join(look_own, by = "owner") |> 
  rename(Jahr = year) |>
  mutate(BasisSzenarienCd = if_else(Jahr < scen_begin, 1, 2),
         PublJahr = scen_begin,
         QuarCd = distnum) |> 
  mutate(BruttoGeschFlaeche = round(BruttoGeschFlaeche, round_rate),
         AnzWhgStat = round(AnzWhgStat, round_rate),
         WohnungsflProPers = round(WohnungsflProPers, round_rate),
         PersProWhg = round(PersProWhg, round_rate))

# housing data for dwh
# DWH requirement: NAs as 0
# as.character to avoid things like 9.12e-04 etc. in csv file
hou_dwh <- comb_dat |> 
  mutate(across(everything(), ~ replace_na(.x, replace = 0))) |>
  mutate(across(everything(), as.character)) |>
  mutate(QuarCd = case_when(
    nchar(QuarCd) == 2 ~ paste0("0", QuarCd),
    nchar(QuarCd) == 3 ~ QuarCd
  )) |>
  select(Jahr, PublJahr, VersionArtCd, BasisSzenarienCd, QuarCd, EigentumGrundstkCd,
         BruttoGeschFlaeche, AnzWhgStat, PersProWhg, WohnungsflProPers)

# check
# sort(unique(hou_dwh$QuarCd))
  
# output
write_delim(hou_dwh, file.path(dwh_path, "DM_WOHNEN.csv"), delim = ";")

# data for control plots
hou_control_dat <- comb_dat |> 
  select(Jahr, scenario, district, owner, 
         WohnungsflProPers, BruttoGeschFlaeche, PersProWhg, pop, AnzWhgStat) |> 
         pivot_longer(cols = c(WohnungsflProPers, BruttoGeschFlaeche, 
                               PersProWhg, pop, AnzWhgStat),
             names_to = "type") |> 
  mutate(scenario = factor(scenario, levels = scenarios),
         owner = factor(owner, levels = uni_w),
         district = factor(district, levels = uni_d))



# data for control plots --------------------------------------------------

out <- list()
out$dem_control_dat <- dem_control_dat
out$car_control_dat <- car_control_dat
out$hou_control_dat <- hou_control_dat
return(out)

}











  
















