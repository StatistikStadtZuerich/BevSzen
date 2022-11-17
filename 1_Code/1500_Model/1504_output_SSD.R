
# header ------------------------------------------------------------------

# calculate values for Schulraumplanung according request by SSD
# nr. of children by district and age group, middle scenario, KaReB according BZO2040
# (age groups: 5-6, 7-12, 13-15)
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
req_path <- paste0(req_path, "pop_scen_SSD.csv")

# factor for age grouping
age_groups <- c("5- und 6-Jährige",	"7- bis 12-Jährige",	"13- bis 15-Jährige")
uni_kids <- factor(age_groups, levels = age_groups)

# store existing (i.e. BZO16) data
# (this requires a complete run of the model beforehand with the BZO2016 KaReB)
pop_middle_16 <- pop_middle

# run the model and build output data -------------------------------------

run_scen(
  scenarios = c("middle"),
  modules = c("all"))
source(paste0(code_path, "1500/1501_model_outputs.r"))

pop_middle %>%
  filter(year %in% c(2021, 2037, 2040, 2045, 2050),
         age >= 5 & age <= 15) %>%
  mutate(age1 = factor(case_when(age <= 6 ~ uni_kids[1],
                                 age <= 12 ~ uni_kids[2],
                                 TRUE ~ uni_kids[3])),
         pop = round(pop, 0)) %>%
  group_by(district, year, age1) %>%
  summarise(pop = sum_NA(pop),
            .groups = "drop") %>%
  pivot_wider(names_from = age1, values_from = pop) %>%
  left_join(lookup_map, by = "district") %>% 
  arrange(QuarSort) %>% 
  select(-QuarCd, -QuarSort) %>%
  write_delim(req_path, delim = ";")


# checks ------------------------------------------------------------------

# input data
car_dat_comb <- read_csv(paste0(inp_path, "KaReB_2016.csv")) %>%
  mutate(bzo = "16") %>%
  union(read_csv(paste0(inp_path, "KaReB.csv")) %>%
          mutate(bzo = "40")) %>%
  rename(year = PublJahr) %>%
  pivot_longer(
    cols = car_initial,
    names_to = "initial",
    values_to = "area"
  ) %>%
  left_join(look_car, by = "initial") %>%
  mutate(distnum = as.numeric(QuarCd)) %>%
  left_join(look_reg, by = "distnum") %>%
  mutate(
    cat = factor(category, car_category),
    residence = factor(
      case_when(
        WohnanteilCd == 1 ~ uni_e[1],
        WohnanteilCd == 2 ~ uni_e[2],
        TRUE ~ uni_e[3]
      )
    ),
    plot = factor(if_else(ArealCd == 1, uni_p[1], uni_p[2]), uni_p),
    owner = factor(if_else(EigentumGrundstkCd == 1, uni_w[1], uni_w[2]), uni_w),
    ha = area / 10000
  ) %>%
  select(year, residence, plot, district, owner, cat, ha, bzo)

# plot bzo 16 vs 40 input data for scenario begin
car_dat_comb %>% 
  group_by(year, bzo, cat, owner) %>%
  summarise(ha = sum(ha),
            .groups = "drop") %>%
  filter(year == scen_begin) %>%
  sszplot(aes_x = "owner", aes_y = "ha", aes_fill = "bzo",
        wrap = "cat",
        geom = "col",
        fix_col = 2,
        name = "15A1_bzo_16_vs_40",
        title = paste("BZO 16 vs. 40 for year", scen_begin))

car_dat_comb %>% 
  group_by(year, bzo, cat, owner, district) %>%
  summarise(ha = sum(ha),
            .groups = "drop") %>%
  filter(year == scen_begin) %>%
  sszplot(aes_x = "owner", aes_y = "ha", aes_fill = "bzo",
          wrap = "district",
          geom = "col",
          fix_col = 2,
          name = "15A2_car_dcat",
          width = 14, height = 10,
          multi = car_dat_comb %>% select(cat) %>% distinct %>% pull(),
          multif = "filter(cat == x)")

# plot bzo40 input data for scenario begin and 1 yr prev
car_dat_comb %>% 
  group_by(year, bzo, cat, owner) %>%
  summarise(ha = sum(ha),
            .groups = "drop") %>%
  filter(year %in% c(scen_begin, scen_begin-1),
         bzo == 40) %>% 
  sszplot(aes_x = "owner", aes_y = "ha", aes_fill = "year",
          wrap = "cat",
          geom = "col",
          fix_col = 2,
          name = "15A3_bzo40_last_2_years", 
          title = paste("BZO 40 for years", scen_begin-1, "and", scen_begin))

car_dat_comb %>% 
  group_by(year, bzo, cat, owner, district) %>%
  summarise(ha = sum(ha),
            .groups = "drop") %>%
  filter(year %in% c(scen_begin, scen_begin-1),
         bzo == 40) %>% 
  sszplot(aes_x = "owner", aes_y = "ha", aes_fill = "year",
          wrap = "district",
          geom = "col",
          fix_col = 2,
          name = "15A4_bzo40_last_2_years_per district",
          width = 14, height = 10,
          multi = car_dat_comb %>% select(cat) %>% distinct %>% pull(),
          multif = "filter(cat == x)")

# scenario results for the different BZOs
# total
pop_middle_16 %>%
  mutate(bzo = 16) %>%
  union(pop_middle %>% mutate(bzo = 40)) %>%
  group_by(bzo, year) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "pop",  aes_fill = "bzo",
          geom = "col",
          fix_col = 2,
          labs_x = "year", labs_y = "frequency",
          name = "15A5_pop_yd_bzo_16_vs_40",
          width = 10, height = 7)

# by district
pop_middle_16 %>%
  mutate(bzo = 16) %>%
  union(pop_middle %>% mutate(bzo = 40)) %>%
  group_by(bzo, year, district) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "pop",  aes_fill = "bzo",
          geom = "col",
          fix_col = 2,
          labs_x = "year", labs_y = "frequency",
          name = "15A6_pop_yd_bzo_16_vs_40",
          width = 10, height = 7,
          multi = uni_d)

# by age group
pop_middle_16 %>%
  mutate(bzo = 16) %>%
  union(pop_middle %>% mutate(bzo = 40)) %>%
  mutate(age_group = age %/% 10 + 1) %>%
  mutate(age_group = if_else(age_group > 9, 9, age_group)) %>%
  group_by(bzo, year, age_group) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "pop",  aes_fill = "bzo",
          geom = "col",
          fix_col = 2,
          labs_x = "year", labs_y = "frequency",
          angle = 90,
          name = "15A7_pop_ya_bzo_16_vs_40",
          width = 20, height = 14,
          wrap = "age_group", ncol = 3)


# scenario results from last year and this year in comparison
xlp <- "//szh.loc/ssz/data/Projekte/BevSzen/2021_BZO2040/3_Resultate/3000_Auswertungen/3_Zukuenftige-Bevoelkerungsentwicklung_BZO2040_nach-Altersklasse-Quartier-Jahr.xlsx"
sheets <- readxl::excel_sheets(xlp)
sheets <- sheets[2:length(sheets)]
cn <- c("district", "total", 1:11)

for (i in sheets) {
  readin <- readxl::read_xlsx(xlp, sheet = i, skip = 10, col_names = cn, n_max = 31) %>%
    select(-2) %>%
    pivot_longer(cols = c(-1), names_to = "age_group", values_to = "pop") %>%
    mutate(age_group = as.numeric(age_group)) %>%
    mutate(age_group = if_else(age_group > 9, 9, age_group)) %>%
    mutate(year = as.integer(i), model_year = 21) %>%
    mutate(district = if_else(district == "Mühlebach", "Muehlebach", district)) %>%
    mutate(district = if_else(district == "Höngg", "Hoengg", district))
  
  if (i == sheets[1]) {
    pop_middle_prevyear <- readin
  } else {
    pop_middle_prevyear <- pop_middle_prevyear %>% union(readin)
  }
}




pop_prev_current <- pop_middle %>%
  mutate(model_year = 22) %>%
  mutate(age_group = age %/% 10 + 1) %>%
  mutate(age_group = if_else(age_group > 9, 9, age_group)) %>%
  filter(year %in% as.integer(sheets)) %>%
  group_by(district, age_group, year) %>%
  summarise(pop = sum(pop)) %>%
  mutate(model_year = 22) %>%
  union(pop_middle_prevyear)

# total
pop_prev_current %>%
  group_by(model_year, year) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "pop",  aes_fill = "model_year",
          geom = "col",
          fix_col = 2,
          labs_x = "year", labs_y = "frequency",
          name = "15A8_pop_y_bzo40_21_vs_22",
          width = 10, height = 7)

# by district
pop_prev_current %>%
  group_by(model_year, year, district) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "pop",  aes_fill = "model_year",
          geom = "col",
          fix_col = 2,
          labs_x = "year", labs_y = "frequency",
          name = "15A9_pop_yd_bzo40_21_vs_22",
          width = 10, height = 7,
          wrap = "district")

# by age group
pop_prev_current %>%
  group_by(model_year, year, age_group) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "pop",  aes_fill = "model_year",
          geom = "col",
          fix_col = 2,
          labs_x = "year", labs_y = "frequency",
          angle = 90,
          name = "15B1_pop_ya_bzo40_21_vs_22",
          width = 20, height = 14,
          wrap = "age_group", ncol = 3)



# cleanup work ------------------------------------------------------------

# exchange KaReB file back to original
file.rename(paste0(inp_path, "KaReB.csv"), paste0(inp_path, "KaReB_2040.csv"))
file.rename(paste0(inp_path, "KaReB_2016.csv"), paste0(inp_path, "KaReB.csv"))
