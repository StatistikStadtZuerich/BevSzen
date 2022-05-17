# header ------------------------------------------------------------------
# model outputs

# paths, parameters, functions --------------------------------------------

# general functions already available?
if (!exists("para")) {

  # working directory
  library(here)
  setwd(paste0(here(), "/2020phase2/"))

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
}


# data import: past -------------------------------------------------------

# population
# in contrast to rate calculations: population at the end of the year
pop_past <- read_csv(pop_od) %>%
  rename(year = StichtagDatJahr, age = AlterVCd, pop = AnzBestWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    district = factor(distr, uni_d),
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o)
  ) %>%
  select(district, year, age, sex, origin, pop) %>%
  group_by(district, year, age, sex, origin) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(scenario = uni_c[1])

# births
bir_past <- read_csv(bir_od) %>%
  rename(year = EreignisDatJahr, bir = AnzGebuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    district = factor(distr, uni_d),
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o)
  ) %>%
  select(district, year, sex, origin, bir) %>%
  group_by(district, year, sex, origin) %>%
  summarize(bir = sum(bir)) %>%
  ungroup() %>%
  mutate(scenario = uni_c[1])

# deaths
dea_past <- read_csv(dea_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, dea = AnzSterWir) %>%
  mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s)) %>%
  group_by(year, age, sex) %>%
  summarize(dea = sum(dea)) %>%
  ungroup() %>%
  mutate(scenario = uni_c[1])

# immigration
imm_past <- read_csv(imm_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, imm = AnzZuzuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, origin, imm) %>%
  group_by(district, year, age, sex, origin) %>%
  summarize(imm = sum(imm)) %>%
  ungroup() %>%
  mutate(scenario = uni_c[1])

# emigration
emi_past <- read_csv(emi_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, emi = AnzWezuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, origin, emi) %>%
  group_by(district, year, age, sex, origin) %>%
  summarize(emi = sum(emi)) %>%
  ungroup() %>%
  mutate(scenario = uni_c[1])

# relocation
rel_past <- read_csv(rel_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, rel = AnzUmzuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  rename(distr_after = distr) %>%
  left_join(look_dis, by = c("QuarBisherCd" = "QuarCd")) %>%
  rename(distr_before = distr) %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
    district_before = factor(distr_before, uni_d),
    district_after = factor(distr_after, uni_d)
  ) %>%
  select(district_before, district_after, year, age, sex, origin, rel) %>%
  group_by(district_before, district_after, year, age, sex, origin) %>%
  summarize(rel = sum(rel)) %>%
  ungroup() %>%
  mutate(scenario = uni_c[1])

# naturalization
nat_past <- read_csv(nat_od) %>%
  filter((HerkunftBisherCd == 2) & (HerkunftCd == 1)) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, nat = AnzEinbWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, nat) %>%
  arrange(district, year, age, sex) %>%
  mutate(scenario = uni_c[1])

# previous published population scenarios
# age: only age groups available on open data

look_dis_temp <- look_dis %>%
  mutate(
    Quar_num = as.numeric(QuarCd),
    QuarSort = if_else(Quar_num < 20, 10, Quar_num)
  ) %>%
  select(QuarSort, distr) %>%
  distinct()

sce <- read_csv(sce_od) %>%
  rename(year = StichtagDatJahr, pop = AnzBestWir) %>%
  filter(year >= scen_begin) %>%
  left_join(look_dis_temp, by = "QuarSort") %>%
  mutate(age_class = factor(case_when(
    AlterV10Sort == 1 ~ age_3t[1],
    AlterV10Sort == 2 ~ age_3t[2],                               
    AlterV10Sort == 3 ~ age_3t[3],     
    AlterV10Sort == 4 ~ age_3t[4],    
    AlterV10Sort == 5 ~ age_3t[5],       
    AlterV10Sort == 6 ~ age_3t[6],           
    AlterV10Sort == 7 ~ age_3t[7],     
    AlterV10Sort == 8 ~ age_3t[8],         
    AlterV10Sort == 9 ~ age_3t[9],     
    TRUE ~ age_3t[10]), levels = age_3t),
    sex = factor(if_else(SexSort == 1, uni_s[1], uni_s[2]), uni_s),
    origin = factor(if_else(HeimatRegionSort == 1, uni_o[1], uni_o[2]), uni_o),
    district = factor(distr, uni_d),
    scenario = case_when(
      VersionArtSort == 1 ~ uni_c[2],
      VersionArtSort == 2 ~ uni_c[3],
      TRUE ~ uni_c[4]
    )
  ) %>%
  select(district, year, age_class, sex, origin, scenario, pop) %>%
  group_by(district, year, age_class, sex, origin, scenario) %>%
  summarize(pop = sum(pop),
            .groups = "drop")



# data import: future (lower, middle, upper scenario) ---------------------

# population
pop_lower <- read_csv(paste0(data_path, "5_Outputs/lower/population_future.csv")) %>%
  mutate(scenario = uni_c[2])

pop_middle <- read_csv(paste0(data_path, "5_Outputs/middle/population_future.csv")) %>%
  mutate(scenario = uni_c[3])

pop_upper <- read_csv(paste0(data_path, "5_Outputs/upper/population_future.csv")) %>%
  mutate(scenario = uni_c[4])

# births
bir_lower <- read_csv(paste0(data_path, "5_Outputs/lower/births_future.csv")) %>%
  mutate(scenario = uni_c[2])

bir_middle <- read_csv(paste0(data_path, "5_Outputs/middle/births_future.csv")) %>%
  mutate(scenario = uni_c[3])

bir_upper <- read_csv(paste0(data_path, "5_Outputs/upper/births_future.csv")) %>%
  mutate(scenario = uni_c[4])

# demographic processes
dem_lower <- read_csv(paste0(data_path, "5_Outputs/lower/demographic-processes_future.csv")) %>%
  mutate(scenario = uni_c[2])

dem_middle <- read_csv(paste0(data_path, "5_Outputs/middle/demographic-processes_future.csv")) %>%
  mutate(scenario = uni_c[3])

dem_upper <- read_csv(paste0(data_path, "5_Outputs/upper/demographic-processes_future.csv")) %>%
  mutate(scenario = uni_c[4])

dem_future <- dem_lower %>%
  bind_rows(dem_middle) %>%
  bind_rows(dem_upper)

# naturalization
nat_lower <- read_csv(paste0(data_path, "5_Outputs/lower/naturalization_future.csv")) %>%
  mutate(scenario = uni_c[2])

nat_middle <- read_csv(paste0(data_path, "5_Outputs/middle/naturalization_future.csv")) %>%
  mutate(scenario = uni_c[3])

nat_upper <- read_csv(paste0(data_path, "5_Outputs/upper/naturalization_future.csv")) %>%
  mutate(scenario = uni_c[4])

nat_future <- nat_lower %>%
  bind_rows(nat_middle) %>%
  bind_rows(nat_upper)



# population --------------------------------------------------------------

# past and future
pop <- pop_past %>%
  bind_rows(pop_lower) %>%
  bind_rows(pop_middle) %>%
  bind_rows(pop_upper) %>%
  mutate(
    district = factor(district, uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )

# selected years (e.g. for population pyramids)
y_sel1 <- c(date_end, scen_end_public)
y_sel2 <- c(date_end, rev(seq(scen_end, scen_begin, by = -10)))


# yc
pop_yc <- pop %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(pop_yc,
  aes_x = "year", aes_y = "pop", aes_col = "scenario",
  labs_y = "population",
  scale_y = c(0, NA),
  name = "1500_pop_yc"
)

# yas
pop_yas <- pop %>%
  filter((scenario %in% uni_c[c(1, 3)]) &
    (year %in% y_sel1)) %>%
  group_by(year, age, sex) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(
    pop_pyramid = if_else(sex == uni_s[1], -pop, pop),
    year_factor = as.factor(year)
  )

sszplot(pop_yas,
  aes_x = "age", aes_y = "pop_pyramid", aes_col = "sex",
  aes_ltyp = "year_factor",
  labs_y = "population",
  i_y = 0,
  quotes = c(
    quote(coord_flip()),
    quote(scale_linetype_manual(values = c("dotted", "solid")))
  ),
  width = 6, height = 6,
  name = "1501_pop_yas"
)

# dy
pop_dy <- pop %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(pop_dy,
  aes_x = "year", aes_y = "pop",
  wrap = "district", ncol = 4,
  labs_y = "population",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1502_pop_dy"
)

# dyo
pop_dyo <- pop %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year, origin) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(pop_dyo,
  aes_x = "year", aes_y = "pop", aes_col = "origin",
  wrap = "district", ncol = 4,
  labs_y = "population",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1503_pop_dyo"
)

# dyo: foreign population (proportion)
pop_dyo_prop <- pop_dyo %>%
  pivot_wider(names_from = "origin", values_from = "pop") %>%
  mutate(foreign_prop = foreign / (Swiss + foreign) * 100)

sszplot(pop_dyo_prop,
  aes_x = "year", aes_y = "foreign_prop",
  wrap = "district", ncol = 4,
  labs_y = "foreign proportion in %",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1504_pop_dyo_foreign"
)


# yoc
pop_yoc <- pop %>%
  group_by(year, origin, scenario) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(pop_yoc,
  aes_x = "year", aes_y = "pop", aes_col = "scenario",
  labs_y = "population",
  grid = c(".", "origin"),
  scale_y = c(0, NA),
  name = "1505_pop_yoc"
)

# yoc: foreign population (proportion)
pop_yoc_prop <- pop_yoc %>%
  pivot_wider(names_from = "origin", values_from = "pop") %>%
  mutate(foreign_prop = foreign / (Swiss + foreign) * 100)

sszplot(pop_yoc_prop,
  aes_x = "year", aes_y = "foreign_prop", aes_col = "scenario",
  labs_y = "foreign proportion in %",
  scale_y = c(0, NA),
  name = "1506_pop_yoc_foreign"
)


# dyas
pop_dyas <- pop %>%
  filter((scenario %in% uni_c[c(1, 3)]) &
    (year %in% y_sel1)) %>%
  group_by(district, year, age, sex) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(
    pop_pyramid = if_else(sex == uni_s[1], -pop, pop),
    year_factor = as.factor(year)
  )

sszplot(pop_dyas,
  aes_x = "age", aes_y = "pop_pyramid", aes_col = "sex",
  aes_ltyp = "year_factor",
  labs_y = "population",
  i_y = 0,
  width = 14, height = 28,
  quotes = c(
    quote(facet_wrap(~district, ncol = 4, scales = "free_x")),
    quote(coord_flip()),
    quote(scale_linetype_manual(values = c("dotted", "solid")))
  ),
  name = "1507_pop_dyas"
)




# population: new and previous scenarios ----------------------------------

# total population
new_prev <- factor(c("new", "previous"))

pop_yc_new <- pop_yc %>%
  mutate(cat = new_prev[1])

pop_yc_prev <- sce %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(pop),
            .groups = "drop") %>%
  mutate(cat = new_prev[2])

pop_yc_new_prev <- pop_yc_new %>%
  bind_rows(pop_yc_prev)

sszplot(pop_yc_new_prev,
  aes_x = "year", aes_y = "pop",
  aes_col = "scenario", aes_ltyp = "cat",
  labs_y = "population",
  scale_y = c(0, NA),
  name = "1510_pop_yc_new_prev"
)

# age: 80plus, by yso 
pop_80p_new <- pop_middle %>%
  filter((age >= 80) &
           (year %in% uniy_scen_public)) %>%  
  group_by(year, sex, origin) %>% 
    summarize(pop = sum_NA(pop), 
              .groups = "drop") %>% 
  mutate(sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o),
    cat = new_prev[1])  

pop_80p_prev <- sce %>%
  filter((age_class %in% age_3t[9:10]) &
           (scenario == text_c[3]) &
           (year %in% sel_years)) %>% 
  group_by(year, sex, origin) %>% 
    summarize(pop = sum_NA(pop), 
              .groups = "drop") %>% 
  mutate(cat = new_prev[2])

# WHY not piped into plot? used for total (age 80plus, without so)
pop_80p_new_prev <- pop_80p_new %>%
  bind_rows(pop_80p_prev)

sszplot(pop_80p_new_prev,
  aes_x = "year", aes_y = "pop", aes_col = "cat",
  labs_y = "people",
  grid = c("sex", "origin"),
  scale_y = c(0, NA),
  width = 10, height = 6,
  name = "1511_pop_80plus_so"
)

# age: 80plus, by y 
pop_80p_new_prev %>% 
  group_by(year, cat) %>% 
    summarize(pop = sum_NA(pop), 
              .groups = "drop") %>% 
sszplot(
  aes_x = "year", aes_y = "pop", aes_col = "cat",
  labs_y = "people",
  scale_y = c(0, NA),
  width = 8, height = 5,
  name = "1512_pop_80plus"
)





# birth -------------------------------------------------------------------

# past and future
bir <- bir_past %>%
  bind_rows(bir_lower) %>%
  bind_rows(bir_middle) %>%
  bind_rows(bir_upper) %>%
  mutate(
    district = factor(district, uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )

# yc
bir_yc <- bir %>%
  group_by(year, scenario) %>%
  summarize(bir = sum(bir)) %>%
  ungroup()

sszplot(bir_yc,
  aes_x = "year", aes_y = "bir", aes_col = "scenario",
  labs_y = "births per year",
  scale_y = c(0, NA),
  name = "1520_bir_yc"
)

# yoc
bir_yoc <- bir %>%
  group_by(year, origin, scenario) %>%
  summarize(bir = sum(bir)) %>%
  ungroup()

sszplot(bir_yoc,
  aes_x = "year", aes_y = "bir", aes_col = "scenario",
  labs_y = "births per year",
  grid = c(".", "origin"),
  scale_y = c(0, NA),
  name = "1521_bir_yoc"
)

# dyo
bir_dyo <- bir %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year, origin) %>%
  summarize(bir = sum(bir)) %>%
  ungroup()

sszplot(bir_dyo,
  aes_x = "year", aes_y = "bir", aes_col = "origin",
  wrap = "district", ncol = 4,
  labs_y = "births per year",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1522_bir_dyo"
)

# dyo: foreign population (proportion)
bir_dyo_prop <- bir_dyo %>%
  pivot_wider(names_from = "origin", values_from = "bir") %>%
  mutate(foreign_prop = foreign / (Swiss + foreign) * 100)

sszplot(bir_dyo_prop,
  aes_x = "year", aes_y = "foreign_prop",
  wrap = "district", ncol = 4,
  labs_y = "foreign proportion in %",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1523_bir_dyo_foreign"
)



# death -------------------------------------------------------------------

# past and future
dea <- dem_future %>%
  select(year, age, sex, scenario, dea) %>%
  group_by(year, age, sex, scenario) %>%
  summarize(dea = sum(dea)) %>%
  ungroup() %>%
  bind_rows(dea_past) %>%
  left_join(look_a3, by = "age") %>%
  mutate(sex = factor(sex, levels = uni_s))

# yc
dea_yc <- dea %>%
  group_by(year, scenario) %>%
  summarize(dea = sum(dea)) %>%
  ungroup()

sszplot(dea_yc,
  aes_x = "year", aes_y = "dea", aes_col = "scenario",
  labs_y = "deaths per year",
  scale_y = c(0, NA),
  name = "1530_dea_yc"
)

# yac
dea_yac <- dea %>%
  group_by(year, age_3, scenario) %>%
  summarize(dea = sum(dea)) %>%
  ungroup()

sszplot(dea_yac,
  aes_x = "year", aes_y = "dea", aes_col = "scenario",
  labs_y = "deaths per year",
  wrap = "age_3", ncol = 5,
  scale_y = c(0, NA),
  width = 12, height = 6,
  name = "1531_dea_yac"
)

# ys
dea_ys <- dea %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(year, sex) %>%
  summarize(dea = sum(dea)) %>%
  ungroup()

sszplot(dea_ys,
  aes_x = "year", aes_y = "dea", aes_col = "sex",
  labs_y = "deaths per year",
  scale_y = c(0, NA),
  geom = "line",
  name = "1532_dea_ys"
)

# yas
dea_yas <- dea %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(year, age_3, sex) %>%
  summarize(dea = sum(dea)) %>%
  ungroup()

sszplot(dea_yas,
  aes_x = "year", aes_y = "dea", aes_col = "sex",
  labs_y = "deaths per year",
  wrap = "age_3", ncol = 5,
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 12, height = 6,
  name = "1533_dea_yas"
)



# immigration -------------------------------------------------------------

# past and future
imm <- dem_future %>%
  select(district, year, age, sex, origin, scenario, imm) %>%
  bind_rows(imm_past) %>%
  left_join(look_a3, by = "age") %>%
  mutate(
    district = factor(district, uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )

# yc
imm_yc <- imm %>%
  group_by(year, scenario) %>%
  summarize(imm = sum(imm)) %>%
  ungroup()

sszplot(imm_yc,
  aes_x = "year", aes_y = "imm", aes_col = "scenario",
  labs_y = "immigration per year",
  scale_y = c(0, NA),
  name = "1540_imm_yc"
)

# ya
imm_ya <- imm %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(year, age_3) %>%
  summarize(imm = sum(imm)) %>%
  ungroup()

sszplot(imm_ya,
  aes_x = "year", aes_y = "imm",
  labs_y = "immigration per year",
  wrap = "age_3", ncol = 5,
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 12, height = 6,
  name = "1541_imm_ya"
)

# yao
imm_yao <- imm %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(year, age_3, origin) %>%
  summarize(imm = sum(imm)) %>%
  ungroup()

sszplot(imm_yao,
  aes_x = "year", aes_y = "imm", aes_col = "origin",
  labs_y = "immigration per year",
  wrap = "age_3", ncol = 5,
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 12, height = 6,
  name = "1542_imm_yao"
)

# dyo
imm_dyo <- imm %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year, origin) %>%
  summarize(imm = sum(imm)) %>%
  ungroup()

sszplot(imm_dyo,
  aes_x = "year", aes_y = "imm", aes_col = "origin",
  wrap = "district", ncol = 4,
  labs_y = "immigration per year",
  scale_y = c(0, NA),
  gridscale = "free_y",
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1543_imm_dyo"
)

# dyo: foreign population (proportion)
imm_dyo_prop <- imm_dyo %>%
  pivot_wider(names_from = "origin", values_from = "imm") %>%
  mutate(foreign_prop = foreign / (Swiss + foreign) * 100)

sszplot(imm_dyo_prop,
  aes_x = "year", aes_y = "foreign_prop",
  wrap = "district", ncol = 4,
  labs_y = "foreign proportion in %",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1544_imm_dyo_foreign"
)



# emigration --------------------------------------------------------------

# past and future
emi <- dem_future %>%
  select(district, year, age, sex, origin, scenario, emi) %>%
  bind_rows(emi_past) %>%
  left_join(look_a3, by = "age") %>%
  mutate(
    district = factor(district, uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )


# yc
emi_yc <- emi %>%
  group_by(year, scenario) %>%
  summarize(emi = sum(emi)) %>%
  ungroup()

sszplot(emi_yc,
  aes_x = "year", aes_y = "emi", aes_col = "scenario",
  labs_y = "emigration per year",
  scale_y = c(0, NA),
  name = "1550_emi_yc"
)

# ya
emi_ya <- emi %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(year, age_3) %>%
  summarize(emi = sum(emi)) %>%
  ungroup()

sszplot(emi_ya,
  aes_x = "year", aes_y = "emi",
  labs_y = "emigration per year",
  wrap = "age_3", ncol = 5,
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 12, height = 6,
  name = "1551_emi_ya"
)

# yao
emi_yao <- emi %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(year, age_3, origin) %>%
  summarize(emi = sum(emi)) %>%
  ungroup()

sszplot(emi_yao,
  aes_x = "year", aes_y = "emi", aes_col = "origin",
  labs_y = "emigration per year",
  wrap = "age_3", ncol = 5,
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 12, height = 6,
  name = "1552_emi_yao"
)

# dyo
emi_dyo <- emi %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year, origin) %>%
  summarize(emi = sum(emi)) %>%
  ungroup()

sszplot(emi_dyo,
  aes_x = "year", aes_y = "emi", aes_col = "origin",
  wrap = "district", ncol = 4,
  labs_y = "emigration per year",
  scale_y = c(0, NA),
  gridscale = "free_y",
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1553_emi_dyo"
)


# dyo: foreign population (proportion)
emi_dyo_prop <- emi_dyo %>%
  pivot_wider(names_from = "origin", values_from = "emi") %>%
  mutate(foreign_prop = foreign / (Swiss + foreign) * 100)

sszplot(emi_dyo_prop,
  aes_x = "year", aes_y = "foreign_prop",
  wrap = "district", ncol = 4,
  labs_y = "foreign proportion in %",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1553_emi_dyo_foreign"
)



# net migration -----------------------------------------------------------

# calculate net migration
net <- as_tibble(expand_grid(
  district = uni_d,
  year = date_start:scen_end,
  age = age_min:age_max,
  sex = uni_s,
  origin = uni_o,
  scenario = uni_c
)) %>%
  left_join(look_a3, by = "age") %>%
  left_join(imm, by = c(
    "district", "year", "age",
    "sex", "origin", "scenario", "age_3"
  )) %>%
  left_join(emi, by = c(
    "district", "year", "age",
    "sex", "origin", "scenario", "age_3"
  )) %>%
  replace_na(list(imm = 0, emi = 0)) %>%
  mutate(net = if_else(
    ((year < scen_begin) & (scenario != uni_c[1])) |
      ((year >= scen_begin) & (scenario == uni_c[1])), NA_real_, imm - emi
  ))

# yc
net_yc <- net %>%
  group_by(year, scenario) %>%
  summarize(net = sum(net)) %>%
  ungroup()

sszplot(net_yc,
  aes_x = "year", aes_y = "net", aes_col = "scenario",
  labs_y = "net migration per year",
  scale_y = c(0, NA),
  i_y = 0,
  name = "1560_net_yc"
)

# dyo
net_dyo <- net %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year, origin) %>%
  summarize(net = sum_NA(net)) %>%
  ungroup()

sszplot(net_dyo,
  aes_x = "year", aes_y = "net", aes_col = "origin",
  wrap = "district", ncol = 4,
  labs_y = "net migration per year",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1561_net_dyo"
)

# foreign proportion: not possible to calculate for net migration
# cannot be applied here
# e.g. low values in the denominator if negative and positive value


# naturalization ----------------------------------------------------------

# past and future
nat <- nat_future %>%
  bind_rows(nat_past) %>%
  left_join(look_a3, by = "age") %>%
  mutate(
    district = factor(district, levels = uni_d),
    sex = factor(sex, levels = uni_s)
  )

# yc
nat_yc <- nat %>%
  group_by(year, scenario) %>%
  summarize(nat = sum(nat)) %>%
  ungroup()

sszplot(nat_yc,
  aes_x = "year", aes_y = "nat", aes_col = "scenario",
  labs_y = "naturalizations per year",
  scale_y = c(0, NA),
  name = "1570_nat_yc"
)

# dy
nat_dy <- nat %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year) %>%
  summarize(nat = sum(nat)) %>%
  ungroup()

sszplot(nat_dy,
  aes_x = "year", aes_y = "nat",
  wrap = "district", ncol = 4,
  labs_y = "naturalizations per year",
  scale_y = c(0, NA),
  i_x = scen_begin,
  width = 10, height = 14,
  name = "1571_nat_dy"
)



# relocation --------------------------------------------------------------

# categories
uni_cat <- c("data", "from immigration*", "from emigration*")

# past: no district
# WHY? since for future data only one district available
# one district does not make sense for relocation
# (the district comes from immigration* and emigration*)
rel_past_yaso <- rel_past %>%
  select(year, age, sex, origin, scenario, rel) %>%
  group_by(year, age, sex, origin, scenario) %>%
  summarize(rel = sum(rel)) %>%
  ungroup() %>%
  mutate(cat = factor(uni_cat[1], levels = uni_cat))

# future: no district
rel_future_yaso <- dem_future %>%
  select(year, age, sex, origin, scenario, rei, ree) %>%
  group_by(year, age, sex, origin, scenario) %>%
  summarize(
    rei = sum(rei),
    ree = sum(ree)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c("rei", "ree"), values_to = "rel",
    names_to = "category"
  ) %>%
  mutate(cat = factor(if_else(category == "rei",
    uni_cat[2], uni_cat[3]
  ), levels = uni_cat)) %>%
  select(year, age, sex, origin, scenario, rel, cat)

# past and future
rel <- rel_past_yaso %>%
  bind_rows(rel_future_yaso)

# yct (t: cat)
rel_yct <- rel %>%
  group_by(year, scenario, cat) %>%
  summarize(rel = sum(rel)) %>%
  ungroup()

sszplot(rel_yct,
  aes_x = "year", aes_y = "rel", aes_col = "scenario",
  aes_ltyp = "cat",
  labs_y = "relocations per year",
  scale_y = c(0, NA),
  quotes = quote(scale_linetype_manual(values = c("dashed", "solid", "dotted"))),
  name = "1580_rel_yct"
)
