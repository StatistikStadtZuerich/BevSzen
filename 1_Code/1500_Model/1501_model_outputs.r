# header ------------------------------------------------------------------
# model outputs

# paths, parameters, functions --------------------------------------------

# source(paste0(here::here(),"/1_code/0000_general/general_init.R"))
# init()

# start time
t0 <- Sys.time()

# data import: past -------------------------------------------------------

# population
# in contrast to rate calculations: population at the end of the year
pop_past <- read_csv(pop_od) %>%
  rename(year = StichtagDatJahr, age = AlterVCd, pop = AnzBestWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    district = factor(distr, uni_d),
    sex = fact_if(SexCd, uni_s),
    origin = fact_if(HerkunftCd, uni_o)
  ) %>%
  select(district, year, age, sex, origin, pop) %>%
  group_by(district, year, age, sex, origin) %>%
  summarize(pop = sum(pop),
            .groups = "drop") %>%
  mutate(scenario = uni_c[1])

# births
bir_past <- read_csv(bir_od) %>%
  rename(year = EreignisDatJahr, age_mother = AlterVMutterCd, bir = AnzGebuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    district = factor(distr, uni_d),
    sex = fact_if(SexCd, uni_s),
    origin = fact_if(HerkunftCd, uni_o)
  ) %>%
  select(district, year, age_mother, sex, origin, bir) %>%
  group_by(district, year, age_mother, sex, origin) %>%
  summarize(bir = sum(bir),
            .groups = "drop") %>%
  mutate(scenario = uni_c[1])


# deaths
dea_past <- read_csv(dea_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, dea = AnzSterWir) %>%
  mutate(sex = fact_if(SexCd, uni_s)) %>%
  group_by(year, age, sex) %>%
  summarize(dea = sum(dea),
            .groups = "drop") %>%
  mutate(scenario = uni_c[1])

# immigration
imm_past <- read_csv(imm_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, imm = AnzZuzuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = fact_if(SexCd, uni_s),
    origin = fact_if(HerkunftCd, uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, origin, imm) %>%
  group_by(district, year, age, sex, origin) %>%
  summarize(imm = sum(imm),
            .groups = "drop") %>%
  mutate(scenario = uni_c[1])

# emigration
emi_past <- read_csv(emi_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, emi = AnzWezuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = fact_if(SexCd, uni_s),
    origin = fact_if(HerkunftCd, uni_o),
    district = factor(distr, uni_d)
  ) %>%
  select(district, year, age, sex, origin, emi) %>%
  group_by(district, year, age, sex, origin) %>%
  summarize(emi = sum(emi),
            .groups = "drop") %>%
  mutate(scenario = uni_c[1])

# relocation
rel_past <- read_csv(rel_od) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, rel = AnzUmzuWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  rename(distr_after = distr) %>%
  left_join(look_dis, by = c("QuarBisherCd" = "QuarCd")) %>%
  rename(distr_before = distr) %>%
  mutate(
    sex = fact_if(SexCd, uni_s),
    origin = fact_if(HerkunftCd, uni_o),
    district_before = factor(distr_before, uni_d),
    district_after = factor(distr_after, uni_d)
  ) %>%
  select(district_before, district_after, year, age, sex, origin, rel) %>%
  group_by(district_before, district_after, year, age, sex, origin) %>%
  summarize(rel = sum(rel),
            .groups = "drop") %>%
  mutate(scenario = uni_c[1])

# naturalization
nat_past <- read_csv(nat_od) %>%
  filter((HerkunftBisherCd == 2) & (HerkunftCd == 1)) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, nat = AnzEinbWir) %>%
  left_join(look_dis, by = "QuarCd") %>%
  mutate(
    sex = fact_if(SexCd, uni_s),
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
    sex = fact_if(SexSort, uni_s),
    origin = fact_if(HerkunftSort, uni_o),
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


# population for birth versions
pop_middle_birth_lower <- read_csv(paste0(data_path, "5_Outputs/middle_birth_lower/population_future.csv")) %>%
  mutate(scenario = uni_cb[2])

pop_middle_birth_upper <- read_csv(paste0(data_path, "5_Outputs/middle_birth_upper/population_future.csv")) %>%
  mutate(scenario = uni_cb[4])



# births
bir_lower <- read_csv(paste0(data_path, "5_Outputs/lower/births_future.csv")) %>%
  mutate(scenario = uni_c[2])

bir_middle <- read_csv(paste0(data_path, "5_Outputs/middle/births_future.csv")) %>%
  mutate(scenario = uni_c[3])

bir_upper <- read_csv(paste0(data_path, "5_Outputs/upper/births_future.csv")) %>%
  mutate(scenario = uni_c[4])


# births for birth versions
bir_middle_birth_lower <- read_csv(paste0(data_path, "5_Outputs/middle_birth_lower/births_future.csv")) %>%
  mutate(scenario = uni_cb[2])

bir_middle_birth_upper <- read_csv(paste0(data_path, "5_Outputs/middle_birth_upper/births_future.csv")) %>%
  mutate(scenario = uni_cb[4])


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

# past and future (scenarios)
pop <- pop_past %>%
  bind_rows(pop_lower) %>%
  bind_rows(pop_middle) %>%
  bind_rows(pop_upper) %>%
  mutate(
    district = factor(district, uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )

# past and future (birth versions)
pop_birth_versions <- pop_past %>%
  bind_rows(pop_middle_birth_lower) %>%
  bind_rows(pop_middle) %>%
  bind_rows(pop_middle_birth_upper) %>%
  mutate(
    district = factor(district, uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )



# selected years (e.g. for population pyramids)
y_sel1 <- c(date_end, scen_end_public)
y_sel2 <- c(date_end, rev(seq(scen_end, scen_begin, by = -10)))


# # yc
pop_yc <- pop %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(pop),
            .groups = "drop") %>%
  filter(year <= scen_end_public)

# plot 1500

# text to plot
text_yc <- pop_yc %>%
  filter(year %in% c(date_end, scen_end_public)) %>%
  mutate(cat = if_else(year == date_end, "begin", "end")) %>%
  pivot_wider(names_from = "cat", values_from = "pop") %>%
  fill(begin) %>%
  filter(year == scen_end_public) %>%
  mutate(end_round = round(end / round_people_scen) * round_people_scen,
         delta = end - begin,
         delta_round = round(delta / round_people_scen) * round_people_scen,
         text = paste0(scenario, " scenario: ", end_round, " (", delta_round, ")"),
         plot = "1500") %>%
  arrange(desc(scenario)) %>%
  select(plot, text)

# yas
pop_yas <- pop %>%
  filter((scenario %in% uni_c[c(1, 3)]) &
    (year %in% y_sel1)) %>%
  group_by(year, age, sex) %>%
  summarize(pop = sum(pop),
            .groups = "drop") %>%
  mutate(
    pop_pyramid = if_else(sex == uni_s[1], -pop, pop),
    year_factor = as.factor(year)
  )

# plot 1501

# text to plot
text_pop_yas <- pop_yas %>%
  left_join(look_a3, by = "age") %>%
  rename(age_class = age_3) %>%
  mutate(age_class = factor(if_else(age_class %in% c("80-89", "90+"), "80+",
                                    as.character(age_class)))) %>%
  filter(year %in% c(date_end, scen_end_public)) %>%
  group_by(year, age_class) %>%
    summarize(pop = sum_NA(pop),
              .groups = "drop") %>%
  mutate(cat = if_else(year == date_end, "begin", "end")) %>%
  pivot_wider(names_from = "cat", values_from = "pop") %>%
  arrange(age_class, year) %>%
  fill(begin) %>%
  filter(year == scen_end_public) %>%
  mutate(delta = end - begin,
         percent = (delta) / begin * 100,
         delta_round = round(delta / round_people_scen) * round_people_scen,
         percent_round = round(percent, round_prop_scen),
         text = paste0(age_class, ": ", delta_round, " (", percent_round, "%)"),
         plot = "1501") %>%
  select(plot, text)

# dy
pop_dy <- pop %>%
  filter(scenario %in% uni_c[c(1, 3)]) %>%
  group_by(district, year) %>%
  summarize(pop = sum(pop),
            .groups = "drop")

# plots 1502, 1503, 1504, 1505, 1506, 1507, 1508

# text to plot
# WHY preparation (and not directly piped)? data used for the map (see below)
text_pop_dy_prep <- pop_dy %>%
  filter(year %in% c(date_end, scen_end_public)) %>%
  mutate(cat = if_else(year == date_end, "begin", "end")) %>%
  pivot_wider(names_from = "cat", values_from = "pop") %>%
  fill(begin) %>%
  filter(year == scen_end_public) %>%
  mutate(delta = end - begin,
         percent = delta / begin * 100,
         delta_round = round(delta / round_people_scen) * round_people_scen,
         percent_round = round(percent, round_prop_scen),
         text = paste0(district, ": ", delta_round, " (", percent_round, "%)"),
         plot = "1508")

text_pop_dy <- text_pop_dy_prep %>%
  arrange(desc(delta_round)) %>%
  select(plot, text)

# data for map
# WHY a lookup table? to get the district number
lookup_map <- look_dis %>%
    mutate(QuarCd = if_else(distr == "Kreis 1", "010", QuarCd),
           QuarSort = as.numeric(QuarCd)) %>%
    rename(district = distr) %>%
    distinct()

text_pop_dy_prep %>%
  left_join(lookup_map, by = "district") %>%
  mutate(percent_text = paste0(percent_round, "%")) %>%
  select(QuarSort, district, percent_round, percent_text) %>%
  write_csv(paste0(out_path, "/map_pop_dy.csv"))

# plot 1509

# population (children) for the scenarios 
pop_yac_children <- pop |> 
  filter(age < age_5[3]) |> 
  left_join(look_a5, by = "age") |> 
  group_by(year, scenario, age_5) |> 
    summarize(pop = sum_NA(pop), .groups = "drop")

# plot 1509a

# population (children) for the birth versions 
pop_yav_children <- pop_birth_versions |> 
  filter(age < age_5[3]) |> 
  mutate(scenario = factor(scenario, levels = uni_cb)) |> 
  left_join(look_a5, by = "age") |> 
  group_by(year, scenario, age_5) |> 
    summarize(pop = sum_NA(pop), .groups = "drop")

# plot 1509b


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
  bind_rows(pop_yc_prev) %>% 
  filter(year <= scen_end_public)

# plot 1510

# comparison: for the last year in the previous prediction
comp_year <- min(scen_end_public, max(pop_yc_prev$year))

# text to plot 
text_yc_new_prev <- pop_yc_new_prev %>% 
  filter(year == comp_year) %>% 
  pivot_wider(names_from = "cat", values_from = "pop") %>% 
  mutate(diff = new - previous,
    previous_round = round(previous / round_people_scen) * round_people_scen,
    new_round = round(new / round_people_scen) * round_people_scen,
    diff_round = round(diff / round_people_scen) * round_people_scen, 
    percent = diff / previous * 100,
    percent_round = round(percent, round_prop_scen_a),
    text = paste0(scenario, " scenario: ", previous_round, " (", scen_begin-1, "), ", 
                  new_round, " (", scen_begin, ", ", diff_round, ", ", 
                  percent_round, "%), comparison for year ", comp_year),
    plot = "1510") %>% 
  arrange(desc(scenario)) %>% 
  select(plot, text)

# plots 1511, 1512: age: 80plus

# age categories, by y
# WHY renamed? same name as in previous prediction
pop_age_new <- pop_middle %>%
  left_join(look_a3, by = "age") %>% 
  rename(age_class = age_3) %>% 
  filter(year %in% uniy_scen_public) %>% 
  group_by(year, age_class) %>%
    summarize(pop = sum_NA(pop),
              .groups = "drop") %>% 
    mutate(cat = new_prev[1])

pop_age_prev <- sce %>%
  filter((year %in% uniy_scen_public) &
             (scenario == text_c[3])) %>% 
  group_by(year, age_class) %>%
    summarize(pop = sum_NA(pop),
              .groups = "drop") %>% 
    mutate(cat = new_prev[2])  

# one category for 80+
# WHY not defined as a separate category before? 
# published population scenarios on open data have two separate categories for 80-89 and 90+

# WHY not piped to plot? data also used to filter for must important age groups 
# (frequent customer needs: young, elderly; plots used for slides)
pop_age_new_prev <- pop_age_new %>%
  bind_rows(pop_age_prev) %>%
  mutate(age_class = factor(if_else(age_class %in% c("80-89", "90+"), "80+", as.character(age_class)))) %>%
  group_by(year, age_class, cat) %>%
  summarize(
    pop = sum_NA(pop),
    .groups = "drop"
  )
  
# plot 1513

# text to plot
text_pop_age_new_prev <- pop_age_new_prev %>% 
  filter(year == comp_year) %>%
  pivot_wider(names_from = "cat", values_from = "pop") %>%  
  mutate(percent = (new - previous) / previous * 100, 
    percent_round = round(percent, round_prop_scen_a),
    text = paste0(age_class, ": ", percent_round, "%, comparison for year ", comp_year), 
    plot = "1513") %>%
  select(plot, text)

# plots 1514, 1515, 1516

# plots 1520, 1521, 1522, 1523: birth
 
# plots 1530, 1531, 1532, 1533: death 

# plots 1540, 1541, 1542, 1543, 1544: immigration 

# plots 1550, 1551, 1552, 1553, 1554: emigration # 

# plots 1560, 1561: net migration 
# 
# plots 1570, 1571: naturalization  

# plot 1580: relocation

# text of the plots (for presentations, slides) ---------------------------

text_yc %>% 
  bind_rows(text_yc_new_prev) %>% 
  bind_rows(text_pop_dy) %>%   
  bind_rows(text_pop_yas) %>%  
  bind_rows(text_pop_age_new_prev) %>%    
  write_csv(paste0(out_path, "/text_plots.csv"))  

# log info
cat_log(paste0(
  "model outputs: ",
  capture.output(Sys.time() - t0)
))
