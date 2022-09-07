
# header ------------------------------------------------------------------

# calculate values for Schulraumplanung according request by SSD
# nr. of children by district and age group, middle scenario, KaReB according BZO2040
# (age groups: 5-6, 7-12, 13-15)
# for the years 2021, 2037, 2040, 2045, 2050

# prep work ---------------------------------------------------------------
# general functions (without dependence on parameters)
source("1_Code/0000_General/0002_general_without-parameters.r")

# exchange KaReB file
inp_path <- paste0(data_path, "1_Input/")
file.rename(paste0(inp_path, "KaReB.csv"), paste0(inp_path, "KaReB_actual.csv"))
file.rename(paste0(inp_path, "KaReB_2040.csv"), paste0(inp_path, "KaReB.csv"))

# output path creation
ssd_path <- paste0(data_path, "8_SSD/")
if (!dir.exists(ssd_path)) {
  dir.create(ssd_path, recursive = TRUE)
}
ssd_path <- paste0(ssd_path, "pop_scen_SSD.csv")

# factor for age grouping
age_groups <- c("5- und 6-Jährige",	"7- bis 12-Jährige",	"13- bis 15-Jährige")
uni_kids <- factor(age_groups, levels = age_groups)

# run the model and build output data -------------------------------------

run_scen(
  scenarios = c("middle"),
  modules = c("all"))

pop_middle %>%
  filter(year %in% c(2021, 2037, 2040, 2045, 2050),
         age >= 5 & age <= 15) %>%
  mutate(age1 = factor(if_else(age <=6, uni_kids[1],
                        if_else(age <=12, uni_kids[2],
                                uni_kids[3]))),
         pop = round(pop, 0)) %>%
  group_by(district, year, age1) %>%
  summarise(pop = sum_NA(pop),
            .groups = "drop") %>%
  pivot_wider(names_from = age1, values_from = pop) %>%
  left_join(lookup_map, by = "district") %>% 
  arrange(QuarSort) %>% 
  select(-QuarCd, -QuarSort) %>%
  write_delim(ssd_path, delim = ";")


# checks ------------------------------------------------------------------

# input data
car_dat_comb <- read_csv(paste0(inp_path, "KaReB_actual.csv")) %>%
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

car_dat_comb %>% 
  group_by(year, bzo, cat) %>%
  summarise(ha = sum(ha),
            .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "ha", aes_col = "bzo",
        wrap = "cat",
        fix_col = 2)

car_dat_comb %>% select(bzo) %>% distinct() %>% tally()


# 
pop_middle %>%
  group_by(year) %>%
  summarise(pop = sum_NA(pop), .groups = "drop") %>%
  sszplot(aes_x = "year", aes_y = "pop",
          labs_x = "year", labs_y = "frequency")

# cleanup work ------------------------------------------------------------

# exchange KaReB file back to original
file.rename(paste0(inp_path, "KaReB.csv"), paste0(inp_path, "KaReB_2040.csv"))
file.rename(paste0(inp_path, "KaReB_actual.csv"), paste0(inp_path, "KaReB.csv"))
