# header ------------------------------------------------------------------
# general: packages, functions, levels, colors
# (without dependence on parameters, i.e. scenarios)


# packages ----------------------------------------------------------------

library(tidyverse)
library(dvmisc) # for expand_grid
library(gridExtra) # for ggplot on multiple pages
library(nlme) # needed for mgcv, see below
library(mgcv) # for gam
library(flexclust) # weighted kmeans clustering
library(ggrepel) # to label lines (when too many for a normal legend)
library(zoo) # moving average (no confusion with filter function in base R)
library(ckanr) # download of open data directly from ckan
library(scales) # for pretty axis breaks
library(rlang) # for functions: symbols, quasiquotation (!!, !!!, :=)
library(gtools) # invalid function (to check for NA, NULL, NaN), e.g. in constrained regression
library(this.path) # to extract the current file name
library(modelr) # add_predictions



# paths, import parameters ------------------------------------------------

# path for code
code_path <- "1_Code/"

# path for data
data_path <- "2_Data/"

# log file
log_file <- "2_Data/6_Log/log.txt"

# import parameter
para <- read_delim(paste0(data_path, "3_Parameter/parameter.csv"), ";", lazy = FALSE) %>%
  select(parameter, lower, middle, upper)


# general functions -------------------------------------------------------

# functions
sum_NA <- function(x) {
  sum(x, na.rm = TRUE)
}
max_NA <- function(x) {
  max(x, na.rm = TRUE)
}

# cat to log file
cat_log <- function(...) {
  cat(...,
    file = log_file, sep = "\n", append = TRUE
  )
}


# options -----------------------------------------------------------------

# no scientic notation
options(scipen = 999)


# lookup tables -----------------------------------------------------------

# districts (34 numbers)
# used to transfer the open data files (with 34 districts) to 31 regions
look_dis <- read_csv2(paste0(data_path, "2_Lookup/lookupDistrict.csv"), lazy = FALSE) %>%
  select(QuarCd, distr)

# regions (31 regions)
look_reg <- mutate(look_dis, distnum = if_else(distr == "Kreis 1", 10, as.numeric(QuarCd))) %>%
  select(distnum, distr) %>%
  unique() %>%
  mutate(district = factor(distr, levels = unique(look_dis$distr))) %>%
  select(-distr)

# capacity, reserves (car)
car_initial <- c("Kapazitaet", "Bestand", "Reserve", "Inanspruchnahme")
car_category <- c("capacity", "buildings", "reserve", "usage")
look_car <- tibble(initial = car_initial, category = car_category)

# project status
pro_initial <- c("projektiert, Infoplan", "projektiert, andere", "eingereicht", "bewilligt", "Bau begonnen", "fertiggestellt", "sistiert")
pro_category <- c("scheduled, Infoplan", "scheduled, other", "submitted", "approved", "construction started", "completed", "on hold")
look_pro <- tibble(code = as.double(1:7), initial = pro_initial, category = pro_category) %>%
  mutate(status = factor(category, levels = pro_category))



# unique levels -----------------------------------------------------------

# districts
text_d <- unique(look_dis$distr)
uni_d <- factor(text_d, levels = text_d)

# sex
text_s <- c("male", "female")
uni_s <- factor(text_s, levels = text_s)

# origin
text_o <- c("Swiss", "foreign")
uni_o <- factor(text_o, levels = text_o)

# region
text_r <- c("Zurich", "Switzerland")
uni_r <- factor(text_r, levels = text_r)

# residence portion
text_e <- c("minimum portion", "real portion", "maximum portion")
uni_e <- factor(text_e, levels = text_e)

# plot construction
text_p <- c("with plot construction", "without plot construction")
uni_p <- factor(text_p, levels = text_p)

# property owner
text_w <- c("cooperative housing", "private housing")
uni_w <- factor(text_w, levels = text_w)

# project status
uni_t <- factor(pro_category, levels = pro_category)

# indicator (new or removed apartments)
text_i <- c("new", "removed")
uni_i <- factor(text_i, levels = text_i)

# past and scenarios
text_c <- c("past", "lower", "middle", "upper")
uni_c <- factor(text_c, levels = text_c)



# categories (t = text) and associated lookup tables ----------------------

# age category 1
age_1 <- c(30, 40)
age_1t <- c("15-29", "30-39", "40-49")

look_a1 <- tibble(age = 15:49) %>%
  mutate(age_1 = factor(if_else(age < age_1[1], age_1t[1],
    if_else(age < age_1[2], age_1t[2], age_1t[3])
  ), levels = age_1t))

# age category 2
age_2 <- seq(25, 40, by = 5)
age_2t <- c("15-24", "25-29", "30-34", "35-39", "40-49")

look_a2 <- tibble(age = 15:49) %>%
  mutate(age_2 = factor(if_else(age < age_2[1], age_2t[1],
    if_else(age < age_2[2], age_2t[2],
      if_else(age < age_2[3], age_2t[3],
        if_else(age < age_2[4], age_2t[4], age_2t[5])
      )
    )
  ), levels = age_2t))


# age category 3
age_3 <- seq(0, 90, by = 10)

age_3t <- c(
  "0-9", "10-19", "20-29", "30-39", "40-49",
  "50-59", "60-69", "70-79", "80-89", "90+"
)

look_a3 <- tibble(age = 0:120) %>%
  mutate(age_3 = factor(case_when(
    age < age_3[2] ~ age_3t[1],
    age < age_3[3] ~ age_3t[2],
    age < age_3[4] ~ age_3t[3],
    age < age_3[5] ~ age_3t[4],
    age < age_3[6] ~ age_3t[5],
    age < age_3[7] ~ age_3t[6],
    age < age_3[8] ~ age_3t[7],
    age < age_3[9] ~ age_3t[8],
    age < age_3[10] ~ age_3t[9],
    TRUE ~ age_3t[10]
  ), levels = age_3t))

# age category 4
age_4 <- seq(0, 80, by = 20)

age_4t <- c("0-19", "20-39", "40-59", "60-79", "80+")

look_a4 <- tibble(age = 0:120) %>%
  mutate(age_4 = factor(case_when(
    age < age_4[2] ~ age_4t[1],
    age < age_4[3] ~ age_4t[2],
    age < age_4[4] ~ age_4t[3],
    age < age_4[5] ~ age_4t[4],
    TRUE ~ age_4t[5]
  ), levels = age_4t))



# colors, graphics --------------------------------------------------------

# colors (e.g. for sex, origin)
col_6 <- c("#005CA9", "#83072A", "#EB5E04", "#FBBA00", "#007229", "#9B9B9B")
# plot(1:6, 1:6, col = col_6, pch = 16, cex = 3)

# grey
col_grey <- "grey90"

# color for district
col_d <- colorRampPalette(col_6)(31)
# plot(1:31, 1:31, col = col_d, pch = 16, cex = 3)

# color for sex
col_s <- col_6[c(1, 3)]
# plot(1:2, 1:2, col = col_s, pch = 16, cex = 3)

# color for origin
col_o <- col_6[c(3, 2)]
# plot(1:2, 1:2, col = col_o, pch = 16, cex = 3)

# color for region
col_r <- col_6[c(1, 3)]
# plot(1:2, 1:2, col = col_r, pch = 16, cex = 3)

# color for property owner
col_w <- col_6[c(2, 5)]
# plot(1:2, 1:2, col = col_w, pch = 16, cex = 3)

# color for residence portion
col_e <- col_6[c(4, 3, 2)]
# plot(1:3, 1:3, col = col_e, pch = 16, cex = 3)

# color for project status
col_t <- col_6
# plot(1:6, 1:6, col = col_t, pch = 16, cex = 3)

# color for indicator (new or removed projects)
col_i <- col_6[c(1, 6)]
# plot(1:2, 1:2, col = col_i, pch = 16, cex = 3)

# themes: neutral design for ggplot
neutral <- theme_bw() + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_rect(colour = "grey85"),
  panel.border = element_rect(colour = "grey85")
)



# open data (od) ----------------------------------------------------------

# population (pop)
pop_od <- "https://data.stadt-zuerich.ch/dataset/80d5c8af-b389-41d2-b6d8-d0deb1639f00/resource/b2abdef7-3e3f-4883-8033-6787a1561987/download/bev390od3903.csv"


# birth (bir)
bir_od <- "https://data.stadt-zuerich.ch/dataset/aef0654e-1691-49a2-b5fd-2fb220b78bfd/resource/6b066954-c9ce-4438-be0a-6ab01b3e525b/download/bev570od5702.csv"


# death (dea)
dea_od <- "https://data.stadt-zuerich.ch/dataset/bev_todesfaelle_jahr_alter_geschlecht_herkunft_quartier_od5703/download/BEV570OD5703.csv"

# death (dea, data of the Federal Statistical Office FSO)
dea_fso_od <- "https://data.stadt-zuerich.ch/dataset/bfs_bev_sterberaten_jahr_alter_geschlecht_herkunft_od5708/download/BEV570OD5708.csv"

# immigration (imm)
# migration in the City of Zurich; across the city border
imm_od <- "https://data.stadt-zuerich.ch/dataset/bev_zuz_jahr_quartier_alter_geschlecht_herkunft_od5704/download/BEV570OD5704.csv"

# emigration (emi)
# migration out of the City of Zurich; across the city border
emi_od <- "https://data.stadt-zuerich.ch/dataset/bev_wegz_jahr_quartier_alter_geschlecht_herkunft_od5705/download/BEV570OD5705.csv"

# relocation (rel)
# migration within the City of Zurich; inside the city border
rel_od <- "https://data.stadt-zuerich.ch/dataset/bev_umzuege_jahr_quartier_alter_geschlecht_herkunft_od5706/download/BEV570OD5706.csv"

# naturalization (nat)
# on the open data platform the data consist both of naturalization and denaturalization
nat_od <- "https://data.stadt-zuerich.ch/dataset/bev_brw_jahr_alter_geschlecht_herkunft_quartier_od5707/download/BEV570OD5707.csv"

# living space (spa)
spa_od <- "https://data.stadt-zuerich.ch/dataset/bau_best_whg_geb_gebmwhg_wfl_pers_statzone_jahr_od6981/download/BAU698OD6981.csv"

# population scenarios on open data (sce)
sce_od <- "https://data.stadt-zuerich.ch/dataset/bev_szenarien_od3440/download/BEV344OD3440.csv"



# specific functions ------------------------------------------------------

# plot functions
source(paste0(code_path, "0000_General/0004_plot-functions.r"))

# constrained regression
source(paste0(code_path, "0000_General/0011_constrained-regression.r"))

# life expectancy
source(paste0(code_path, "0000_General/0012_life-expectancy.r"))
