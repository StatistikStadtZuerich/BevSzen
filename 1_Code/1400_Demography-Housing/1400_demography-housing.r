# header ------------------------------------------------------------------
# demography and housing model



# paths, general ----------------------------------------------------------

# source(paste0(here::here(), "/1_code/0000_general/general_utils.R"))
# util_gf()

# start time
t0 <- Sys.time()


# import, data preparation ------------------------------------------------

# population: import

# in contrast to rate calculations: population at the end of the year
# WHY: the rates will be applied here to the population of the previous year

pop_import <- read_csv(pop_od, lazy = FALSE) %>%
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
  ungroup()

# population: all cases
pop <- as_tibble(expand_grid(
  district = uni_d,
  year = date_start:date_end,
  age = age_min:age_max,
  sex = uni_s,
  origin = uni_o
)) %>%
  left_join(pop_import, by = c("district", "year", "age", "sex", "origin")) %>%
  replace_na(list(pop = 0))

# birth: fertility rate
fer <- read_csv(paste0(exp_path, "/birth_fertility_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    sex = uni_s[2],
    origin = factor(origin, levels = uni_o)
  ) %>%
  select(district, year, age, sex, origin, fer)

# birth: origin change
cha <- read_csv(paste0(exp_path, "/birth_origin-change_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    origin = factor(origin, levels = uni_o)
  )

# birth: proportion male
pro_male <- read_csv(paste0(exp_path, "/birth_sex-ratio_future.csv"), lazy = FALSE)

# death: mortality rate
mor <- read_csv(paste0(exp_path, "/mortality_future.csv"), lazy = FALSE) %>%
  mutate(sex = factor(sex, levels = uni_s))

# immigration*: immigration* rate
ims_rate <- read_csv(paste0(exp_path, "/immigration-star_rate-dy_future.csv"), lazy = FALSE) %>%
  mutate(district = factor(district, levels = uni_d))

# immigration*: sex and origin proportion
ims_prop_so <- read_csv(paste0(exp_path, "/immigration-star_prop-so-dy_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )

# immigration*: age proportion
ims_prop_a <- read_csv(paste0(exp_path, "/immigration-star_prop-a-dyso_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )


# #immigration (for ims-plot)
# imm_past <- read_csv(imm_od) %>% 
#   rename(year = EreignisDatJahr) %>%    
#   left_join(look_dis, by = "QuarCd") %>% 
#   mutate(district = factor(distr, uni_d)) %>%  
#   group_by(district, year) %>% 
#   summarize(ims = sum(AnzZuzuWir),
#             .groups = "drop")
# 
# 
# #emigration (for ems-plot)
# emi_past <- read_csv(emi_od) %>% 
#   rename(year = EreignisDatJahr) %>%    
#   left_join(look_dis, by = "QuarCd") %>% 
#   mutate(district = factor(distr, uni_d)) %>% 
#   group_by(district, year) %>% 
#   summarize(ems = sum(AnzWezuWir),
#             .groups = "drop")
# 
# #immigration* (for plot)
# ims_past <- read_csv(rel_od) %>% 
#   rename(year = EreignisDatJahr, ims = AnzUmzuWir) %>% 
#   left_join(look_dis, by = "QuarCd") %>% 
#   mutate(district = factor(distr, uni_d)) %>% 
#   select(district, year, ims) %>%       
#   bind_rows(imm_past) %>%      
#   group_by(district, year) %>% 
#   summarize(ims = sum(ims),
#             .groups = "drop")
# 
# #emigration* (for plot)
# ems_past <- read_csv(rel_od) %>% 
#   rename(year = EreignisDatJahr, ems = AnzUmzuWir) %>% 
#   left_join(look_dis, by = c("QuarBisherCd" = "QuarCd")) %>% 
#   mutate(district = factor(distr, uni_d)) %>% 
#   select(district, year, ems) %>%       
#   bind_rows(emi_past) %>%      
#   group_by(district, year) %>% 
#   summarize(ems = sum(ems),
#             .groups = "drop")        
    
# emigration*: emigration* rate
ems_rate <- read_csv(paste0(exp_path, "/emigration-star_rate-dy_future.csv"), lazy = FALSE) %>%
  mutate(district = factor(district, levels = uni_d))

# emigration*: sex and origin proportion
ems_prop_so <- read_csv(paste0(exp_path, "/emigration-star_prop-so-dy_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )

# emigration*: age proportion
ems_prop_a <- read_csv(paste0(exp_path, "/emigration-star_prop-a-dyso_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    sex = factor(sex, levels = uni_s),
    origin = factor(origin, levels = uni_o)
  )

# relocation, immigration
rei <- read_csv(paste0(exp_path, "/relocation_immigration_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    origin = factor(origin, levels = uni_o)
  )

# relocation, emigration
ree <- read_csv(paste0(exp_path, "/relocation_emigration_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    origin = factor(origin, levels = uni_o)
  )

# naturalization
nat <- read_csv(paste0(exp_path, "/naturalization_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(district, levels = uni_d),
    sex = factor(sex, levels = uni_s),
    origin = uni_o[2]
  ) %>%
  rename(rate_nat = rate_nat_dyas) %>%
  select(district, year, age, sex, origin, rate_nat)

# housing model
hou <- read_csv(paste0(exp_path, "/housing-model_population_d.csv"), lazy = FALSE) %>%
  mutate(district = factor(district, levels = uni_d)) %>%
  rename(pop_limit = pop)


# loop over years ---------------------------------------------------------

# last year of data,
pop_last <- filter(pop, year == date_end)

# years in the future
future <- scen_begin:scen_end

# outputs
out_bir <- NULL
out_dem <- NULL
out_pop <- NULL
out_nat <- NULL
out_bal <- NULL # WHY? for balance checks
out_pop_smooth <- NULL # WHY? for smoothing checks

# loop over years
for (iyear in future) {

  # iyear <- 2022

  # population at the begin of the year = population at the end of the previous year
  if (iyear == min(future)) {
    popu <- select(pop_last, -year)
  } else {
    popu <- select(pop_end_year, -year)
  }

  # births (fertility rate * women in the population)
  bir_do <- fer %>%
    filter(year == iyear) %>%
    left_join(popu, by = c("district", "age", "sex", "origin")) %>%
    replace_na(list(fer = 0, pop = 0)) %>%
    mutate(bir = pop * fer / 100) %>%
    group_by(district, origin) %>%
    summarize(bir = sum(bir),
              .groups = "drop")

  # sum(bir_do$bir)

  # births: origin changes (from mother to baby)
  bir_do_new <- cha %>%
    filter(year == iyear) %>%
    select(-year) %>%
    right_join(bir_do, by = c("district", "origin")) %>%
    mutate(
      change = bir * cha / 100,
      keep = bir - change
    ) %>%
    select(district, origin, change, keep) %>%
    pivot_longer(
      cols = c("change", "keep"),
      names_to = "category", values_to = "bir"
    ) %>%
    mutate(new_origin = case_when(
      category == "keep" ~ origin,
      origin == uni_o[1] ~ uni_o[2],
      TRUE ~ uni_o[1]
    )) %>%
    select(district, new_origin, bir) %>%
    rename(origin = new_origin) %>%
    group_by(district, origin) %>%
    summarize(bir = sum(bir),
              .groups = "drop")

  # births: with variable 'sex'

  # why age -1? still perspective at the begin of the year
  # the newborn babies are one year younger than the population at age zero
  # ageing of the population is executed at the very end of the code

  pro_male_value <- filter(pro_male, year == iyear)

  bir <- bir_do_new %>%
    mutate(
      pro_male = pro_male_value$pro_male,
      male = bir * pro_male / 100,
      female = bir - male
    ) %>%
    select(-c(bir, pro_male)) %>%
    pivot_longer(
      cols = uni_s,
      names_to = "sex", values_to = "bir"
    ) %>%
    mutate(
      age = -1,
      sex = factor(sex, levels = uni_s)
    ) %>%
    select(district, age, sex, origin, bir) %>%
    arrange(district, sex, origin)

  # sum(bir_do$bir)
  # sum(bir_do_new$bir)
  # sum(bir$bir)

  # deaths (mortality rate * population)
  dea <- mor %>%
    filter(year == iyear) %>%
    right_join(popu, by = c("age", "sex")) %>%
    replace_na(list(mor = 0, pop = 0)) %>%
    mutate(dea = pop * mor / 100) %>%
    select(district, age, sex, origin, dea)

  # sum(dea$dea)

  # immigration*
  ims <- popu %>%
    group_by(district) %>%
      summarize(pop = sum(pop)) %>%
    ungroup() %>%
    left_join(filter(ims_rate, year == iyear), by = "district") %>%
    mutate(ims_d = pop * rate / 100) %>%
    # here: keep the year in order to join the right year below
    select(-c(pop, rate)) %>%
    # until here: immigration* by district (31 rows)
    left_join(ims_prop_so, by = c("district", "year")) %>%
    mutate(ims_dso = ims_d * prop / 100) %>%
    select(-c(ims_d, prop)) %>%
    # until here: immigration* by district, sex, origin (31*2*2 = 124 rows)
    left_join(ims_prop_a, by = c("district", "year", "sex", "origin")) %>%
    mutate(ims = ims_dso * prop / 100) %>%
    select(district, age, sex, origin, ims)
  # result: immigration* by district, age, sex, origin (31*121*2*2 = 15004 rows)


  # sum(ims$ims)

  # emigration*
  ems <- popu %>%
    group_by(district) %>%
    summarize(pop = sum(pop)) %>%
    ungroup() %>%
    left_join(filter(ems_rate, year == iyear), by = "district") %>%
    mutate(ems_d = pop * rate / 100) %>%
    # here: keep the year in order to join the right year below
    select(-c(pop, rate)) %>%
    # until here: emigration* by district (31 rows)
    left_join(ems_prop_so, by = c("district", "year")) %>%
    mutate(ems_dso = ems_d * prop / 100) %>%
    select(-c(ems_d, prop)) %>%
    # until here: emigration* by district, sex, origin (31*2*2 = 124 rows)
    left_join(ems_prop_a, by = c("district", "year", "sex", "origin")) %>%
    mutate(ems = ems_dso * prop / 100) %>%
    select(district, age, sex, origin, ems)
  # result: emigration* by district, age, sex, origin (31*121*2*2 = 15004 rows)

  # sum(ems$ems)


  # combine the demographic processes

  dem <- as_tibble(expand_grid(
    district = uni_d,
    age = (-1):age_max,
    sex = uni_s,
    origin = uni_o
  )) %>%
    left_join(popu, by = c("district", "age", "sex", "origin")) %>%
    left_join(bir, by = c("district", "age", "sex", "origin")) %>%
    left_join(dea, by = c("district", "age", "sex", "origin")) %>%
    left_join(ims, by = c("district", "age", "sex", "origin")) %>%
    left_join(ems, by = c("district", "age", "sex", "origin")) %>%
    replace_na(list(pop = 0, bir = 0, dea = 0, ims = 0, ems = 0)) %>%
    # not more than the entire population can die...
    # therefore, calculate effective deaths
    mutate(
      dea_eff = pmin(dea, pop),
      pop_bir_dea = pop + bir - dea_eff
    )

  # sum(dem$pop)
  # sum(bir$bir) - sum(dea$dea)
  # sum(dem$pop_bir_dea)
  # sum(dem$pop) + sum(bir$bir) - sum(dea$dea)

  # dem %>%
  #     select(pop, bir, dea, dea_eff, ims, ems, pop_bir_dea) %>%
  #     summarize_all(list(sum))

  # balance (on district level)
  # if not enough space: decrease immigration, increase emigration
  # if space left: increase immigration, decrease emigration

  hou_year <- hou %>%
    filter(year == iyear) %>%
    select(-year)

  bal <- dem %>%
    group_by(district) %>%
    summarize(
      pop_bir_dea = sum(pop_bir_dea),
      ims = sum(ims),
      ems = sum(ems),
      .groups = "drop"
    ) %>%
    # theoretical population:
    # pop + birth - death + immigration* - emigration*
    mutate(pop_theo = pop_bir_dea + ims - ems) %>%
    left_join(hou_year, by = "district") %>%
    # tests if correction (ims3, ems3 are correct)
    # mutate(ims = if_else(district == "Kreis 1", 5, ims)) %>%
    # mutate(ems = if_else(district == "Leimbach", 50, ems)) %>%
    mutate(
      differ = pop_limit - pop_theo,
      differ_ims = if_else(differ < 0,
        less_ims / 100 * differ,
        more_ims / 100 * differ
      ),
      differ_ems = if_else(differ < 0,
        (1 - less_ims / 100) * differ,
        (1 - more_ims / 100) * differ
      ),
      new_ims = ims + differ_ims,
      new_ems = ems - differ_ems,
      # immigration* and emigration* cannot be negative
      new_ims2 = pmax(0, new_ims),
      new_ems2 = pmax(0, new_ems),
      # case: not enough space
      # if the decrease of immigration* is not enough to reach
      # the pop limit (since immigration* is already decreased to zero),
      # then emigration* is increased
      new_ems3 = if_else(new_ims < 0, new_ems2 + abs(new_ims),
        new_ems2
      ),
      # case: too much space
      # if the decrease of emigration* is not enough to reach
      # the pop limit (since emigration* is already decreased to zero),
      # then immigration* is increased
      new_ims3 = if_else(new_ems < 0, new_ims2 + abs(new_ems),
        new_ims2
      ),
      factor_ims = new_ims3 / ims,
      factor_ems = new_ems3 / ems,
      check = pop_bir_dea + new_ims3 - new_ems3 - pop_limit
    )

  # sum(bal$differ)
  # sum(abs(bal$check))



  # apply the correction factors to the immigration* and emigration* by
  # district, age, sex, origin (same factor by district)
  dem_factor <- bal %>%
    select(district, factor_ims, factor_ems) %>%
    right_join(dem, by = "district") %>%
    mutate(
      ims_eff = ims * factor_ims,
      ems_eff = ems * factor_ems,
      pop_temp = pop_bir_dea + ims_eff - ems_eff,
      pop_end_year = pmax(0, pop_temp)
    ) %>%
    select(
      district, age, sex, origin,
      bir, dea_eff, ims, ems, ims_eff, ems_eff, pop_bir_dea, pop_temp, pop_end_year
    ) %>%
    rename(ims_initial = ims, ems_initial = ems)

  # sum(dem_factor$pop_end_year)

  # with naturalization (only on end-year-population)

  # the naturalization rate of all new born babies of zero is correct
  # WHY? the origin changes at birth has already been calculated in the model

  pop_nat_temp <- nat %>%
    filter(year == iyear) %>%
    select(-year) %>%
    right_join(select(
      dem_factor, district, age, sex,
      origin, pop_end_year
    ),
    by = c("district", "age", "sex", "origin")
    ) %>%
    replace_na(list(rate_nat = 0)) %>%
    arrange(district, age, sex, origin) %>%
    mutate(
      Swiss = if_else(origin == uni_o[1], pop_end_year,
        pop_end_year * rate_nat / 100
      ),
      foreign = if_else(origin == uni_o[1], 0,
        pop_end_year * (1 - rate_nat / 100)
      )
    )

  # WHY split the pipe here? to calculate the future amount of naturalizations
  # count, not only rates

  pop_nat <- pop_nat_temp %>%
    select(-c(origin, pop_end_year)) %>%
    pivot_longer(
      cols = uni_o,
      names_to = "origin", values_to = "pop_end_year"
    ) %>%
    mutate(origin = factor(origin, levels = uni_o)) %>%
    select(district, age, sex, origin, pop_end_year) %>%
    group_by(district, age, sex, origin) %>%
    summarize(pop_end_year = sum(pop_end_year),
              .groups = "drop")

  nat_count <- pop_nat_temp %>%
    filter((origin == uni_o[2]) & (age >= 0)) %>%
    group_by(district, age, sex) %>%
    summarize(nat = sum(Swiss),
              .groups = "drop")

  # determine immigration and relocation from immigration*
  rei_year <- rei %>%
    filter(year == iyear) %>%
    select(-year)

  imm <- dem_factor %>%
    filter(age >= 0) %>%
    select(district, age, sex, origin, ims_eff) %>%
    left_join(rei_year, by = c("district", "age", "origin")) %>%
    mutate(
      rel = ims_eff * prop_rel_dyao / 100,
      imm = ims_eff - rel
    ) %>%
    select(district, age, sex, origin, imm, rel)

  # sum(imm$imm)
  # sum(imm$rel)

  # determine emigration and relocation from emigration*
  ree_year <- ree %>%
    filter(year == iyear) %>%
    select(-year)

  emi <- dem_factor %>%
    filter(age >= 0) %>%
    select(district, age, sex, origin, ems_eff) %>%
    left_join(ree_year, by = c("district", "age", "origin")) %>%
    mutate(
      rel = ems_eff * prop_rel_dyao / 100,
      emi = ems_eff - rel
    ) %>%
    select(district, age, sex, origin, emi, rel)

  # sum(emi$emi)
  # sum(emi$rel)

  # age plus 1, and with variable 'year'
  pop_end_year_age <- pop_nat %>%
    mutate(
      age = age + 1,
      year = iyear
    ) %>%
    select(district, year, age, sex, origin, pop_end_year) %>%
    filter(age <= age_max)

  #smoothing over age
  pop_end_year_smooth <- pop_end_year_age %>% 
    group_by(district, year, origin, sex) %>%    
    arrange(age) %>%
    mutate(pop = pmax(0, predict(
      loess(pop_end_year ~ age, span = deh_span, degree = 1, na.action = na.aggregate)
    ))) %>%
    ungroup()  
  
  #population: output
  pop_end_year <- pop_end_year_smooth %>% 
    select(-pop_end_year)
  
  # outputs: birth (separate, since no 'age' variable), with variable 'year'
  out_bir <- bir %>%
    mutate(year = iyear) %>%
    select(district, year, sex, origin, bir) %>%
    bind_rows(out_bir)

  # outputs: demographic processes, with variable 'year'
  out_dem <- dem_factor %>%
    select(
      district, age, sex, origin,
      dea_eff, ims_eff, ems_eff,
      ims_initial, ems_initial, pop_bir_dea, pop_end_year
    ) %>%
    filter(age >= 0) %>%
    rename(dea = dea_eff, ims = ims_eff, ems = ems_eff) %>%
    left_join(imm, by = c("district", "age", "sex", "origin")) %>%
    rename(rei = rel) %>%
    left_join(emi, by = c("district", "age", "sex", "origin")) %>%
    rename(ree = rel) %>%
    mutate(year = iyear) %>%
    select(
      district, year, age, sex, origin,
      dea, ims, imm, rei, ems, emi, ree,
      ims_initial, ems_initial, pop_bir_dea, pop_end_year
    ) %>%
    bind_rows(out_dem)

  # outputs: end of year population
  out_pop <- pop_end_year %>%
    bind_rows(out_pop)
  
  # outputs: population before and after smoothing  
  out_pop_smooth <- pop_end_year_smooth %>%
    bind_rows(out_pop_smooth)    

  # outputs: naturalization (separate, since no 'origin' variable), with variable 'year'
  out_nat <- nat_count %>%
    mutate(year = iyear) %>%
    select(district, year, age, sex, nat) %>%
    bind_rows(out_nat)

  # outputs: balance (for checks), with variable 'year'
  out_bal <- bal %>%
    mutate(year = iyear) %>%
    bind_rows(out_bal)

  # end of loop over years
}


# # balance checks ----------------------------------------------------------
# 
# # enough immigration* and/or emigration* for correction?
# # if no records: new_ims and new_ems were enough
# # no need to use new_ims2/new_ems2 and new_ims3/new/ems3
# enough_ims_ems <- out_bal %>%
#   filter((new_ims < 0) | (new_ems < 0))
# 
# 
# # balance: check = pop_bir_dea + new_ims3 - new_ems3 - pop_limit
# # if the balance works, the new population should be the population-limit
# # i.e. the result should be zero
# range(out_bal$check)
# 
# # difference: population limit minus population (based on migration trends)
# # negative value: not enough space (immigration decreased, emigration increased)
# # positive value: too much space (immigration increased, emigration decreased)
# 
# pos_neg <- c("positive", "negative")
# 
# dat_pos_neg <- out_bal %>%
#   mutate(
#     differ_prop = differ / pop_limit * 100,   
#     cat = factor(if_else(differ < 0, pos_neg[2], pos_neg[1]),
#     levels = pos_neg
#   ))
#   
#   
# #plot: absolute difference  
#   sszplot(dat_pos_neg,
#     aes_x = "year", aes_y = "differ", aes_fill = "cat",
#     geom = "col", gridscale = "free_y",
#     labs_x = "", 
#     labs_y = "people ('population limit' minus 'population based on trends')", 
#     angle = 90, wrap = "district", ncol = 4,
#     name = "1400_population-limit_absolute-difference",
#     width = 16, height = 16
#   )
# 
# #plot: relative difference  
#   sszplot(dat_pos_neg,
#     aes_x = "year", aes_y = "differ_prop", aes_fill = "cat",
#     geom = "col", 
#     labs_x = "", 
#     labs_y = "difference in % ('population limit' minus 'population based on trends')", 
#     angle = 90, wrap = "district", ncol = 4,
#     name = "1401_population-limit_relative-difference",
#     width = 16, height = 16
#   )
# 
# 
# 
# # plot: migration correction ----------------------------------------------
#   
# cat <- c("past", "initial", "corrected")
# proc <- c("immigration*", "emigration*")  
#   
# ims_ems_past <- ims_past %>% 
#     left_join(ems_past, by = c("district", "year")) %>% 
#     rename(ims_past = ims, ems_past = ems) %>% 
#     pivot_longer(c(ims_past, ems_past),
#     names_to = "category", values_to = "people")  
# 
# ims_ems_future <- out_bal %>% 
#   select(district, year, ims, ems, new_ims3, new_ems3) %>% 
#   pivot_longer(c(ims, ems, new_ims3, new_ems3),
#     names_to = "category", values_to = "people")
# 
# ims_ems_past %>%
#   bind_rows(ims_ems_future) %>%
#   mutate(
#     cat = factor(case_when(
#       category %in% c("ims_past", "ems_past") ~ cat[1],
#       category %in% c("ims", "ems") ~ cat[2],
#       TRUE ~ cat[3]
#     ), levels = cat),
#     process = factor(if_else(category %in% c("ims_past", "ims", "new_ims3"),
#       proc[1], proc[2]
#     ), levels = proc)
#   ) %>% 
#   sszplot(
#     aes_x = "year", aes_y = "people", aes_col = "cat",
#     wrap = "process",
#     labs_x = "", labs_y = "migration per year",
#     scale_y = c(0, NA),
#     name = "1402_migration-correction",
#     width = 10, height = 5,
#     multi = uni_d
#   )
#     
#   
# 
# 
# # plot: smoothing over age ------------------------------------------------
# 
# # selected years
# sel_years <- uniy_scen[(uniy_scen %% 5) == 0]
# 
# # plot levels
# sel_lev <- c("initial", "smoothed")
# 
# out_pop_smooth %>%
#   pivot_longer(c(pop_end_year, pop), names_to = "category", values_to = "pop") %>%
#   mutate(cat = factor(if_else(category == "pop_end_year",
#     sel_lev[1], sel_lev[2]), levels = sel_lev),
#     pop_pyramid = if_else(sex == uni_s[1], -pop, pop),) %>%
#   filter(year %in% sel_years) %>% 
#   sszplot(
#     aes_x = "age", aes_y = "pop_pyramid", aes_col = "sex",
#     aes_ltyp = "cat",
#     labs_y = "population",
#     i_y = 0,
#     width = 18, height = 7,
#     quotes = c(
#       quote(facet_grid(origin~year, scales = "free_x")),
#       quote(coord_flip()),
#       quote(scale_linetype_manual(values = c("dotted", "solid")))
#     ),
#     name = "1403_population-age-smoothing",
#     multi = uni_d
#   )
# 
# 
# 
# # plot: Wollishofen, population and migration (by yao) ----------------------
# 
# # why Wollishofen? need to check age patterns of this district
# # why named d21? 21 is the number for district Wollishofen
# 
# catego <- c("immigration", "emigration", "net migration", "population")
# line_catego <- c("initial", "final")
# 
# # why not piped into plot? to get limits, and another plot (without origin)
# 
# pop_mig_21_yao <- out_dem %>%
#   filter((district == "Wollishofen") & year >= 2022) %>%
#   select(year, age, origin, ims_initial, ims, ems_initial, ems, pop_bir_dea, pop_end_year) %>%
#   mutate(
#     net_initial = ims_initial - ems_initial,
#     net = ims - ems
#   ) %>%
#   pivot_longer(
#     cols = c(
#       "ims_initial", "ims", "ems_initial", "ems",
#       "net_initial", "net", "pop_bir_dea", "pop_end_year"
#     ),
#     names_to = "category", values_to = "people"
#   ) %>%
#   group_by(year, age, origin, category) %>%
#   summarize(
#     people = sum_NA(people),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     cat = factor(case_when(
#       category %in% c("ims", "ims_initial") ~ catego[1],
#       category %in% c("ems", "ems_initial") ~ catego[2],
#       category %in% c("net", "net_initial") ~ catego[3],
#       TRUE ~ catego[4]
#     ), levels = catego),
#     line_cat = factor(if_else(
#       category %in% c("ims_initial", "ems_initial", "net_initial", "pop_bir_dea"),
#       line_catego[1], line_catego[2]
#     ), levels = line_catego))
#   
# upper_lim_yao <- pop_mig_21_yao %>%   
#   group_by(cat) %>% 
#     summarize(upper = max_NA(people),
#               .groups = "drop")
# 
# # is it possible to set the y-limits per group?
# 
#   sszplot(pop_mig_21_yao,
#     aes_x = "age", aes_y = "people", aes_col = "origin", aes_ltyp = "line_cat",
#     labs_y = "people",
#     wrap = "cat", ncol = 4,
#     scale_y = c(0, max(upper_lim_yao$upper)),
#     width = 12, height = 5,
#     i_x = seq(0, age_max, by = 20),    
#     i_y = 0,
#     name = "1404_pop_migration_d21yao",
#     multi = uniy_scen
#   )
#   
# 
# 
# # plot: Wollishofen, population and migration (by ya) ---------------------
# 
# pop_mig_21_ya <- pop_mig_21_yao %>% 
#     group_by(year, age, cat, line_cat) %>% 
#       summarize(people = sum_NA(people))
#   
# upper_lim_ya <- pop_mig_21_ya %>%   
#   group_by(cat) %>% 
#     summarize(upper = max_NA(people),
#               .groups = "drop")
# 
# # is it possible to set the y-limits per group?
# 
# 
#   sszplot(pop_mig_21_ya,
#     aes_x = "age", aes_y = "people", aes_ltyp = "line_cat",
#     labs_y = "people",
#     wrap = "cat", ncol = 4,
#     scale_y = c(0, max(upper_lim_ya$upper)),
#     width = 12, height = 5,
#     i_x = seq(0, age_max, by = 20),    
#     i_y = 0,
#     name = "1405_pop_migration_d21ya",
#     multi = uniy_scen
#   )  
  
  
 # balance for Wollishofen 
out_bal %>% 
  filter(district == "Wollishofen") %>% 
  arrange(year) %>% 
  select(year, pop_bir_dea, ims, ems, pop_theo, pop_limit, 
         differ, differ_ims, differ_ems, 
         new_ims, new_ems, new_ims2, new_ems2, new_ims3, new_ems3) %>% 
  write_csv(paste0(out_path, "/balance_Wollishofen.csv"))  


# export the results ------------------------------------------------------

# births
out_bir %>%
  arrange(district, year, sex, origin) %>%
  write_csv(paste0(out_path, "/births_future.csv"))

# demographic processes
out_dem %>%
  select(district, year, age, sex, origin, dea, ims, imm, rei, ems, emi, ree) %>% 
  arrange(district, year, age, sex, origin) %>%
  write_csv(paste0(out_path, "/demographic-processes_future.csv"))

# population (end of year)
out_pop %>%
  arrange(district, year, age, sex, origin) %>%
  write_csv(paste0(out_path, "/population_future.csv"))

# naturalization
out_nat %>%
  arrange(district, year, age, sex) %>%
  write_csv(paste0(out_path, "/naturalization_future.csv"))

# log info
cat_log(paste0(
  "demography and housing: ",
  capture.output(Sys.time() - t0)
))
