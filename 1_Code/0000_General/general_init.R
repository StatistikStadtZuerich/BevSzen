#' load general functions and parameters
#' 
#' this function loads all necessary functions, parameters and variables, creates paths, lookup tables etc.
#' and assigns them to the global environment. It has to be loaded at the very beginning.
#'
#' @param i_scen which scenario should be run
#' @param var_file location of the variables file relative to project path
#'
#' @return list of parameters and variables
#' @export
#'
#' @examples util_gf("lower")
init <- function(i_scen = "middle", var_file = "/2_Data/3_Parameter/variables.yml"){
  # packages
  require(tidyverse)
  require(dvmisc) # for expand_grid
  require(gridExtra) # for ggplot on multiple pages
  require(nlme) # needed for mgcv, see below
  require(mgcv) # for gam
  require(flexclust) # weighted kmeans clustering
  require(ggrepel) # to label lines (when too many for a normal legend)
  require(zoo) # moving average (no confusion with filter function in base R)
  require(ckanr) # download of open data directly from ckan
  require(scales) # for pretty axis breaks
  require(rlang) # for functions: symbols, quasiquotation (!!, !!!, :=)
  require(gtools) # invalid function (to check for NA, NULL, NaN), e.g. in constrained regression
  # require(this.path) # to extract the current file name
  require(modelr) # add_predictions
  require(quarto) #for programmatically rendering quarto
  
  # no scientic notation
  options(scipen = 999)
    
  # general stuff (without dependency on parameters) ----------------------------------
  # read general variables for common use
  vars <- yaml::read_yaml(paste0(here::here(), var_file), eval.expr = TRUE, fileEncoding = "UTF-8")
  vars$i_scen <- i_scen
  
  # import parameters
  para <- read_delim(paste0(here::here(), vars$para_file), ";", lazy = FALSE) %>%
    select(parameter, lower, middle, upper) %>%
    pivot_longer(cols = lower:upper, names_to = "scenario") %>%
    filter(scenario == i_scen) %>%
    select(parameter, value)
  
  # read parameters (depend on scenario)
  for (i_para in 1:nrow(para)) {
    assign(para$parameter[i_para], para$value[i_para], envir = .GlobalEnv)
  }
  para <- para %>%
    pivot_wider(names_from = parameter) %>%
    as.list()
  

  # create variables --------------------------------------------------------
  # lookup tables
  vars$look_dis <- read_csv2(paste0(here::here(), vars$dis_file), lazy = FALSE) %>%
    select(QuarCd, distr)

  
  vars$look_reg <- mutate(vars$look_dis, distnum = if_else(distr == "Kreis 1", 10, as.numeric(QuarCd))) %>%
    select(distnum, distr) %>%
    unique() %>%
    mutate(district = factor(distr, levels = unique(vars$look_dis$distr))) %>%
    select(-distr)
  
  vars$look_car <- tibble(initial = vars$car_initial, category = vars$car_category)
  
  vars$look_pro <- tibble(code = as.double(1:7), initial = vars$pro_initial, category = vars$pro_category) %>%
    mutate(status = factor(category, levels = vars$pro_category))
  
  vars$look_a1 <- tibble(age = 15:49) %>%
    mutate(age_1 = factor(if_else(age < vars$age_1[1], vars$age_1t[1],
                                  if_else(age < vars$age_1[2], vars$age_1t[2], vars$age_1t[3])
    ), levels = vars$age_1t))
  
  vars$look_a2 <- tibble(age = 15:49) %>%
    mutate(age_2 = factor(if_else(age < vars$age_2[1], vars$age_2t[1],
                                  if_else(age < vars$age_2[2], vars$age_2t[2],
                                          if_else(age < vars$age_2[3], vars$age_2t[3],
                                                  if_else(age < vars$age_2[4], vars$age_2t[4], vars$age_2t[5])
                                          )
                                  )
    ), levels = vars$age_2t))
  
  vars$look_a3 <- tibble(age = 0:120) %>%
    mutate(age_3 = factor(case_when(
      age < vars$age_3[2] ~ vars$age_3t[1],
      age < vars$age_3[3] ~ vars$age_3t[2],
      age < vars$age_3[4] ~ vars$age_3t[3],
      age < vars$age_3[5] ~ vars$age_3t[4],
      age < vars$age_3[6] ~ vars$age_3t[5],
      age < vars$age_3[7] ~ vars$age_3t[6],
      age < vars$age_3[8] ~ vars$age_3t[7],
      age < vars$age_3[9] ~ vars$age_3t[8],
      age < vars$age_3[10] ~ vars$age_3t[9],
      TRUE ~ vars$age_3t[10]
    ), levels = vars$age_3t))
  
  vars$look_a4 <- tibble(age = 0:120) %>%
    mutate(age_4 = factor(case_when(
      age < vars$age_4[2] ~ vars$age_4t[1],
      age < vars$age_4[3] ~ vars$age_4t[2],
      age < vars$age_4[4] ~ vars$age_4t[3],
      age < vars$age_4[5] ~ vars$age_4t[4],
      TRUE ~ vars$age_4t[5]
    ), levels = vars$age_4t))

  # unique levels
  vars$text_d <- unique(vars$look_dis$distr)
  vars$uni_d <- factor(vars$text_d, levels = vars$text_d)
  vars$uni_s <- factor(vars$text_s, levels = vars$text_s)
  vars$uni_o <- factor(vars$text_o, levels = vars$text_o)
  vars$uni_r <- factor(vars$text_r, levels = vars$text_r)
  vars$uni_e <- factor(vars$text_e, levels = vars$text_e)
  vars$uni_p <- factor(vars$text_p, levels = vars$text_p)
  vars$uni_w <- factor(vars$text_w, levels = vars$text_w)
  vars$uni_t <- factor(vars$pro_category, levels = vars$pro_category)
  vars$uni_i <- factor(vars$text_i, levels = vars$text_i)
  vars$uni_c <- factor(vars$text_c, levels = vars$text_c)
  
  vars$look_own <- tibble(EigentumGrundstkCd = as.integer(labels(vars$uni_w)), owner = levels(vars$uni_w))
  
  # colors
  vars$col_d <- colorRampPalette(vars$col_6)(31)
  # plot(1:31, 1:31, col = col_d, pch = 16, cex = 3)
  vars$col_s <- vars$col_6[c(1, 3)]
  # plot(1:2, 1:2, col = col_s, pch = 16, cex = 3)
  vars$col_o <- vars$col_6[c(3, 2)]
  # plot(1:2, 1:2, col = col_o, pch = 16, cex = 3)
  vars$col_r <- vars$col_6[c(1, 3)]
  # plot(1:2, 1:2, col = col_r, pch = 16, cex = 3)
  vars$col_w <- vars$col_6[c(2, 5)]
  # plot(1:2, 1:2, col = col_w, pch = 16, cex = 3)
  vars$col_e <- vars$col_6[c(4, 3, 2)]
  # plot(1:3, 1:3, col = col_e, pch = 16, cex = 3)
  vars$col_t <- vars$col_6
  # plot(1:6, 1:6, col = col_t, pch = 16, cex = 3)
  vars$col_i <- vars$col_6[c(1, 6)]
  # plot(1:2, 1:2, col = col_i, pch = 16, cex = 3)
  
  # themes: neutral design for ggplot
  vars$neutral <- theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour = vars$col_grey),
    panel.border = element_rect(colour = vars$col_grey)
  )

  # utility functions
  source(paste0(vars$code_path, "/0000_general/general_utils.r"))
  
  # plot functions
  source(paste0(vars$code_path, "/0000_general/general_sszplot.r"))
  
  # constrained regression
  source(paste0(vars$code_path, "/0000_general/general_constrained-regression.r"))
  
  # life expectancy
  source(paste0(vars$code_path, "/0000_general/general_life-expectancy.r"))
  
  # migration functions
  source(paste0(vars$code_path, "/0000_general/general_migration-functions.r"))
  
  # relocation functions
  source(paste0(vars$code_path, "/0000_general/general_relocation-function.r"))
     
  # general stuff (with dependency on parameters) ----------------------------------
  
  # path for results
  vars$res_path <- paste0(vars$res_path, i_scen)
  dir_ex_create(vars$res_path)
  
  # path for exports (rates)
  vars$exp_path <- paste0(vars$data_path, vars$exp_path, i_scen, "/")
  dir_ex_create(vars$exp_path)
  
  # path for outputs (future: population and demographic processes)
  vars$out_path <- paste0(vars$data_path, vars$out_path, i_scen, "/")
  dir_ex_create(vars$out_path)
  
  # year: base period
  vars$uniy_bir_base <- para$bir_base_begin:para$bir_base_end
  
  # year: future
  vars$uniy_scen <- para$scen_begin:para$scen_end
  
  # year: future (publications)
  vars$uniy_scen_public <- para$scen_begin:para$scen_end_public
  
  # projects not realized
  vars$look_not <- tibble(
    status = factor(vars$pro_category, levels = vars$pro_category),
    not_realized = c(
      para$pro_not_scheduled_ip, para$pro_not_scheduled_other,
      para$pro_not_submitted,
      para$pro_not_approved, para$pro_not_started,
      para$pro_not_completed, para$pro_not_onhold
    )
  )
  
  # color for year (base period)
  vars$col_y_base <- colorRampPalette(vars$col_6)(length(vars$uniy_bir_base))
  
  # colour for time distributions
  vars$col_time <- c(
    rep(vars$col_grey, length(vars$uniy_bir_base)),
    colorRampPalette(vars$col_6[1:5])(length(vars$uniy_scen))
  )
  
  # assign variables to global environment
  for (i_var in 1:length(vars)) {
    assign(labels(vars[i_var]), vars[[i_var]], envir = .GlobalEnv)
  }
  
  # combine all parameters into one list
  append(para, vars)
}