#' Load objects that do not depend on the scenario (run only once)
#' 
#' This function loads all necessary functions, parameters and variables, 
#' creates paths, lookup tables etc. that do not depend on the scenario
#' It has to be loaded at the very beginning.
#'
#' @param i_data_path data path
#'
#' @return list of paths and variables that do not depend on the scenario
#' @export
#'
#' @examples init_once(i_data_path = "~/v/Szen/BevSzen/2026")



init_once <- function(i_data_path){

  
  # packages ----------------------------------------------------------------

  library(ckanr) # download of open data directly from ckan  
  library(dvmisc) # for expand_grid
  library(flexclust) # weighted kmeans clustering  
  library(ggrepel) # to label lines (when too many for a normal legend)  
  library(gridExtra) # for ggplot on multiple pages  
  library(gt) # tables in quarto book
  library(gtools) # invalid function (to check for NA, NULL, NaN), e.g. in constrained regression  
  library(mgcv) # for gam   
  library(modelr) # add_predictions  
  library(nlme) # needed for mgcv  
  library(plotly) # interactive plots
  library(quarto) #for programmatically rendering quarto
  library(rlang) # for functions: symbols, quasiquotation (!!, !!!, :=) 
  library(rprojroot) # R project root
  library(scales) # for pretty axis breaks  
  library(this.path) #for this.dir function
  library(tidyverse) #tidyverse functionality
  library(zoo) # moving average (no confusion with filter function in base R)  


  # paths: folders ----------------------------------------------------------
  
  # root of R project (.Rproj file)
  # why? here::here() not reliable, when rendering the quarto book
  root <- find_root(is_rstudio_project)

  # path for code
  vars <- list()
  vars$code_path <- file.path(root, "R")
  
  # external data path
  vars$ext_path <- i_data_path  

  # path for data
  vars$data_path <- file.path(vars$ext_path, "2_Daten")

  # path for exports (rates)
  vars$exp_path <- file.path(vars$ext_path, "2_Daten/4_Raten") 

  # path for outputs (future: population and demographic processes)
  vars$out_path <- file.path(vars$ext_path, "2_Daten/5_Outputs")  
  
  # log path
  vars$log_path <- file.path(vars$ext_path, "2_Daten/6_Log")    

  # output path for DWH data
  vars$DWH_path <- file.path(vars$ext_path, "2_Daten/7_DWH")  
  
  # path for results
  vars$res_path <- file.path(vars$ext_path, "3_Resultate")

  # output path for quarto book
  vars$book_path <- file.path(vars$ext_path, "3_Resultate/book")  
 
  
  # paths: files ------------------------------------------------------------
  
  # capacity and reserves (data not on open data yet) 
  vars$car_path <- file.path(vars$ext_path, "2_Daten/1_Input/KaReB.csv") 
  vars$car_path_rev <- file.path(vars$ext_path, "2_Daten/1_Input/KaReB_BZO2026.csv") 
  
  # projects (data not on open data yet) 
  vars$pro_path <- file.path(vars$ext_path, "2_Daten/1_Input/BEV347OD3470.csv")   
  
  # evaluation files (with previous scenarios for population and processes)  
  vars$eval_pop_path <- file.path(vars$ext_path, "2_Daten/1_Input/dwh_scen_pop.csv") 
  vars$eval_prc_path <- file.path(vars$ext_path, "2_Daten/1_Input/dwh_scen_prc.csv") 
  
  # log file
  vars$log_file <- file.path(vars$ext_path, "2_Daten/6_Log/log.txt")  
  
  # parameter file
  vars$para_file <- file.path(vars$ext_path, "2_Daten/3_Parameter/parameter.csv")  
  
 
  
  

  # functions ---------------------------------------------------------------
  
  # utility functions
  source(file.path(vars$code_path, "0000_General/0010_utils.R"))
  
  # constrained regression
  source(file.path(vars$code_path, "0000_General/0020_constrained-regression.R"))
  
  # life expectancy
  source(file.path(vars$code_path, "0000_General/0021_life-expectancy.R"))
  
  # relocation functions
  source(file.path(vars$code_path, "0000_General/0023_relocation-function.R"))
  
  

  # create folders ----------------------------------------------------------

  # why not when the folder-paths are defined?  
  # first, the paths are needed to load the utils
  # second, the the dir_ex_create has to be loaded from the utils
  # third, the folders can be created
  
  dir_ex_create(vars$ext_path)
  dir_ex_create(vars$data_path)  
  dir_ex_create(vars$exp_path)    
  dir_ex_create(vars$out_path)  
  dir_ex_create(vars$log_path)  
  dir_ex_create(vars$res_path) 
  dir_ex_create(vars$book_path)  
  dir_ex_create(vars$DWH_path)    
  

  # paths: open data --------------------------------------------------------

  # population (pop)
  vars$pop_od <- "https://data.stadt-zuerich.ch/dataset/80d5c8af-b389-41d2-b6d8-d0deb1639f00/resource/b2abdef7-3e3f-4883-8033-6787a1561987/download/bev390od3903.csv"
  
  # birth (bir)
  vars$bir_od <- "https://data.stadt-zuerich.ch/dataset/aef0654e-1691-49a2-b5fd-2fb220b78bfd/resource/6b066954-c9ce-4438-be0a-6ab01b3e525b/download/bev570od5702.csv"
  
  # death (dea)
  vars$dea_od <- "https://data.stadt-zuerich.ch/dataset/bev_todesfaelle_jahr_alter_geschlecht_herkunft_quartier_od5703/download/BEV570OD5703.csv"
  
  # death (dea, data of the Federal Statistical Office FSO)
  vars$dea_fso_od <- "https://data.stadt-zuerich.ch/dataset/bfs_bev_sterberaten_jahr_alter_geschlecht_herkunft_od5708/download/BEV570OD5708.csv"
  
  # immigration (imm)
  # migration in the City of Zurich; across the city border
  vars$imm_od <- "https://data.stadt-zuerich.ch/dataset/bev_zuz_jahr_quartier_alter_geschlecht_herkunft_od5704/download/BEV570OD5704.csv"
  
  # emigration (emi)
  # migration out of the City of Zurich; across the city border
  vars$emi_od <- "https://data.stadt-zuerich.ch/dataset/bev_wegz_jahr_quartier_alter_geschlecht_herkunft_od5705/download/BEV570OD5705.csv"
  
  # relocation (rel)
  # migration within the City of Zurich; inside the city border
  vars$rel_od <- "https://data.stadt-zuerich.ch/dataset/bev_umzuege_jahr_quartier_alter_geschlecht_herkunft_od5706/download/BEV570OD5706.csv"
  
  # naturalization (nat)
  # on the open data platform the data consist both of naturalization and denaturalization
  vars$nat_od <- "https://data.stadt-zuerich.ch/dataset/bev_brw_jahr_alter_geschlecht_herkunft_quartier_od5707/download/BEV570OD5707.csv"
  
  # living space (spa)
  vars$spa_od <- "https://data.stadt-zuerich.ch/dataset/bau_best_whg_geb_gebmwhg_wfl_pers_statzone_jahr_od6981/download/BAU698OD6981.csv"
  
  # population scenarios on open data (sce)
  vars$sce_od <- "https://data.stadt-zuerich.ch/dataset/bev_szenarien_od3440/download/BEV344OD3440.csv"
  


  

  # lookup tables -----------------------------------------------------------

  # 34 districts  
  vars$look_dis <- tibble(
    QuarCd = c(
      "011","012","013","014","021","023","024","031","033","034",
      "041","042","044","051","052","061","063","071","072","073",
      "074","081","082","083","091","092","101","102","111","115",
      "119","121","122","123"
    ),
    QuarLang = c(
      "Rathaus","Hochschulen","Lindenhof","City","Wollishofen","Leimbach","Enge",
      "Alt-Wiedikon","Friesenberg","Sihlfeld","Werd","Langstrasse","Hard",
      "Gewerbeschule","Escher Wyss","Unterstrass","Oberstrass","Fluntern",
      "Hottingen","Hirslanden","Witikon","Seefeld","Mühlebach","Weinegg",
      "Albisrieden","Altstetten","Höngg","Wipkingen","Affoltern","Oerlikon",
      "Seebach","Saatlen","Schwamendingen-Mitte","Hirzenbach"
    ),
    distr = c(
      rep("Kreis 1", 4),
      "Wollishofen","Leimbach","Enge","Alt-Wiedikon","Friesenberg","Sihlfeld",
      "Werd","Langstrasse","Hard","Gewerbeschule","Escher Wyss","Unterstrass",
      "Oberstrass","Fluntern","Hottingen","Hirslanden","Witikon","Seefeld",
      "Muehlebach","Weinegg","Albisrieden","Altstetten","Hoengg","Wipkingen",
      "Affoltern","Oerlikon","Seebach","Saatlen","Schwamendingen-Mitte","Hirzenbach"
    )
  )  
  
  # 31 regions (Kreis 1 aggregated)   
  vars$look_reg <- mutate(vars$look_dis, distnum = if_else(distr == "Kreis 1", 10, as.numeric(QuarCd))) %>%
    select(distnum, distr) %>%
    unique() %>%
    mutate(district = factor(distr, levels = unique(vars$look_dis$distr))) %>%
    select(-distr) 
  
  # selected districts (small, medium, large population)  
  vars$d_select <- c("Werd", "Hottingen", "Altstetten")
  
  # capacity, reserves (car)
  vars$car_initial <- c("Kapazitaet", "Bestand", "Reserve", "Inanspruchnahme")
  vars$car_category <- c("capacity", "buildings", "reserve", "usage")
  vars$look_car <- tibble(initial = vars$car_initial, category = vars$car_category)
  
  # project status
  vars$pro_initial <- c("projektiert, Infoplan", "projektiert, andere", "eingereicht", "bewilligt", "Bau begonnen", "fertiggestellt", "sistiert")
  vars$pro_category <- c("scheduled, Infoplan", "scheduled, other", "submitted", "approved", "construction started", "completed", "on hold")
  vars$look_pro <- tibble(code = as.double(1:7), initial = vars$pro_initial, category = vars$pro_category) |> 
    mutate(status = factor(category, levels = vars$pro_category))
  
  
  # unique levels -----------------------------------------------------------
  
  # districts
  vars$text_d <- unique(vars$look_dis$distr)
  vars$uni_d <- factor(vars$text_d, levels = vars$text_d)    
  
  # sex
  vars$text_s <- c("male", "female")
  vars$uni_s <- factor(vars$text_s, levels = vars$text_s)
  
  # origin
  vars$text_o <- c("Swiss", "foreign")
  vars$uni_o <- factor(vars$text_o, levels = vars$text_o)
  
  # region
  vars$text_r <- c("Zurich", "Switzerland")
  vars$uni_r <- factor(vars$text_r, levels = vars$text_r)
  
  # residence portion
  vars$text_e <- c("minimum portion", "real portion", "maximum portion")
  vars$uni_e <- factor(vars$text_e, levels = vars$text_e)
  
  # plot construction
  vars$text_p <- c("with plot construction", "without plot construction")
  vars$uni_p <- factor(vars$text_p, levels = vars$text_p)
  
  # property owner
  vars$text_w <- c("cooperative housing", "private housing")
  vars$uni_w <- factor(vars$text_w, levels = vars$text_w)
  vars$look_own <- tibble(EigentumGrundstkCd = as.integer(labels(vars$uni_w)), 
                          owner = levels(vars$uni_w))  
  
  # project status
  vars$uni_t <- factor(vars$pro_category, levels = vars$pro_category)
  
  # indicator (new or removed apartments)
  vars$text_i <- c("new", "removed")
  vars$uni_i <- factor(vars$text_i, levels = vars$text_i)
  
  # past and scenarios
  vars$text_c <- c("past", "lower", "middle", "upper")
  vars$uni_c <- factor(vars$text_c, levels = vars$text_c)
  
  # birth versions
  vars$text_cb <- c("past", "middle_birth_lower", "middle", "middle_birth_upper")  
  vars$uni_cb <- factor(vars$text_cb, levels = vars$text_cb)     
  


  # categories and associated lookup tables ---------------------------------

  # age category 1
  vars$age_1 <- c(30, 40)
  vars$age_1t <- c("15-29", "30-39", "40-49")
  
  vars$look_a1 <- tibble(age = 15:49) %>%
    mutate(age_1 = factor(if_else(age < vars$age_1[1], vars$age_1t[1],
                                  if_else(age < vars$age_1[2], vars$age_1t[2], vars$age_1t[3])
    ), levels = vars$age_1t))  
  
  
  # age category 2
  vars$age_2 <- seq(25, 40, by = 5)
  vars$age_2t <- c("15-24", "25-29", "30-34", "35-39", "40-49")
  
  vars$look_a2 <- tibble(age = 15:49) %>%
    mutate(age_2 = factor(if_else(age < vars$age_2[1], vars$age_2t[1],
                                  if_else(age < vars$age_2[2], vars$age_2t[2],
                                          if_else(age < vars$age_2[3], vars$age_2t[3],
                                                  if_else(age < vars$age_2[4], vars$age_2t[4], vars$age_2t[5])
                                          )
                                  )
    ), levels = vars$age_2t))  
  
  

  # age category 3
  vars$age_3 <- seq(0, 90, by = 10)
  
  vars$age_3t <- c(
    "0-9", "10-19", "20-29", "30-39", "40-49",
    "50-59", "60-69", "70-79", "80-89", "90+"
  )
  
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
  
  
  # age category 4
  vars$age_4 <- seq(0, 80, by = 20)
  vars$age_4t <- c("0-19", "20-39", "40-59", "60-79", "80+")
  
  vars$look_a4 <- tibble(age = 0:120) %>%
    mutate(age_4 = factor(case_when(
      age < vars$age_4[2] ~ vars$age_4t[1],
      age < vars$age_4[3] ~ vars$age_4t[2],
      age < vars$age_4[4] ~ vars$age_4t[3],
      age < vars$age_4[5] ~ vars$age_4t[4],
      TRUE ~ vars$age_4t[5]
    ), levels = vars$age_4t)) 
  
  
  # age category 5
  vars$age_5 <- seq(0, 15, by = 5)
  vars$age_5t <- c("0-4", "5-9", "10-14", "15+")
  
  vars$look_a5 <- tibble(age = 0:120) %>%
    mutate(age_5 = factor(case_when(
      age < vars$age_5[2] ~ vars$age_5t[1],
      age < vars$age_5[3] ~ vars$age_5t[2],
      age < vars$age_5[4] ~ vars$age_5t[3],
      TRUE ~ vars$age_5t[4]
    ), levels = vars$age_5t))    
  
  
  # scenarios and versions for DWH
  vars$look_scen_dwh <- tibble(
    scenario = c(vars$text_c[2:4], vars$text_cb[c(2, 4)]),
    VersionArtCd = 1:5
  ) 
  
  # VersionArtCd (in all publication years)
  # 1: lower scenario
  # 2: middle scenario
  # 3: upper scenario

  # VersionArtCd (as of publication year 2024)
  # 4: lower birth version
  # 5: upper birth versions

  
  

  # colors, graphics --------------------------------------------------------

  # color palette
  vars$col_6 <- c("#3431DE", "#DB247D", "#FF720C", "#FBB900", "#1F9E31", "#7C7C7C")
  # plot(1:6, 1:6, col = vars$col_6, pch = 16, cex = 3)
  
  # old colors
  # c("#005CA9", "#83072A", "#EB5E04", "#FBBA00", "#007229", "#9B9B9B")

  # new colors see:
  # https://github.com/StatistikStadtZuerich/zuericolors/blob/main/R/palettes.R
  # "qual6" = c("#3431DE", "#DB247D", "#1F9E31", "#FBB900", "#23C3F1", "#FF720C")
  # "seq6gry" = c("#FAFAFA", "#EAEAEA", "#D6D6D6", "#B8B8B8", "#7C7C7C", "#545454")  
  # "gender3" = c("#349894", "#FFD736", "#986AD5")  
  
  #default color
  vars$col_1 <- vars$col_6[1]  
  
  # grey
  vars$col_grey <- "grey90"
  
  # colors for district, sex etc.
  vars$col_d <- colorRampPalette(vars$col_6)(31)
  # plot(1:31, 1:31, col = vars$col_d, pch = 16, cex = 3)
  vars$col_a1 <- vars$col_6[c(1, 3, 4)]  
  # plot(1:3, 1:3, col = vars$col_a1, pch = 16, cex = 3)  
  vars$col_a2 <- vars$col_6[c(1, 3, 4, 5, 6)] 
  # plot(1:5, 1:5, col = vars$col_a2, pch = 16, cex = 3)    
  vars$col_s <- c("#FFD736", "#349894")
  # vars$col_s <- vars$col_6[c(1, 3)]
  # plot(1:2, 1:2, col = vars$col_s, pch = 16, cex = 3)
  vars$col_o <- vars$col_6[c(3, 2)]
  # plot(1:2, 1:2, col = vars$col_o, pch = 16, cex = 3)
  vars$col_r <- vars$col_6[c(1, 3)]
  # plot(1:2, 1:2, col = vars$col_r, pch = 16, cex = 3)
  vars$col_w <- vars$col_6[c(2, 5)]
  # plot(1:2, 1:2, col = vars$col_w, pch = 16, cex = 3)
  vars$col_e <- vars$col_6[c(4, 3, 2)]
  # plot(1:3, 1:3, col = vars$col_e, pch = 16, cex = 3)
  vars$col_t <- vars$col_6
  # plot(1:6, 1:6, col = vars$col_t, pch = 16, cex = 3)
  vars$col_i <- vars$col_6[c(1, 6)]
  # plot(1:2, 1:2, col = vars$col_i, pch = 16, cex = 3)
  vars$col_c <- c("black", vars$col_6[c(2, 5, 4)]) # past: black instead of blue
  # vars$col_c <- vars$col_6[c(1, 2, 5, 4)]  # colors from webpage
  # plot(1:4, 1:4, col = vars$col_c, pch = 16, cex = 3)  
  
  
  
  # themes: neutral design for ggplot
  vars$neutral <- theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour = vars$col_grey),
    panel.border = element_rect(colour = vars$col_grey)
  )
  
    
  # options -----------------------------------------------------------------
  
  # output
  vars$pdf_output <- FALSE
  
  # round rate (digits, when rates are rounded) 
  vars$round_rate <- 4
  
  # no scientic notation
  options(scipen = 999)  
  
  

  # model parameter ---------------------------------------------------------

  # import model parameters (for all scenarios)
  vars$para <- read_delim(vars$para_file, ";", lazy = FALSE) |> 
    select(parameter, lower, middle, upper, 
           middle_birth_lower, middle_birth_upper, 
           lower_car_rev, middle_car_rev, upper_car_rev) |> 
    pivot_longer(
      cols = c(lower, middle, upper, 
               middle_birth_lower, middle_birth_upper, 
               lower_car_rev, middle_car_rev, upper_car_rev),
      names_to = "scenario"
    )

  # output ------------------------------------------------------------------

  return(vars)

}






#' Load objects that do depend on the scenario (run per scenario)
#' 
#' This function loads all objects that vary by scenario
#'
#' @param i_scen scenario
#' @param vars_init_once list of variables that do not depend on the scenario
#'
#' @return list of paths variables (both dependent and independent of the scenario)
#' @export
#'
#' @examples init_scen(i_scen = "middle", vars_init_once = vars_once)



init_scen <- function(i_scen = "middle", vars_init_once = vars_once){
  
  # initialized variables ---------------------------------------------------
  
  vars <- vars_init_once
  
  
  # scenario ----------------------------------------------------------------
  
  vars$i_scen <- i_scen  
  
  
  # model parameter ---------------------------------------------------------
  
  # list with all model parameters (depending on the scenario)
  para <- vars$para |> 
    filter(scenario == i_scen) |> 
    select(parameter, value) |> 
    pivot_wider(names_from = parameter) |> 
    as.list()    
  
  
  # scenario-dependent paths ------------------------------------------------
  
  # path for exports (rates)
  vars$exp_path_scen <- file.path(vars$exp_path, i_scen)
  dir_ex_create(vars$exp_path_scen)
  
  # path for outputs (future: population and demographic processes)
  vars$out_path_scen <- file.path(vars$out_path, i_scen)
  dir_ex_create(vars$out_path_scen)    
  
  
  # scenario-dependent objects ----------------------------------------------
  
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
  
  # colors for time distributions
  vars$col_time <- c(
    rep(vars$col_grey, length(vars$uniy_bir_base)),
    colorRampPalette(vars$col_6[1:5])(length(vars$uniy_scen))
  )
  
  # output: combine all parameters into one list
  return(append(para, vars))
  
}


