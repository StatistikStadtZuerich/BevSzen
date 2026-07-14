#' Births: sex ratio, future values
#'
#' @param params parameter vector
#'
#' @returns list with future birth sex ratio
#'          pro_male: proportion of male babies (past data)
#'          pro_male_pred: proportion of male babies (prediction)
#' 
#' @export
#'
#' @examples
birth_sex_ratio_fun <- function(params = params){

# start time
t0 <- Sys.time()
  
# parameter ---------------------------------------------------------------

# paths
bir_od <- params$bir_od
exp_path_scen <- params$exp_path_scen
log_file <- params$log_file

# time
scen_begin <- params$scen_begin
scen_end <- params$scen_end

# birth parameters
bir_sex_ratio_begin <- params$bir_sex_ratio_begin
bir_sex_ratio_end <- params$bir_sex_ratio_end

# text and lookup
uni_s <- params$uni_s

# options
round_rate <- params$round_rate

  

# start time
t0 <- Sys.time()

# import, proportion male -------------------------------------------------

# proportion male
pro_male <- read_csv(bir_od) %>%
  rename(year = EreignisDatJahr, bir = AnzGebuWir) %>%
  mutate(sex = fact_if(SexCd, uni_s)) %>%
  select(year, sex, bir) %>%
  group_by(year, sex) %>%
  summarize(
    bir = sum(bir),
    .groups = "drop"
  ) %>%
  spread(key = sex, value = bir) %>%
  mutate(pro_male = round(male / (male + female) * 100, round_rate))

# plot 0190

# prediction --------------------------------------------------------------

# model
# the sex proportion could depend on age
# however, a simple model is used: mean only

# mean
pred_mean <- pro_male %>%
  filter(year >= bir_sex_ratio_begin & year <= bir_sex_ratio_end) %>%
  summarize(
    pred_mean = mean(pro_male),
    .groups = "drop"
  )

# plot 0191

# export the results ------------------------------------------------------

# proportion male: prediction
pro_male_pred <- tibble(
  year = scen_begin:scen_end,
  pro_male = round(pred_mean$pred_mean, round_rate)
)

# export
write_csv(pro_male_pred, file.path(exp_path_scen, "birth_sex-ratio_future.csv"))

# log info
cat_log(
  text = paste0("proportion male babies: ", capture.output(Sys.time() - t0)),
  log_file = log_file
)



# outputs -----------------------------------------------------------------

out <- list()
out$pro_male <- pro_male
out$pro_male_pred <- pro_male_pred

return(out)

}




