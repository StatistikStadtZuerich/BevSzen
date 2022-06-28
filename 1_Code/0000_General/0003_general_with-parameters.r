# header ------------------------------------------------------------------
# General: levels, colors, functions
# (with dependence on parameters, i.e. scenarios)


# paths -------------------------------------------------------------------

# generic results are saved in middle scenario
if (!exists("i_scen")) i_scen <- "middle"

# path for results (graphics)
res_path <- paste0("3_Results/", i_scen, "/")

if (!dir.exists(res_path)) {
  dir.create(res_path, recursive = TRUE
  )
}

# path for exports (rates)
exp_path <- paste0(data_path, "4_Rates/", i_scen, "/")

if (!dir.exists(exp_path)) {
  dir.create(exp_path, recursive = TRUE
  )
}

# path for outputs (future: population and demographic processes)
out_path <- paste0(data_path, "5_Outputs/", i_scen, "/")

if (!dir.exists(out_path)) {
  dir.create(out_path, recursive = TRUE
  )
}


# unique levels -----------------------------------------------------------

# year: base period
uniy_bir_base <- bir_base_begin:bir_base_end

# year: future
uniy_scen <- scen_begin:scen_end

# year: future (publications)
uniy_scen_public <- scen_begin:scen_end_public


# lookup tables -----------------------------------------------------------

# projects not realized
look_not <- tibble(
  status = factor(pro_category, levels = pro_category),
  not_realized = c(
    pro_not_scheduled_ip, pro_not_scheduled_other,
    pro_not_submitted,
    pro_not_approved, pro_not_started,
    pro_not_completed, pro_not_onhold
  )
)



# colors ------------------------------------------------------------------

# color for year (base period)
col_y_base <- colorRampPalette(col_6)(length(uniy_bir_base))

# colour for time distributions
col_time <- c(
  rep(col_grey, length(uniy_bir_base)),
  colorRampPalette(col_6[1:5])(length(uniy_scen))
)


# specific functions ------------------------------------------------------

# migration functions
source(paste0(code_path, "0000_General/0013_migration-functions.r"))

# relocation functions
source(paste0(code_path, "0000_General/0014_relocation-function.r"))
