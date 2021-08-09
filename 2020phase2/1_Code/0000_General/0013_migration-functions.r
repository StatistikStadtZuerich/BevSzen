#-------------------------------------------------------------------
#migration functions
#-rate (by district, year)
#-proportion of sex/origin (by district, year)
#-proportion of age (by district, year, sex, origin)
#
#
#rok/bad, June 2021
#-------------------------------------------------------------------


#migration rate per district and year (immigration, emigration)
      
   mig_rate_dy <- function(mig_path, mig_vari, mig_district,
                          mig_name, mig_number, graph_path, ex_path, 
                          mis_base_begin, mis_base_end, 
                          mis_rate_window_thres, mis_rate_prop_trend, 
                          mis_rate_thres_percent, mis_rate_lower_thres){
      
        #variables
            #WHY only migration path and variable name?
            #that are the only variables changed (when evaluation immigration or emigration)
            #relocation and population variables remain the same
      
 
        #-------------------------------------------------------------------
        #import and data preparation
        #-------------------------------------------------------------------
              
        #migration (immigration or emigration)  
            mig <- read_csv(mig_path) %>% 
                rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>% 
                left_join(look_dis, by = "QuarCd") %>% 
                mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                    district = factor(distr, uni_d)) %>% 
                select(district, year, age, sex, origin, mig) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(mig = sum(mig)) %>%
                ungroup()

        #relocation
            #only migration to a certain district
            #WHY? this is needed to calculate immigration* (i.e. migration to a certain district)

            rel <- read_csv(rel_od) %>%
                rename(year = EreignisDatJahr, age = AlterVCd, dis = mig_district, rel = AnzUmzuWir) %>% 
                left_join(look_dis, c("dis" = "QuarCd")) %>% 
                mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                    district = factor(distr, uni_d)) %>%
                select(district, year, age, sex, origin, rel) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(rel = sum(rel)) %>%
                ungroup()
            
        #migration* (i.e. migration to a certain district, 'migration star' = mis)
            mis <- bind_rows(rename(mig, mis = mig),
                rename(rel, mis = rel)) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(mis = sum(mis)) %>%
                ungroup()

        #population
            #year: begin of year population

            pop <- read_csv(pop_od) %>%
                rename(age = AlterVCd, pop = AnzBestWir) %>%
                left_join(look_dis, by = "QuarCd") %>%
                mutate(year = StichtagDatJahr + 1,
                    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                    district = factor(distr, uni_d)) %>%
                select(district, year, age, sex, origin, pop) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(pop = sum(pop)) %>%
                ungroup()


        #-------------------------------------------------------------------
        #migration* rate (per district and year)
        #-------------------------------------------------------------------

        #mis and pop: aggregate
            mis_dy <- group_by(mis, district, year) %>%
                    summarize(mis = sum(mis)) %>%
                ungroup()

            pop_dy <- group_by(pop, district, year) %>%
                    summarize(pop = sum(pop)) %>%
                ungroup()

        #migration* rate (based on all possible cases)
            mis_rate_dy <- as_tibble(expand_grid(
                district = uni_d,
                year = (date_start+1):date_end)) %>%
                left_join(pop_dy, by = c("district", "year")) %>%
                left_join(mis_dy, by = c("district", "year")) %>%
                replace_na(list(pop = 0, ims = 0)) %>%
                mutate(mis_rate_dy = if_else(pop == 0, NA_real_, round(mis / pop * 100, round_rate)))

        #years of the past (for plot)
            year_past <- (date_start+1):date_end
            year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

        #plot
            p00 <- ggplot(data = mis_rate_dy) +
                geom_vline(xintercept = year_past_5, color = col_grey) +
                geom_line(aes(x = year, y = mis_rate_dy), color = col_6[1]) +
                facet_wrap(~district, ncol = 4) +
                labs(x = "year", y = paste0(mig_name, "* rate (in % per year)")) +
                neutral

            ggsave(paste0(graph_path, mig_number, "00_", mig_name, "-star-rate_by-district-year.pdf"),
                plot = p00, width = 12, height = 14)


        #-------------------------------------------------------------------
        #prediction: future migration* rate (per year and district)
        #-------------------------------------------------------------------

        #base years
            mis_base <- filter(mis_rate_dy,
                    (year >= mis_base_begin) & (year <= mis_base_end))

        #prediction: constrained regression
            mis_pred <- con_reg(data = mis_base, x = "year", y = "mis_rate_dy",
                      group_cols = "district",
                      window = mis_rate_window_thres, base_t0 = mis_base_begin,
                      szen_t0 = szen_begin, szen_t1 = szen_end,
                      prop_trend = mis_rate_prop_trend, thres_percent = mis_rate_thres_percent,
                      lower_thres = mis_rate_lower_thres, upper_thres = NA)

        #past and prediction
            mis_past_pred <- as_tibble(expand_grid(
                district = uni_d,
                year = (date_start+1):szen_end)) %>%
                left_join(select(mis_rate_dy, district, year, mis_rate_dy),
                    by = c("district", "year")) %>%
                left_join(mis_pred, by = c("district", "year")) %>%
                mutate(rate_all = if_else(year <= mis_base_end, mis_rate_dy, pred_roll))

        #plot

            #levels
                time_lev <- c("past", "future")

            #plot data
                plot_dat_mis_pred <- select(mis_past_pred, district, year, rate_all) %>%
                    mutate(time = factor(if_else(year <= mis_base_end,
                        time_lev[1], time_lev[2]), levels = time_lev))

            p01 <- ggplot(data = plot_dat_mis_pred) +
                geom_vline(xintercept = c(mis_base_begin, mis_base_end), color = col_grey, linetype = 1) +
                geom_line(aes(x = year, y = rate_all, linetype = time), color = col_6[1]) +
                facet_wrap(~district, ncol = 4) +
                labs(x = "year", y = paste0(mig_name, "* rate (in % per year)"), linetype = "") +
                neutral

            ggsave(paste0(graph_path, mig_number, "01_", mig_name, "-star-rate_by-district-year_predicition.pdf"),
                plot = p01, width = 12, height = 14)


        #plot (more detailed: regression and limits)
            p02 <- ggplot(data = mis_past_pred) +
                geom_vline(xintercept = c(mis_base_begin, mis_base_end), color = col_grey, linetype = 1) +
                geom_point(aes(x = year, y = mis_rate_dy), color = col_6[1]) +
                geom_line(aes(x = year, y = pred), linetype = 2, color = col_6[1]) +
                geom_line(aes(x = year, y = pred_mean), col = col_6[1], linetype = 3) +
                geom_line(aes(x = year, y = pred_roll), col = col_6[1]) +
                facet_wrap(~district, ncol = 4) +
                labs(x = "year", y = paste0(mig_name, "* rate (in % per year)")) +
                neutral

            ggsave(paste0(graph_path, mig_number, "02_", mig_name, "-star-rate_by-district-year_predicition-details.pdf"),
                plot = p02, width = 12, height = 14)

        #export preparation
            ex_mig_rate_dy <- mutate(mis_past_pred,
                    rate = round(rate_all, round_rate)) %>%
                filter(year >= szen_begin) %>%
                select(district, year, rate) %>%
                arrange(district, year)

        #export the data
            write_csv(ex_mig_rate_dy, ex_path)

        #output (to get an idea of the exported output)
            return(list(ex_mig_rate_dy))

                }




#in migration (immigration, emigration): proportion of sex and origin, by distict and year

    mig_prop_so_dy <- function(mig_path, mig_vari, mig_district,
                              mig_name, mig_number, graph_path, ex_path,
                              mis_so_base_begin, mis_so_base_end,
                              mis_so_window_thres, mis_so_prop_trend,
                              mis_so_thres_percent, mis_so_lower_thres, ...){

        #variables
            #WHY only migration path and variable name?
            #that are the only variables changed (when evaluation immigration or emigration)
            #relocation variables remain the same


        #-------------------------------------------------------------------
        #import and data preparation
        #-------------------------------------------------------------------

        #migration (immigration or emigration)
            mig <- read_csv(mig_path) %>%
                rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>%
                left_join(look_dis, by = "QuarCd") %>%
                mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                    district = factor(distr, uni_d)) %>%
                select(district, year, age, sex, origin, mig) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(mig = sum(mig)) %>%
                ungroup()

        #relocation
            #only migration to a certain district
            #WHY? this is needed to calculate immigration* (i.e. migration to a certain district)

            rel <- read_csv(rel_od) %>%
                rename(year = EreignisDatJahr, age = AlterVCd, dis = mig_district, rel = AnzUmzuWir) %>% 
                left_join(look_dis, c("dis" = "QuarCd")) %>% 
                mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                    district = factor(distr, uni_d)) %>%
                select(district, year, age, sex, origin, rel) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(rel = sum(rel)) %>%
                ungroup()


        #migration* (i.e. migration to a certain district, 'migration star' = mis)
            mis <- bind_rows(rename(mig, mis = mig),
                rename(rel, mis = rel)) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(mis = sum(mis)) %>%
                ungroup()

        #-------------------------------------------------------------------
        #migration: distribution of sex and origin
        #-------------------------------------------------------------------

        #ims and pop: aggregate
              mis_dy <- group_by(mis, district, year) %>%
                      summarize(mis = sum(mis)) %>%
                  ungroup()

        #possible cases
            cas_dyso <- as_tibble(expand_grid(
                district = uni_d,
                year = (date_start+1):date_end,
                sex = uni_s,
                origin = uni_o))

        #distribution of sex and origin per year and district
            #comment: a proportion not a rate

            mis_dyso <- group_by(mis, district, year, sex, origin) %>%
                    summarize(mis_dyso = sum(mis)) %>%
                ungroup() %>%
                left_join(rename(mis_dy, mis_dy = mis),
                    by = c("district", "year")) %>%
                right_join(cas_dyso, by = c("district", "year", "sex", "origin")) %>%
                replace_na(list(mis_dyso = 0, mis_dy = 0)) %>%
                mutate(mis_prop_dyso = if_else(mis_dy == 0, NA_real_, round(mis_dyso / mis_dy * 100, round_prop)))

        #years of the past (for plot)
            year_past <- (date_start+1):date_end
            year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

        #plot
            p03 <- ggplot(data = mis_dyso) +
                geom_vline(xintercept = year_past_5, color = col_grey) +
                geom_line(aes(x = year, y = mis_prop_dyso, color = sex, linetype = origin)) +
                facet_wrap(~district, ncol = 4) +
                scale_colour_manual(values = col_s) +
                labs(x = "year", y = paste0("proportion in % (per district and year, in ", mig_name, "*)"), color = "", linetype = "") +
                neutral

            ggsave(paste0(graph_path, mig_number, "03_", mig_name, "_proportion-sex-origin_by-district-year.pdf"),
                plot = p03, width = 12, height = 14)



        #-------------------------------------------------------------------
        #distribution of sex and origin: prediction
        #-------------------------------------------------------------------

        #base years
            mis_so_base <- filter(mis_dyso,
                    (year >= mis_so_base_begin) & (year <= mis_so_base_end))


        #prediction: constrained regression

            #Why no upper threshold?
            #proportions per district/year will be standardized to 100 percent anyways

            mis_so_pred <- con_reg(data = mis_so_base, x = "year", y = "mis_prop_dyso",
                      group_cols = c("district", "sex", "origin"),
                      window = mis_so_window_thres, base_t0 = mis_so_base_begin,
                      szen_t0 = szen_begin, szen_t1 = szen_end,
                      prop_trend = mis_so_prop_trend, thres_percent = mis_so_thres_percent,
                      lower_thres = mis_so_lower_thres, upper_thres = NA)

        #standardize proportions per district/year to 100 percent
            mis_so_pred_stand <- group_by(mis_so_pred, district, year) %>%
                    summarize(pred_roll_sum = sum_NA(pred_roll)) %>%
                ungroup() %>%
                right_join(mis_so_pred, by = c("district", "year")) %>%
                mutate(pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, round(pred_roll / pred_roll_sum * 100, round_prop)))

        #past and prediction
            mis_so_past_pred <- as_tibble(expand_grid(
                district = uni_d,
                year = (date_start+1):szen_end,
                sex = uni_s,
                origin = uni_o)) %>%
                left_join(select(mis_dyso, district, year, sex, origin, mis_prop_dyso),
                    by = c("district", "year", "sex", "origin")) %>%
                left_join(mis_so_pred_stand, by = c("district", "year", "sex", "origin")) %>%
                mutate(prop_all = if_else(year <= mis_so_base_end, mis_prop_dyso, pred_roll_stand))

        #plot
            p04 <- ggplot(data = mis_so_past_pred) +
                geom_vline(xintercept = c(mis_so_base_begin, mis_so_base_end), color = col_grey, linetype = 1) +
                geom_line(aes(x = year, y = prop_all, color = sex, linetype = origin)) +
                facet_wrap(~district, ncol = 4) +
                scale_colour_manual(values = col_s) +
                labs(x = "year", y = paste0("proportion in % (per district and year, in ", mig_name, "*)"), color = "", linetype = "") +
                neutral

            ggsave(paste0(graph_path, mig_number, "04_", mig_name, "_proportion-sex-origin_by-district-year_prediction.pdf"),
                plot = p04, width = 12, height = 14)


        #export preparation
            ex_mig_prop_dy <- mutate(mis_so_past_pred,
                    prop = round(prop_all, round_prop)) %>%
                filter(year >= szen_begin) %>%
                select(district, year, sex, origin, prop) %>%
                arrange(district, year, sex, origin)

        #export the data
            write_csv(ex_mig_prop_dy, ex_path)



        #output (to get an idea of the exported output)
            return(list(ex_mig_prop_dy))

                }




#in migration (immigration, emigration): age proportion (by distict, year, sex, origin)

    mig_prop_a_dyso <- function(mig_path, mig_vari, mig_district,
                              mig_name, mig_number, graph_path, ex_path,
                              mis_age_min, mis_age_max,
                              mis_age_window_years,
                              mis_age_base_begin, mis_age_base_end,
                              mis_age_window_thres, mis_age_prop_trend,
                              mis_age_thres_percent, mis_age_lower_thres,...){


        #variables
            #WHY only migration path and variable name?
            #that are the only variables changed (when evaluation immigration or emigration)
            #relocation variables remain the same

        #-------------------------------------------------------------------
        #import and data preparation
        #-------------------------------------------------------------------

        #migration (immigration or emigration)
            mig <- read_csv(mig_path) %>%
                rename(year = EreignisDatJahr, age = AlterVCd, mig = mig_vari) %>%
                left_join(look_dis, by = "QuarCd") %>%
                mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                    district = factor(distr, uni_d)) %>%
                select(district, year, age, sex, origin, mig) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(mig = sum(mig)) %>%
                ungroup()

        #relocation
            #only migration to a certain district
            #WHY? this is needed to calculate immigration* (i.e. migration to a certain district)

            rel <- read_csv(rel_od) %>%
                rename(year = EreignisDatJahr, age = AlterVCd, dis = mig_district, rel = AnzUmzuWir) %>% 
                left_join(look_dis, c("dis" = "QuarCd")) %>% 
                mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
                    origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
                    district = factor(distr, uni_d)) %>%
                select(district, year, age, sex, origin, rel) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(rel = sum(rel)) %>%
                ungroup()


        #migration* (i.e. migration to a certain district, 'migration star' = mis)
            mis <- bind_rows(rename(mig, mis = mig),
                rename(rel, mis = rel)) %>%
                group_by(district, year, age, sex, origin) %>%
                    summarize(mis = sum(mis)) %>%
                ungroup()


        #-------------------------------------------------------------------
        #immigration*: per district, year, sex, origin
        #-------------------------------------------------------------------

        #WHY? to see where the denominator of the age proportion is low

        #possible cases: dyso
            cas_dyso <- as_tibble(expand_grid(
                district = uni_d,
                year = date_start:date_end,
                sex = uni_s,
                origin = uni_o))

        #migration*: dyso
            mis_dyso <- group_by(mis, district, year, sex, origin) %>%
                    summarize(mis_dyso = sum(mis)) %>%
                ungroup() %>%
                right_join(cas_dyso, by = c("district", "year", "sex", "origin")) %>%
                replace_na(list(mis_dyso = 0))

        #years of the past (for plot)
            year_past <- date_start:date_end
            year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

        #plot
            p10 <- ggplot(data = mis_dyso) +
                geom_vline(xintercept = year_past_5, color = col_grey) +
                geom_line(aes(x = year, y = mis_dyso, color = sex, linetype = origin)) +
                facet_wrap(~district, ncol = 4) +
                scale_colour_manual(values = col_s) +
                labs(x = "year", y = paste0(mig_name, "* per year"), color = "", linetype = "") +
                neutral

            ggsave(paste0(graph_path, mig_number, "10_", mig_name, "-star_per-district-year-sex-origin.pdf"),
                plot = p10, width = 12, height = 14)


        #plot
            p11 <- ggplot(data = mis_dyso) +
                geom_vline(xintercept = year_past_5, color = col_grey) +
                geom_line(aes(x = year, y = mis_dyso, color = sex, linetype = origin)) +
                facet_wrap(~district, ncol = 4, scales = "free") +
                scale_colour_manual(values = col_s) +
                labs(x = "year", y = paste0(mig_name, "* per year"), color = "", linetype = "") +
                neutral

            ggsave(paste0(graph_path, mig_number, "11_", mig_name, "-star_per-district-year-sex-origin_free-scales.pdf"),
                plot = p11, width = 12, height = 14)



        #-------------------------------------------------------------------
        #age proportion: per district, year, sex, origin
        #-------------------------------------------------------------------

        #migration*: dyaso (already summarized before)
            mis_dyaso <- as_tibble(expand_grid(
                district = uni_d,
                year = date_start:date_end,
                age = mis_age_min:mis_age_max,
                sex = uni_s,
                origin = uni_o)) %>%
                left_join(mis, by = c("district", "year", "age", "sex", "origin")) %>%
                rename(mis_dyaso = mis) %>%
                replace_na(list(mis_dyaso = 0)) %>%
                left_join(mis_dyso, by = c("district", "year", "sex", "origin")) %>%
                arrange(district, year, sex, origin, age) %>%
                mutate(mis_prop_a = if_else(mis_dyso == 0, NA_real_, round(mis_dyaso / mis_dyso * 100, round_prop)))



        #plot: focus age distribution
            #test: x <- uni_d[2]

            #years (subjectively selected)
                #WHY with rev? To have the last year in the plot
                years_plot <- rev(seq(date_end, date_start, by = -8))

            #colors
                col_years <- colorRampPalette(col_6[1:5])(length(years_plot))

            p12 <- function(x){
                ggplot(data = filter(mis_dyaso, (district == x) & (year %in% years_plot))) +
                    geom_line(aes(x = age, y = mis_prop_a, color = as.factor(year))) +
                    facet_grid(sex ~ origin) +
                    scale_colour_manual(values = col_years) +
                    labs(x = "age", y = "proportion in %", color = "year") +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "12_", mig_name,
                    "-star_age-proportion_per-district-year-sex-origin_focus-age.pdf"),
                width = 11, height = 8)

                # lapply(uni_d, p12) #when this code is within a function: pdf cannot be opened
                for(i in 1:length(uni_d)){plot(p12(uni_d[i]))}

            dev.off()


        #plot: focus years

            #age (subjectively selected)
                age_plot <- seq(0, 60, by = 20)

            #colors
                col_age <- colorRampPalette(col_6[1:5])(length(age_plot))


            p13 <- function(x){
                ggplot(data = filter(mis_dyaso, (district == x) & (age %in% age_plot))) +
                    geom_line(aes(x = year, y = mis_prop_a, color = as.factor(age))) +
                    facet_grid(sex ~ origin) +
                    scale_colour_manual(values = col_age) +
                    labs(x = "year", y = "proportion in %", color = "age") +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "13_", mig_name,
                    "-star_age-proportion_per-district-year-sex-origin_focus-years.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p13(uni_d[i]))}

            dev.off()



        #-------------------------------------------------------------------
        #moving average over years (by district, age, sex, origin)
        #-------------------------------------------------------------------

        #moving average
            mis_ma <- select(mis_dyaso, district, year, age, sex, origin, mis_dyaso) %>%
                group_by(district, age, sex, origin) %>%
                    arrange(year) %>%
                    mutate(mis_roll = rollmean(mis_dyaso, k = mis_age_window_years, fill = NA)) %>%
                ungroup()

        #plot preparation
            ma_lev <- c("initial", "moving average")

            mis_ma_plot <- gather(mis_ma, `mis_dyaso`, `mis_roll`, key = category, value = mis) %>%
                mutate(cat = factor(if_else(category == "mis_dyaso",
                    ma_lev[1], ma_lev[2]), levels = ma_lev)) %>%
                select(district, year, age, sex, origin, cat, mis)


        #plot: focus age distribution
            #test: x <- uni_d[3]

            #years (subjectively selected)
                years_not_NA <- sort(unique(mis_ma$year[!is.na(mis_ma$mis_roll)]))
                years_plot_ma <- seq(min(years_not_NA), max(years_not_NA), by = 6)

            p14 <- function(x){
                ggplot(data = filter(mis_ma_plot, (district == x) & (year %in% years_plot_ma))) +
                    geom_line(aes(x = age, y = mis, color = cat)) +
                    facet_grid(as.factor(year) ~ origin*sex) +
                    scale_colour_manual(values = col_6[1:2]) +
                    labs(x = "age", y = paste0(mig_name, "* per year"), color = "") +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "14_", mig_name,
                    "-star_moving-average-over-year_focus-age.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p14(uni_d[i]))}

            dev.off()


        #plot: focus years

            #age (subjectively selected)
                age_plot_ma <- seq(0, 60, by = 20)

            p15 <- function(x){
                ggplot(data = filter(mis_ma_plot, (district == x) & (age %in% age_plot_ma))) +
                    geom_line(aes(x = year, y = mis, color = cat)) +
                    facet_grid(as.factor(age) ~ origin*sex) +
                    scale_colour_manual(values = col_age) +
                    labs(x = "year", y = paste0(mig_name, "* per year"), color = "") +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "15_", mig_name,
                    "-star_moving-average-over-year_focus-years.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p15(uni_d[i]))}

            dev.off()



        #-------------------------------------------------------------------
        #age proportion (after moving average)
        #-------------------------------------------------------------------

        #preparation (e.g. without years with NA)
            mis_ma_prep <- filter(mis_ma, year %in% years_not_NA) %>%
                select(district, year, age, sex, origin, mis_roll) %>%
                rename(mis_dyaso = mis_roll)

        #age proportion
            mis_age_prop_ma <- group_by(mis_ma_prep, district, year, sex, origin) %>%
                        summarize(mis_dyso = sum(mis_dyaso)) %>%
                    ungroup() %>%
                right_join(mis_ma_prep, by = c("district", "year", "sex", "origin")) %>%
                mutate(prop_a_ma = if_else(mis_dyso == 0, NA_real_, round(mis_dyaso / mis_dyso * 100, round_prop))) %>%
                select(district, year, age, sex, origin, prop_a_ma) %>%
                arrange(district, year, sex, origin, age)


        #plot: focus age distribution
            #test: x <- uni_d[3]

            #years
                #WHY like this with rev? To have the last year with data in the plot
                years_plot_ma_prop <- rev(seq(max(years_not_NA), min(years_not_NA), by = -6))

            #colors
                col_years_ma_prop <- colorRampPalette(col_6[1:5])(length(years_plot_ma_prop))

            p16 <- function(x){
                ggplot(data = filter(mis_age_prop_ma, (district == x) & (year %in% years_plot_ma_prop))) +
                    geom_line(aes(x = age, y = prop_a_ma, color = as.factor(year))) +
                    facet_grid(sex ~ origin) +
                    scale_colour_manual(values = col_years_ma_prop) +
                    labs(x = "age", y = "proportion in %", color = "year") +
                    ggtitle(as.character(x)) +
                    neutral}


            pdf(paste0(graph_path, mig_number, "16_", mig_name,
                    "-star_age-proportion_after-moving-average_focus-age.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p16(uni_d[i]))}

            dev.off()


        #plot: focus years

            #age (subjectively selected)
                age_plot_ma_prop <- seq(0, 60, by = 20)

            #colors
                col_age_ma_prop <- colorRampPalette(col_6[1:5])(length(age_plot_ma_prop))

            p17 <- function(x){
                ggplot(data = filter(mis_age_prop_ma, (district == x) & (age %in% age_plot_ma_prop))) +
                    geom_line(aes(x = year, y = prop_a_ma, color = as.factor(age))) +
                    facet_grid(sex ~ origin) +
                    scale_colour_manual(values = col_age_ma_prop) +
                    labs(x = "year", y = "proportion in %", color = "age") +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "17_", mig_name,
                    "-star_age-proportion_after-moving-average_focus-years.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p17(uni_d[i]))}

            dev.off()


        #-------------------------------------------------------------------
        #fit gam to age proportion
        #-------------------------------------------------------------------

        #with gam (duration: approx. 6 minutes)

            # details see: https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/smooth.terms.html
            # initial smooth term 'tp' did not provide an appropriate fit ('bumps')
            # therefore changed to 'ad' (adaptive smoother)
            # ad: should be applied, when 'the degree of smoothing should itself vary with the covariates to be smoothed'

            t0 <- Sys.time()

            prop_fit <- arrange(mis_age_prop_ma, district, year, sex, origin, age) %>%
                group_by(district, year, sex, origin) %>%
                    mutate(prop_fit = pmax(0, gam(prop_a_ma ~ s(age, bs = "ad"))$fitted.values)) %>%
                ungroup()

            t1 <- Sys.time()
            duration <- t1 - t0


        #plot preparation
            fit_lev <- c("initial", "with gam")

            mis_fit_plot <- gather(prop_fit, `prop_a_ma`, `prop_fit`, key = category, value = prop) %>%
                mutate(cat = factor(if_else(category == "prop_a_ma",
                    fit_lev[1], fit_lev[2]), levels = fit_lev)) %>%
                select(district, year, age, sex, origin, cat, prop)

        #plot: focus age distribution
            #test: x <- uni_d[3]

            #years (subjectively selected)
                years_plot_fit <- seq(min(years_not_NA), max(years_not_NA), by = 6)

            p18 <- function(x){
                ggplot(data = filter(mis_fit_plot, (district == x) & (year %in% years_plot_fit))) +
                    geom_line(aes(x = age, y = prop, color = cat)) +
                    facet_grid(as.factor(year) ~ origin*sex) +
                    scale_colour_manual(values = col_6[1:2]) +
                    labs(x = "age", y = "proportion in %", color = "") +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "18_", mig_name,
                    "-star_proportion_with-gam_focus-age.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p18(uni_d[i]))}

            dev.off()

        #-------------------------------------------------------------------
        #Constrained regression
        #-------------------------------------------------------------------

        #years: base period
            years_temp <- mis_age_base_begin:mis_age_base_end
            years_base <- years_temp[years_temp %in% years_not_NA]

        #data for base period
            prop_base <- filter(prop_fit, year %in% years_base)

        #prediction (duration: approx. 30 seconds)
            #Why no upper threshold?
            #proportions will be standardized to 100 percent anyways

            prop_pred <- con_reg(data = prop_base, x = "year", y = "prop_fit",
                      group_cols = c("district", "age", "sex", "origin"),
                      window = mis_age_window_thres, base_t0 = min(years_base),
                      szen_t0 = max(years_base)+1, szen_t1 = szen_end,
                      prop_trend = mis_age_prop_trend, thres_percent = mis_age_thres_percent,
                      lower_thres = mis_age_lower_thres, upper_thres = NA)



        #limit prediction period
            prop_pred_begin <- filter(prop_pred, year >= szen_begin)

        #standardize the sum of the proportions to 100 percent
            prop_stand <- group_by(prop_pred_begin, district, year, sex, origin) %>%
                    summarize(pred_roll_sum = sum_NA(pred_roll)) %>%
                ungroup() %>%
                right_join(prop_pred_begin, by = c("district", "year", "sex", "origin")) %>%
                mutate(pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_, round(pred_roll / pred_roll_sum * 100, round_prop)))

        #past and prediction
            mis_a_past_pred <- as_tibble(expand_grid(
                district = uni_d,
                year = (date_start+1):szen_end,
                age = mis_age_min:mis_age_max,
                sex = uni_s,
                origin = uni_o)) %>%
                left_join(select(mis_dyaso, district, year, age, sex, origin, mis_prop_a),
                    by = c("district", "year", "age", "sex", "origin")) %>%
                left_join(select(prop_stand, district, year, age, sex, origin, pred_roll_stand),
                    by = c("district", "year", "age", "sex", "origin")) %>%
                mutate(prop_a = if_else(year <= mis_age_base_end, mis_prop_a, pred_roll_stand))

        #export preparation
            ex_mig_prop_a_dyso <- mutate(mis_a_past_pred,
                    prop = round(prop_a, round_prop)) %>%
                filter(year >= szen_begin) %>%
                select(district, year, age, sex, origin, prop) %>%
                arrange(district, year, sex, origin, age)

        #export the data
            write_csv(ex_mig_prop_a_dyso, ex_path)



        #colors
            col_time <- c(rep(col_grey, length((date_start+1):date_end)),
                colorRampPalette(col_6[1:5])(length(szen_begin:szen_end)))
            # plot(1:length(col_time), 1:length(col_time), col = col_time, pch = 16, cex = 2)


        #plot: focus age distribution
            #test: x <- uni_d[3]

            p19 <- function(x){
                ggplot(data = filter(mis_a_past_pred, district == x)) +
                    geom_line(aes(x = age, y = prop_a, color = as.factor(year))) +
                    facet_grid(origin ~ sex) +
                    scale_colour_manual(values = col_time) +
                    labs(x = "age", y = "proportion in %", color = "") +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "19_", mig_name,
                    "-star_age-proportion_by-district-year-sex-origin_past-future.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p19(uni_d[i]))}

            dev.off()




        #plot levels
            time_lev <- c("past", "future")

        #plot data
            plot_a_past_pred <- mutate(mis_a_past_pred,
                time = factor(if_else(year <= mis_age_base_end,
                time_lev[1], time_lev[2]), levels = time_lev))

        #plot: focus age distribution
            #test: x <- uni_d[3]
            #WHY this plot: it is recommendable to look precisely at the age plots over years
            #therefore, a plot that focuses on certain years

            #years
                year_temp <- (date_start + 1):szen_end
                year_plot <- seq(min(year_temp), max(year_temp), by = 8)

            #colors
                col_years_plot <- colorRampPalette(col_6[1:5])(length(year_plot))

            p20 <- function(x){
                ggplot(data = filter(plot_a_past_pred, (district == x) & (year %in% year_plot))) +
                    geom_line(aes(x = age, y = prop_a, color = as.factor(year), size = time, alpha = time)) +
                    facet_grid(sex ~ origin) +
                    scale_colour_manual(values = col_years_plot) +
                    scale_size_manual(values = c(0.2, 1.2)) +
                    scale_alpha_manual(values = c(0.4, 1)) +
                    labs(x = "age", y = "proportion in %", color = "year", size = "") +
                    guides(alpha = FALSE) +
                    ggtitle(as.character(x)) +
                    neutral}

            pdf(paste0(graph_path, mig_number, "20_", mig_name,
                    "-star_age-proportion_by-district-year-sex-origin_past-future_focus-age.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p20(uni_d[i]))}

            dev.off()


        #plot: focus years

            #age (subjectively selected)
                age_plot_pred <- seq(0, 60, by = 20)

            #colors
                col_age_pred <- colorRampPalette(col_6[1:5])(length(age_plot_pred))

            p21 <- function(x){
                ggplot(data = filter(plot_a_past_pred, (district == x) & (age %in% age_plot_ma_prop))) +
                    geom_vline(xintercept = c(mis_age_base_begin, mis_age_base_end), color = col_grey, linetype = 1) +
                    geom_line(aes(x = year, y = prop_a, color = as.factor(age))) +
                    facet_grid(sex ~ origin) +
                    scale_colour_manual(values = col_age_pred) +
                    labs(x = "year", y = "proportion in %", color = "age") +
                    ggtitle(as.character(x)) +
                    neutral}


            pdf(paste0(graph_path, mig_number, "21_", mig_name,
                    "-star_age-proportion_by-district-year-sex-origin_past-future_focus-years.pdf"),
                width = 11, height = 8)

                for(i in 1:length(uni_d)){plot(p21(uni_d[i]))}

            dev.off()

        #output (to get an idea of the exported output)
            return(list(ex_mig_prop_a_dyso))

                }


    
    