#-------------------------------------------------------------------
#Birth: Fertility Rate, phase 2
#
#
#
#rok/bad, February 2021
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#paths, general
#-------------------------------------------------------------------

#working directory
    setwd(paste0(here::here(), "/2020phase2/"))

#general (e.g. packages, colors)
    source("1_Code/0000_General/0000_general_phase2.r")

#birth: result path (for images)
    bir_res <- "3_Results/0100_Birth/"
    
#birth: export path (for future fertility rates)
    bir_exp <- "2_Data/4_Rates/"  


    
#-------------------------------------------------------------------
#import and data preparation
#-------------------------------------------------------------------

#birth
    #age: only births of women at 'fertile age'
    #origin: here origin of mother
    
    bir <- read_csv(bir_od) %>% 
        rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) %>% 
        filter((age >= bir_age_begin) & (age <= bir_age_end)) %>% 
        left_join(look_dis, by = "QuarCd") %>% 
        mutate(origin = factor(if_else(HerkunftMutterCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>% 
        select(district, year, age, origin, bir) %>%     
        group_by(district, year, age, origin) %>% 
            summarize(bir = sum(bir)) %>% 
        ungroup() 
  
    
#population
    #year: begin of year population
    #age: only women at 'fertile age'
  
    pop <- read_csv(pop_od) %>%   
        rename(age = AlterVCd, pop = AnzBestWir) %>%   
        filter((SexCd == 2) & (age >= bir_age_begin) & (age <= bir_age_end)) %>%       
        left_join(look_dis, by = "QuarCd") %>%       
        mutate(year = StichtagDatJahr + 1,
            origin = factor(if_else(HerkunftCd == 1, uni_o[1], uni_o[2]), uni_o),
            district = factor(distr, uni_d)) %>%   
        select(district, year, age, origin, pop) %>%    
        group_by(district, year, age, origin) %>% 
            summarize(pop = sum(pop)) %>% 
        ungroup()  
    

#-------------------------------------------------------------------
#fertility
#-------------------------------------------------------------------
          
#values for all possible cases 
    
    #WHY with age categories? to calculate TFR by age category    

    cas <- as_tibble(expand_grid(district = uni_d,
        year = (date_start+1):date_end,
        age = bir_age_begin:bir_age_end,
        origin = uni_o)) %>% 
        left_join(pop, by = c("district", "year", "age", "origin")) %>% 
        left_join(bir, by = c("district", "year", "age", "origin")) %>% 
        replace_na(list(pop = 0, bir = 0)) %>% 
        left_join(look_a1, by = "age") %>% 
        left_join(look_a2, by = "age")    
    
#fertility by year, age
    fer_ya <- group_by(cas, age, year) %>% 
            summarize(pop = sum(pop), 
                      bir = sum(bir)) %>% 
        ungroup() %>% 
        mutate(fer_ya = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>% 
        select(age, year, fer_ya)      
    
#fertility by year, age, origin
    fer_yao <- group_by(cas, year, age, origin) %>% 
            summarize(pop = sum(pop), 
                      bir = sum(bir)) %>% 
        ungroup() %>% 
        mutate(fer_yao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>% 
        select(year, age, origin, fer_yao)    
    
#fertility by district, year, age, origin
    #is already aggregated by district, year, age, origin
    fer_dyao <- mutate(cas, fer_dyao = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate)))      

    
#-------------------------------------------------------------------
#TFR (total fertility rate) 
#-------------------------------------------------------------------
        
#WHY? plots to define the base period for fertility prediction    
    
#TFR by year
    tfr_y <- group_by(fer_ya, year) %>% 
            summarize(tfr_y = sum_NA(fer_ya / 100)) %>% 
        ungroup()  
    
    # year5 <- tfr_y$year[tfr_y$year %% 5 == 0]

    # p100 <- ggplot(data = tfr_y) + 
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +
    #     geom_line(aes(x = year, y = tfr_y), color = col_6[1]) + 
    #     geom_point(aes(x = year, y = tfr_y), color = col_6[1]) +       
    #     labs(x = "year", y = "TFR") +
    #     scale_x_continuous(breaks = pretty_breaks()) +
    #     # expand_limits(y = 0) +
    #     neutral
    #     
    # ggsave(paste0(bir_res, "0100_TFR_by-year.pdf"), 
    #     plot = p100, width = 7, height = 4) 
    
    sszplot(tfr_y, aes_x = "year", aes_y = "tfr_y",
            labs_y = "TFR", i_x = "5",
            name = "0100_TFR_by-year")
    
    
#TFR by year, origin
    tfr_yo <- group_by(fer_yao, year, origin) %>% 
            summarize(tfr_yo = sum_NA(fer_yao / 100)) %>% 
        ungroup()    
        
    # p101 <- ggplot(data = tfr_yo) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +     
    #     geom_line(aes(x = year, y = tfr_yo, color = origin)) +
    #     geom_point(aes(x = year, y = tfr_yo, color = origin)) +      
    #     labs(x = "year", y = "TFR", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +      
    #     scale_colour_manual(values = col_o) +
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0101_TFR_by-year-origin.pdf"), 
    #     plot = p101, width = 7, height = 4)    
    
    sszplot(tfr_yo, aes_x = "year", aes_y = "tfr_yo", aes_col = "origin",
            labs_y = "TFR", i_x = "5",
            name = "0101_TFR_by-year-origin")        
    
#TFR by year, age1
    tfr_ya1 <- left_join(fer_ya, look_a1, by = "age") %>% 
        group_by(year, age_1) %>% 
            summarize(tfr_ya1 = sum_NA(fer_ya / 100)) %>% 
        ungroup()      

    # p102 <- ggplot(data = tfr_ya1) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +   
    #     geom_line(aes(x = year, y = tfr_ya1, color = age_1)) +
    #     geom_point(aes(x = year, y = tfr_ya1, color = age_1)) +      
    #     labs(x = "year", y = "TFR", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +      
    #     scale_colour_manual(values = colorRampPalette(col_6)(length(age_1t))) + 
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0102_TFR_by-year-age1.pdf"), 
    #     plot = p102, width = 7, height = 4)      
    
    sszplot(tfr_ya1, aes_x = "year", aes_y = "tfr_ya1", aes_col = "age_1",
            i_x = "5", labs_y = "TFR",
            name = "0102_TFR_by-year-age1")  
    
    
#TFR by year, age2   
    tfr_ya2 <- left_join(fer_ya, look_a2, by = "age") %>% 
        group_by(year, age_2) %>% 
            summarize(tfr_ya2 = sum_NA(fer_ya / 100)) %>% 
        ungroup()       
    
    # p103 <- ggplot(data = tfr_ya2) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +
    #     geom_line(aes(x = year, y = tfr_ya2, color = age_2)) +
    #     geom_point(aes(x = year, y = tfr_ya2, color = age_2)) +
    #     labs(x = "year", y = "TFR", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +
    #     scale_colour_manual(values = colorRampPalette(col_6)(length(age_2t))) +
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0103_TFR_by-year-age2.pdf"), 
    #     plot = p103, width = 7, height = 4)     
    
    sszplot(tfr_ya2, aes_x = "year", aes_y = "tfr_ya2", aes_col = "age_2",
            i_x = "5", labs_y = "TFR",
            name = "0103_TFR_by-year-age2")      
    
    
#TFR by year, age1, origin  
    tfr_ya1o <- left_join(fer_yao, look_a1, by = "age") %>% 
        group_by(year, age_1, origin) %>% 
            summarize(tfr_ya1o = sum_NA(fer_yao / 100)) %>% 
        ungroup()       
    
    # p104 <- ggplot(data = tfr_ya1o) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") + 
    #     geom_line(aes(x = year, y = tfr_ya1o, color = origin)) +
    #     geom_point(aes(x = year, y = tfr_ya1o, color = origin)) +      
    #     facet_grid(~age_1) + 
    #     labs(x = "year", y = "TFR", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +      
    #     scale_colour_manual(values = col_o) +
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0104_TFR_by-year-age1-origin.pdf"), 
    #     plot = p104, width = 12, height = 4)     
  
    
    sszplot(tfr_ya1o, aes_x = "year", aes_y = "tfr_ya1o", aes_col = "origin",
            grid = "age_1", labs_y = "TFR", i_x = "5",
            name = "0104_TFR_by-year-age1-origin", width = 12) 
    
#TFR by year, age2, origin  
    tfr_ya2o <- left_join(fer_yao, look_a2, by = "age") %>% 
        group_by(year, age_2, origin) %>% 
            summarize(tfr_ya2o = sum_NA(fer_yao / 100)) %>% 
        ungroup()       
    
    # p105 <- ggplot(data = tfr_ya2o) +
    #     geom_vline(xintercept = year5, col = col_grey, linetype = "dashed") +
    #     geom_line(aes(x = year, y = tfr_ya2o, color = origin)) +
    #     geom_point(aes(x = year, y = tfr_ya2o, color = origin)) +
    #     facet_grid(~age_2) +
    #     labs(x = "year", y = "TFR", color = "") +
    #     scale_x_continuous(breaks = pretty_breaks()) +
    #     scale_colour_manual(values = col_o) +
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0105_TFR_by-year-age2-origin.pdf"), 
    #     plot = p105, width = 16, height = 5)     
        
    sszplot(tfr_ya2o, aes_x = "year", aes_y = "tfr_ya2o", aes_col = "origin",
            grid = "age_2", labs_y = "TFR", i_x = "5",
            name = "0105_TFR_by-year-age2", width = 16, height = 5) 

#-------------------------------------------------------------------
#fertility plots (before any corrections)
#-------------------------------------------------------------------

#plot: fertility by district, year, age, origin
    
    # p110 <- function(x){
    #     ggplot(data = filter(fer_dyao, (district == x) & (year >= bir_base_begin))) +
    #         geom_line(aes(x = age, y = fer_dyao, color = origin)) +
    #         facet_wrap( ~ as.factor(year)) +
    #         scale_colour_manual(values = col_o) +   
    #         labs(x = "age", y = "fertility rate (in % per year)", color = "") +
    #         ggtitle(as.character(x)) +
    #         neutral}


    pdf(paste0(bir_res, "0110_fertility_by-district-year-age-origin.pdf"),
        width = 9, height = 6)

        # lapply(uni_d, p110)
        lapply(uni_d, function(x) {
          sszplot(filter(fer_dyao, (district == x) & (year >= bir_base_begin)),
                  aes_x = "age", aes_y = "fer_dyao", aes_col = "origin",
                  wrap = "as.factor(year)", labs_y = "fertility rate (in % per year)",
                  title = as.character(x),
                  geom = "line") 
          }
        )

    dev.off()
    
#plot: fertility by year, age, origin
    #  p111 <- ggplot(data = filter(fer_yao, year >= bir_base_begin)) +
    #     geom_line(aes(x = age, y = fer_yao, color = origin)) +
    #     facet_wrap(~ as.factor(year)) +
    #     scale_colour_manual(values = col_o) +   
    #     labs(x = "age", y = "fertility rate (in % per year)", color = "") +
    #     neutral
    #    
    # ggsave(paste0(bir_res, "0111_fertility_by-year-age-origin.pdf"), 
    #     plot = p111, width = 11, height = 8)      

    
    sszplot(filter(fer_yao, year >= bir_base_begin),
            aes_x = "age", aes_y = "fer_yao", aes_col = "origin",
            wrap = "as.factor(year)", labs_y = "fertility rate (in % per year)",
            geom = "line",
            name = "0111_fertility_by-year-age-origin")
    
#plot: fertility by year, age
    #  p112 <- ggplot(data = filter(fer_ya, year >= bir_base_begin)) +
    #     geom_line(aes(x = age, y = fer_ya), color = col_6[1]) +
    #     facet_wrap(~ as.factor(year)) +
    #     labs(x = "age", y = "fertility rate (in % per year)") +
    #     neutral
    #    
    # ggsave(paste0(bir_res, "0112_fertility_by-year-age.pdf"), 
    #     plot = p112, width = 11, height = 8)      
            
    sszplot(filter(fer_ya, year >= bir_base_begin),
            aes_x = "age", aes_y = "fer_ya", wrap = "as.factor(year)",
            labs_y = "fertility rate (in % per year)", geom = "line",
            name = "0112_fertility_by-year-age") 
    
    
#-------------------------------------------------------------------
#fertility: replace values in tails, base period only
#-------------------------------------------------------------------
                     
#cumulative sums vs. thresholds
    #lowcum: cumulative sum from the lower tail of the age distribution
    #upcum: cumulative sum from the supper tail of the age distribution
    
    fer_tail <- filter(fer_dyao, year >= bir_base_begin) %>% 
        left_join(fer_yao, by = c("year", "age", "origin")) %>% 
        left_join(fer_ya, by = c("year", "age")) %>% 
        arrange(district, year, origin, age) %>% 
        group_by(district, year, origin) %>% 
            mutate(low_cum = cumsum(pop),
                   up_cum = rev(cumsum(rev(pop)))) %>% 
        ungroup() %>% 
        mutate(min_cum = pmin(low_cum, up_cum),
               fer = if_else(min_cum < bir_thres_const, bir_thres_value, 
                          if_else(min_cum < bir_thres_overall, fer_ya, 
                              if_else(min_cum < bir_thres_origin, fer_yao, fer_dyao))))

#corrected fertility: plot preparation
    cor_level <- c("initial", "corrected")    
    
    fer_cor <- select(fer_tail, district, year, origin, age, fer_dyao, fer) %>% 
        gather(`fer_dyao`, `fer`, key = category, value = fer) %>% 
        mutate(cat = factor(if_else(category == "fer_dyao", 
            cor_level[1], cor_level[2]), levels = cor_level)) %>% 
        select(district, year, origin, age, cat, fer)
    
#plot: initial vs. corrected fertility
    #test: x <- uni_d[2]
    
    # p120 <- function(x){
    #     ggplot(data = filter(fer_cor, district == x)) +
    #         geom_line(aes(x = age, y = fer, color = origin, linetype = cat)) +
    #         facet_wrap( ~ as.factor(year)) +
    #         scale_colour_manual(values = col_o) +   
    #         labs(x = "age", y = "fertility rate (in % per year)", color = "", linetype = "") +
    #         ggtitle(as.character(x)) +
    #         neutral}

    
    pdf(paste0(bir_res, "0120_fertility_tail-correction.pdf"),
        width = 13, height = 8)

        # lapply(uni_d, p120)
        lapply(uni_d, function(x) {
          sszplot(filter(fer_cor, district == x),
                  aes_x = "age", aes_y = "fer", aes_col = "origin", aes_ltyp = "cat",
                  wrap = "as.factor(year)",
                  labs_y = "fertility rate (in % per year)",
                  title = as.character(x), geom = "line") 
          }
        )

    dev.off()    
    
#plot: cumulative population from the tails
    
    p121 <- function(x){
        ggplot(data = filter(fer_tail, district == x)) +
            geom_hline(yintercept = bir_thres_origin, col = col_grey) +  
            geom_hline(yintercept = bir_thres_overall, col = col_grey, linetype = "dashed") +  
            geom_hline(yintercept = bir_thres_const, col = col_grey, linetype = "dotted") +          
            geom_line(aes(x = age, y = low_cum, color = origin)) +
            geom_line(aes(x = age, y = up_cum, color = origin)) +        
            facet_wrap( ~ as.factor(year)) +
            scale_colour_manual(values = col_o) +          
            labs(x = "age", y = "cumulative population from tails", color = "") +
            ggtitle(as.character(x)) +
            neutral}
 
    # fer_tail <- fer_tail %>%
    #   pivot_longer(c(low_cum, up_cum), names_to = "low_up", values_to = "cumul")
    #     p121 <- function(x) {
    #       sszplot(filter(fer_tail, district == x),
    #               aes_x = "age", aes_y = "cumul", aes_col = "origin",
    #               wrap = "as.factor(year)",
    #               i_y = c(bir_thres_origin, bir_thres_overall, bir_thres_const),
    #               labs_y = "cumulative population from tails", fix_col = col_o,
    #               title = as.character(x), geom = "line")
    #     }
    
    pdf(paste0(bir_res, "0121_cumulative-population-from_tails.pdf"),
        width = 13, height = 8)

        lapply(uni_d, p121)

    dev.off()    
        
    
#-------------------------------------------------------------------
#fit gam to corrected fertility rate
#-------------------------------------------------------------------
     
#with gam   
    
    # smoothing parameter see: https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/smooth.terms.html    
        
    # bs="tp". These are low rank isotropic smoothers of any number of covariates. 
    # By isotropic is meant that rotation of the covariate coordinate system will not change the result of smoothing. 
    # By low rank is meant that they have far fewer coefficients than there are data to smooth. 
    # They are reduced rank versions of the thin plate splines and use the thin plate spline penalty. 
    # They are the default smooth for s terms because there is a defined sense in which they are 
    # the optimal smoother of any given basis dimension/rank (Wood, 2003). 
       
    fer_fit <- arrange(fer_tail, district, year, origin, age) %>% 
        group_by(district, year, origin) %>% 
            mutate(fer_fit = pmax(0, gam(fer ~ s(age, bs = "tp"))$fitted.values)) %>% 
        ungroup()
    
#plot preparation
    fit_lev <- c("corrected", "with gam")
    
    fit_dat <- select(fer_fit, district, year, origin, age, fer, fer_fit) %>% 
        gather(`fer`, `fer_fit`, key = category, value = fer) %>% 
        mutate(cat = factor(if_else(category == "fer", 
            fit_lev[1], fit_lev[2]), levels = fit_lev)) %>% 
        select(district, year, age, origin, cat, fer)  
    
#plot: 
    #test: x <- uni_d[2]
    
    # p130 <- function(x){
    #     ggplot(data = filter(fit_dat, district == x)) +
    #         geom_line(aes(x = age, y = fer, color = origin, linetype = cat)) +
    #         facet_wrap( ~ as.factor(year)) +
    #         scale_colour_manual(values = col_o) +  
    #         labs(x = "age", y = "fertility rate (in % per year)", color = "", linetype = "") +
    #         ggtitle(as.character(x)) +
    #         neutral}
    
    pdf(paste0(bir_res, "0130_fertility_fit.pdf"),
        width = 13, height = 8)

        # lapply(uni_d, p130)
        lapply(uni_d, function(x) {
          sszplot(filter(fit_dat, district == x),
                  aes_x = "age", aes_y = "fer", aes_col = "origin", aes_ltyp = "cat",
                  wrap = "as.factor(year)",
                  labs_y = "fertility rate (in % per year)",
                  title = as.character(x),
                  geom = "line")
          }
        )
        
    dev.off()     
    
    
    
#-------------------------------------------------------------------
#prediction
#-------------------------------------------------------------------

#constrained regression 
    #(proportion of linear model and mean, within bandwidth)

    fer_pred <- con_reg(data = fer_fit, x = "year", y = "fer_fit", 
              group_cols = c("district", "age", "origin"),
              window = bir_window_thres, base_t0 = bir_base_begin,
              szen_t0 = szen_begin, szen_t1 = szen_end, 
              prop_trend = bir_prop_trend, thres_percent = bir_thres_percent, 
              lower_thres = bir_lower_thres, upper_thres = bir_upper_thres) %>% 
        #with the input data (WHY? to assess the regression)
            left_join(select(fer_fit, district, year, age, origin, fer_fit), 
                by = c("district", "year", "age", "origin")) %>%       
        #output generation, one rate variable for both future and past
            left_join(select(fer_dyao, district, year, age, origin, fer_dyao), 
                by = c("district", "year", "age", "origin")) %>% 
            mutate(fer_all = if_else(year <= bir_base_end, fer_dyao, pred_roll)) 
        

#-------------------------------------------------------------------
#plot the predictions: age distribution by district and year
#-------------------------------------------------------------------
  
#plot
    # p140 <- function(x){
    #     ggplot(data = filter(fer_pred, origin == x)) +
    #         geom_line(aes(x = age, y = fer_all, color = as.factor(year))) +
    #         facet_wrap(~district) +
    #         labs (x = "age", y = "fertility rate (in % per year)", color = "") +
    #         scale_colour_manual(values = col_time) +  
    #         scale_y_continuous(limits = c(0, bir_plot_lim)) +
    #         ggtitle(as.character(x)) +
    #         neutral}
    # 
    
    pdf(paste0(bir_res, "0140_fertility-prediction_by-district.pdf"),
        width = 12, height = 14)

    # lapply(uni_o, p140)
    lapply(uni_o, function(x) {
      sszplot(filter(fer_pred, origin == x),
              aes_x = "age", aes_y = "fer_all", aes_col = "as.factor(year)",
              wrap = "district",
              labs_y = "fertility rate (in % per year)",
              scale_y = c(0, bir_plot_lim),
              title = as.character(x),
              geom = "line")
      }
    )
    
    dev.off()  
        
#-------------------------------------------------------------------
#plot the precitions: along year, for selected age
#-------------------------------------------------------------------
     
#selected age
    uni_age <- sort(unique(fer_pred$age))
    sel_age <- uni_age[(uni_age %% 5) == 0]    
    
#time plot function (x: district, y: origin)
    timeplot <- function(x, y){
      
        age_dat <- filter(fer_pred, (origin == y) & (age %in% sel_age))
             
        # ggplot(data = filter(age_dat, district == x)) +
        #     geom_point(aes(x = year, y = fer_fit), col = col_o[uni_o == y]) +
        #     geom_line(aes(x = year, y = pred), col = col_o[uni_o == y], linetype = 2) +    
        #     geom_line(aes(x = year, y = pred_mean), col = col_o[uni_o == y], linetype = 3) +            
        #     geom_line(aes(x = year, y = pred_roll), col = col_o[uni_o == y]) + 
        #     labs (x = "", y = "fertility rate (in % per year)") +        
        #     facet_wrap(~age, ncol = 4, labeller = label_both) +        
        #     ggtitle(paste0(uni_o[uni_o == y], ": ", as.character(x))) +
        #     neutral
        # 
        
        sszplot(filter(age_dat, district == x),
                aes_x = "year", aes_y = "fer_fit", fix_col = 1,
                labs_y = "fertility rate (in % per year)",
                wrap = "age", col = 4,
                scale_y = c(0, bir_plot_lim),
                title = paste0(uni_o[uni_o == y], ": ", as.character(x)),
                geom = "point")
        } 
        
#plot: Swiss
    p141 <- function(x){timeplot(x, y = uni_o[1])}
   
    pdf(paste0(bir_res, "0141_fertility-prediction_by-district_along-year_Swiss.pdf"),
        width = 11, height = 5)
    
        lapply(uni_d, p141)

    dev.off()          
    
#plot: foreign
    p142 <- function(x){timeplot(x, y = uni_o[2])}
   
    pdf(paste0(bir_res, "0142_fertility-prediction_by-district_along-year_foreign.pdf"),
        width = 11, height = 5)
    
        lapply(uni_d, p142)

    dev.off()       
    
    
    
#-------------------------------------------------------------------
#fit gam to future fertility rates
#-------------------------------------------------------------------
    
#with gam (only for prediction years)    
    pred_fit <- filter(fer_pred, year >= szen_begin) %>% 
        arrange(district, year, origin, age) %>%        
        group_by(district, year, origin) %>%    
            mutate(pred_fit = pmax(0, gam(pred_roll ~ s(age, bs = "tp"))$fitted.values)) %>% 
        ungroup()
    
#plot prediction for selected years
    sel_years <- uniy_szen[(uniy_szen %% 10) == 0]
    
    sel_lev <- c("initial", "with gam")

    sel_dat <- gather(pred_fit, `pred_roll`, `pred_fit`, key = category, value = fer) %>% 
        mutate(cat = factor(if_else(category == "pred_roll", 
            sel_lev[1], sel_lev[2]), levels = sel_lev)) %>% 
        filter(year %in% sel_years)      
  
#plot
    #test: x <- uni_d[2]
    
    # p150 <- function(x){
    #     ggplot(data = filter(sel_dat, district == x)) +
    #         geom_line(aes(x = age, y = fer, color = cat)) +
    #         facet_grid(origin ~ as.factor(year)) +
    #         scale_colour_manual(values = col_6[1:2]) +   
    #         labs(x = "age", y = "fertility rate (in % per year)", color = "") +
    #         ggtitle(as.character(x)) +
    #         neutral}
    

    pdf(paste0(bir_res, "0150_fertility-prediction_before-after-fit.pdf"),
        width = 10, height = 6)

        # lapply(uni_d, p150)
        lapply(uni_d, function(x) {
          sszplot(filter(sel_dat, district == x),
                  aes_x = "age", aes_y = "fer", aes_col = "cat",
                  grid = c("origin", "as.factor(year)"),
                  labs_y = "fertility rate (in % per year)",
                  title = as.character(x),
                  geom = "line")
          }
        )

    dev.off()    
 
       
#-------------------------------------------------------------------
#export fertility rates
#-------------------------------------------------------------------

#prepare the export data
    fer_ex <- mutate(pred_fit, fer = round(pred_fit, round_rate)) %>% 
        filter(year >= szen_begin) %>%       
        select(district, year, age, origin, fer) %>% 
        arrange(district, year, age, origin)

#export
    write_csv(fer_ex, paste0(bir_exp, "birth_fertility_future.csv"))
   
    
#-------------------------------------------------------------------
#TFR (total fertility rate) 
#-------------------------------------------------------------------
       
#fertility rate of past and future
    fer_ex_past <- rename(fer_dyao, fer = fer_dyao) %>% 
        select(district, year, age, origin, fer) %>% 
        arrange(district, year, age, origin)

#export the data of the past (for model evaluation later on)    
    write_csv(fer_ex_past, paste0(bir_exp, "birth_fertility_past.csv"))

#TFR
    tfr_dyo <- bind_rows(fer_ex_past, fer_ex) %>% 
        group_by(district, year, origin) %>% 
            summarize(tfr = sum_NA(fer/100)) %>% 
        ungroup()
    
#plot
    # p160 <- ggplot(data = tfr_dyo) + 
    #     geom_vline(xintercept = c(bir_base_begin, szen_begin), color = col_grey, linetype = 1) +
    #     geom_line(aes(x = year, y = tfr, color = origin)) +  
    #     facet_wrap(~district, ncol = 4) +
    #     labs (x = "", y = "TFR", color = "") +  
    #     scale_colour_manual(values = col_o) + 
    #     neutral
    # 
    # ggsave(paste0(bir_res, "0160_TFR_by-district-origin.pdf"), 
    #     plot = p160, width = 12, height = 14) 
    
    sszplot(tfr_dyo,
            aes_x = "year", aes_y = "tfr", aes_col = "origin",
            i_x = c(bir_base_begin, szen_begin),
            wrap = "district", col = 4,
            geom = "line",
            name = "0160_TFR_by-district-origin", width = 12, height = 14)   
   
#-------------------------------------------------------------------
#TFR by age class 
#-------------------------------------------------------------------
      
#TFR by age class
    tfr_a1 <- bind_rows(fer_ex_past, fer_ex) %>% 
        left_join(look_a1, by = "age") %>%      
        group_by(district, year, origin, age_1) %>% 
            summarize(tfr = sum_NA(fer / 100)) %>% 
        ungroup()
    
#plot
    # p161 <- function(x){
    #     ggplot(data = filter(tfr_a1, origin == x)) + 
    #         geom_vline(xintercept = c(bir_base_begin, szen_begin), color = col_grey, linetype = 1) +        
    #         geom_line(aes(x = year, y = tfr, color = age_1)) +  
    #         scale_colour_manual(values = colorRampPalette(col_6)(length(age_1t))) + 
    #         facet_wrap(~district, ncol = 4) +
    #         labs (x = "", y = "TFR", color = "age") + 
    #         ggtitle(as.character(x)) +        
    #         neutral}
    
    pdf(paste0(bir_res, "0161_TFR_by-district-origin-age1.pdf"), 
        width = 12, height = 14)    
        
        # lapply(uni_o, p161) 
        lapply(uni_o, function(x) {
          sszplot(filter(tfr_a1, origin == x),
                  aes_x = "year", aes_y = "tfr", aes_col = "age_1",
                  i_x = c(bir_base_begin, szen_begin),
                  labs_col = "age",
                  wrap = "district", col = 4,
                  geom = "line",
                  title = as.character(x))  
          }
        )
               
    dev.off()
    
     
#-------------------------------------------------------------------
#TFR by age class (more detailled)
#-------------------------------------------------------------------
      
#TFR by age class
    tfr_a2 <- bind_rows(fer_ex_past, fer_ex) %>% 
        left_join(look_a2, by = "age") %>%        
        group_by(district, year, origin, age_2) %>% 
            summarize(tfr = sum_NA(fer / 100)) %>% 
        ungroup()
    
#plot
    # p162 <- function(x){
    #     ggplot(data = filter(tfr_a2, origin == x)) + 
    #         geom_vline(xintercept = c(bir_base_begin, szen_begin), color = "grey90", linetype = 1) +        
    #         geom_line(aes(x = year, y = tfr, color = age_2)) +  
    #         scale_colour_manual(values = colorRampPalette(col_6)(length(age_2t))) + 
    #         facet_wrap(~district, ncol = 4) +
    #         labs (x = "", y = "TFR", color = "age") + 
    #         ggtitle(as.character(x)) +        
    #         neutral}
    
    pdf(paste0(bir_res, "0162_TFR_by-district-origin-age2.pdf"), 
        width = 12, height = 14)    
        
        # lapply(uni_o, p162)
        lapply(uni_o, function(x) {
          sszplot(filter(tfr_a2, origin == x),
                  aes_x = "year", aes_y = "tfr", aes_col = "age_2",
                  i_x = c(bir_base_begin, szen_begin),
                  labs_col = "age",
                  wrap = "district", col = 4,
                  geom = "line",
                  title = as.character(x))  
          }
        )        
        
    dev.off()    
        
    
    


