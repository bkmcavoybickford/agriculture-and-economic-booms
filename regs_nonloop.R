################################
### Regressions by states
################################

stateregs_prodval <- list()
for (i in unique(countylevel_all$statefip)){
  stateregs_prodval[[paste0(i)]] <- feols(wins_prodvalgrowth_2012_1978 ~ wins_growth_2012_1978 + `1978` +
                                            prodval_1978 + prodval_1978^2 + `1978`^2 |
                                            statefip,
                                          data = subset(countylevel_all, statefip == i),
                                          vcov_conley(cutoff = 100),
                                          weights =~ weight_1978)
}

results_stateregs <- bind_rows(
  lapply(names(stateregs_prodval), function(nm) {
    broom::tidy(stateregs_prodval[[nm]]) %>%
      mutate(statefip = nm)
  })
) |>
  filter(term == "wins_growth_2012_1978") |>
  mutate(est = case_when(p.value > 0.1 ~ paste0(round(estimate, 3)),
                         p.value > 0.05 ~ paste0(round(estimate, 3), " + "),
                         p.value > 0.01 ~ paste0(round(estimate, 3), " * "),
                         p.value > 0.001 ~ paste0(round(estimate, 3), " ** "),
                         p.value >= 0 ~ paste0(round(estimate, 3), " *** ")),
         statefip = str_pad(statefip, 2, side = "left", pad = "0")) |>
  left_join(states, by = c("statefip" = "STATEFP")) |>
  st_as_sf()

ggplot(results_stateregs) + geom_sf(aes(fill = estimate)) + 
  geom_sf_text(aes(label = est), size = 2) +
  theme_void() + scale_fill_viridis_c(trans = scales::pseudo_log_trans()) +
  labs(fill = "")
ggsave("Output/Figures/stateregs_prodval.png", width = 6, height = 4)

## And for farm acre value
stateregs_farmacrevalue <- list()
for (i in unique(countylevel_all$statefip)){
  stateregs_farmacrevalue[[paste0(i)]] <- feols(wins_farmacrevaluegrowth_2012_1978 ~ wins_growth_2012_1978 + `1978` +
                                                  farmacrevalue_1978 + farmacrevalue_1978^2 + `1978`^2 |
                                                  statefip,
                                                data = subset(countylevel_all, statefip == i),
                                                vcov_conley(cutoff = 100),
                                                weights =~ weight_1978)
}

results_stateregs <- bind_rows(
  lapply(names(stateregs_farmacrevalue), function(nm) {
    broom::tidy(stateregs_farmacrevalue[[nm]]) %>%
      mutate(statefip = nm)
  })
) |>
  filter(term == "wins_growth_2012_1978") |>
  mutate(est = case_when(p.value > 0.1 ~ paste0(round(estimate, 3)),
                         p.value > 0.05 ~ paste0(round(estimate, 3), " + "),
                         p.value > 0.01 ~ paste0(round(estimate, 3), " * "),
                         p.value > 0.001 ~ paste0(round(estimate, 3), " ** "),
                         p.value >= 0 ~ paste0(round(estimate, 3), " *** ")),
         statefip = str_pad(statefip, 2, side = "left", pad = "0")) |>
  left_join(states, by = c("statefip" = "STATEFP")) |>
  st_as_sf()

ggplot(results_stateregs) + geom_sf(aes(fill = estimate)) + 
  geom_sf_text(aes(label = est), size = 2) +
  theme_void() + scale_fill_viridis_c(trans = scales::pseudo_log_trans()) +
  labs(fill = "")
ggsave("Output/Figures/stateregs_farmacrevalue.png", width = 6, height = 4)

#########################################
### Splines
#########################################

countylevel_spline <- countylevel_all |>
  dplyr::rename("X1978" = `1978`)


spline_3_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                              bs(wins_growth_2012_1978, df = 3) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_3_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_3_1978_2012), 2)))
ggsave("Output/Figures/spline_3_1978_2012.png")

spline_4_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                              bs(wins_growth_2012_1978, df = 4) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_4_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = " Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_4_1978_2012), 2)))
ggsave("Output/Figures/spline_4_1978_2012.png")

spline_5_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                              bs(wins_growth_2012_1978, df = 5) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_5_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_5_1978_2012), 2)))
ggsave("Output/Figures/spline_5_1978_2012.png")

spline_6_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                              bs(wins_growth_2012_1978, df = 6) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_6_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_6_1978_2012), 2)))
ggsave("Output/Figures/spline_6_1978_2012.png")

spline_7_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                              bs(wins_growth_2012_1978, df = 7) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_7_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_7_1978_2012), 2)))
ggsave("Output/Figures/spline_7_1978_2012.png")

spline_10_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                               bs(wins_growth_2012_1978, df = 10) + X1978 +
                               cropland_1978 + cropland_1978^2 + X1978^2 |
                               statefip,
                             data = countylevel_spline,
                             vcov_conley(cutoff = 100),
                             weights =~ weight_1978)
plot_slopes(spline_10_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_10_1978_2012), 2)))
ggsave("Output/Figures/spline_10_1978_2012.png")

spline_13_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                               bs(wins_growth_2012_1978, df = 13) + X1978 +
                               cropland_1978 + cropland_1978^2 + X1978^2 |
                               statefip,
                             data = countylevel_spline,
                             vcov_conley(cutoff = 100),
                             weights =~ weight_1978)
plot_slopes(spline_13_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_13_1978_2012), 2)))
ggsave("Output/Figures/spline_13_1978_2012.png")

spline_lin_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                                bs(wins_growth_2012_1978, df = 6, degree = 1) + X1978 +
                                cropland_1978 + cropland_1978^2 + X1978^2 |
                                statefip,
                              data = countylevel_spline,
                              vcov_conley(cutoff = 100),
                              weights =~ weight_1978)
plot_slopes(spline_lin_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_lin_1978_2012), 2)))
ggsave("Output/Figures/spline_lin_1978_2012.png")

spline_quad_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                                 bs(wins_growth_2012_1978, df = 7, degree = 2) + X1978 +
                                 cropland_1978 + cropland_1978^2 + X1978^2 |
                                 statefip,
                               data = countylevel_spline,
                               vcov_conley(cutoff = 100),
                               weights =~ weight_1978)
plot_slopes(spline_quad_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_quad_1978_2012), 2)))
ggsave("Output/Figures/spline_quad_1978_2012.png")

spline_cubic_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                                  bs(wins_growth_2012_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_2012), 2)))
ggsave("Output/Figures/spline_cubic_1978_2012.png")

spline_quart_1978_2012 <- feols(wins_croplandgrowth_2012_1978 ~ 
                                  bs(wins_growth_2012_1978, df = 9, degree = 4) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_quart_1978_2012, variables = "wins_growth_2012_1978",
            condition = "wins_growth_2012_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_quart_1978_2012), 2)))
ggsave("Output/Figures/spline_quart_1978_2012.png")


## Setting it with different ending years
spline_cubic_1978_2007 <- feols(wins_croplandgrowth_2007_1978 ~ 
                                  bs(wins_growth_2007_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_2007, variables = "wins_growth_2007_1978",
            condition = "wins_growth_2007_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_2007), 2)))
ggsave("Output/Figures/spline_cubic_1978_2007.png")

spline_cubic_1978_2002 <- feols(wins_croplandgrowth_2002_1978 ~ 
                                  bs(wins_growth_2002_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_2002, variables = "wins_growth_2002_1978",
            condition = "wins_growth_2002_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_2002), 2)))
ggsave("Output/Figures/spline_cubic_1978_2002.png")

spline_cubic_1978_1997 <- feols(wins_croplandgrowth_1997_1978 ~ 
                                  bs(wins_growth_1997_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1997, variables = "wins_growth_1997_1978",
            condition = "wins_growth_1997_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1997), 2)))
ggsave("Output/Figures/spline_cubic_1978_1997.png")

spline_cubic_1978_1992 <- feols(wins_croplandgrowth_1992_1978 ~ 
                                  bs(wins_growth_1992_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1992, variables = "wins_growth_1992_1978",
            condition = "wins_growth_1992_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1992), 2)))
ggsave("Output/Figures/spline_cubic_1978_1992.png")

spline_cubic_1978_1987 <- feols(wins_croplandgrowth_1987_1978 ~ 
                                  bs(wins_growth_1987_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1987, variables = "wins_growth_1987_1978",
            condition = "wins_growth_1987_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1987), 2)))
ggsave("Output/Figures/spline_cubic_1978_1987.png")

spline_cubic_1978_1982 <- feols(wins_croplandgrowth_1982_1978 ~ 
                                  bs(wins_growth_1982_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1982, variables = "wins_growth_1982_1978",
            condition = "wins_growth_1982_1978") + mytheme + labs(x = "Growth",
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1982), 2)))
ggsave("Output/Figures/spline_cubic_1978_1982.png")

##############################
### Interactions
##############################

fullspec_2012_cropint <- feols(wins_croplandgrowth_2012_1978 ~ 
                                 wins_growth_2012_1978*wins_cropland_1978 + wins_1978 +
                                 wins_cropland_1978^2 + wins_1978^2 |
                                 statefip,
                               data = countylevel_all,
                               vcov_conley(cutoff = 100),
                               weights =~ weight_1978)

fullspec_2012_empint <- feols(wins_croplandgrowth_2012_1978 ~ 
                                wins_growth_2012_1978*wins_1978 +
                                wins_cropland_1978 + wins_cropland_1978^2 + wins_1978^2 |
                                statefip,
                              data = countylevel_all,
                              vcov_conley(cutoff = 100),
                              weights =~ weight_1978)

fullspec_2012_bothint <- feols(wins_croplandgrowth_2012_1978 ~ 
                                 wins_growth_2012_1978*wins_1978*wins_cropland_1978 + 
                                 wins_cropland_1978^2 + wins_1978^2 |
                                 statefip,
                               data = countylevel_all,
                               vcov_conley(cutoff = 100),
                               weights =~ weight_1978)

modelsummary(models = list(allbases_wins_cropland$`1978_2012`, fullspec_2012_cropint, 
                           fullspec_2012_empint, fullspec_2012_bothint),
             stars = TRUE, fmt = 5,
             coef_map = c("wins_growth_2012_1978" = "Employment Growth",
                          "wins_growth_2012_1978:wins_cropland_1978" = "Growth x 1978 Cropland",
                          "wins_growth_2012_1978:wins_1978" = "Growth x 1978 Employed Pop.",
                          "wins_growth_2012_1978:wins_1978:wins_cropland_1978" = "Growth x 1978 Cropland x 1978 Employed Pop."),
             gof_function = check_controls(),
             gof_map = gm,
             title = "\\label{crop_cl_het}Winsorized Change in Total Cropland, County Level, 1978 to 2012",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  save_tt("Output/Tables/fullspec_countylevel_het.tex", overwrite = TRUE)


prodval_2012_prodint <- feols(wins_prodvalgrowth_2012_1978 ~ 
                                 wins_growth_2012_1978*wins_prodval_1978 + wins_1978 +
                                 wins_prodval_1978^2 + wins_1978^2 |
                                 statefip,
                               data = countylevel_all,
                               vcov_conley(cutoff = 100),
                               weights =~ weight_1978)

prodval_2012_empint <- feols(wins_prodvalgrowth_2012_1978 ~ 
                                wins_growth_2012_1978*wins_1978 +
                                wins_prodval_1978 + wins_prodval_1978^2 + wins_1978^2 |
                                statefip,
                              data = countylevel_all,
                              vcov_conley(cutoff = 100),
                              weights =~ weight_1978)

prodval_2012_bothint <- feols(wins_prodvalgrowth_2012_1978 ~ 
                                 wins_growth_2012_1978*wins_1978*wins_prodval_1978 + 
                                 wins_prodval_1978^2 + wins_1978^2 |
                                 statefip,
                               data = countylevel_all,
                               vcov_conley(cutoff = 100),
                               weights =~ weight_1978)

modelsummary(models = list(allbases_wins_prodval$`1978_2012`, prodval_2012_prodint, 
                           prodval_2012_empint, prodval_2012_bothint),
             stars = TRUE, fmt = 5,
             coef_map = c("wins_growth_2012_1978" = "Employment Growth",
                          "wins_growth_2012_1978:wins_prodval_1978" = "Growth x 1978 Production Value",
                          "wins_growth_2012_1978:wins_1978" = "Growth x 1978 Employed Pop.",
                          "wins_growth_2012_1978:wins_1978:wins_prodval_1978" = "Growth x 1978 Production Value x 1978 Employed Pop."),
             gof_function = check_controls(),
             gof_map = gm,
             title = "\\label{prodval_cl_het}Winsorized Change in Agricultural Production Value, County Level, 1978 to 2012",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  save_tt("Output/Tables/prodval_countylevel_het.tex", overwrite = TRUE)

#################################
### Robustness checks
#################################

## For tradables (NAICS code 3) only

allbases_ss_tradable <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_croplandgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | wins_growth_", j, "_", i, 
                                      " ~ wins_shiftsharetradable_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_ss_tradable[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_ss_tradable <- list("Starting 1978" = 
                                       list("1982" = allbases_ss_tradable$`1978_1982`,
                                            "1987" = allbases_ss_tradable$`1978_1987`,
                                            "1992" = allbases_ss_tradable$`1978_1992`,
                                            "1997" = allbases_ss_tradable$`1978_1997`,
                                            "2002" = allbases_ss_tradable$`1978_2002`,
                                            "2007" = allbases_ss_tradable$`1978_2007`,
                                            "2012" = allbases_ss_tradable$`1978_2012`),
                                     "Starting 1982" =
                                       list("1987" = allbases_ss_tradable$`1982_1987`,
                                            "1992" = allbases_ss_tradable$`1982_1992`,
                                            "1997" = allbases_ss_tradable$`1982_1997`,
                                            "2002" = allbases_ss_tradable$`1982_2002`,
                                            "2007" = allbases_ss_tradable$`1982_2007`,
                                            "2012" = allbases_ss_tradable$`1982_2012`),
                                     "Starting 1987" =
                                       list("1992" = allbases_ss_tradable$`1987_1992`,
                                            "1997" = allbases_ss_tradable$`1987_1997`,
                                            "2002" = allbases_ss_tradable$`1987_2002`,
                                            "2007" = allbases_ss_tradable$`1987_2007`,
                                            "2012" = allbases_ss_tradable$`1987_2012`),
                                     "Starting 1992" =
                                       list("1997" = allbases_ss_tradable$`1992_1997`,
                                            "2002" = allbases_ss_tradable$`1992_2002`,
                                            "2007" = allbases_ss_tradable$`1992_2007`,
                                            "2012" = allbases_ss_tradable$`1992_2012`),
                                     "Starting 1997" =
                                       list("2002" = allbases_ss_tradable$`1997_2002`,
                                            "2007" = allbases_ss_tradable$`1997_2007`,
                                            "2012" = allbases_ss_tradable$`1997_2012`),
                                     "Starting 2002" =
                                       list("2007" = allbases_ss_tradable$`2002_2007`,
                                            "2012" = allbases_ss_tradable$`2002_2012`),
                                     "Starting 2007" =
                                       list("2012" = allbases_ss_tradable$`2007_2012`))

modelsummary(allbases_ordered_ss_tradable,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_wins_growth_", x), "Fitted Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{crop_allbases_ss_tradable}Winsorized Change in Total Cropland, County Level, Shift-Share of Manufacturing Only",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_countylevel_allbases_tradable.tex", overwrite = TRUE)

allbases_prodval_ss_tradable <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | wins_growth_", j, "_", i, 
                                      " ~ wins_shiftsharetradable_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_ss_tradable[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_ordered_ss_tradable <- list("Starting 1978" = 
                                               list("1982" = allbases_prodval_ss_tradable$`1978_1982`,
                                                    "1987" = allbases_prodval_ss_tradable$`1978_1987`,
                                                    "1992" = allbases_prodval_ss_tradable$`1978_1992`,
                                                    "1997" = allbases_prodval_ss_tradable$`1978_1997`,
                                                    "2002" = allbases_prodval_ss_tradable$`1978_2002`,
                                                    "2007" = allbases_prodval_ss_tradable$`1978_2007`,
                                                    "2012" = allbases_prodval_ss_tradable$`1978_2012`),
                                             "Starting 1982" =
                                               list("1987" = allbases_prodval_ss_tradable$`1982_1987`,
                                                    "1992" = allbases_prodval_ss_tradable$`1982_1992`,
                                                    "1997" = allbases_prodval_ss_tradable$`1982_1997`,
                                                    "2002" = allbases_prodval_ss_tradable$`1982_2002`,
                                                    "2007" = allbases_prodval_ss_tradable$`1982_2007`,
                                                    "2012" = allbases_prodval_ss_tradable$`1982_2012`),
                                             "Starting 1987" =
                                               list("1992" = allbases_prodval_ss_tradable$`1987_1992`,
                                                    "1997" = allbases_prodval_ss_tradable$`1987_1997`,
                                                    "2002" = allbases_prodval_ss_tradable$`1987_2002`,
                                                    "2007" = allbases_prodval_ss_tradable$`1987_2007`,
                                                    "2012" = allbases_prodval_ss_tradable$`1987_2012`),
                                             "Starting 1992" =
                                               list("1997" = allbases_prodval_ss_tradable$`1992_1997`,
                                                    "2002" = allbases_prodval_ss_tradable$`1992_2002`,
                                                    "2007" = allbases_prodval_ss_tradable$`1992_2007`,
                                                    "2012" = allbases_prodval_ss_tradable$`1992_2012`),
                                             "Starting 1997" =
                                               list("2002" = allbases_prodval_ss_tradable$`1997_2002`,
                                                    "2007" = allbases_prodval_ss_tradable$`1997_2007`,
                                                    "2012" = allbases_prodval_ss_tradable$`1997_2012`),
                                             "Starting 2002" =
                                               list("2007" = allbases_prodval_ss_tradable$`2002_2007`,
                                                    "2012" = allbases_prodval_ss_tradable$`2002_2012`),
                                             "Starting 2007" =
                                               list("2012" = allbases_prodval_ss_tradable$`2007_2012`))

modelsummary(allbases_prodval_ordered_ss_tradable,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_wins_growth_", x), "Fitted Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{prodval_allbases_ss_tradable}Winsorized Change in Agricultural Production Value, County Level, Shift-Share of Manufacturing Only",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_prodval_countylevel_allbases_tradable.tex", overwrite = TRUE)

## And farm acre value
allbases_farmacrevalue_ss_tradable <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_farmacrevaluegrowth_", j, "_", i, 
                                      " ~ 1 | statefip | wins_growth_", j, "_", i, 
                                      " ~ wins_shiftsharetradable_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_farmacrevalue_ss_tradable[[paste0(i, "_", j)]] <- run
  }
}

allbases_farmacrevalue_ordered_ss_tradable <- list("Starting 1978" = 
                                                     list("1982" = allbases_farmacrevalue_ss_tradable$`1978_1982`,
                                                          "1987" = allbases_farmacrevalue_ss_tradable$`1978_1987`,
                                                          "1992" = allbases_farmacrevalue_ss_tradable$`1978_1992`,
                                                          "1997" = allbases_farmacrevalue_ss_tradable$`1978_1997`,
                                                          "2002" = allbases_farmacrevalue_ss_tradable$`1978_2002`,
                                                          "2007" = allbases_farmacrevalue_ss_tradable$`1978_2007`,
                                                          "2012" = allbases_farmacrevalue_ss_tradable$`1978_2012`),
                                                   "Starting 1982" =
                                                     list("1987" = allbases_farmacrevalue_ss_tradable$`1982_1987`,
                                                          "1992" = allbases_farmacrevalue_ss_tradable$`1982_1992`,
                                                          "1997" = allbases_farmacrevalue_ss_tradable$`1982_1997`,
                                                          "2002" = allbases_farmacrevalue_ss_tradable$`1982_2002`,
                                                          "2007" = allbases_farmacrevalue_ss_tradable$`1982_2007`,
                                                          "2012" = allbases_farmacrevalue_ss_tradable$`1982_2012`),
                                                   "Starting 1987" =
                                                     list("1992" = allbases_farmacrevalue_ss_tradable$`1987_1992`,
                                                          "1997" = allbases_farmacrevalue_ss_tradable$`1987_1997`,
                                                          "2002" = allbases_farmacrevalue_ss_tradable$`1987_2002`,
                                                          "2007" = allbases_farmacrevalue_ss_tradable$`1987_2007`,
                                                          "2012" = allbases_farmacrevalue_ss_tradable$`1987_2012`),
                                                   "Starting 1992" =
                                                     list("1997" = allbases_farmacrevalue_ss_tradable$`1992_1997`,
                                                          "2002" = allbases_farmacrevalue_ss_tradable$`1992_2002`,
                                                          "2007" = allbases_farmacrevalue_ss_tradable$`1992_2007`,
                                                          "2012" = allbases_farmacrevalue_ss_tradable$`1992_2012`),
                                                   "Starting 1997" =
                                                     list("2002" = allbases_farmacrevalue_ss_tradable$`1997_2002`,
                                                          "2007" = allbases_farmacrevalue_ss_tradable$`1997_2007`,
                                                          "2012" = allbases_farmacrevalue_ss_tradable$`1997_2012`),
                                                   "Starting 2002" =
                                                     list("2007" = allbases_farmacrevalue_ss_tradable$`2002_2007`,
                                                          "2012" = allbases_farmacrevalue_ss_tradable$`2002_2012`),
                                                   "Starting 2007" =
                                                     list("2012" = allbases_farmacrevalue_ss_tradable$`2007_2012`))

modelsummary(allbases_farmacrevalue_ordered_ss_tradable,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_wins_growth_", x), "Fitted Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{farmacrevalue_allbases_ss_tradable}Winsorized Change in Value for an Acre of Farmland, County Level, Shift-Share of Manufacturing Only",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_farmacrevalue_countylevel_allbases_tradable.tex", overwrite = TRUE)


allbases_ordered_fs_tradable <- list("Starting 1978" = 
                                       list("1982" = summary(allbases_ss_tradable$`1978_1982`, stage = 1),
                                            "1987" = summary(allbases_ss_tradable$`1978_1987`, stage = 1),
                                            "1992" = summary(allbases_ss_tradable$`1978_1992`, stage = 1),
                                            "1997" = summary(allbases_ss_tradable$`1978_1997`, stage = 1),
                                            "2002" = summary(allbases_ss_tradable$`1978_2002`, stage = 1),
                                            "2007" = summary(allbases_ss_tradable$`1978_2007`, stage = 1),
                                            "2012" = summary(allbases_ss_tradable$`1978_2012`, stage = 1)),
                                     "Starting 1982" =
                                       list("1987" = summary(allbases_ss_tradable$`1982_1987`, stage = 1),
                                            "1992" = summary(allbases_ss_tradable$`1982_1992`, stage = 1),
                                            "1997" = summary(allbases_ss_tradable$`1982_1997`, stage = 1),
                                            "2002" = summary(allbases_ss_tradable$`1982_2002`, stage = 1),
                                            "2007" = summary(allbases_ss_tradable$`1982_2007`, stage = 1),
                                            "2012" = summary(allbases_ss_tradable$`1982_2012`, stage = 1)),
                                     "Starting 1987" =
                                       list("1992" = summary(allbases_ss_tradable$`1987_1992`, stage = 1),
                                            "1997" = summary(allbases_ss_tradable$`1987_1997`, stage = 1),
                                            "2002" = summary(allbases_ss_tradable$`1987_2002`, stage = 1),
                                            "2007" = summary(allbases_ss_tradable$`1987_2007`, stage = 1),
                                            "2012" = summary(allbases_ss_tradable$`1987_2012`, stage = 1)),
                                     "Starting 1992" =
                                       list("1997" = summary(allbases_ss_tradable$`1992_1997`, stage = 1),
                                            "2002" = summary(allbases_ss_tradable$`1992_2002`, stage = 1),
                                            "2007" = summary(allbases_ss_tradable$`1992_2007`, stage = 1),
                                            "2012" = summary(allbases_ss_tradable$`1992_2012`, stage = 1)),
                                     "Starting 1997" =
                                       list("2002" = summary(allbases_ss_tradable$`1997_2002`, stage = 1),
                                            "2007" = summary(allbases_ss_tradable$`1997_2007`, stage = 1),
                                            "2012" = summary(allbases_ss_tradable$`1997_2012`, stage = 1)),
                                     "Starting 2002" =
                                       list("2007" = summary(allbases_ss_tradable$`2002_2007`, stage = 1),
                                            "2012" = summary(allbases_ss_tradable$`2002_2012`, stage = 1)),
                                     "Starting 2007" =
                                       list("2012" = summary(allbases_ss_tradable$`2007_2012`, stage = 1)))

modelsummary(allbases_ordered_fs_tradable,
             shape = "rbind",
             stars = TRUE,
             coef_rename = function(x) {
               ifelse(grepl("^wins_shiftshare", x), "Shift-Share Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{crop_allbases_1s_tradable}County-Level Shift-Share, First Stage, Manufacturing Only",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/firststage_countylevel_allbases_tradable.tex", overwrite = TRUE)


## And for naics2 instead
allbases_ss_naics2 <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_croplandgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | wins_growth_", j, "_", i, 
                                      " ~ wins_shiftsharenaics2_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_ss_naics2[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_ss_naics2 <- list("Starting 1978" = 
                                     list("1982" = allbases_ss_naics2$`1978_1982`,
                                          "1987" = allbases_ss_naics2$`1978_1987`,
                                          "1992" = allbases_ss_naics2$`1978_1992`,
                                          "1997" = allbases_ss_naics2$`1978_1997`,
                                          "2002" = allbases_ss_naics2$`1978_2002`,
                                          "2007" = allbases_ss_naics2$`1978_2007`,
                                          "2012" = allbases_ss_naics2$`1978_2012`),
                                   "Starting 1982" =
                                     list("1987" = allbases_ss_naics2$`1982_1987`,
                                          "1992" = allbases_ss_naics2$`1982_1992`,
                                          "1997" = allbases_ss_naics2$`1982_1997`,
                                          "2002" = allbases_ss_naics2$`1982_2002`,
                                          "2007" = allbases_ss_naics2$`1982_2007`,
                                          "2012" = allbases_ss_naics2$`1982_2012`),
                                   "Starting 1987" =
                                     list("1992" = allbases_ss_naics2$`1987_1992`,
                                          "1997" = allbases_ss_naics2$`1987_1997`,
                                          "2002" = allbases_ss_naics2$`1987_2002`,
                                          "2007" = allbases_ss_naics2$`1987_2007`,
                                          "2012" = allbases_ss_naics2$`1987_2012`),
                                   "Starting 1992" =
                                     list("1997" = allbases_ss_naics2$`1992_1997`,
                                          "2002" = allbases_ss_naics2$`1992_2002`,
                                          "2007" = allbases_ss_naics2$`1992_2007`,
                                          "2012" = allbases_ss_naics2$`1992_2012`),
                                   "Starting 1997" =
                                     list("2002" = allbases_ss_naics2$`1997_2002`,
                                          "2007" = allbases_ss_naics2$`1997_2007`,
                                          "2012" = allbases_ss_naics2$`1997_2012`),
                                   "Starting 2002" =
                                     list("2007" = allbases_ss_naics2$`2002_2007`,
                                          "2012" = allbases_ss_naics2$`2002_2012`),
                                   "Starting 2007" =
                                     list("2012" = allbases_ss_naics2$`2007_2012`))

modelsummary(allbases_ordered_ss_naics2,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_wins_growth_", x), "Fitted Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{crop_allbases_ss_naics2}Winsorized Change in Total Cropland, County Level, Shift-Share with 2-Digit NAICS Codes",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_countylevel_allbases_naics2.tex", overwrite = TRUE)

allbases_prodval_ss_naics2 <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | wins_growth_", j, "_", i, 
                                      " ~ wins_shiftsharenaics2_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_ss_naics2[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_ordered_ss_naics2 <- list("Starting 1978" = 
                                             list("1982" = allbases_prodval_ss_naics2$`1978_1982`,
                                                  "1987" = allbases_prodval_ss_naics2$`1978_1987`,
                                                  "1992" = allbases_prodval_ss_naics2$`1978_1992`,
                                                  "1997" = allbases_prodval_ss_naics2$`1978_1997`,
                                                  "2002" = allbases_prodval_ss_naics2$`1978_2002`,
                                                  "2007" = allbases_prodval_ss_naics2$`1978_2007`,
                                                  "2012" = allbases_prodval_ss_naics2$`1978_2012`),
                                           "Starting 1982" =
                                             list("1987" = allbases_prodval_ss_naics2$`1982_1987`,
                                                  "1992" = allbases_prodval_ss_naics2$`1982_1992`,
                                                  "1997" = allbases_prodval_ss_naics2$`1982_1997`,
                                                  "2002" = allbases_prodval_ss_naics2$`1982_2002`,
                                                  "2007" = allbases_prodval_ss_naics2$`1982_2007`,
                                                  "2012" = allbases_prodval_ss_naics2$`1982_2012`),
                                           "Starting 1987" =
                                             list("1992" = allbases_prodval_ss_naics2$`1987_1992`,
                                                  "1997" = allbases_prodval_ss_naics2$`1987_1997`,
                                                  "2002" = allbases_prodval_ss_naics2$`1987_2002`,
                                                  "2007" = allbases_prodval_ss_naics2$`1987_2007`,
                                                  "2012" = allbases_prodval_ss_naics2$`1987_2012`),
                                           "Starting 1992" =
                                             list("1997" = allbases_prodval_ss_naics2$`1992_1997`,
                                                  "2002" = allbases_prodval_ss_naics2$`1992_2002`,
                                                  "2007" = allbases_prodval_ss_naics2$`1992_2007`,
                                                  "2012" = allbases_prodval_ss_naics2$`1992_2012`),
                                           "Starting 1997" =
                                             list("2002" = allbases_prodval_ss_naics2$`1997_2002`,
                                                  "2007" = allbases_prodval_ss_naics2$`1997_2007`,
                                                  "2012" = allbases_prodval_ss_naics2$`1997_2012`),
                                           "Starting 2002" =
                                             list("2007" = allbases_prodval_ss_naics2$`2002_2007`,
                                                  "2012" = allbases_prodval_ss_naics2$`2002_2012`),
                                           "Starting 2007" =
                                             list("2012" = allbases_prodval_ss_naics2$`2007_2012`))

modelsummary(allbases_prodval_ordered_ss_naics2,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_wins_growth_", x), "Fitted Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{prodval_allbases_ss_naics2}Winsorized Change in Agricultural Production Value, County Level, Shift-Share with 2-Digit NAICS Codes",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_prodval_countylevel_allbases_naics2.tex", overwrite = TRUE)

#########################
### Unweighted
#########################

allbases_prodval_nowt <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, " + wins_", i, " + wins_prodval_", i,
                                      " + wins_prodval_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100))")))
    allbases_prodval_nowt[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_nowt_ordered <- list("Starting 1978" = 
                                        list("1982" = allbases_prodval_nowt$`1978_1982`,
                                             "1987" = allbases_prodval_nowt$`1978_1987`,
                                             "1992" = allbases_prodval_nowt$`1978_1992`,
                                             "1997" = allbases_prodval_nowt$`1978_1997`,
                                             "2002" = allbases_prodval_nowt$`1978_2002`,
                                             "2007" = allbases_prodval_nowt$`1978_2007`,
                                             "2012" = allbases_prodval_nowt$`1978_2012`),
                                      "Starting 1982" =
                                        list("1987" = allbases_prodval_nowt$`1982_1987`,
                                             "1992" = allbases_prodval_nowt$`1982_1992`,
                                             "1997" = allbases_prodval_nowt$`1982_1997`,
                                             "2002" = allbases_prodval_nowt$`1982_2002`,
                                             "2007" = allbases_prodval_nowt$`1982_2007`,
                                             "2012" = allbases_prodval_nowt$`1982_2012`),
                                      "Starting 1987" =
                                        list("1992" = allbases_prodval_nowt$`1987_1992`,
                                             "1997" = allbases_prodval_nowt$`1987_1997`,
                                             "2002" = allbases_prodval_nowt$`1987_2002`,
                                             "2007" = allbases_prodval_nowt$`1987_2007`,
                                             "2012" = allbases_prodval_nowt$`1987_2012`),
                                      "Starting 1992" =
                                        list("1997" = allbases_prodval_nowt$`1992_1997`,
                                             "2002" = allbases_prodval_nowt$`1992_2002`,
                                             "2007" = allbases_prodval_nowt$`1992_2007`,
                                             "2012" = allbases_prodval_nowt$`1992_2012`),
                                      "Starting 1997" =
                                        list("2002" = allbases_prodval_nowt$`1997_2002`,
                                             "2007" = allbases_prodval_nowt$`1997_2007`,
                                             "2012" = allbases_prodval_nowt$`1997_2012`),
                                      "Starting 2002" =
                                        list("2007" = allbases_prodval_nowt$`2002_2007`,
                                             "2012" = allbases_prodval_nowt$`2002_2012`),
                                      "Starting 2007" =
                                        list("2012" = allbases_prodval_nowt$`2007_2012`))

modelsummary(allbases_prodval_nowt_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^wins_growth_", x), "Winsorized Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_nowt}Winsorized Change in Agricultural Production Value, County Level And Unweighted",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_nowt.tex", overwrite = TRUE)

###########################
### Non-agriculture
###########################

allbases_prodval_nonag <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ wins_growthnonag_", j, "_", i, " + `", i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_nonag[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_prodval_nonag <- list("Starting 1978" = 
                                         list("1982" = allbases_prodval_nonag$`1978_1982`,
                                              "1987" = allbases_prodval_nonag$`1978_1987`,
                                              "1992" = allbases_prodval_nonag$`1978_1992`,
                                              "1997" = allbases_prodval_nonag$`1978_1997`,
                                              "2002" = allbases_prodval_nonag$`1978_2002`,
                                              "2007" = allbases_prodval_nonag$`1978_2007`,
                                              "2012" = allbases_prodval_nonag$`1978_2012`),
                                       "Starting 1982" =
                                         list("1987" = allbases_prodval_nonag$`1982_1987`,
                                              "1992" = allbases_prodval_nonag$`1982_1992`,
                                              "1997" = allbases_prodval_nonag$`1982_1997`,
                                              "2002" = allbases_prodval_nonag$`1982_2002`,
                                              "2007" = allbases_prodval_nonag$`1982_2007`,
                                              "2012" = allbases_prodval_nonag$`1982_2012`),
                                       "Starting 1987" =
                                         list("1992" = allbases_prodval_nonag$`1987_1992`,
                                              "1997" = allbases_prodval_nonag$`1987_1997`,
                                              "2002" = allbases_prodval_nonag$`1987_2002`,
                                              "2007" = allbases_prodval_nonag$`1987_2007`,
                                              "2012" = allbases_prodval_nonag$`1987_2012`),
                                       "Starting 1992" =
                                         list("1997" = allbases_prodval_nonag$`1992_1997`,
                                              "2002" = allbases_prodval_nonag$`1992_2002`,
                                              "2007" = allbases_prodval_nonag$`1992_2007`,
                                              "2012" = allbases_prodval_nonag$`1992_2012`),
                                       "Starting 1997" =
                                         list("2002" = allbases_prodval_nonag$`1997_2002`,
                                              "2007" = allbases_prodval_nonag$`1997_2007`,
                                              "2012" = allbases_prodval_nonag$`1997_2012`),
                                       "Starting 2002" =
                                         list("2007" = allbases_prodval_nonag$`2002_2007`,
                                              "2012" = allbases_prodval_nonag$`2002_2012`),
                                       "Starting 2007" =
                                         list("2012" = allbases_prodval_nonag$`2007_2012`))

modelsummary(allbases_ordered_prodval_nonag,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growthnonag_)",
             coef_rename = function(x) {
               ifelse(grepl("^wins_growthnonag_", x), "Winsorized Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_nonag}Winsorized Change in Agricultural Production Value, County Level, Incl. More Industries",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_nonag.tex", overwrite = TRUE)

###############################
### Shift-share first-stage
###############################

allbases_ordered_fs <- list("Starting 1978" = 
                              list("1982" = summary(allbases_prodval_ss$`1978_1982`, stage = 1),
                                   "1987" = summary(allbases_prodval_ss$`1978_1987`, stage = 1),
                                   "1992" = summary(allbases_prodval_ss$`1978_1992`, stage = 1),
                                   "1997" = summary(allbases_prodval_ss$`1978_1997`, stage = 1),
                                   "2002" = summary(allbases_prodval_ss$`1978_2002`, stage = 1),
                                   "2007" = summary(allbases_prodval_ss$`1978_2007`, stage = 1),
                                   "2012" = summary(allbases_prodval_ss$`1978_2012`, stage = 1)),
                            "Starting 1982" =
                              list("1987" = summary(allbases_prodval_ss$`1982_1987`, stage = 1),
                                   "1992" = summary(allbases_prodval_ss$`1982_1992`, stage = 1),
                                   "1997" = summary(allbases_prodval_ss$`1982_1997`, stage = 1),
                                   "2002" = summary(allbases_prodval_ss$`1982_2002`, stage = 1),
                                   "2007" = summary(allbases_prodval_ss$`1982_2007`, stage = 1),
                                   "2012" = summary(allbases_prodval_ss$`1982_2012`, stage = 1)),
                            "Starting 1987" =
                              list("1992" = summary(allbases_prodval_ss$`1987_1992`, stage = 1),
                                   "1997" = summary(allbases_prodval_ss$`1987_1997`, stage = 1),
                                   "2002" = summary(allbases_prodval_ss$`1987_2002`, stage = 1),
                                   "2007" = summary(allbases_prodval_ss$`1987_2007`, stage = 1),
                                   "2012" = summary(allbases_prodval_ss$`1987_2012`, stage = 1)),
                            "Starting 1992" =
                              list("1997" = summary(allbases_prodval_ss$`1992_1997`, stage = 1),
                                   "2002" = summary(allbases_prodval_ss$`1992_2002`, stage = 1),
                                   "2007" = summary(allbases_prodval_ss$`1992_2007`, stage = 1),
                                   "2012" = summary(allbases_prodval_ss$`1992_2012`, stage = 1)),
                            "Starting 1997" =
                              list("2002" = summary(allbases_prodval_ss$`1997_2002`, stage = 1),
                                   "2007" = summary(allbases_prodval_ss$`1997_2007`, stage = 1),
                                   "2012" = summary(allbases_prodval_ss$`1997_2012`, stage = 1)),
                            "Starting 2002" =
                              list("2007" = summary(allbases_prodval_ss$`2002_2007`, stage = 1),
                                   "2012" = summary(allbases_prodval_ss$`2002_2012`, stage = 1)),
                            "Starting 2007" =
                              list("2012" = summary(allbases_prodval_ss$`2007_2012`, stage = 1)))

modelsummary(allbases_ordered_fs,
             shape = "rbind",
             stars = TRUE,
             coef_rename = function(x) {
               ifelse(grepl("^wins_shiftshare_", x), "Shift-Share Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{allbases_1s}County-Level Shift-Share, First Stage",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/firststage_countylevel_allbases.tex", overwrite = TRUE)

#############################
### Making more graphs
#############################

modelplot(models = allbases_prodval_ordered$`Starting 1978`,
          coef_map = c("wins_growth_2012_1978" = "2012",
                       "wins_growth_2007_1978" = "2007",
                       "wins_growth_2002_1978" = "2002",
                       "wins_growth_1997_1978" = "1997",
                       "wins_growth_1992_1978" = "1992",
                       "wins_growth_1987_1978" = "1987",
                       "wins_growth_1982_1978" = "1982")) +
  labs(x = "") +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none") + geom_vline(xintercept = 0)
ggsave("Output/figures/prodval_countylevel.png", width = 6, height = 4)

modelplot(models = allbases_cropland_ordered$`Starting 1978`,
          coef_map = c("wins_growth_2012_1978" = "2012",
                       "wins_growth_2007_1978" = "2007",
                       "wins_growth_2002_1978" = "2002",
                       "wins_growth_1997_1978" = "1997",
                       "wins_growth_1992_1978" = "1992",
                       "wins_growth_1987_1978" = "1987",
                       "wins_growth_1982_1978" = "1982")) +
  labs(x = "") +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none") + geom_vline(xintercept = 0)
ggsave("Output/figures/cropland_countylevel.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_prodval_ordered$`Starting 1978`,
                          "Shift-Share" = allbases_prodval_ordered_ss$`Starting 1978`),
               coef_map = c("wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982",
                            "fit_wins_growth_2012_1978" = "2012",
                            "fit_wins_growth_2007_1978" = "2007",
                            "fit_wins_growth_2002_1978" = "2002",
                            "fit_wins_growth_1997_1978" = "1997",
                            "fit_wins_growth_1992_1978" = "1992",
                            "fit_wins_growth_1987_1978" = "1987",
                            "fit_wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_prodval.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_cropland_ordered$`Starting 1978`,
                          "Shift-Share" = allbases_cropland_ordered_ss$`Starting 1978`),
               coef_map = c("wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982",
                            "fit_wins_growth_2012_1978" = "2012",
                            "fit_wins_growth_2007_1978" = "2007",
                            "fit_wins_growth_2002_1978" = "2002",
                            "fit_wins_growth_1997_1978" = "1997",
                            "fit_wins_growth_1992_1978" = "1992",
                            "fit_wins_growth_1987_1978" = "1987",
                            "fit_wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_cropland.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_cropvalratio_ordered$`Starting 1978`,
                          "Shift-Share" = allbases_cropvalratio_ordered_ss$`Starting 1978`),
               coef_map = c("wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982",
                            "fit_wins_growth_2012_1978" = "2012",
                            "fit_wins_growth_2007_1978" = "2007",
                            "fit_wins_growth_2002_1978" = "2002",
                            "fit_wins_growth_1997_1978" = "1997",
                            "fit_wins_growth_1992_1978" = "1992",
                            "fit_wins_growth_1987_1978" = "1987",
                            "fit_wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_cropvalratio.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_machineryper_ordered$`Starting 1978`,
                          "Shift-Share" = allbases_machineryper_ordered_ss$`Starting 1978`),
               coef_map = c("wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982",
                            "fit_wins_growth_2012_1978" = "2012",
                            "fit_wins_growth_2007_1978" = "2007",
                            "fit_wins_growth_2002_1978" = "2002",
                            "fit_wins_growth_1997_1978" = "1997",
                            "fit_wins_growth_1992_1978" = "1992",
                            "fit_wins_growth_1987_1978" = "1987",
                            "fit_wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_machineryper.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_profitmargin_ordered$`Starting 1987`,
                          "Shift-Share" = allbases_profitmargin_ordered_ss$`Starting 1987`),
               coef_map = c("wins_growth_2012_1987" = "2012",
                            "wins_growth_2007_1987" = "2007",
                            "wins_growth_2002_1987" = "2002",
                            "wins_growth_1997_1987" = "1997",
                            "wins_growth_1992_1987" = "1992",
                            "wins_growth_1987_1987" = "1987",
                            "wins_growth_1982_1987" = "1982",
                            "fit_wins_growth_2012_1987" = "2012",
                            "fit_wins_growth_2007_1987" = "2007",
                            "fit_wins_growth_2002_1987" = "2002",
                            "fit_wins_growth_1997_1987" = "1997",
                            "fit_wins_growth_1992_1987" = "1992",
                            "fit_wins_growth_1987_1987" = "1987",
                            "fit_wins_growth_1982_1987" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_profitmargin.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_fertilizerper_ordered$`Starting 1978`,
                          "Shift-Share" = allbases_fertilizerper_ordered_ss$`Starting 1978`),
               coef_map = c("wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982",
                            "fit_wins_growth_2012_1978" = "2012",
                            "fit_wins_growth_2007_1978" = "2007",
                            "fit_wins_growth_2002_1978" = "2002",
                            "fit_wins_growth_1997_1978" = "1997",
                            "fit_wins_growth_1992_1978" = "1992",
                            "fit_wins_growth_1987_1978" = "1987",
                            "fit_wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_fertilizerper.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_farms_ordered$`Starting 1978`,
                          "Shift-Share" = allbases_farms_ordered_ss$`Starting 1978`),
               coef_map = c("wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982",
                            "fit_wins_growth_2012_1978" = "2012",
                            "fit_wins_growth_2007_1978" = "2007",
                            "fit_wins_growth_2002_1978" = "2002",
                            "fit_wins_growth_1997_1978" = "1997",
                            "fit_wins_growth_1992_1978" = "1992",
                            "fit_wins_growth_1987_1978" = "1987",
                            "fit_wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_farms.png", width = 6, height = 4)

p <- modelplot(models = c("Normal" = allbases_netreturn_ordered$`Starting 1987`,
                          "Shift-Share" = allbases_netreturn_ordered_ss$`Starting 1987`),
               coef_map = c("wins_growth_2012_1987" = "2012",
                            "wins_growth_2007_1987" = "2007",
                            "wins_growth_2002_1987" = "2002",
                            "wins_growth_1997_1987" = "1997",
                            "wins_growth_1992_1987" = "1992",
                            "wins_growth_1987_1987" = "1987",
                            "wins_growth_1982_1987" = "1982",
                            "fit_wins_growth_2012_1987" = "2012",
                            "fit_wins_growth_2007_1987" = "2007",
                            "fit_wins_growth_2002_1987" = "2002",
                            "fit_wins_growth_1997_1987" = "1997",
                            "fit_wins_growth_1992_1987" = "1992",
                            "fit_wins_growth_1987_1987" = "1987",
                            "fit_wins_growth_1982_1987" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Normal", "Shift-Share"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/shiftshare_comp_netreturn.png", width = 6, height = 4)


p <- modelplot(models = c("Main" = allbases_prodval_ordered$`Starting 1978`,
                          "Simplified" = allbases_prodval_ordered_simple$`Starting 1978`,
                          "Unweighted" = allbases_prodval_nowt_ordered$`Starting 1978`,
                          "Normalized/Winsorized" = allbases_prodval_ordered_norm$`Starting 1978`),
               coef_map = c("norm_growth_2012_1978" = "2012",
                            "norm_growth_2007_1978" = "2007",
                            "norm_growth_2002_1978" = "2002",
                            "norm_growth_1997_1978" = "1997",
                            "norm_growth_1992_1978" = "1992",
                            "norm_growth_1987_1978" = "1987",
                            "norm_growth_1982_1978" = "1982",
                            "growth_2012_1978" = "2012",
                            "growth_2007_1978" = "2007",
                            "growth_2002_1978" = "2002",
                            "growth_1997_1978" = "1997",
                            "growth_1992_1978" = "1992",
                            "growth_1987_1978" = "1987",
                            "growth_1982_1978" = "1982",
                            "wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Main", "Simplified", "Unweighted", 
                                                  "Normalized/Winsorized"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/prodval_robustness.png", width = 6, height = 4)

models <- c(allbases_prodval_ordered$`Starting 1978`,
            allbases_prodval_ordered$`Starting 1982`,
            allbases_prodval_ordered$`Starting 1987`,
            allbases_prodval_ordered$`Starting 1992`,
            allbases_prodval_ordered$`Starting 1997`,
            allbases_prodval_ordered$`Starting 2002`,
            allbases_prodval_ordered$`Starting 2007`)

names(models) <- sapply(models, function(m) {
  cf <- grep("^wins_growth_", names(coef(m)), value = TRUE)[1]
  paste("Starting", sub(".*_(\\d{4})$", "\\1", cf))
})
p <- modelplot(models = models,
               coef_omit = "^(?!wins_growth_)",
               coef_rename = function(x) sub(".*_(\\d{4})_\\d{4}$", "\\1", x)) +
  labs(x = "",
       color = "Starting Year") +
  theme(text = element_text(family = "Times New Roman")) +
  geom_vline(xintercept = 0)
p$data$model <- str_extract(p$data$model, paste(c("1978", "1982", "1987", "1992",
                                                  "1997", "2002", "2007"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev)
ggsave("Output/Figures/prodval_allbases.png", width = 6, height = 4)

p <- modelplot(models = c("Cropland" = allbases_cropland_ordered$`Starting 1978`,
                          "Farmland" = allbases_farmland_ordered$`Starting 1978`,
                          "Irrigated Land" = allbases_irrland_ordered$`Starting 1978`,
                          "Non-Irrigated Land" = allbases_nonirrland_ordered$`Starting 1978`),
               coef_map = c("wins_growth_2012_1978" = "2012",
                            "wins_growth_2007_1978" = "2007",
                            "wins_growth_2002_1978" = "2002",
                            "wins_growth_1997_1978" = "1997",
                            "wins_growth_1992_1978" = "1992",
                            "wins_growth_1987_1978" = "1987",
                            "wins_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Cropland", "Farmland", "Irrigated Land", 
                                                  "Non-Irrigated Land"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/cropland_vars.png", width = 6, height = 4)

################################
### Census data
################################

allbases_censusemp <- list()
for (i in c(1980, 1990, 2000, 2010)) {
  for (j in c(1990, 2000, 2010, 2015)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_censusempgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, " + wins_", i, " + wins_censusemp_", i,
                                      " + wins_censusemp_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_censusemp[[paste0(i, "_", j)]] <- run
  }
}

allbases_censusemp_ordered <- list("Starting 1980" = 
                           list("1990" = allbases_censusemp$`1980_1990`,
                                "2000" = allbases_censusemp$`1980_2000`,
                                "2010" = allbases_censusemp$`1980_2010`,
                                "2015" = allbases_censusemp$`1980_2015`),
                         "Starting 1990" = 
                           list("2000" = allbases_censusemp$`1990_2000`,
                                "2010" = allbases_censusemp$`1990_2010`,
                                "2015" = allbases_censusemp$`1990_2015`),
                         "Starting 2000" = 
                           list("2010" = allbases_censusemp$`2000_2010`,
                                "2015" = allbases_censusemp$`2000_2015`),
                         "Starting 2010" = 
                           list("2015" = allbases_censusemp$`2010_2015`))

modelsummary(allbases_censusemp_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^wins_growth_", x), "Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{censusemp_allbases}Winsorized Change in Employed Population in Agriculture and Related Industries, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/censusemp_countylevel_allbases.tex", overwrite = TRUE)

allbases_censusemp_ss <- list()
for (i in c(1980, 1990, 2000, 2010)) {
  for (j in c(1990, 2000, 2010, 2015)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_censusempgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | wins_growth_", j, "_", i, 
                                      " ~ wins_shiftshare_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_censusemp_ss[[paste0(i, "_", j)]] <- run
  }
}

allbases_censusemp_ordered_ss <- list("Starting 1980" = 
                                     list("1990" = allbases_censusemp_ss$`1980_1990`,
                                          "2000" = allbases_censusemp_ss$`1980_2000`,
                                          "2010" = allbases_censusemp_ss$`1980_2010`,
                                          "2015" = allbases_censusemp_ss$`1980_2015`),
                                   "Starting 1990" = 
                                     list("2000" = allbases_censusemp_ss$`1990_2000`,
                                          "2010" = allbases_censusemp_ss$`1990_2010`,
                                          "2015" = allbases_censusemp_ss$`1990_2015`),
                                   "Starting 2000" = 
                                     list("2010" = allbases_censusemp_ss$`2000_2010`,
                                          "2015" = allbases_censusemp_ss$`2000_2015`),
                                   "Starting 2010" = 
                                     list("2015" = allbases_censusemp_ss$`2010_2015`))

modelsummary(allbases_censusemp_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_wins_growth_", x), "Fitted Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{censusemp_allbases_ss}Winsorized Change in Employed Population in Agriculture and Related Industries, County Level Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/shiftshare_censusemp_countylevel_allbases.tex", overwrite = TRUE)

###########################
### Putting in the Rotemberg weights
###########################

modshares_cc <- unique(subset(cbp_mod_shares, year == 1978)$countycode)
master_rot <- countylevel_all |>
  select(growth_2012_1978, prodvalgrowth_2012_1978, countycode) |>
  filter(countycode %in% modshares_cc & !is.na(growth_2012_1978) & !is.na(prodvalgrowth_2012_1978)) |>
  arrange(countycode)
master_cc <- unique(master_rot$countycode)

shares_rot <- cbp_mod_shares |>
  ungroup() |>
  filter(year == 1978) |>
  select(countycode, naics4, emppct) |>
  filter(naics4 %in% commonnaics & countycode %in% master_cc) |>
  complete(countycode = master_cc, naics4 = commonnaics,
           fill = list(emppct = 0)) |>
  pivot_wider(values_from = emppct, names_from = naics4) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  arrange(countycode)
Z_rot <- colnames(select(shares_rot, -c(countycode)))
shifts_rot <- cbp_mod_shifts |>
  ungroup() |>
  filter(year == 1978 | year == 2012) |>
  complete(naics4, year, fill = list(emp = 0)) |>
  group_by(naics4) |>
  summarize(growth = if_else(emp[year == 1978] == 0, NA,
                             (emp[year == 2012]-emp[year == 1978])/emp[year == 1978])) |>
  mutate(growth = winsorize(growth)) |>
  filter(naics4 %in% commonnaics & naics4 %in% Z_rot & !is.na(growth))
Z_rot <- shifts_rot$naics4
shares_rot <- shares_rot |> select(countycode, all_of(Z_rot))

rotweights <- bw(master_rot, "prodvalgrowth_2012_1978",
                 "growth_2012_1978", controls = NULL, weight = NULL, shares_rot, Z_rot, shifts_rot,
                 "growth") |>
  select(naics4, alpha) |>
  arrange(desc(alpha)) |>
  head(5) |>
  mutate(Industry = case_when(naics4 == 7225 ~ "Restaurants and Other Eating Places",
                              naics4 == 3152 ~ "Cut and Sew Apparel Manufacturing",
                              naics4 == 7211 ~ "Traveler Accomodation",
                              naics4 == 7139 ~ "Other Amusement and Recreation",
                              naics4 == 4251 ~ "Electronic Markets, Agents, and Brokers",
                              naics4 == 5613 ~ "Employment Services",
                              naics4 == 3132 ~ "Fabric Mills",
                              naics4 == 6241 ~ "Individual and Family Services",
                              naics4 == 3162 ~ "Footwear Manufacturing",
                              naics4 == 6216 ~ "Home Health Care Services",
                              naics4 == 6213 ~ "Offices of Other Health Practitioners")) |> 
  select("NAICS Code" = naics4, Industry, "Rotemberg weight" = alpha) |>
  tt(caption = "\\label{rotweights}Rotemberg Weights for Shift-Shares")

shares_rot_tradable <- cbp_mod_shares |>
  ungroup() |>
  filter(year == 1978) |>
  mutate(n1 = str_sub(naics4, 1, 1)) |>
  filter(n1 == 3) |>
  select(countycode, naics4, emppct) |>
  filter(naics4 %in% commonnaics & countycode %in% master_cc) |>
  complete(countycode = master_cc, naics4 = commonnaics,
           fill = list(emppct = 0)) |>
  pivot_wider(values_from = emppct, names_from = naics4) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  arrange(countycode)
Z_rot_tradable <- colnames(select(shares_rot_tradable, -c(countycode)))
shifts_rot_tradable <- cbp_mod_shifts |>
  ungroup() |>
  mutate(n1 = str_sub(naics4, 1, 1)) |>
  filter(n1 == 3) |>
  filter(year == 1978 | year == 2012) |>
  complete(naics4, year, fill = list(emp = 0)) |>
  group_by(naics4) |>
  summarize(growth = if_else(emp[year == 1978] == 0, NA,
                             (emp[year == 2012]-emp[year == 1978])/emp[year == 1978])) |>
  mutate(growth = winsorize(growth)) |>
  filter(naics4 %in% commonnaics & naics4 %in% Z_rot & !is.na(growth))
Z_rot_tradable <- shifts_rot_tradable$naics4
shares_rot_tradable <- shares_rot_tradable |> select(countycode, all_of(Z_rot_tradable))

rotweights_tradable <- bw(master_rot, "prodvalgrowth_2012_1978",
                 "growth_2012_1978", controls = NULL, weight = NULL, 
                 shares_rot_tradable, Z_rot_tradable, shifts_rot_tradable,
                 "growth") |>
  select(naics4, alpha) |>
  arrange(desc(alpha)) |>
  head(5) |>
  mutate(Industry = case_when(naics4 == 7225 ~ "Restaurants and Other Eating Places",
                              naics4 == 3152 ~ "Cut and Sew Apparel Manufacturing",
                              naics4 == 7211 ~ "Traveler Accomodation",
                              naics4 == 7139 ~ "Other Amusement and Recreation",
                              naics4 == 4251 ~ "Electronic Markets, Agents, and Brokers",
                              naics4 == 5613 ~ "Employment Services",
                              naics4 == 3132 ~ "Fabric Mills",
                              naics4 == 6241 ~ "Individual and Family Services",
                              naics4 == 3162 ~ "Footwear Manufacturing",
                              naics4 == 6216 ~ "Home Health Care Services",
                              naics4 == 3363 ~ "Motor Vehicle Parts Manufacturing",
                              naics4 == 3131 ~ "Fiber, Yarn, and Thread Mills",
                              naics4 == 6213 ~ "Offices of Other Health Practitioners")) |> 
  select("NAICS Code" = naics4, Industry, "Rotemberg weight" = alpha) |>
  tt()


shares_rot_naics2 <- cbp_mod_shares_naics2 |>
  ungroup() |>
  filter(year == 1978) |>
  select(countycode, naics2, emppct) |>
  filter(naics2 %in% commonnaics2 & countycode %in% master_cc) |>
  complete(countycode = master_cc, naics2 = commonnaics2,
           fill = list(emppct = 0)) |>
  pivot_wider(values_from = emppct, names_from = naics2) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  arrange(countycode)
Z_rot_naics2 <- colnames(select(shares_rot_naics2, -c(countycode)))
shifts_rot_naics2 <- cbp_mod_shifts_naics2 |>
  ungroup() |>
  filter(year == 1978 | year == 2012) |>
  complete(naics2, year, fill = list(emp = 0)) |>
  group_by(naics2) |>
  summarize(growth = if_else(emp[year == 1978] == 0, NA,
                             (emp[year == 2012]-emp[year == 1978])/emp[year == 1978])) |>
  filter(naics2 %in% commonnaics2 & naics2 %in% Z_rot_naics2 & !is.na(growth))
Z_rot_naics2 <- shifts_rot_naics2$naics2
shares_rot_naics2 <- shares_rot_naics2 |> select(countycode, all_of(Z_rot_naics2))

rotweights_naics2 <- bw(master_rot, "prodvalgrowth_2012_1978",
                          "growth_2012_1978", controls = NULL, weight = NULL, 
                          shares_rot_naics2, Z_rot_naics2, shifts_rot_naics2,
                          "growth") |>
  select(naics2, alpha) |>
  arrange(desc(alpha)) |>
  head(5) |>
  mutate(Industry = case_when(naics2 == 72 ~ "Accommodation and Food Services",
                              naics2 == 31 ~ "Food and Textile Manufacturing",
                              naics2 == 56 ~ "Administrative Support and Waste Removal",
                              naics2 == 71 ~ "Arts, Entertainment, and Recreation",
                              naics2 == 33 ~ "Other Manufacturing",
                              .default = NA)) |> 
  select("NAICS Code" = naics2, Industry, "Rotemberg weight" = alpha) |>
  tt()

rbind2(rotweights, rotweights_tradable, use_names = TRUE, headers = FALSE) |>
  rbind2(rotweights_naics2, use_names = TRUE, headers = FALSE) |>
  group_tt(i = list("Panel A: Main Shift-Share" = 1,
                    "Panel B: Manufacturing Only" = 6,
                    "Panel C: 2-Digit NAICS Codes" = 11)) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/rotweights.tex", overwrite = TRUE)

## Shock diagnostics
shockdiag <- list()
for (i in c(1992, 1997, 2002, 2007)) {
  for (j in c(1997, 2002, 2007, 2012)) {
    if (i >= j) {
      print("NA")
    } else {
      run <- eval(parse(text = paste0(
        "feols(wins_growth_", j, "_", i,
        " ~ wins_unemprategrowth_", j, "_", i, " + wins_totpopgrowth_",
        j, "_", i, " | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
      )))
      shockdiag[[paste0(i, "_", j)]] <- run
    }
  }
}

shockdiag_ordered <- list(
  "Starting 1992" = list("1997" = shockdiag$`1992_1997`,
                         "2002" = shockdiag$`1992_2002`,
                         "2007" = shockdiag$`1992_2007`,
                         "2012" = shockdiag$`1992_2012`),
  "Starting 1997" = list("2002" = shockdiag$`1997_2002`,
                         "2007" = shockdiag$`1997_2007`,
                         "2012" = shockdiag$`1997_2012`),
  "Starting 2002" = list("2007" = shockdiag$`2002_2007`,
                         "2012" = shockdiag$`2002_2012`),
  "Starting 2007" = list("2012" = shockdiag$`2007_2012`))

modelsummary(shockdiag_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_rename = function(x) {
               ifelse(grepl("^wins_unemprategrowth_", x), "Unemployment Rate Growth", 
                      "Population Growth")
             },
             gof_map = gm_small,
             title = paste0("\\label{shockdiag}Winsorized Change in Employed Population Growth by Unemployment and Population Growth"),
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt(paste0("Output/Tables/shockdiag.tex"), overwrite = TRUE)

## And for the shift-share
shockdiag_ss <- list()
for (i in c(1992, 1997, 2002, 2007)) {
  for (j in c(1997, 2002, 2007, 2012)) {
    if (i >= j) {
      print("NA")
    } else {
      run <- eval(parse(text = paste0(
        "feols(wins_shiftshare_", j, "_", i,
        " ~ wins_unemprategrowth_", j, "_", i, " + wins_totpopgrowth_",
        j, "_", i, " | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
      )))
      shockdiag_ss[[paste0(i, "_", j)]] <- run
    }
  }
}

shockdiag_ss_ordered <- list(
  "Starting 1992" = list("1997" = shockdiag_ss$`1992_1997`,
                         "2002" = shockdiag_ss$`1992_2002`,
                         "2007" = shockdiag_ss$`1992_2007`,
                         "2012" = shockdiag_ss$`1992_2012`),
  "Starting 1997" = list("2002" = shockdiag_ss$`1997_2002`,
                         "2007" = shockdiag_ss$`1997_2007`,
                         "2012" = shockdiag_ss$`1997_2012`),
  "Starting 2002" = list("2007" = shockdiag_ss$`2002_2007`,
                         "2012" = shockdiag_ss$`2002_2012`),
  "Starting 2007" = list("2012" = shockdiag_ss$`2007_2012`))

modelsummary(shockdiag_ss_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_rename = function(x) {
               ifelse(grepl("^wins_unemprategrowth_", x), "Unemployment Rate Growth", 
                      "Population Growth")
             },
             gof_map = gm_small,
             title = paste0("\\label{shockdiag_ss}Winsorized Change in Shift-Share Employed Population Growth by Unemployment and Population Growth"),
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt(paste0("Output/Tables/shockdiag_shiftshare.tex"), overwrite = TRUE)


###############################
### Building tables for ownership types
###############################

ownership_ols <- list("Sole Proprietorship" = allbases_wins_family_ordered$`Starting 1978`,
                      "Partnership" = allbases_wins_partnership_ordered$`Starting 1978`,
                      "Family Corp." = allbases_wins_familycorp_ordered$`Starting 1978`,
                      "Non-Family Corp." = allbases_wins_corp_ordered$`Starting 1978`,
                      "Other" = allbases_wins_otherorg_ordered$`Starting 1978`)

modelsummary(ownership_ols,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^wins_growth_", x), "Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{ownership_ols}Winsorized Change in Acres by Organizational Structure, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/ownership_ols.tex", overwrite = TRUE) 

ownership_ss <- list("Sole Proprietorship" = allbases_family_ordered_ss$`Starting 1978`,
                      "Partnership" = allbases_partnership_ordered_ss$`Starting 1978`,
                      "Family Corp." = allbases_familycorp_ordered_ss$`Starting 1978`,
                      "Non-Family Corp." = allbases_corp_ordered_ss$`Starting 1978`,
                      "Other" = allbases_otherorg_ordered_ss$`Starting 1978`)

modelsummary(ownership_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_wins_growth_", x), "Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{ownership_ss}Winsorized Change in Acres by Organizational Structure, County Level Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/ownership_ss.tex", overwrite = TRUE) 

####################################
### Doing robustness checks to omitting various regions
####################################

prodval_noncorn <- feols(wins_prodvalgrowth_2012_1978 ~ 
                                                   wins_growth_2012_1978 + wins_1978 +
                                                   wins_prodval_1978^2 + wins_1978^2 |
                                                   statefip,
                                                 data = subset(countylevel_all, !(countycode %in% corncounties)),
                                                 vcov_conley(cutoff = 100),
                                                 weights =~ weight_1978)
prodval_novirginia <- feols(wins_prodvalgrowth_2012_1978 ~ 
                              wins_growth_2012_1978 + wins_1978 +
                              wins_prodval_1978^2 + wins_1978^2 |
                              statefip,
                            data = subset(countylevel_all, statefip != 51),
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
prodval_nosmall <- feols(wins_prodvalgrowth_2012_1978 ~ 
                           wins_growth_2012_1978 + wins_1978 +
                           wins_prodval_1978^2 + wins_1978^2 |
                           statefip,
                         data = subset(countylevel_all, cropland_1978 > 5000),
                         vcov_conley(cutoff = 100),
                         weights =~ weight_1978)

modelsummary(models = list("Main" = allbases_wins_prodval_ordered$`Starting 1978`$`2012`,
                           "Minus Virginia" = prodval_novirginia,
                           "Counties with <1000 Acres Corn, 1978" = prodval_noncorn,
                           "Counties with >5000 Acres Cropland, 1978" = prodval_nosmall),
             stars = TRUE, 
             fmt = 5,
             coef_map = c("wins_growth_2012_1978" = "Employment Growth",
                          "wins_growth_2012_1978:wins_prodval_1978" = "Growth x 1978 Production Value",
                          "wins_growth_2012_1978:wins_1978" = "Growth x 1978 Employed Pop.",
                          "wins_growth_2012_1978:wins_1978:wins_prodval_1978" = "Growth x 1978 Production Value x 1978 Employed Pop."),
             gof_function = check_controls(),
             gof_map = gm,
             title = "\\label{prodval_robust}Winsorized Change in Agricultural Production Value, County Level, 1978 to 2012",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  save_tt("Output/Tables/prodval_miscrobustness.tex", overwrite = TRUE)