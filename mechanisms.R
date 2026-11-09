####################
### Interacting with the people employed per capita in 1978
####################

allbases_percap <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(pctl_croplandgrowth_", j, "_", i, 
                                      " ~ pctl_growth_", j, "_", i, "*emppercap_", i, 
                                      " + `", i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_percap[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_percap <- list("Starting 1978" = 
                                  list("1982" = allbases_percap$`1978_1982`,
                                       "1987" = allbases_percap$`1978_1987`,
                                       "1992" = allbases_percap$`1978_1992`,
                                       "1997" = allbases_percap$`1978_1997`,
                                       "2002" = allbases_percap$`1978_2002`,
                                       "2007" = allbases_percap$`1978_2007`,
                                       "2012" = allbases_percap$`1978_2012`),
                                "Starting 1982" =
                                  list("1987" = allbases_percap$`1982_1987`,
                                       "1992" = allbases_percap$`1982_1992`,
                                       "1997" = allbases_percap$`1982_1997`,
                                       "2002" = allbases_percap$`1982_2002`,
                                       "2007" = allbases_percap$`1982_2007`,
                                       "2012" = allbases_percap$`1982_2012`),
                                "Starting 1987" =
                                  list("1992" = allbases_percap$`1987_1992`,
                                       "1997" = allbases_percap$`1987_1997`,
                                       "2002" = allbases_percap$`1987_2002`,
                                       "2007" = allbases_percap$`1987_2007`,
                                       "2012" = allbases_percap$`1987_2012`),
                                "Starting 1992" =
                                  list("1997" = allbases_percap$`1992_1997`,
                                       "2002" = allbases_percap$`1992_2002`,
                                       "2007" = allbases_percap$`1992_2007`,
                                       "2012" = allbases_percap$`1992_2012`),
                                "Starting 1997" =
                                  list("2002" = allbases_percap$`1997_2002`,
                                       "2007" = allbases_percap$`1997_2007`,
                                       "2012" = allbases_percap$`1997_2012`),
                                "Starting 2002" =
                                  list("2007" = allbases_percap$`2002_2007`,
                                       "2012" = allbases_percap$`2002_2012`),
                                "Starting 2007" =
                                  list("2012" = allbases_percap$`2007_2012`))

modelsummary(allbases_ordered_percap,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("pctl_growth_", x) & grepl("emppercap", x),
                      "Pctl. Growth x Employed Pop./Square Meter",
                      ifelse(grepl("pctl_growth_", x),
                             "Pctl. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_percap}Percentile Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "I also control for the employed population per square meter. 
             County size comes from IPUMS NHGIS data.
             Employed population is always measured as of the starting year to avoid exogeneity concerns.")|>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_percap.tex", overwrite = TRUE)

## And doing the same for production value

allbases_prodval_percap <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                      " ~ pctl_growth_", j, "_", i, "*emppercap_", i, 
                                      " + `", i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_percap[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_ordered_percap <- list("Starting 1978" = 
                                  list("1982" = allbases_prodval_percap$`1978_1982`,
                                       "1987" = allbases_prodval_percap$`1978_1987`,
                                       "1992" = allbases_prodval_percap$`1978_1992`,
                                       "1997" = allbases_prodval_percap$`1978_1997`,
                                       "2002" = allbases_prodval_percap$`1978_2002`,
                                       "2007" = allbases_prodval_percap$`1978_2007`,
                                       "2012" = allbases_prodval_percap$`1978_2012`),
                                "Starting 1982" =
                                  list("1987" = allbases_prodval_percap$`1982_1987`,
                                       "1992" = allbases_prodval_percap$`1982_1992`,
                                       "1997" = allbases_prodval_percap$`1982_1997`,
                                       "2002" = allbases_prodval_percap$`1982_2002`,
                                       "2007" = allbases_prodval_percap$`1982_2007`,
                                       "2012" = allbases_prodval_percap$`1982_2012`),
                                "Starting 1987" =
                                  list("1992" = allbases_prodval_percap$`1987_1992`,
                                       "1997" = allbases_prodval_percap$`1987_1997`,
                                       "2002" = allbases_prodval_percap$`1987_2002`,
                                       "2007" = allbases_prodval_percap$`1987_2007`,
                                       "2012" = allbases_prodval_percap$`1987_2012`),
                                "Starting 1992" =
                                  list("1997" = allbases_prodval_percap$`1992_1997`,
                                       "2002" = allbases_prodval_percap$`1992_2002`,
                                       "2007" = allbases_prodval_percap$`1992_2007`,
                                       "2012" = allbases_prodval_percap$`1992_2012`),
                                "Starting 1997" =
                                  list("2002" = allbases_prodval_percap$`1997_2002`,
                                       "2007" = allbases_prodval_percap$`1997_2007`,
                                       "2012" = allbases_prodval_percap$`1997_2012`),
                                "Starting 2002" =
                                  list("2007" = allbases_prodval_percap$`2002_2007`,
                                       "2012" = allbases_prodval_percap$`2002_2012`),
                                "Starting 2007" =
                                  list("2012" = allbases_prodval_percap$`2007_2012`))

modelsummary(allbases_prodval_ordered_percap,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("pctl_growth_", x) & grepl("emppercap", x),
                      "Pctl. Growth x Employed Pop./Square Meter",
                      ifelse(grepl("pctl_growth_", x),
                             "Pctl. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_percap}Percentile Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "I also control for the employed population per square meter. 
             County size comes from IPUMS NHGIS data.
             Employed population is always measured as of the starting year to avoid exogeneity concerns.")|>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_percap.tex", overwrite = TRUE)


## Interacting with the amount of drought in the time frame
allbases_drought <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_croplandgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, "*wins_drought_", i,
                                      "_", j, " + wins_", i, " + wins_cropland_", i,
                                      " + wins_cropland_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_drought[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_drought <- list("Starting 1978" = 
                                   list("1982" = allbases_drought$`1978_1982`,
                                        "1987" = allbases_drought$`1978_1987`,
                                        "1992" = allbases_drought$`1978_1992`,
                                        "1997" = allbases_drought$`1978_1997`,
                                        "2002" = allbases_drought$`1978_2002`,
                                        "2007" = allbases_drought$`1978_2007`,
                                        "2012" = allbases_drought$`1978_2012`),
                                 "Starting 1982" =
                                   list("1987" = allbases_drought$`1982_1987`,
                                        "1992" = allbases_drought$`1982_1992`,
                                        "1997" = allbases_drought$`1982_1997`,
                                        "2002" = allbases_drought$`1982_2002`,
                                        "2007" = allbases_drought$`1982_2007`,
                                        "2012" = allbases_drought$`1982_2012`),
                                 "Starting 1987" =
                                   list("1992" = allbases_drought$`1987_1992`,
                                        "1997" = allbases_drought$`1987_1997`,
                                        "2002" = allbases_drought$`1987_2002`,
                                        "2007" = allbases_drought$`1987_2007`,
                                        "2012" = allbases_drought$`1987_2012`),
                                 "Starting 1992" =
                                   list("1997" = allbases_drought$`1992_1997`,
                                        "2002" = allbases_drought$`1992_2002`,
                                        "2007" = allbases_drought$`1992_2007`,
                                        "2012" = allbases_drought$`1992_2012`),
                                 "Starting 1997" =
                                   list("2002" = allbases_drought$`1997_2002`,
                                        "2007" = allbases_drought$`1997_2007`,
                                        "2012" = allbases_drought$`1997_2012`),
                                 "Starting 2002" =
                                   list("2007" = allbases_drought$`2002_2007`,
                                        "2012" = allbases_drought$`2002_2012`),
                                 "Starting 2007" =
                                   list("2012" = allbases_drought$`2007_2012`))

modelsummary(allbases_ordered_drought,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_drought", x),
                      "Employment Growth x Palmer Drought Index",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_drought}Winsorized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Drought is represented using the Palmer Drought Severity Index, averaged from the starting year to the given year. 
             The drought variable is also included as a control by itself.
             Lower values of the Palmer index indicate more drought. The drought measure is winsorized also.") |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_drought.tex", overwrite = TRUE)

## And interacting with the amount of precipitation
allbases_precip <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_croplandgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, "*wins_precip + wins_", 
                                      i, " + wins_cropland_", i,
                                      " + wins_cropland_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_precip[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_precip <- list("Starting 1978" = 
                                  list("1982" = allbases_precip$`1978_1982`,
                                       "1987" = allbases_precip$`1978_1987`,
                                       "1992" = allbases_precip$`1978_1992`,
                                       "1997" = allbases_precip$`1978_1997`,
                                       "2002" = allbases_precip$`1978_2002`,
                                       "2007" = allbases_precip$`1978_2007`,
                                       "2012" = allbases_precip$`1978_2012`),
                                "Starting 1982" =
                                  list("1987" = allbases_precip$`1982_1987`,
                                       "1992" = allbases_precip$`1982_1992`,
                                       "1997" = allbases_precip$`1982_1997`,
                                       "2002" = allbases_precip$`1982_2002`,
                                       "2007" = allbases_precip$`1982_2007`,
                                       "2012" = allbases_precip$`1982_2012`),
                                "Starting 1987" =
                                  list("1992" = allbases_precip$`1987_1992`,
                                       "1997" = allbases_precip$`1987_1997`,
                                       "2002" = allbases_precip$`1987_2002`,
                                       "2007" = allbases_precip$`1987_2007`,
                                       "2012" = allbases_precip$`1987_2012`),
                                "Starting 1992" =
                                  list("1997" = allbases_precip$`1992_1997`,
                                       "2002" = allbases_precip$`1992_2002`,
                                       "2007" = allbases_precip$`1992_2007`,
                                       "2012" = allbases_precip$`1992_2012`),
                                "Starting 1997" =
                                  list("2002" = allbases_precip$`1997_2002`,
                                       "2007" = allbases_precip$`1997_2007`,
                                       "2012" = allbases_precip$`1997_2012`),
                                "Starting 2002" =
                                  list("2007" = allbases_precip$`2002_2007`,
                                       "2012" = allbases_precip$`2002_2012`),
                                "Starting 2007" =
                                  list("2012" = allbases_precip$`2007_2012`))

modelsummary(allbases_ordered_precip,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!.*wins_growth_)(?!.*wins_precip)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_precip", x),
                      "Employment Growth x Precipitation",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", ifelse(grepl("wins_precip", x),
                                                               "Precipitation", x)))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_precip}Winsorized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Precipitation is the yearly amount of precipitation in a county from 1948 to 1977, averaged over those years. 
             The precipitation variable is also included as a control by itself, and is winsorized.") |>
  theme_tt("resize", width = 0.75) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_precip.tex", overwrite = TRUE)

## And estimating without the interaction
allbases_precip_noint <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(pctl_croplandgrowth_", j, "_", i, 
                                      " ~ pctl_growth_", j, "_", i, " + precip + `", 
                                      i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_precip_noint[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_precip_noint <- list("Starting 1978" = 
                                        list("1982" = allbases_precip_noint$`1978_1982`,
                                             "1987" = allbases_precip_noint$`1978_1987`,
                                             "1992" = allbases_precip_noint$`1978_1992`,
                                             "1997" = allbases_precip_noint$`1978_1997`,
                                             "2002" = allbases_precip_noint$`1978_2002`,
                                             "2007" = allbases_precip_noint$`1978_2007`,
                                             "2012" = allbases_precip_noint$`1978_2012`),
                                      "Starting 1982" =
                                        list("1987" = allbases_precip_noint$`1982_1987`,
                                             "1992" = allbases_precip_noint$`1982_1992`,
                                             "1997" = allbases_precip_noint$`1982_1997`,
                                             "2002" = allbases_precip_noint$`1982_2002`,
                                             "2007" = allbases_precip_noint$`1982_2007`,
                                             "2012" = allbases_precip_noint$`1982_2012`),
                                      "Starting 1987" =
                                        list("1992" = allbases_precip_noint$`1987_1992`,
                                             "1997" = allbases_precip_noint$`1987_1997`,
                                             "2002" = allbases_precip_noint$`1987_2002`,
                                             "2007" = allbases_precip_noint$`1987_2007`,
                                             "2012" = allbases_precip_noint$`1987_2012`),
                                      "Starting 1992" =
                                        list("1997" = allbases_precip_noint$`1992_1997`,
                                             "2002" = allbases_precip_noint$`1992_2002`,
                                             "2007" = allbases_precip_noint$`1992_2007`,
                                             "2012" = allbases_precip_noint$`1992_2012`),
                                      "Starting 1997" =
                                        list("2002" = allbases_precip_noint$`1997_2002`,
                                             "2007" = allbases_precip_noint$`1997_2007`,
                                             "2012" = allbases_precip_noint$`1997_2012`),
                                      "Starting 2002" =
                                        list("2007" = allbases_precip_noint$`2002_2007`,
                                             "2012" = allbases_precip_noint$`2002_2012`),
                                      "Starting 2007" =
                                        list("2012" = allbases_precip_noint$`2007_2012`))

modelsummary(allbases_ordered_precip_noint,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!.*pctl_growth_)(?!.*precip)",
             coef_rename = function(x) {
               ifelse(grepl("pctl_growth_", x) & grepl("precip", x),
                      "Pctl. Employment Growth x Precipitation",
                      ifelse(grepl("pctl_growth_", x),
                             "Pctl. Employment Growth", ifelse(grepl("precip", x),
                                                               "Precipitation", x)))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_precip_noint}Percentile Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Precipitation is the yearly amount of precipitation in a county from 1948 to 1977, averaged over those years. 
             The precipitation variable is included as a control by itself and not interacted.") |>
  theme_tt("resize", width = 0.8) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_precip_noint.tex", overwrite = TRUE)


## Doing the same for production value
allbases_prodval_drought <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, "*wins_drought_", i,
                                      "_", j, " + wins_", i, " + wins_prodval_", i,
                                      " + wins_prodval_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_drought[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_ordered_drought <- list("Starting 1978" = 
                                   list("1982" = allbases_prodval_drought$`1978_1982`,
                                        "1987" = allbases_prodval_drought$`1978_1987`,
                                        "1992" = allbases_prodval_drought$`1978_1992`,
                                        "1997" = allbases_prodval_drought$`1978_1997`,
                                        "2002" = allbases_prodval_drought$`1978_2002`,
                                        "2007" = allbases_prodval_drought$`1978_2007`,
                                        "2012" = allbases_prodval_drought$`1978_2012`),
                                 "Starting 1982" =
                                   list("1987" = allbases_prodval_drought$`1982_1987`,
                                        "1992" = allbases_prodval_drought$`1982_1992`,
                                        "1997" = allbases_prodval_drought$`1982_1997`,
                                        "2002" = allbases_prodval_drought$`1982_2002`,
                                        "2007" = allbases_prodval_drought$`1982_2007`,
                                        "2012" = allbases_prodval_drought$`1982_2012`),
                                 "Starting 1987" =
                                   list("1992" = allbases_prodval_drought$`1987_1992`,
                                        "1997" = allbases_prodval_drought$`1987_1997`,
                                        "2002" = allbases_prodval_drought$`1987_2002`,
                                        "2007" = allbases_prodval_drought$`1987_2007`,
                                        "2012" = allbases_prodval_drought$`1987_2012`),
                                 "Starting 1992" =
                                   list("1997" = allbases_prodval_drought$`1992_1997`,
                                        "2002" = allbases_prodval_drought$`1992_2002`,
                                        "2007" = allbases_prodval_drought$`1992_2007`,
                                        "2012" = allbases_prodval_drought$`1992_2012`),
                                 "Starting 1997" =
                                   list("2002" = allbases_prodval_drought$`1997_2002`,
                                        "2007" = allbases_prodval_drought$`1997_2007`,
                                        "2012" = allbases_prodval_drought$`1997_2012`),
                                 "Starting 2002" =
                                   list("2007" = allbases_prodval_drought$`2002_2007`,
                                        "2012" = allbases_prodval_drought$`2002_2012`),
                                 "Starting 2007" =
                                   list("2012" = allbases_prodval_drought$`2007_2012`))

modelsummary(allbases_prodval_ordered_drought,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_drought", x),
                      "Employment Growth x Palmer Drought Index",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_drought}Winsorized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Drought is represented using the Palmer Drought Severity Index, averaged from the starting year to the given year. 
             The drought variable is also included as a control by itself and is winsorized.
             Lower values of the Palmer index indicate more drought.") |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_drought.tex", overwrite = TRUE)

droughtlist <- list("Agricultural Production Value" = allbases_prodval_ordered_drought$`Starting 1978`,
                    "Total Cropland" = allbases_ordered_drought$`Starting 1978`)

modelsummary(droughtlist,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_drought", x),
                      "Employment Growth x Palmer Drought Index",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{drought}Winsorized Change in Variables, County Level, Starting 1978",
             output = "tinytable",
             escape = FALSE,
             notes = "Drought is represented using the Palmer Drought Severity Index, averaged from the starting year to the given year. 
             The drought variable is also included as a control by itself.
             Lower values of the Palmer index indicate more drought. The drought measure is winsorized also.") |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/droughtsmall.tex", overwrite = TRUE)

## And interacting with the amount of precipitation
allbases_prodval_precip <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, "*wins_precip + wins_", 
                                      i, " + wins_prodval_", i,
                                      " + wins_prodval_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_precip[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_ordered_precip <- list("Starting 1978" = 
                                  list("1982" = allbases_prodval_precip$`1978_1982`,
                                       "1987" = allbases_prodval_precip$`1978_1987`,
                                       "1992" = allbases_prodval_precip$`1978_1992`,
                                       "1997" = allbases_prodval_precip$`1978_1997`,
                                       "2002" = allbases_prodval_precip$`1978_2002`,
                                       "2007" = allbases_prodval_precip$`1978_2007`,
                                       "2012" = allbases_prodval_precip$`1978_2012`),
                                "Starting 1982" =
                                  list("1987" = allbases_prodval_precip$`1982_1987`,
                                       "1992" = allbases_prodval_precip$`1982_1992`,
                                       "1997" = allbases_prodval_precip$`1982_1997`,
                                       "2002" = allbases_prodval_precip$`1982_2002`,
                                       "2007" = allbases_prodval_precip$`1982_2007`,
                                       "2012" = allbases_prodval_precip$`1982_2012`),
                                "Starting 1987" =
                                  list("1992" = allbases_prodval_precip$`1987_1992`,
                                       "1997" = allbases_prodval_precip$`1987_1997`,
                                       "2002" = allbases_prodval_precip$`1987_2002`,
                                       "2007" = allbases_prodval_precip$`1987_2007`,
                                       "2012" = allbases_prodval_precip$`1987_2012`),
                                "Starting 1992" =
                                  list("1997" = allbases_prodval_precip$`1992_1997`,
                                       "2002" = allbases_prodval_precip$`1992_2002`,
                                       "2007" = allbases_prodval_precip$`1992_2007`,
                                       "2012" = allbases_prodval_precip$`1992_2012`),
                                "Starting 1997" =
                                  list("2002" = allbases_prodval_precip$`1997_2002`,
                                       "2007" = allbases_prodval_precip$`1997_2007`,
                                       "2012" = allbases_prodval_precip$`1997_2012`),
                                "Starting 2002" =
                                  list("2007" = allbases_prodval_precip$`2002_2007`,
                                       "2012" = allbases_prodval_precip$`2002_2012`),
                                "Starting 2007" =
                                  list("2012" = allbases_prodval_precip$`2007_2012`))

modelsummary(allbases_prodval_ordered_precip,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!.*wins_growth_)(?!.*wins_precip)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_precip", x),
                      "Employment Growth x Precipitation",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", ifelse(grepl("wins_precip", x),
                                                               "Precipitation", x)))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_precip}Winsorized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Precipitation is the yearly amount of precipitation in a county from 1948 to 1977, averaged over those years. 
             The precipitation variable is also included as a control by itself and is winsorized.") |>
  theme_tt("resize", width = 0.75) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_precip.tex", overwrite = TRUE)


preciplist <- list("Agricultural Production Value" = allbases_prodval_ordered_precip$`Starting 1978`,
                   "Total Cropland" = allbases_ordered_precip$`Starting 1978`)

modelsummary(preciplist,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!.*wins_growth_)(?!.*wins_precip)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_precip", x),
                      "Employment Growth x Precipitation",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", ifelse(grepl("wins_precip", x),
                                                         "Precipitation", x)))
             },
             gof_map = gm_small,
             title = "\\label{precip}Winsorized Change in Variables, County Level, Starting 1978",
             output = "tinytable",
             escape = FALSE,
             notes = "Precipitation is the yearly amount of precipitation in a county from 1948 to 1977, averaged over those years. 
             The precipitation variable is also included as a control by itself and is winsorized.") |>
  theme_tt("resize", width = 0.75) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/precipsmall.tex", overwrite = TRUE)

## And interacting with 1980 unemployment rates
allbases_unemp <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(pctl_croplandgrowth_", j, "_", i, 
                                      " ~ pctl_growth_", j, "_", i, "*unemp_rate_1980 + `", 
                                      i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_unemp[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_unemp <- list("Starting 1978" = 
                                 list("1982" = allbases_unemp$`1978_1982`,
                                      "1987" = allbases_unemp$`1978_1987`,
                                      "1992" = allbases_unemp$`1978_1992`,
                                      "1997" = allbases_unemp$`1978_1997`,
                                      "2002" = allbases_unemp$`1978_2002`,
                                      "2007" = allbases_unemp$`1978_2007`,
                                      "2012" = allbases_unemp$`1978_2012`),
                               "Starting 1982" =
                                 list("1987" = allbases_unemp$`1982_1987`,
                                      "1992" = allbases_unemp$`1982_1992`,
                                      "1997" = allbases_unemp$`1982_1997`,
                                      "2002" = allbases_unemp$`1982_2002`,
                                      "2007" = allbases_unemp$`1982_2007`,
                                      "2012" = allbases_unemp$`1982_2012`),
                               "Starting 1987" =
                                 list("1992" = allbases_unemp$`1987_1992`,
                                      "1997" = allbases_unemp$`1987_1997`,
                                      "2002" = allbases_unemp$`1987_2002`,
                                      "2007" = allbases_unemp$`1987_2007`,
                                      "2012" = allbases_unemp$`1987_2012`),
                               "Starting 1992" =
                                 list("1997" = allbases_unemp$`1992_1997`,
                                      "2002" = allbases_unemp$`1992_2002`,
                                      "2007" = allbases_unemp$`1992_2007`,
                                      "2012" = allbases_unemp$`1992_2012`),
                               "Starting 1997" =
                                 list("2002" = allbases_unemp$`1997_2002`,
                                      "2007" = allbases_unemp$`1997_2007`,
                                      "2012" = allbases_unemp$`1997_2012`),
                               "Starting 2002" =
                                 list("2007" = allbases_unemp$`2002_2007`,
                                      "2012" = allbases_unemp$`2002_2012`),
                               "Starting 2007" =
                                 list("2012" = allbases_unemp$`2007_2012`))

modelsummary(allbases_ordered_unemp,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!.*pctl_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("pctl_growth_", x) & grepl("unemp", x),
                      "Pctl. Employment Growth x 1980 Unemployment Rate",
                      ifelse(grepl("pctl_growth_", x),
                             "Pctl. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_unemp}Percentile Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "The unemployment rate comes from the 1980 census and is also included as a control by itself.") |>
  theme_tt("resize", width = 0.8) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_unemp.tex", overwrite = TRUE)


##################################
### Splitting by whether industries pollute or not
##################################

allbases_pollute <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(pctl_croplandgrowth_", j, "_", i, 
                                      " ~ pctl_pollute_growth_", j, "_", i, " + `", i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_pollute[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_pollute <- list("Starting 1978" = 
                                   list("1982" = allbases_pollute$`1978_1982`,
                                        "1987" = allbases_pollute$`1978_1987`,
                                        "1992" = allbases_pollute$`1978_1992`,
                                        "1997" = allbases_pollute$`1978_1997`,
                                        "2002" = allbases_pollute$`1978_2002`,
                                        "2007" = allbases_pollute$`1978_2007`,
                                        "2012" = allbases_pollute$`1978_2012`),
                                 "Starting 1982" =
                                   list("1987" = allbases_pollute$`1982_1987`,
                                        "1992" = allbases_pollute$`1982_1992`,
                                        "1997" = allbases_pollute$`1982_1997`,
                                        "2002" = allbases_pollute$`1982_2002`,
                                        "2007" = allbases_pollute$`1982_2007`,
                                        "2012" = allbases_pollute$`1982_2012`),
                                 "Starting 1987" =
                                   list("1992" = allbases_pollute$`1987_1992`,
                                        "1997" = allbases_pollute$`1987_1997`,
                                        "2002" = allbases_pollute$`1987_2002`,
                                        "2007" = allbases_pollute$`1987_2007`,
                                        "2012" = allbases_pollute$`1987_2012`),
                                 "Starting 1992" =
                                   list("1997" = allbases_pollute$`1992_1997`,
                                        "2002" = allbases_pollute$`1992_2002`,
                                        "2007" = allbases_pollute$`1992_2007`,
                                        "2012" = allbases_pollute$`1992_2012`),
                                 "Starting 1997" =
                                   list("2002" = allbases_pollute$`1997_2002`,
                                        "2007" = allbases_pollute$`1997_2007`,
                                        "2012" = allbases_pollute$`1997_2012`),
                                 "Starting 2002" =
                                   list("2007" = allbases_pollute$`2002_2007`,
                                        "2012" = allbases_pollute$`2002_2012`),
                                 "Starting 2007" =
                                   list("2012" = allbases_pollute$`2007_2012`))

modelsummary(allbases_ordered_pollute,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_pollute_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^pctl_pollute_growth_", x), "Pctl. Employment Growth, Polluting Industries", x)
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_pollute}Percentile Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases_pollute.tex", overwrite = TRUE)

allbases_nopollute <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(pctl_croplandgrowth_", j, "_", i, 
                                      " ~ pctl_nopollute_growth_", j, "_", i, " + `", i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_nopollute[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_nopollute <- list("Starting 1978" = 
                                     list("1982" = allbases_nopollute$`1978_1982`,
                                          "1987" = allbases_nopollute$`1978_1987`,
                                          "1992" = allbases_nopollute$`1978_1992`,
                                          "1997" = allbases_nopollute$`1978_1997`,
                                          "2002" = allbases_nopollute$`1978_2002`,
                                          "2007" = allbases_nopollute$`1978_2007`,
                                          "2012" = allbases_nopollute$`1978_2012`),
                                   "Starting 1982" =
                                     list("1987" = allbases_nopollute$`1982_1987`,
                                          "1992" = allbases_nopollute$`1982_1992`,
                                          "1997" = allbases_nopollute$`1982_1997`,
                                          "2002" = allbases_nopollute$`1982_2002`,
                                          "2007" = allbases_nopollute$`1982_2007`,
                                          "2012" = allbases_nopollute$`1982_2012`),
                                   "Starting 1987" =
                                     list("1992" = allbases_nopollute$`1987_1992`,
                                          "1997" = allbases_nopollute$`1987_1997`,
                                          "2002" = allbases_nopollute$`1987_2002`,
                                          "2007" = allbases_nopollute$`1987_2007`,
                                          "2012" = allbases_nopollute$`1987_2012`),
                                   "Starting 1992" =
                                     list("1997" = allbases_nopollute$`1992_1997`,
                                          "2002" = allbases_nopollute$`1992_2002`,
                                          "2007" = allbases_nopollute$`1992_2007`,
                                          "2012" = allbases_nopollute$`1992_2012`),
                                   "Starting 1997" =
                                     list("2002" = allbases_nopollute$`1997_2002`,
                                          "2007" = allbases_nopollute$`1997_2007`,
                                          "2012" = allbases_nopollute$`1997_2012`),
                                   "Starting 2002" =
                                     list("2007" = allbases_nopollute$`2002_2007`,
                                          "2012" = allbases_nopollute$`2002_2012`),
                                   "Starting 2007" =
                                     list("2012" = allbases_nopollute$`2007_2012`))

modelsummary(allbases_ordered_nopollute,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_nopollute_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^pctl_nopollute_growth_", x), "Pctl. Employment Growth, Less Polluting Industries", x)
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_nopollute}Percentile Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases_nopollute.tex", overwrite = TRUE)

allbases_bothpollute <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_croplandgrowth_", j, "_", i, 
                                      " ~ wins_nopollute_growth_", j, "_", i, 
                                      " + wins_pollute_growth_", j, "_", i, " + wins_", 
                                      i, " + wins_cropland_", i,
                                      " + wins_cropland_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_bothpollute[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_bothpollute <- list("Starting 1978" = 
                                       list("1982" = allbases_bothpollute$`1978_1982`,
                                            "1987" = allbases_bothpollute$`1978_1987`,
                                            "1992" = allbases_bothpollute$`1978_1992`,
                                            "1997" = allbases_bothpollute$`1978_1997`,
                                            "2002" = allbases_bothpollute$`1978_2002`,
                                            "2007" = allbases_bothpollute$`1978_2007`,
                                            "2012" = allbases_bothpollute$`1978_2012`),
                                     "Starting 1982" =
                                       list("1987" = allbases_bothpollute$`1982_1987`,
                                            "1992" = allbases_bothpollute$`1982_1992`,
                                            "1997" = allbases_bothpollute$`1982_1997`,
                                            "2002" = allbases_bothpollute$`1982_2002`,
                                            "2007" = allbases_bothpollute$`1982_2007`,
                                            "2012" = allbases_bothpollute$`1982_2012`),
                                     "Starting 1987" =
                                       list("1992" = allbases_bothpollute$`1987_1992`,
                                            "1997" = allbases_bothpollute$`1987_1997`,
                                            "2002" = allbases_bothpollute$`1987_2002`,
                                            "2007" = allbases_bothpollute$`1987_2007`,
                                            "2012" = allbases_bothpollute$`1987_2012`),
                                     "Starting 1992" =
                                       list("1997" = allbases_bothpollute$`1992_1997`,
                                            "2002" = allbases_bothpollute$`1992_2002`,
                                            "2007" = allbases_bothpollute$`1992_2007`,
                                            "2012" = allbases_bothpollute$`1992_2012`),
                                     "Starting 1997" =
                                       list("2002" = allbases_bothpollute$`1997_2002`,
                                            "2007" = allbases_bothpollute$`1997_2007`,
                                            "2012" = allbases_bothpollute$`1997_2012`),
                                     "Starting 2002" =
                                       list("2007" = allbases_bothpollute$`2002_2007`,
                                            "2012" = allbases_bothpollute$`2002_2012`),
                                     "Starting 2007" =
                                       list("2012" = allbases_bothpollute$`2007_2012`))

modelsummary(allbases_ordered_bothpollute,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_nopollute_growth|wins_pollute_growth)",
             coef_rename = function(x) {
               x <- ifelse(grepl("wins_nopollute_growth", x), "Employment Growth, Less Polluting Industries", x)
               x <- ifelse(grepl("wins_pollute_growth", x), "Employment Growth, More Polluting Industries", x)
               x
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_bothpollute}Winsorized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases_bothpollute.tex", overwrite = TRUE)

## Doing the same for production value
allbases_prodval_bothpollute <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ wins_nopollute_growth_", j, "_", i, 
                                      " + wins_pollute_growth_", j, "_", i, " + wins_", 
                                      i, " + wins_prodval_", i,
                                      " + wins_prodval_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_bothpollute[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_ordered_bothpollute <- list("Starting 1978" = 
                                       list("1982" = allbases_prodval_bothpollute$`1978_1982`,
                                            "1987" = allbases_prodval_bothpollute$`1978_1987`,
                                            "1992" = allbases_prodval_bothpollute$`1978_1992`,
                                            "1997" = allbases_prodval_bothpollute$`1978_1997`,
                                            "2002" = allbases_prodval_bothpollute$`1978_2002`,
                                            "2007" = allbases_prodval_bothpollute$`1978_2007`,
                                            "2012" = allbases_prodval_bothpollute$`1978_2012`),
                                     "Starting 1982" =
                                       list("1987" = allbases_prodval_bothpollute$`1982_1987`,
                                            "1992" = allbases_prodval_bothpollute$`1982_1992`,
                                            "1997" = allbases_prodval_bothpollute$`1982_1997`,
                                            "2002" = allbases_prodval_bothpollute$`1982_2002`,
                                            "2007" = allbases_prodval_bothpollute$`1982_2007`,
                                            "2012" = allbases_prodval_bothpollute$`1982_2012`),
                                     "Starting 1987" =
                                       list("1992" = allbases_prodval_bothpollute$`1987_1992`,
                                            "1997" = allbases_prodval_bothpollute$`1987_1997`,
                                            "2002" = allbases_prodval_bothpollute$`1987_2002`,
                                            "2007" = allbases_prodval_bothpollute$`1987_2007`,
                                            "2012" = allbases_prodval_bothpollute$`1987_2012`),
                                     "Starting 1992" =
                                       list("1997" = allbases_prodval_bothpollute$`1992_1997`,
                                            "2002" = allbases_prodval_bothpollute$`1992_2002`,
                                            "2007" = allbases_prodval_bothpollute$`1992_2007`,
                                            "2012" = allbases_prodval_bothpollute$`1992_2012`),
                                     "Starting 1997" =
                                       list("2002" = allbases_prodval_bothpollute$`1997_2002`,
                                            "2007" = allbases_prodval_bothpollute$`1997_2007`,
                                            "2012" = allbases_prodval_bothpollute$`1997_2012`),
                                     "Starting 2002" =
                                       list("2007" = allbases_prodval_bothpollute$`2002_2007`,
                                            "2012" = allbases_prodval_bothpollute$`2002_2012`),
                                     "Starting 2007" =
                                       list("2012" = allbases_prodval_bothpollute$`2007_2012`))

modelsummary(allbases_prodval_ordered_bothpollute,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_nopollute_growth|wins_pollute_growth)",
             coef_rename = function(x) {
               x <- ifelse(grepl("wins_nopollute_growth", x), "Employment Growth, Less Polluting Industries", x)
               x <- ifelse(grepl("wins_pollute_growth", x), "Employment Growth, More Polluting Industries", x)
               x
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_bothpollute}Winsorized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_bothpollute.tex", overwrite = TRUE)

dat <- modelplot(allbases_prodval_ordered_bothpollute$`Starting 1978`,
                 draw = FALSE) |>
  filter(grepl("pollute_growth", term)) |>
  mutate(variable = ifelse(grepl("nopollute", term),
                           "Pctl. Non-Polluting Growth",
                           "Pctl. Polluting Growth")) |>
  rename(year = model)

ggplot(dat, aes(x = estimate, y = year, color = variable)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0) + theme_minimal() +
  scale_y_discrete(limits = rev) +
  labs(x = "", y = "", color = "") +
  theme(text = element_text(family = "Times New Roman"))

ggsave("Output/Figures/prodval_bothpollute.png", width = 6, height = 4)

#####################################
### Doing the per capita regression by quintiles in 1978
#####################################

for (q in 1:5) {
  allbases_percap_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_croplandgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + cropland_", i,
                                        " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_emppercap == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_percap_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_ordered_percap_quint <- list(
    "Starting 1978" = list("1982" = allbases_percap_quint$`1978_1982`,
                           "1987" = allbases_percap_quint$`1978_1987`,
                           "1992" = allbases_percap_quint$`1978_1992`,
                           "1997" = allbases_percap_quint$`1978_1997`,
                           "2002" = allbases_percap_quint$`1978_2002`,
                           "2007" = allbases_percap_quint$`1978_2007`,
                           "2012" = allbases_percap_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_percap_quint$`1982_1987`,
                           "1992" = allbases_percap_quint$`1982_1992`,
                           "1997" = allbases_percap_quint$`1982_1997`,
                           "2002" = allbases_percap_quint$`1982_2002`,
                           "2007" = allbases_percap_quint$`1982_2007`,
                           "2012" = allbases_percap_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_percap_quint$`1987_1992`,
                           "1997" = allbases_percap_quint$`1987_1997`,
                           "2002" = allbases_percap_quint$`1987_2002`,
                           "2007" = allbases_percap_quint$`1987_2007`,
                           "2012" = allbases_percap_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_percap_quint$`1992_1997`,
                           "2002" = allbases_percap_quint$`1992_2002`,
                           "2007" = allbases_percap_quint$`1992_2007`,
                           "2012" = allbases_percap_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_percap_quint$`1997_2002`,
                           "2007" = allbases_percap_quint$`1997_2007`,
                           "2012" = allbases_percap_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_percap_quint$`2002_2007`,
                           "2012" = allbases_percap_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_percap_quint$`2007_2012`))
  
  assign(paste0("allbases_ordered_percap_quint",q), allbases_ordered_percap_quint)
  
  modelsummary(allbases_ordered_percap_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{crop_allbases_percap_q", q, 
                              "}Percentile Change in Total Cropland, County Level, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_countylevel_empquint", q, ".tex"), overwrite = TRUE)
}

## And for crop production value
for (q in 1:5) {
  allbases_cropval_percap_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_cropvalgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + cropval_", i,
                                        " + cropval_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_emppercap == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_cropval_percap_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_cropval_ordered_percap_quint <- list(
    "Starting 1978" = list("1982" = allbases_cropval_percap_quint$`1978_1982`,
                           "1987" = allbases_cropval_percap_quint$`1978_1987`,
                           "1992" = allbases_cropval_percap_quint$`1978_1992`,
                           "1997" = allbases_cropval_percap_quint$`1978_1997`,
                           "2002" = allbases_cropval_percap_quint$`1978_2002`,
                           "2007" = allbases_cropval_percap_quint$`1978_2007`,
                           "2012" = allbases_cropval_percap_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_cropval_percap_quint$`1982_1987`,
                           "1992" = allbases_cropval_percap_quint$`1982_1992`,
                           "1997" = allbases_cropval_percap_quint$`1982_1997`,
                           "2002" = allbases_cropval_percap_quint$`1982_2002`,
                           "2007" = allbases_cropval_percap_quint$`1982_2007`,
                           "2012" = allbases_cropval_percap_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_cropval_percap_quint$`1987_1992`,
                           "1997" = allbases_cropval_percap_quint$`1987_1997`,
                           "2002" = allbases_cropval_percap_quint$`1987_2002`,
                           "2007" = allbases_cropval_percap_quint$`1987_2007`,
                           "2012" = allbases_cropval_percap_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_cropval_percap_quint$`1992_1997`,
                           "2002" = allbases_cropval_percap_quint$`1992_2002`,
                           "2007" = allbases_cropval_percap_quint$`1992_2007`,
                           "2012" = allbases_cropval_percap_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_cropval_percap_quint$`1997_2002`,
                           "2007" = allbases_cropval_percap_quint$`1997_2007`,
                           "2012" = allbases_cropval_percap_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_cropval_percap_quint$`2002_2007`,
                           "2012" = allbases_cropval_percap_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_cropval_percap_quint$`2007_2012`))
  
  assign(paste0("allbases_cropval_ordered_percap_quint",q), allbases_cropval_ordered_percap_quint)
  
  modelsummary(allbases_cropval_ordered_percap_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{cropval_allbases_percap_q", q, 
                              "}Percentile Change in Crop Production Value, County Level, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_cropval_countylevel_empquint", q, ".tex"), overwrite = TRUE)
}

for (q in 1:5) {
  allbases_prodval_percap_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + prodval_", i,
                                        " + prodval_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_emppercap == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_prodval_percap_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_prodval_ordered_percap_quint <- list(
    "Starting 1978" = list("1982" = allbases_prodval_percap_quint$`1978_1982`,
                           "1987" = allbases_prodval_percap_quint$`1978_1987`,
                           "1992" = allbases_prodval_percap_quint$`1978_1992`,
                           "1997" = allbases_prodval_percap_quint$`1978_1997`,
                           "2002" = allbases_prodval_percap_quint$`1978_2002`,
                           "2007" = allbases_prodval_percap_quint$`1978_2007`,
                           "2012" = allbases_prodval_percap_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_prodval_percap_quint$`1982_1987`,
                           "1992" = allbases_prodval_percap_quint$`1982_1992`,
                           "1997" = allbases_prodval_percap_quint$`1982_1997`,
                           "2002" = allbases_prodval_percap_quint$`1982_2002`,
                           "2007" = allbases_prodval_percap_quint$`1982_2007`,
                           "2012" = allbases_prodval_percap_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_prodval_percap_quint$`1987_1992`,
                           "1997" = allbases_prodval_percap_quint$`1987_1997`,
                           "2002" = allbases_prodval_percap_quint$`1987_2002`,
                           "2007" = allbases_prodval_percap_quint$`1987_2007`,
                           "2012" = allbases_prodval_percap_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_prodval_percap_quint$`1992_1997`,
                           "2002" = allbases_prodval_percap_quint$`1992_2002`,
                           "2007" = allbases_prodval_percap_quint$`1992_2007`,
                           "2012" = allbases_prodval_percap_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_prodval_percap_quint$`1997_2002`,
                           "2007" = allbases_prodval_percap_quint$`1997_2007`,
                           "2012" = allbases_prodval_percap_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_prodval_percap_quint$`2002_2007`,
                           "2012" = allbases_prodval_percap_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_prodval_percap_quint$`2007_2012`))
  
  assign(paste0("allbases_prodval_ordered_percap_quint",q), allbases_prodval_ordered_percap_quint)
  
  modelsummary(allbases_prodval_ordered_percap_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{prodval_allbases_percap_q", q, 
                              "}Percentile Change in Agricultural Production Value, County Level, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_prodval_countylevel_empquint", q, ".tex"), overwrite = TRUE)
}

## Making a graph of it
p <- modelplot(models = c("Quintile 1" = allbases_prodval_ordered_percap_quint1$`Starting 1978`,
                          "Quintile 2" = allbases_prodval_ordered_percap_quint2$`Starting 1978`,
                          "Quintile 3" = allbases_prodval_ordered_percap_quint3$`Starting 1978`,
                          "Quintile 4" = allbases_prodval_ordered_percap_quint4$`Starting 1978`,
                          "Quintile 5" = allbases_prodval_ordered_percap_quint5$`Starting 1978`),
               coef_map = c("pctl_growth_2012_1978" = "2012",
                            "pctl_growth_2007_1978" = "2007",
                            "pctl_growth_2002_1978" = "2002",
                            "pctl_growth_1997_1978" = "1997",
                            "pctl_growth_1992_1978" = "1992",
                            "pctl_growth_1987_1978" = "1987",
                            "pctl_growth_1982_1978" = "1982",
                            "fit_pctl_growth_2012_1978" = "2012",
                            "fit_pctl_growth_2007_1978" = "2007",
                            "fit_pctl_growth_2002_1978" = "2002",
                            "fit_pctl_growth_1997_1978" = "1997",
                            "fit_pctl_growth_1992_1978" = "1992",
                            "fit_pctl_growth_1987_1978" = "1987",
                            "fit_pctl_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Quintile 1", "Quintile 2",
                                                  "Quintile 3", "Quintile 4",
                                                  "Quintile 5"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/prodval_percap_quint.png", width = 6, height = 4)

for (q in 1:5) {
  allbases_agemp_percap_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_agempgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + agemp_", i,
                                        " + agemp_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_emppercap == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_agemp_percap_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_agemp_ordered_percap_quint <- list(
    "Starting 1978" = list("1982" = allbases_agemp_percap_quint$`1978_1982`,
                           "1987" = allbases_agemp_percap_quint$`1978_1987`,
                           "1992" = allbases_agemp_percap_quint$`1978_1992`,
                           "1997" = allbases_agemp_percap_quint$`1978_1997`,
                           "2002" = allbases_agemp_percap_quint$`1978_2002`,
                           "2007" = allbases_agemp_percap_quint$`1978_2007`,
                           "2012" = allbases_agemp_percap_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_agemp_percap_quint$`1982_1987`,
                           "1992" = allbases_agemp_percap_quint$`1982_1992`,
                           "1997" = allbases_agemp_percap_quint$`1982_1997`,
                           "2002" = allbases_agemp_percap_quint$`1982_2002`,
                           "2007" = allbases_agemp_percap_quint$`1982_2007`,
                           "2012" = allbases_agemp_percap_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_agemp_percap_quint$`1987_1992`,
                           "1997" = allbases_agemp_percap_quint$`1987_1997`,
                           "2002" = allbases_agemp_percap_quint$`1987_2002`,
                           "2007" = allbases_agemp_percap_quint$`1987_2007`,
                           "2012" = allbases_agemp_percap_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_agemp_percap_quint$`1992_1997`,
                           "2002" = allbases_agemp_percap_quint$`1992_2002`,
                           "2007" = allbases_agemp_percap_quint$`1992_2007`,
                           "2012" = allbases_agemp_percap_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_agemp_percap_quint$`1997_2002`,
                           "2007" = allbases_agemp_percap_quint$`1997_2007`,
                           "2012" = allbases_agemp_percap_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_agemp_percap_quint$`2002_2007`,
                           "2012" = allbases_agemp_percap_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_agemp_percap_quint$`2007_2012`))
  
  assign(paste0("allbases_agemp_ordered_percap_quint",q), allbases_agemp_ordered_percap_quint)
  
  modelsummary(allbases_agemp_ordered_percap_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{agemp_allbases_percap_q", q, 
                              "}Percentile Change in Agricultural Employment, County Level, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_agemp_countylevel_empquint", q, ".tex"), overwrite = TRUE)
}

for (q in 1:5) {
  allbases_farmacrevalue_percap_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_farmacrevaluegrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + farmacrevalue_", i,
                                        " + farmacrevalue_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_emppercap == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_farmacrevalue_percap_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_farmacrevalue_ordered_percap_quint <- list(
    "Starting 1978" = list("1982" = allbases_farmacrevalue_percap_quint$`1978_1982`,
                           "1987" = allbases_farmacrevalue_percap_quint$`1978_1987`,
                           "1992" = allbases_farmacrevalue_percap_quint$`1978_1992`,
                           "1997" = allbases_farmacrevalue_percap_quint$`1978_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint$`1978_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint$`1978_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_farmacrevalue_percap_quint$`1982_1987`,
                           "1992" = allbases_farmacrevalue_percap_quint$`1982_1992`,
                           "1997" = allbases_farmacrevalue_percap_quint$`1982_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint$`1982_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint$`1982_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_farmacrevalue_percap_quint$`1987_1992`,
                           "1997" = allbases_farmacrevalue_percap_quint$`1987_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint$`1987_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint$`1987_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_farmacrevalue_percap_quint$`1992_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint$`1992_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint$`1992_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_farmacrevalue_percap_quint$`1997_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint$`1997_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_farmacrevalue_percap_quint$`2002_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_farmacrevalue_percap_quint$`2007_2012`))
  
  assign(paste0("allbases_farmacrevalue_ordered_percap_quint",q), allbases_farmacrevalue_ordered_percap_quint)
  
  modelsummary(allbases_farmacrevalue_ordered_percap_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{farmacrevalue_allbases_percap_q", q, 
                              "}Percentile Change in Farm Acre Value, County Level, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/farmacrevalue_countylevel_empquint", q, ".tex"), overwrite = TRUE)
}

for (q in 1:5) {
  allbases_prodval_agshare_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + prodval_", i,
                                        " + prodval_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_agshare == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_prodval_agshare_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_prodval_ordered_agshare_quint <- list(
    "Starting 1978" = list("1982" = allbases_prodval_agshare_quint$`1978_1982`,
                           "1987" = allbases_prodval_agshare_quint$`1978_1987`,
                           "1992" = allbases_prodval_agshare_quint$`1978_1992`,
                           "1997" = allbases_prodval_agshare_quint$`1978_1997`,
                           "2002" = allbases_prodval_agshare_quint$`1978_2002`,
                           "2007" = allbases_prodval_agshare_quint$`1978_2007`,
                           "2012" = allbases_prodval_agshare_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_prodval_agshare_quint$`1982_1987`,
                           "1992" = allbases_prodval_agshare_quint$`1982_1992`,
                           "1997" = allbases_prodval_agshare_quint$`1982_1997`,
                           "2002" = allbases_prodval_agshare_quint$`1982_2002`,
                           "2007" = allbases_prodval_agshare_quint$`1982_2007`,
                           "2012" = allbases_prodval_agshare_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_prodval_agshare_quint$`1987_1992`,
                           "1997" = allbases_prodval_agshare_quint$`1987_1997`,
                           "2002" = allbases_prodval_agshare_quint$`1987_2002`,
                           "2007" = allbases_prodval_agshare_quint$`1987_2007`,
                           "2012" = allbases_prodval_agshare_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_prodval_agshare_quint$`1992_1997`,
                           "2002" = allbases_prodval_agshare_quint$`1992_2002`,
                           "2007" = allbases_prodval_agshare_quint$`1992_2007`,
                           "2012" = allbases_prodval_agshare_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_prodval_agshare_quint$`1997_2002`,
                           "2007" = allbases_prodval_agshare_quint$`1997_2007`,
                           "2012" = allbases_prodval_agshare_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_prodval_agshare_quint$`2002_2007`,
                           "2012" = allbases_prodval_agshare_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_prodval_agshare_quint$`2007_2012`))
  
  assign(paste0("allbases_prodval_ordered_agshare_quint",q), allbases_prodval_ordered_agshare_quint)
  
  modelsummary(allbases_prodval_ordered_agshare_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{prodval_allbases_agshare_q", q, 
                              "}Percentile Change in Agricultural Production Value, County Level, Ag. Employment Share Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_prodval_countylevel_agsharequint", q, ".tex"), overwrite = TRUE)
}

## Making a graph of it
p <- modelplot(models = c("Quintile 1" = allbases_prodval_ordered_agshare_quint1$`Starting 1978`,
                          "Quintile 2" = allbases_prodval_ordered_agshare_quint2$`Starting 1978`,
                          "Quintile 3" = allbases_prodval_ordered_agshare_quint3$`Starting 1978`,
                          "Quintile 4" = allbases_prodval_ordered_agshare_quint4$`Starting 1978`,
                          "Quintile 5" = allbases_prodval_ordered_agshare_quint5$`Starting 1978`),
               coef_map = c("pctl_growth_2012_1978" = "2012",
                            "pctl_growth_2007_1978" = "2007",
                            "pctl_growth_2002_1978" = "2002",
                            "pctl_growth_1997_1978" = "1997",
                            "pctl_growth_1992_1978" = "1992",
                            "pctl_growth_1987_1978" = "1987",
                            "pctl_growth_1982_1978" = "1982",
                            "fit_pctl_growth_2012_1978" = "2012",
                            "fit_pctl_growth_2007_1978" = "2007",
                            "fit_pctl_growth_2002_1978" = "2002",
                            "fit_pctl_growth_1997_1978" = "1997",
                            "fit_pctl_growth_1992_1978" = "1992",
                            "fit_pctl_growth_1987_1978" = "1987",
                            "fit_pctl_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Quintile 1", "Quintile 2",
                                                  "Quintile 3", "Quintile 4",
                                                  "Quintile 5"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/prodval_agshare_quint.png", width = 6, height = 4)

for (q in 1:5) {
  allbases_cropland_agshare_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_croplandgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + cropland_", i,
                                        " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_agshare == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_cropland_agshare_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_cropland_ordered_agshare_quint <- list(
    "Starting 1978" = list("1982" = allbases_cropland_agshare_quint$`1978_1982`,
                           "1987" = allbases_cropland_agshare_quint$`1978_1987`,
                           "1992" = allbases_cropland_agshare_quint$`1978_1992`,
                           "1997" = allbases_cropland_agshare_quint$`1978_1997`,
                           "2002" = allbases_cropland_agshare_quint$`1978_2002`,
                           "2007" = allbases_cropland_agshare_quint$`1978_2007`,
                           "2012" = allbases_cropland_agshare_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_cropland_agshare_quint$`1982_1987`,
                           "1992" = allbases_cropland_agshare_quint$`1982_1992`,
                           "1997" = allbases_cropland_agshare_quint$`1982_1997`,
                           "2002" = allbases_cropland_agshare_quint$`1982_2002`,
                           "2007" = allbases_cropland_agshare_quint$`1982_2007`,
                           "2012" = allbases_cropland_agshare_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_cropland_agshare_quint$`1987_1992`,
                           "1997" = allbases_cropland_agshare_quint$`1987_1997`,
                           "2002" = allbases_cropland_agshare_quint$`1987_2002`,
                           "2007" = allbases_cropland_agshare_quint$`1987_2007`,
                           "2012" = allbases_cropland_agshare_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_cropland_agshare_quint$`1992_1997`,
                           "2002" = allbases_cropland_agshare_quint$`1992_2002`,
                           "2007" = allbases_cropland_agshare_quint$`1992_2007`,
                           "2012" = allbases_cropland_agshare_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_cropland_agshare_quint$`1997_2002`,
                           "2007" = allbases_cropland_agshare_quint$`1997_2007`,
                           "2012" = allbases_cropland_agshare_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_cropland_agshare_quint$`2002_2007`,
                           "2012" = allbases_cropland_agshare_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_cropland_agshare_quint$`2007_2012`))
  
  assign(paste0("allbases_cropland_ordered_agshare_quint",q), allbases_cropland_ordered_agshare_quint)
  
  modelsummary(allbases_cropland_ordered_agshare_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{cropland_allbases_agshare_q", q, 
                              "}Percentile Change in Total Cropland, County Level, Ag. Employment Share Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_cropland_countylevel_agsharequint", q, ".tex"), overwrite = TRUE)
}

####################################
### Interacted share of labor force in agriculture
####################################

allbases_prodval_agshare <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_prodvalgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, "*wins_agshare_", i, 
                                      " + wins_", i, " + wins_prodval_", i,
                                      " + wins_prodval_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_prodval_agshare[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_ordered_agshare <- list("Starting 1978" = 
                                          list("1982" = allbases_prodval_agshare$`1978_1982`,
                                               "1987" = allbases_prodval_agshare$`1978_1987`,
                                               "1992" = allbases_prodval_agshare$`1978_1992`,
                                               "1997" = allbases_prodval_agshare$`1978_1997`,
                                               "2002" = allbases_prodval_agshare$`1978_2002`,
                                               "2007" = allbases_prodval_agshare$`1978_2007`,
                                               "2012" = allbases_prodval_agshare$`1978_2012`),
                                        "Starting 1982" =
                                          list("1987" = allbases_prodval_agshare$`1982_1987`,
                                               "1992" = allbases_prodval_agshare$`1982_1992`,
                                               "1997" = allbases_prodval_agshare$`1982_1997`,
                                               "2002" = allbases_prodval_agshare$`1982_2002`,
                                               "2007" = allbases_prodval_agshare$`1982_2007`,
                                               "2012" = allbases_prodval_agshare$`1982_2012`),
                                        "Starting 1987" =
                                          list("1992" = allbases_prodval_agshare$`1987_1992`,
                                               "1997" = allbases_prodval_agshare$`1987_1997`,
                                               "2002" = allbases_prodval_agshare$`1987_2002`,
                                               "2007" = allbases_prodval_agshare$`1987_2007`,
                                               "2012" = allbases_prodval_agshare$`1987_2012`),
                                        "Starting 1992" =
                                          list("1997" = allbases_prodval_agshare$`1992_1997`,
                                               "2002" = allbases_prodval_agshare$`1992_2002`,
                                               "2007" = allbases_prodval_agshare$`1992_2007`,
                                               "2012" = allbases_prodval_agshare$`1992_2012`),
                                        "Starting 1997" =
                                          list("2002" = allbases_prodval_agshare$`1997_2002`,
                                               "2007" = allbases_prodval_agshare$`1997_2007`,
                                               "2012" = allbases_prodval_agshare$`1997_2012`),
                                        "Starting 2002" =
                                          list("2007" = allbases_prodval_agshare$`2002_2007`,
                                               "2012" = allbases_prodval_agshare$`2002_2012`),
                                        "Starting 2007" =
                                          list("2012" = allbases_prodval_agshare$`2007_2012`))

modelsummary(allbases_prodval_ordered_agshare,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_agshare", x),
                      "Emp. Growth x Agricultural Employment Share",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_agshare}Winsorized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "I also control for the agricultural employment share by itself, which is winsorized. 
             County size comes from IPUMS NHGIS data.
             Employment values are always measured as of the starting year to avoid exogeneity concerns.")|>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_agshare.tex", overwrite = TRUE)

## Doing the same for cropland
allbases_cropland_agshare <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(wins_croplandgrowth_", j, "_", i, 
                                      " ~ wins_growth_", j, "_", i, "*wins_agshare_", i, 
                                      " + wins_", i, " + wins_cropland_", i,
                                      " + wins_cropland_", i, "^2 + wins_", i, "^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_cropland_agshare[[paste0(i, "_", j)]] <- run
  }
}

allbases_cropland_ordered_agshare <- list("Starting 1978" = 
                                           list("1982" = allbases_cropland_agshare$`1978_1982`,
                                                "1987" = allbases_cropland_agshare$`1978_1987`,
                                                "1992" = allbases_cropland_agshare$`1978_1992`,
                                                "1997" = allbases_cropland_agshare$`1978_1997`,
                                                "2002" = allbases_cropland_agshare$`1978_2002`,
                                                "2007" = allbases_cropland_agshare$`1978_2007`,
                                                "2012" = allbases_cropland_agshare$`1978_2012`),
                                         "Starting 1982" =
                                           list("1987" = allbases_cropland_agshare$`1982_1987`,
                                                "1992" = allbases_cropland_agshare$`1982_1992`,
                                                "1997" = allbases_cropland_agshare$`1982_1997`,
                                                "2002" = allbases_cropland_agshare$`1982_2002`,
                                                "2007" = allbases_cropland_agshare$`1982_2007`,
                                                "2012" = allbases_cropland_agshare$`1982_2012`),
                                         "Starting 1987" =
                                           list("1992" = allbases_cropland_agshare$`1987_1992`,
                                                "1997" = allbases_cropland_agshare$`1987_1997`,
                                                "2002" = allbases_cropland_agshare$`1987_2002`,
                                                "2007" = allbases_cropland_agshare$`1987_2007`,
                                                "2012" = allbases_cropland_agshare$`1987_2012`),
                                         "Starting 1992" =
                                           list("1997" = allbases_cropland_agshare$`1992_1997`,
                                                "2002" = allbases_cropland_agshare$`1992_2002`,
                                                "2007" = allbases_cropland_agshare$`1992_2007`,
                                                "2012" = allbases_cropland_agshare$`1992_2012`),
                                         "Starting 1997" =
                                           list("2002" = allbases_cropland_agshare$`1997_2002`,
                                                "2007" = allbases_cropland_agshare$`1997_2007`,
                                                "2012" = allbases_cropland_agshare$`1997_2012`),
                                         "Starting 2002" =
                                           list("2007" = allbases_cropland_agshare$`2002_2007`,
                                                "2012" = allbases_cropland_agshare$`2002_2012`),
                                         "Starting 2007" =
                                           list("2012" = allbases_cropland_agshare$`2007_2012`))

modelsummary(allbases_cropland_ordered_agshare,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_agshare", x),
                      "Emp. Growth x Agricultural Employment Share",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{cropland_allbases_agshare}Winsorized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "I also control for the agricultural employment share by itself, which is winsorized. 
             County size comes from IPUMS NHGIS data.
             Employment values are always measured as of the starting year to avoid exogeneity concerns.")|>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/cropland_countylevel_agshare.tex", overwrite = TRUE)

agsharelist <- list("Agricultural Production Value" = allbases_prodval_ordered_agshare$`Starting 1978`,
                    "Total Cropland" = allbases_cropland_ordered_agshare$`Starting 1978`)

modelsummary(agsharelist,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!wins_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("wins_growth_", x) & grepl("wins_agshare", x),
                      "Emp. Growth x Agricultural Employment Share",
                      ifelse(grepl("wins_growth_", x),
                             "Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{agshare}Winsorized Change in Variables, County Level, Starting 1978",
             output = "tinytable",
             escape = FALSE,
             notes = "I also control for the agricultural employment share by itself, which is winsorized. 
             Employment values are always measured as of the starting year to avoid exogeneity concerns.")|>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/agsharesmall.tex", overwrite = TRUE)

###############################
### Water quintiles
###############################

for (q in 1:5) {
  allbases_prodval_precip_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + prodval_", i,
                                        " + prodval_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_precip == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
      allbases_prodval_precip_quint[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_prodval_ordered_precip_quint <- list(
    "Starting 1978" = list("1982" = allbases_prodval_precip_quint$`1978_1982`,
                           "1987" = allbases_prodval_precip_quint$`1978_1987`,
                           "1992" = allbases_prodval_precip_quint$`1978_1992`,
                           "1997" = allbases_prodval_precip_quint$`1978_1997`,
                           "2002" = allbases_prodval_precip_quint$`1978_2002`,
                           "2007" = allbases_prodval_precip_quint$`1978_2007`,
                           "2012" = allbases_prodval_precip_quint$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_prodval_precip_quint$`1982_1987`,
                           "1992" = allbases_prodval_precip_quint$`1982_1992`,
                           "1997" = allbases_prodval_precip_quint$`1982_1997`,
                           "2002" = allbases_prodval_precip_quint$`1982_2002`,
                           "2007" = allbases_prodval_precip_quint$`1982_2007`,
                           "2012" = allbases_prodval_precip_quint$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_prodval_precip_quint$`1987_1992`,
                           "1997" = allbases_prodval_precip_quint$`1987_1997`,
                           "2002" = allbases_prodval_precip_quint$`1987_2002`,
                           "2007" = allbases_prodval_precip_quint$`1987_2007`,
                           "2012" = allbases_prodval_precip_quint$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_prodval_precip_quint$`1992_1997`,
                           "2002" = allbases_prodval_precip_quint$`1992_2002`,
                           "2007" = allbases_prodval_precip_quint$`1992_2007`,
                           "2012" = allbases_prodval_precip_quint$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_prodval_precip_quint$`1997_2002`,
                           "2007" = allbases_prodval_precip_quint$`1997_2007`,
                           "2012" = allbases_prodval_precip_quint$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_prodval_precip_quint$`2002_2007`,
                           "2012" = allbases_prodval_precip_quint$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_prodval_precip_quint$`2007_2012`))
  
  assign(paste0("allbases_prodval_ordered_precip_quint",q), allbases_prodval_ordered_precip_quint)
  
  modelsummary(allbases_prodval_ordered_precip_quint,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Pctl. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{prodval_allbases_precip_q", q, 
                              "}Percentile Change in Agricultural Production Value, County Level, Precipitation Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_prodval_countylevel_precipquint", q, ".tex"), overwrite = TRUE)
}

p <- modelplot(models = c("Quintile 1" = allbases_prodval_ordered_precip_quint1$`Starting 1978`,
                          "Quintile 2" = allbases_prodval_ordered_precip_quint2$`Starting 1978`,
                          "Quintile 3" = allbases_prodval_ordered_precip_quint3$`Starting 1978`,
                          "Quintile 4" = allbases_prodval_ordered_precip_quint4$`Starting 1978`,
                          "Quintile 5" = allbases_prodval_ordered_precip_quint5$`Starting 1978`),
               coef_map = c("pctl_growth_2012_1978" = "2012",
                            "pctl_growth_2007_1978" = "2007",
                            "pctl_growth_2002_1978" = "2002",
                            "pctl_growth_1997_1978" = "1997",
                            "pctl_growth_1992_1978" = "1992",
                            "pctl_growth_1987_1978" = "1987",
                            "pctl_growth_1982_1978" = "1982",
                            "fit_pctl_growth_2012_1978" = "2012",
                            "fit_pctl_growth_2007_1978" = "2007",
                            "fit_pctl_growth_2002_1978" = "2002",
                            "fit_pctl_growth_1997_1978" = "1997",
                            "fit_pctl_growth_1992_1978" = "1992",
                            "fit_pctl_growth_1987_1978" = "1987",
                            "fit_pctl_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(c("Quintile 1", "Quintile 2",
                                                  "Quintile 3", "Quintile 4",
                                                  "Quintile 5"), collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/Figures/prodval_precip_quint.png", width = 6, height = 4)

################################
### Testing migration vs unemployment
################################

prodval_unemponly <- list()
for (i in c(1992, 1997, 2002, 2007)) {
  for (j in c(1997, 2002, 2007, 2012)) {
    if (i >= j) {
      print("NA")
    } else {
      run <- eval(parse(text = paste0(
        "feols(pctl_prodvalgrowth_", j, "_", i,
        " ~ pctl_unemprategrowth_", j, "_", i, " + `", i, "` + prodval_", i,
        " + prodval_", i, "^2 + `", i, "`^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
      )))
      prodval_unemponly[[paste0(i, "_", j)]] <- run
    }
  }
}

prodval_unemponly_ordered <- list(
  "Starting 1992" = list("1997" = prodval_unemponly$`1992_1997`,
                         "2002" = prodval_unemponly$`1992_2002`,
                         "2007" = prodval_unemponly$`1992_2007`,
                         "2012" = prodval_unemponly$`1992_2012`),
  "Starting 1997" = list("2002" = prodval_unemponly$`1997_2002`,
                         "2007" = prodval_unemponly$`1997_2007`,
                         "2012" = prodval_unemponly$`1997_2012`),
  "Starting 2002" = list("2007" = prodval_unemponly$`2002_2007`,
                         "2012" = prodval_unemponly$`2002_2012`),
  "Starting 2007" = list("2012" = prodval_unemponly$`2007_2012`))


modelsummary(prodval_unemponly_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_unemprategrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^pctl_unemprategrowth_", x), "Pctl. Unemployment Rate Growth", x)
             },
             gof_map = gm_small,
             title = paste0("\\label{prodval_unemponly}Percentile Change in Agricultural Production Value, County Level, from Unemployment Rate Changes"),
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt(paste0("Output/Tables/prodval_unemponly.tex"), overwrite = TRUE)

prodval_totpoponly <- list()
for (i in c(1992, 1997, 2002, 2007)) {
  for (j in c(1997, 2002, 2007, 2012)) {
    if (i >= j) {
      print("NA")
    } else {
      run <- eval(parse(text = paste0(
        "feols(pctl_prodvalgrowth_", j, "_", i,
        " ~ pctl_totpopgrowth_", j, "_", i, " + `", i, "` + prodval_", i,
        " + prodval_", i, "^2 + `", i, "`^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
      )))
      prodval_totpoponly[[paste0(i, "_", j)]] <- run
    }
  }
}

prodval_totpoponly_ordered <- list(
  "Starting 1992" = list("1997" = prodval_totpoponly$`1992_1997`,
                         "2002" = prodval_totpoponly$`1992_2002`,
                         "2007" = prodval_totpoponly$`1992_2007`,
                         "2012" = prodval_totpoponly$`1992_2012`),
  "Starting 1997" = list("2002" = prodval_totpoponly$`1997_2002`,
                         "2007" = prodval_totpoponly$`1997_2007`,
                         "2012" = prodval_totpoponly$`1997_2012`),
  "Starting 2002" = list("2007" = prodval_totpoponly$`2002_2007`,
                         "2012" = prodval_totpoponly$`2002_2012`),
  "Starting 2007" = list("2012" = prodval_totpoponly$`2007_2012`))

modelsummary(prodval_totpoponly_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_totpopgrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^pctl_totpopgrowth_", x), "Pctl. Population Growth", x)
             },
             gof_map = gm_small,
             title = paste0("\\label{prodval_totpoponly}Percentile Change in Agricultural Production Value, County Level, from Population Changes"),
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt(paste0("Output/Tables/prodval_totpoponly.tex"), overwrite = TRUE)

cropland_unemponly <- list()
for (i in c(1992, 1997, 2002, 2007)) {
  for (j in c(1997, 2002, 2007, 2012)) {
    if (i >= j) {
      print("NA")
    } else {
      run <- eval(parse(text = paste0(
        "feols(pctl_croplandgrowth_", j, "_", i,
        " ~ pctl_unemprategrowth_", j, "_", i, " + `", i, "` + cropland_", i,
        " + cropland_", i, "^2 + `", i, "`^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
      )))
      cropland_unemponly[[paste0(i, "_", j)]] <- run
    }
  }
}

cropland_unemponly_ordered <- list(
  "Starting 1992" = list("1997" = cropland_unemponly$`1992_1997`,
                         "2002" = cropland_unemponly$`1992_2002`,
                         "2007" = cropland_unemponly$`1992_2007`,
                         "2012" = cropland_unemponly$`1992_2012`),
  "Starting 1997" = list("2002" = cropland_unemponly$`1997_2002`,
                         "2007" = cropland_unemponly$`1997_2007`,
                         "2012" = cropland_unemponly$`1997_2012`),
  "Starting 2002" = list("2007" = cropland_unemponly$`2002_2007`,
                         "2012" = cropland_unemponly$`2002_2012`),
  "Starting 2007" = list("2012" = cropland_unemponly$`2007_2012`))

modelsummary(cropland_unemponly_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_unemprategrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^pctl_unemprategrowth_", x), "Pctl. Unemployment Rate Growth", x)
             },
             gof_map = gm_small,
             title = paste0("\\label{cropland_unemponly}Percentile Change in Acres Cropland, County Level, from Unemployment Rate Changes"),
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt(paste0("Output/Tables/cropland_unemponly.tex"), overwrite = TRUE)

cropland_totpoponly <- list()
for (i in c(1992, 1997, 2002, 2007)) {
  for (j in c(1997, 2002, 2007, 2012)) {
    if (i >= j) {
      print("NA")
    } else {
      run <- eval(parse(text = paste0(
        "feols(pctl_croplandgrowth_", j, "_", i,
        " ~ pctl_totpopgrowth_", j, "_", i, " + `", i, "` + cropland_", i,
        " + cropland_", i, "^2 + `", i, "`^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
      )))
      cropland_totpoponly[[paste0(i, "_", j)]] <- run
    }
  }
}

cropland_totpoponly_ordered <- list(
  "Starting 1992" = list("1997" = cropland_totpoponly$`1992_1997`,
                         "2002" = cropland_totpoponly$`1992_2002`,
                         "2007" = cropland_totpoponly$`1992_2007`,
                         "2012" = cropland_totpoponly$`1992_2012`),
  "Starting 1997" = list("2002" = cropland_totpoponly$`1997_2002`,
                         "2007" = cropland_totpoponly$`1997_2007`,
                         "2012" = cropland_totpoponly$`1997_2012`),
  "Starting 2002" = list("2007" = cropland_totpoponly$`2002_2007`,
                         "2012" = cropland_totpoponly$`2002_2012`),
  "Starting 2007" = list("2012" = cropland_totpoponly$`2007_2012`))

modelsummary(cropland_totpoponly_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_totpopgrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^pctl_totpopgrowth_", x), "Pctl. Population Growth", x)
             },
             gof_map = gm_small,
             title = paste0("\\label{cropland_totpoponly}Percentile Change in Acres Cropland, County Level, from Population Changes"),
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt(paste0("Output/Tables/cropland_totpoponly.tex"), overwrite = TRUE)
