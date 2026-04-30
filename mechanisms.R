####################
### Interacting with the people employed per capita in 1978
####################

allbases_percap <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*emppercap_", i, 
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
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("emppercap", x),
                      "Norm. Growth x Employed Pop./Square Meter",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_percap}Normalized Change in Total Cropland, County Level",
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
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*emppercap_", i, 
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
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("emppercap", x),
                      "Norm. Growth x Employed Pop./Square Meter",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_percap}Normalized Change in Agricultural Production Value, County Level",
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
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*drought_", i,
                                      "_", j, " + `", i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("drought", x),
                      "Norm. Employment Growth x Palmer Drought Index",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_drought}Normalized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Drought is represented using the Palmer Drought Severity Index, averaged from the starting year to the given year. 
             The drought variable is also included as a control by itself.
             Lower values of the Palmer index indicate more drought.") |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_drought.tex", overwrite = TRUE)


## And interacting with the amount of precipitation
allbases_precip <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*precip + `", 
                                      i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!.*norm_growth_)(?!.*precip)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("precip", x),
                      "Norm. Employment Growth x Precipitation",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", ifelse(grepl("precip", x),
                                                               "Precipitation", x)))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_precip}Normalized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Precipitation is the yearly amount of precipitation in a county from 1948 to 1977, averaged over those years. 
             The precipitation variable is also included as a control by itself.") |>
  theme_tt("resize", width = 0.75) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_precip.tex", overwrite = TRUE)

## And estimating without the interaction
allbases_precip_noint <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + precip + `", 
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
             coef_omit = "^(?!.*norm_growth_)(?!.*precip)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("precip", x),
                      "Norm. Employment Growth x Precipitation",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", ifelse(grepl("precip", x),
                                                               "Precipitation", x)))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_precip_noint}Normalized Change in Total Cropland, County Level",
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
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*drought_", i,
                                      "_", j, " + `", i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("drought", x),
                      "Norm. Employment Growth x Palmer Drought Index",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_drought}Normalized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Drought is represented using the Palmer Drought Severity Index, averaged from the starting year to the given year. 
             The drought variable is also included as a control by itself.
             Lower values of the Palmer index indicate more drought.") |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_drought.tex", overwrite = TRUE)


## And interacting with the amount of precipitation
allbases_prodval_precip <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*precip + `", 
                                      i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!.*norm_growth_)(?!.*precip)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("precip", x),
                      "Norm. Employment Growth x Precipitation",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", ifelse(grepl("precip", x),
                                                               "Precipitation", x)))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_precip}Normalized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "Precipitation is the yearly amount of precipitation in a county from 1948 to 1977, averaged over those years. 
             The precipitation variable is also included as a control by itself.") |>
  theme_tt("resize", width = 0.75) |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_precip.tex", overwrite = TRUE)


## And interacting with 1980 unemployment rates
allbases_unemp <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*unemp_rate_1980 + `", 
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
             coef_omit = "^(?!.*norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("unemp", x),
                      "Norm. Employment Growth x 1980 Unemployment Rate",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_unemp}Normalized Change in Total Cropland, County Level",
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
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_pollute_growth_", j, "_", i, " + `", i, "` + cropland_", i,
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
             coef_omit = "^(?!norm_pollute_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_pollute_growth_", x), "Norm. Employment Growth, Polluting Industries", x)
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_pollute}Normalized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases_pollute.tex", overwrite = TRUE)

allbases_nopollute <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_nopollute_growth_", j, "_", i, " + `", i, "` + cropland_", i,
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
             coef_omit = "^(?!norm_nopollute_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_nopollute_growth_", x), "Norm. Employment Growth, Less Polluting Industries", x)
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_nopollute}Normalized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases_nopollute.tex", overwrite = TRUE)

allbases_bothpollute <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_nopollute_growth_", j, "_", i, 
                                      " + norm_pollute_growth_", j, "_", i, " + `", 
                                      i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!norm_nopollute_growth|norm_pollute_growth)",
             coef_rename = function(x) {
               x <- ifelse(grepl("norm_nopollute_growth", x), "Norm. Employment Growth, Less Polluting Industries", x)
               x <- ifelse(grepl("norm_pollute_growth", x), "Norm. Employment Growth, More Polluting Industries", x)
               x
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_bothpollute}Normalized Change in Total Cropland, County Level",
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
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ norm_nopollute_growth_", j, "_", i, 
                                      " + norm_pollute_growth_", j, "_", i, " + `", 
                                      i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!norm_nopollute_growth|norm_pollute_growth)",
             coef_rename = function(x) {
               x <- ifelse(grepl("norm_nopollute_growth", x), "Norm. Employment Growth, Less Polluting Industries", x)
               x <- ifelse(grepl("norm_pollute_growth", x), "Norm. Employment Growth, More Polluting Industries", x)
               x
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_bothpollute}Normalized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_bothpollute.tex", overwrite = TRUE)


#####################################
### Doing the per capita regression by quintiles in 1978
#####################################

for (q in 1:5) {
  allbases_percap_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                        " ~ norm_growth_", j, "_", i, 
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
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{crop_allbases_percap_q", q, 
                              "}Normalized Change in Total Cropland, County Level, Employment/Area Quintile ", q),
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
        run <- eval(parse(text = paste0("feols(norm_cropvalgrowth_", j, "_", i, 
                                        " ~ norm_growth_", j, "_", i, 
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
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{cropval_allbases_percap_q", q, 
                              "}Normalized Change in Crop Production Value, County Level, Employment/Area Quintile ", q),
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
        run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                        " ~ norm_growth_", j, "_", i, 
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
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{prodval_allbases_percap_q", q, 
                              "}Normalized Change in Agricultural Production Value, County Level, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_prodval_countylevel_empquint", q, ".tex"), overwrite = TRUE)
}

for (q in 1:5) {
  allbases_agemp_percap_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(norm_agempgrowth_", j, "_", i, 
                                        " ~ norm_growth_", j, "_", i, 
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
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{agemp_allbases_percap_q", q, 
                              "}Normalized Change in Agricultural Employment, County Level, Employment/Area Quintile ", q),
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
        run <- eval(parse(text = paste0("feols(norm_farmacrevaluegrowth_", j, "_", i, 
                                        " ~ norm_growth_", j, "_", i, 
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
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{farmacrevalue_allbases_percap_q", q, 
                              "}Normalized Change in Farm Acre Value, County Level, Employment/Area Quintile ", q),
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
        run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                        " ~ norm_growth_", j, "_", i, 
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
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{prodval_allbases_agshare_q", q, 
                              "}Normalized Change in Agricultural Production Value, County Level, Ag. Employment Share Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/fullspec_prodval_countylevel_agsharequint", q, ".tex"), overwrite = TRUE)
}

for (q in 1:5) {
  allbases_cropland_agshare_quint <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                        " ~ norm_growth_", j, "_", i, 
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
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{cropland_allbases_agshare_q", q, 
                              "}Normalized Change in Total Cropland, County Level, Ag. Employment Share Quintile ", q),
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
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*agshare_", i, 
                                      " + `", i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("agshare", x),
                      "Norm. Growth x Agricultural Employment Share",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_agshare}Normalized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "I also control for the agricultural employment share by itself. 
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
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, "*agshare_", i, 
                                      " + `", i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("norm_growth_", x) & grepl("agshare", x),
                      "Norm. Growth x Agricultural Employment Share",
                      ifelse(grepl("norm_growth_", x),
                             "Norm. Employment Growth", x))
             },
             gof_map = gm_small,
             title = "\\label{cropland_allbases_agshare}Normalized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE,
             notes = "I also control for the agricultural employment share by itself. 
             County size comes from IPUMS NHGIS data.
             Employment values are always measured as of the starting year to avoid exogeneity concerns.")|>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/cropland_countylevel_agshare.tex", overwrite = TRUE)
