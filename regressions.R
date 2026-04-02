## Running the regressions on the data
library(fixest)
library(modelsummary)
library(tinytable)
library(splines)

## Setting a gof_map
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R2", 3,
  "controls", "Controls", 0,
  "Fixed Effects", "State FE", 0,
  )

gm_small <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R2", 3)

check_controls <- function(yes = "✓", no = "✗") {
  function(model) {
    has_fe <- length(model$fixef_vars) > 0
    has_controls <- length(coef(model)) > 2  # more than just the main regressor
    data.frame(
      Controls = if (has_controls) yes else no,
      `Fixed Effects` = if (has_fe) yes else no,
      check.names = FALSE
    )
  }
}

##################################
### County-level descriptive regressions
##################################

base_1982 <- feols(croplandgrowth_1982_1978 ~ growth_1982_1978,
                   data = countylevel_all,
                   vcov_conley(cutoff = 100))

base_1987 <- feols(croplandgrowth_1987_1978 ~ growth_1987_1978,
                   data = countylevel_all,
                   vcov_conley(cutoff = 100))

base_1992 <- feols(croplandgrowth_1992_1978 ~ growth_1992_1978,
                   data = countylevel_all,
                   vcov_conley(cutoff = 100))

base_1997 <- feols(croplandgrowth_1997_1978 ~ growth_1997_1978,
                   data = countylevel_all,
                   vcov_conley(cutoff = 100))

base_2002 <- feols(croplandgrowth_2002_1978 ~ growth_2002_1978,
                   data = countylevel_all,
                   vcov_conley(cutoff = 100))

base_2007 <- feols(croplandgrowth_2007_1978 ~ growth_2007_1978,
                   data = countylevel_all,
                   vcov_conley(cutoff = 100))

base_2012 <- feols(croplandgrowth_2012_1978 ~ growth_2012_1978,
                   data = countylevel_all,
                   vcov_conley(cutoff = 100))

modelsummary(models = list("1982" = base_1982, "1987" = base_1987, "1992" = base_1992, 
                           "1997" = base_1997, "2002" = base_2002, "2007" = base_2007,
                           "2012" = base_2012),
             stars = TRUE,
             coef_omit = "Intercept",
             coef_rename = c("growth_1997_1978" = "Employment Growth",
                             "growth_1992_1978" = "Employment Growth",
                             "growth_1987_1978" = "Employment Growth",
                             "growth_1982_1978" = "Employment Growth",
                             "growth_2002_1978" = "Employment Growth",
                             "growth_2007_1978" = "Employment Growth",
                             "growth_2012_1978" = "Employment Growth"),
             gof_function = check_controls(),
             gof_map = gm,
             title = "\\label{base_crop}Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  save_tt("Output/Tables/base_countylevel.tex", overwrite = TRUE)

modelplot(models = list(base_1982, base_1987, base_1992, base_1997),
          coef_map = c("growth_1997_1978" = "1997",
                       "growth_1992_1978" = "1992",
                       "growth_1987_1978" = "1987",
                       "growth_1982_1978" = "1982")) +
  labs(x = "Effect of Change in Employed Pop. on Change in Cropland, 1978 Basis") +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none") + geom_vline(xintercept = 0)
ggsave("Output/figures/base_countylevel.png", width = 6, height = 4)



## Tossing in a bunch of controls also
fullspec_1982 <- feols(norm_croplandgrowth_1982_1978 ~ norm_growth_1982_1978 + `1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                    data = countylevel_all,
                    vcov_conley(cutoff = 100),
                    weights =~ weight_1978)

fullspec_1987 <- feols(norm_croplandgrowth_1987_1978 ~ norm_growth_1987_1978 + `1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                    data = countylevel_all,
                    vcov_conley(cutoff = 100),
                    weights =~ weight_1978)

fullspec_1992 <- feols(norm_croplandgrowth_1992_1978 ~ norm_growth_1992_1978 + `1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                    data = countylevel_all,
                    vcov_conley(cutoff = 100),
                    weights =~ weight_1978)

fullspec_1997 <- feols(norm_croplandgrowth_1997_1978 ~ norm_growth_1997_1978 + `1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                    data = countylevel_all,
                    vcov_conley(cutoff = 100),
                    weights =~ weight_1978)

fullspec_2002 <- feols(norm_croplandgrowth_2002_1978 ~ norm_growth_2002_1978 + `1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1978)

fullspec_2007 <- feols(norm_croplandgrowth_2007_1978 ~ norm_growth_2007_1978 + `1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1978)

fullspec_2012 <- feols(norm_croplandgrowth_2012_1978 ~ norm_growth_2012_1978 + `1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1978)

modelsummary(models = list("1982" = fullspec_1982, "1987" = fullspec_1987, 
                           "1992" = fullspec_1992, "1997" = fullspec_1997,
                           "2002" = fullspec_2002, "2007" = fullspec_2007,
                           "2012" = fullspec_2012),
             stars = TRUE,
             coef_map = c("norm_growth_1982_1978" = "Norm. Employment Growth",
                          "norm_growth_1987_1978" = "Norm. Employment Growth",
                          "norm_growth_1992_1978" = "Norm. Employment Growth",
                          "norm_growth_1997_1978" = "Norm. Employment Growth",
                          "norm_growth_2002_1978" = "Norm. Employment Growth",
                          "norm_growth_2007_1978" = "Norm. Employment Growth",
                          "norm_growth_2012_1978" = "Norm. Employment Growth"),
             gof_function = check_controls(),
             gof_map = gm,
             title = "\\label{crop_cl}Normalized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
             save_tt("Output/Tables/fullspec_countylevel.tex", overwrite = TRUE)

modelplot(models = list(fullspec_1982, fullspec_1987, fullspec_1992, fullspec_1997),
          coef_map = c("norm_growth_1997_1978" = "1997",
                       "norm_growth_1992_1978" = "1992",
                       "norm_growth_1987_1978" = "1987",
                       "norm_growth_1982_1978" = "1982")) +
  labs(x = "Effect of Change in Employed Pop. on Change in Cropland, 1978 Basis") +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none") + geom_vline(xintercept = 0)
ggsave("Output/figures/fullspec_countylevel.png", width = 6, height = 4)

## Getting it by region
regionregs <- list()
for (i in c("Northeast", "Midwest", "South", "West")){
  regionregs[[paste0("1982_", i)]] <- feols(norm_croplandgrowth_1982_1978 ~ norm_growth_1982_1978 + `1978` +
                           cropland_1978 + cropland_1978^2 + `1978`^2 |
                           statefip,
                         data = subset(countylevel_all, region == i),
                         vcov_conley(cutoff = 100),
                         weights =~ weight_1978)
  
  regionregs[[paste0("1987_", i)]] <- feols(norm_croplandgrowth_1987_1978 ~ norm_growth_1987_1978 + `1978` +
                           cropland_1978 + cropland_1978^2 + `1978`^2 |
                           statefip,
                         data = subset(countylevel_all, region == i),
                         vcov_conley(cutoff = 100),
                         weights =~ weight_1978)
  
  regionregs[[paste0("1992_", i)]] <- feols(norm_croplandgrowth_1992_1978 ~ norm_growth_1992_1978 + `1978` +
                           cropland_1978 + cropland_1978^2 + `1978`^2 |
                           statefip,
                         data = subset(countylevel_all, region == i),
                         vcov_conley(cutoff = 100),
                         weights =~ weight_1978)
  
  regionregs[[paste0("1997_", i)]] <- feols(norm_croplandgrowth_1997_1978 ~ norm_growth_1997_1978 + `1978` +
                           cropland_1978 + cropland_1978^2 + `1978`^2 |
                           statefip,
                         data = subset(countylevel_all, region == i),
                         vcov_conley(cutoff = 100),
                         weights =~ weight_1978)
  
  regionregs[[paste0("2002_", i)]] <- feols(norm_croplandgrowth_2002_1978 ~ norm_growth_2002_1978 + `1978` +
                                              cropland_1978 + cropland_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs[[paste0("2007_", i)]] <- feols(norm_croplandgrowth_2007_1978 ~ norm_growth_2007_1978 + `1978` +
                                              cropland_1978 + cropland_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs[[paste0("2012_", i)]] <- feols(norm_croplandgrowth_2012_1978 ~ norm_growth_2012_1978 + `1978` +
                                              cropland_1978 + cropland_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
}

results <- bind_rows(
  lapply(names(regionregs), function(nm) {
    broom::tidy(regionregs[[nm]]) %>%
      mutate(model = nm)
  })
) |>
  mutate(Year = case_when(term == "norm_growth_1982_1978" ~ 1978,
                          term == "norm_growth_1987_1978" ~ 1987,
                          term == "norm_growth_1992_1978" ~ 1992,
                          term == "norm_growth_1997_1978" ~ 1997,
                          .default = NA),
         region = str_sub(model, 6, -1),
         est = case_when(p.value > 0.1 ~ paste0(round(estimate, 3)),
                         p.value > 0.05 ~ paste0(round(estimate, 3), " + "),
                         p.value > 0.01 ~ paste0(round(estimate, 3), " * "),
                         p.value > 0.001 ~ paste0(round(estimate, 3), " ** "),
                         p.value > 0 ~ paste0(round(estimate, 3), " *** "))) |>
  filter(!is.na(Year)) |>
  select(Year, region, est) |>
  pivot_wider(names_from = "region", values_from = "est")

results |>
  tt(theme = "resize",
     caption = "Regional Coefficients, Normalized and Controlled Model",
     notes = "+ p $<$0.1, * p $<$0.05, ** p $<$0.01, *** p $<$0.001") |>
  save_tt("Output/Tables/fullspec_byregion.tex", overwrite = TRUE)

names(regionregs) <- str_sub(names(regionregs), 6, -1)
regions <- c("Northeast", "Midwest", "South", "West")

p <- modelplot(models = regionregs,
          coef_map = c("norm_growth_2012_1978" = "2012",
                       "norm_growth_2007_1978" = "2007",
                       "norm_growth_2002_1978" = "2002",
                       "norm_growth_1997_1978" = "1997",
                       "norm_growth_1992_1978" = "1992",
                       "norm_growth_1987_1978" = "1987",
                       "norm_growth_1982_1978" = "1982"))
p$data$model <- str_extract(p$data$model, paste(regions, collapse = "|"))
p + labs(x = "") +
  theme(text = element_text(family = "Times New Roman")) + geom_vline(xintercept = 0)
ggsave("Output/figures/fullspec_byreg.png", width = 6, height = 8)

###########################################
### Using a 1992 basis instead
###########################################

fullspec_1992_1997 <- feols(norm_croplandgrowth_1997_1992 ~ norm_growth_1997_1992 + `1992` +
                         cropland_1992 + cropland_1992^2 + `1992`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1992)

fullspec_1992_2002 <- feols(norm_croplandgrowth_2002_1992 ~ norm_growth_2002_1992 + `1992` +
                         cropland_1992 + cropland_1992^2 + `1992`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1992)

fullspec_1992_2007 <- feols(norm_croplandgrowth_2007_1992 ~ norm_growth_2007_1992 + `1992` +
                         cropland_1992 + cropland_1992^2 + `1992`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1992)

fullspec_1992_2012 <- feols(norm_croplandgrowth_2012_1992 ~ norm_growth_2012_1992 + `1992` +
                         cropland_1992 + cropland_1992^2 + `1992`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1992)

modelsummary(models = list("1997" = fullspec_1992_1997,
                           "2002" = fullspec_1992_2002, "2007" = fullspec_1992_2007,
                           "2012" = fullspec_1992_2012),
             stars = TRUE,
             coef_map = c("norm_growth_1997_1992" = "Norm. Employment Growth",
                          "norm_growth_2002_1992" = "Norm. Employment Growth",
                          "norm_growth_2007_1992" = "Norm. Employment Growth",
                          "norm_growth_2012_1992" = "Norm. Employment Growth"),
             gof_function = check_controls(),
             gof_map = gm,
             title = "\\label{crop_cl_1992}Normalized Change in Total Cropland, County Level, 1992 Basis",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  save_tt("Output/Tables/fullspec_countylevel_1992.tex", overwrite = TRUE)

########################################
### Getting all the other years as bases
########################################

allbases <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
     run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                   " ~ norm_growth_", j, "_", i, " + `", i, "` + cropland_", i,
                   " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered <- list("Starting 1978" = 
                           list("1982" = allbases$`1978_1982`,
                                "1987" = allbases$`1978_1987`,
                                "1992" = allbases$`1978_1992`,
                                "1997" = allbases$`1978_1997`,
                                "2002" = allbases$`1978_2002`,
                                "2007" = allbases$`1978_2007`,
                                "2012" = allbases$`1978_2012`),
                         "Starting 1982" =
                           list("1987" = allbases$`1982_1987`,
                                "1992" = allbases$`1982_1992`,
                                "1997" = allbases$`1982_1997`,
                                "2002" = allbases$`1982_2002`,
                                "2007" = allbases$`1982_2007`,
                                "2012" = allbases$`1982_2012`),
                         "Starting 1987" =
                           list("1992" = allbases$`1987_1992`,
                                "1997" = allbases$`1987_1997`,
                                "2002" = allbases$`1987_2002`,
                                "2007" = allbases$`1987_2007`,
                                "2012" = allbases$`1987_2012`),
                         "Starting 1992" =
                           list("1997" = allbases$`1992_1997`,
                                "2002" = allbases$`1992_2002`,
                                "2007" = allbases$`1992_2007`,
                                "2012" = allbases$`1992_2012`),
                         "Starting 1997" =
                           list("2002" = allbases$`1997_2002`,
                                "2007" = allbases$`1997_2007`,
                                "2012" = allbases$`1997_2012`),
                         "Starting 2002" =
                           list("2007" = allbases$`2002_2007`,
                                "2012" = allbases$`2002_2012`),
                         "Starting 2007" =
                           list("2012" = allbases$`2007_2012`))

modelsummary(allbases_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases}Normalized Change in Total Cropland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases.tex", overwrite = TRUE)


####################################
### Doing the same for the other variables
####################################

allbases_farms <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_farmsgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + `", i, "` + farms_", i,
                                      " + farms_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_farms[[paste0(i, "_", j)]] <- run
  }
}

allbases_ord_farms <- list("Starting 1978" = 
                           list("1982" = allbases_farms$`1978_1982`,
                                "1987" = allbases_farms$`1978_1987`,
                                "1992" = allbases_farms$`1978_1992`,
                                "1997" = allbases_farms$`1978_1997`,
                                "2002" = allbases_farms$`1978_2002`,
                                "2007" = allbases_farms$`1978_2007`,
                                "2012" = allbases_farms$`1978_2012`),
                         "Starting 1982" =
                           list("1987" = allbases_farms$`1982_1987`,
                                "1992" = allbases_farms$`1982_1992`,
                                "1997" = allbases_farms$`1982_1997`,
                                "2002" = allbases_farms$`1982_2002`,
                                "2007" = allbases_farms$`1982_2007`,
                                "2012" = allbases_farms$`1982_2012`),
                         "Starting 1987" =
                           list("1992" = allbases_farms$`1987_1992`,
                                "1997" = allbases_farms$`1987_1997`,
                                "2002" = allbases_farms$`1987_2002`,
                                "2007" = allbases_farms$`1987_2007`,
                                "2012" = allbases_farms$`1987_2012`),
                         "Starting 1992" =
                           list("1997" = allbases_farms$`1992_1997`,
                                "2002" = allbases_farms$`1992_2002`,
                                "2007" = allbases_farms$`1992_2007`,
                                "2012" = allbases_farms$`1992_2012`),
                         "Starting 1997" =
                           list("2002" = allbases_farms$`1997_2002`,
                                "2007" = allbases_farms$`1997_2007`,
                                "2012" = allbases_farms$`1997_2012`),
                         "Starting 2002" =
                           list("2007" = allbases_farms$`2002_2007`,
                                "2012" = allbases_farms$`2002_2012`),
                         "Starting 2007" =
                           list("2012" = allbases_farms$`2007_2012`))

modelsummary(allbases_ord_farms,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{farms_allbases}Normalized Change in Number of Farms, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/farms_countylevel_allbases.tex", overwrite = TRUE)

allbases_fav <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_farmacrevaluegrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + `", i, "` + farmacrevalue_", i,
                                      " + farmacrevalue_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_fav[[paste0(i, "_", j)]] <- run
  }
}

allbases_ord_fav <- list("Starting 1978" = 
                           list("1982" = allbases_fav$`1978_1982`,
                                "1987" = allbases_fav$`1978_1987`,
                                "1992" = allbases_fav$`1978_1992`,
                                "1997" = allbases_fav$`1978_1997`,
                                "2002" = allbases_fav$`1978_2002`,
                                "2007" = allbases_fav$`1978_2007`,
                                "2012" = allbases_fav$`1978_2012`),
                         "Starting 1982" =
                           list("1987" = allbases_fav$`1982_1987`,
                                "1992" = allbases_fav$`1982_1992`,
                                "1997" = allbases_fav$`1982_1997`,
                                "2002" = allbases_fav$`1982_2002`,
                                "2007" = allbases_fav$`1982_2007`,
                                "2012" = allbases_fav$`1982_2012`),
                         "Starting 1987" =
                           list("1992" = allbases_fav$`1987_1992`,
                                "1997" = allbases_fav$`1987_1997`,
                                "2002" = allbases_fav$`1987_2002`,
                                "2007" = allbases_fav$`1987_2007`,
                                "2012" = allbases_fav$`1987_2012`),
                         "Starting 1992" =
                           list("1997" = allbases_fav$`1992_1997`,
                                "2002" = allbases_fav$`1992_2002`,
                                "2007" = allbases_fav$`1992_2007`,
                                "2012" = allbases_fav$`1992_2012`),
                         "Starting 1997" =
                           list("2002" = allbases_fav$`1997_2002`,
                                "2007" = allbases_fav$`1997_2007`,
                                "2012" = allbases_fav$`1997_2012`),
                         "Starting 2002" =
                           list("2007" = allbases_fav$`2002_2007`,
                                "2012" = allbases_fav$`2002_2012`),
                         "Starting 2007" =
                           list("2012" = allbases_fav$`2007_2012`))

modelsummary(allbases_ord_fav,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{fav_allbases}Normalized Change in Average Farm Acre Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/farmacrevalue_countylevel_allbases.tex", overwrite = TRUE)

allbases_farmland <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_farmlandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + `", i, "` + farmland_", i,
                                      " + farmland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_farmland[[paste0(i, "_", j)]] <- run
  }
}

allbases_ord_farmland <- list("Starting 1978" = 
                           list("1982" = allbases_farmland$`1978_1982`,
                                "1987" = allbases_farmland$`1978_1987`,
                                "1992" = allbases_farmland$`1978_1992`,
                                "1997" = allbases_farmland$`1978_1997`,
                                "2002" = allbases_farmland$`1978_2002`,
                                "2007" = allbases_farmland$`1978_2007`,
                                "2012" = allbases_farmland$`1978_2012`),
                         "Starting 1982" =
                           list("1987" = allbases_farmland$`1982_1987`,
                                "1992" = allbases_farmland$`1982_1992`,
                                "1997" = allbases_farmland$`1982_1997`,
                                "2002" = allbases_farmland$`1982_2002`,
                                "2007" = allbases_farmland$`1982_2007`,
                                "2012" = allbases_farmland$`1982_2012`),
                         "Starting 1987" =
                           list("1992" = allbases_farmland$`1987_1992`,
                                "1997" = allbases_farmland$`1987_1997`,
                                "2002" = allbases_farmland$`1987_2002`,
                                "2007" = allbases_farmland$`1987_2007`,
                                "2012" = allbases_farmland$`1987_2012`),
                         "Starting 1992" =
                           list("1997" = allbases_farmland$`1992_1997`,
                                "2002" = allbases_farmland$`1992_2002`,
                                "2007" = allbases_farmland$`1992_2007`,
                                "2012" = allbases_farmland$`1992_2012`),
                         "Starting 1997" =
                           list("2002" = allbases_farmland$`1997_2002`,
                                "2007" = allbases_farmland$`1997_2007`,
                                "2012" = allbases_farmland$`1997_2012`),
                         "Starting 2002" =
                           list("2007" = allbases_farmland$`2002_2007`,
                                "2012" = allbases_farmland$`2002_2012`),
                         "Starting 2007" =
                           list("2012" = allbases_farmland$`2007_2012`))

modelsummary(allbases_ord_farmland,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{farmland_allbases}Normalized Change in Total Farmland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/farmland_countylevel_allbases.tex", overwrite = TRUE)

allbases_irrland <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_irrlandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + `", i, "` + irrland_", i,
                                      " + irrland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_irrland[[paste0(i, "_", j)]] <- run
  }
}

allbases_ord_irrland <- list("Starting 1978" = 
                                list("1982" = allbases_irrland$`1978_1982`,
                                     "1987" = allbases_irrland$`1978_1987`,
                                     "1992" = allbases_irrland$`1978_1992`,
                                     "1997" = allbases_irrland$`1978_1997`,
                                     "2002" = allbases_irrland$`1978_2002`,
                                     "2007" = allbases_irrland$`1978_2007`,
                                     "2012" = allbases_irrland$`1978_2012`),
                              "Starting 1982" =
                                list("1987" = allbases_irrland$`1982_1987`,
                                     "1992" = allbases_irrland$`1982_1992`,
                                     "1997" = allbases_irrland$`1982_1997`,
                                     "2002" = allbases_irrland$`1982_2002`,
                                     "2007" = allbases_irrland$`1982_2007`,
                                     "2012" = allbases_irrland$`1982_2012`),
                              "Starting 1987" =
                                list("1992" = allbases_irrland$`1987_1992`,
                                     "1997" = allbases_irrland$`1987_1997`,
                                     "2002" = allbases_irrland$`1987_2002`,
                                     "2007" = allbases_irrland$`1987_2007`,
                                     "2012" = allbases_irrland$`1987_2012`),
                              "Starting 1992" =
                                list("1997" = allbases_irrland$`1992_1997`,
                                     "2002" = allbases_irrland$`1992_2002`,
                                     "2007" = allbases_irrland$`1992_2007`,
                                     "2012" = allbases_irrland$`1992_2012`),
                              "Starting 1997" =
                                list("2002" = allbases_irrland$`1997_2002`,
                                     "2007" = allbases_irrland$`1997_2007`,
                                     "2012" = allbases_irrland$`1997_2012`),
                              "Starting 2002" =
                                list("2007" = allbases_irrland$`2002_2007`,
                                     "2012" = allbases_irrland$`2002_2012`),
                              "Starting 2007" =
                                list("2012" = allbases_irrland$`2007_2012`))

modelsummary(allbases_ord_irrland,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{irrland_allbases}Normalized Change in Irrigated Acres, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/irrland_countylevel_allbases.tex", overwrite = TRUE)

allbases_nonirrland <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_nonirrlandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + `", i, "` + nonirrland_", i,
                                      " + nonirrland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_nonirrland[[paste0(i, "_", j)]] <- run
  }
}

allbases_ord_nonirrland <- list("Starting 1978" = 
                               list("1982" = allbases_nonirrland$`1978_1982`,
                                    "1987" = allbases_nonirrland$`1978_1987`,
                                    "1992" = allbases_nonirrland$`1978_1992`,
                                    "1997" = allbases_nonirrland$`1978_1997`,
                                    "2002" = allbases_nonirrland$`1978_2002`,
                                    "2007" = allbases_nonirrland$`1978_2007`,
                                    "2012" = allbases_nonirrland$`1978_2012`),
                             "Starting 1982" =
                               list("1987" = allbases_nonirrland$`1982_1987`,
                                    "1992" = allbases_nonirrland$`1982_1992`,
                                    "1997" = allbases_nonirrland$`1982_1997`,
                                    "2002" = allbases_nonirrland$`1982_2002`,
                                    "2007" = allbases_nonirrland$`1982_2007`,
                                    "2012" = allbases_nonirrland$`1982_2012`),
                             "Starting 1987" =
                               list("1992" = allbases_nonirrland$`1987_1992`,
                                    "1997" = allbases_nonirrland$`1987_1997`,
                                    "2002" = allbases_nonirrland$`1987_2002`,
                                    "2007" = allbases_nonirrland$`1987_2007`,
                                    "2012" = allbases_nonirrland$`1987_2012`),
                             "Starting 1992" =
                               list("1997" = allbases_nonirrland$`1992_1997`,
                                    "2002" = allbases_nonirrland$`1992_2002`,
                                    "2007" = allbases_nonirrland$`1992_2007`,
                                    "2012" = allbases_nonirrland$`1992_2012`),
                             "Starting 1997" =
                               list("2002" = allbases_nonirrland$`1997_2002`,
                                    "2007" = allbases_nonirrland$`1997_2007`,
                                    "2012" = allbases_nonirrland$`1997_2012`),
                             "Starting 2002" =
                               list("2007" = allbases_nonirrland$`2002_2007`,
                                    "2012" = allbases_nonirrland$`2002_2012`),
                             "Starting 2007" =
                               list("2012" = allbases_nonirrland$`2007_2012`))

modelsummary(allbases_ord_nonirrland,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{nonirrland_allbases}Normalized Change in Non-Irrigated Acres, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/nonirrland_countylevel_allbases.tex", overwrite = TRUE)


#########################################
### Splines
#########################################

##countylevel_filt <- countylevel_all |>
 ## filter(!is.na(norm_croplandgrowth_2012_1978) &
          ## !is.na(norm_growth_2012_1978))

##earth_1978_2012 <- earth(norm_croplandgrowth_2012_1978 ~ norm_growth_2012_1978 + `1978` +
                               ##  cropland_1978 + I(cropland_1978^2) + I(`1978`^2) + as.factor(statefip),
                               ##data = countylevel_filt,
                              ## weights = weight_1978,
                              ## linpreds = c("`1978`", "cropland_1978", "statefip"),
                        ## nk = 1000, thresh = 0.0001)

##plotmo(earth_1978_2012, nresponse = "norm",
      ## degree1 = "norm_growth_2012_1978", degree2 = FALSE)

countylevel_spline <- countylevel_all |>
  dplyr::rename("X1978" = `1978`)


spline_3_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                              bs(norm_growth_2012_1978, df = 3) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_3_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                y = "Marginal Effect")
ggsave("Output/Figures/spline_3_1978_2012.png")

spline_4_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                              bs(norm_growth_2012_1978, df = 4) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_4_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_4_1978_2012.png")

spline_5_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                              bs(norm_growth_2012_1978, df = 5) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_5_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_5_1978_2012.png")

spline_6_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                              bs(norm_growth_2012_1978, df = 6) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_6_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_6_1978_2012.png")

spline_7_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                              bs(norm_growth_2012_1978, df = 7) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_7_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_7_1978_2012.png")

spline_10_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                              bs(norm_growth_2012_1978, df = 10) + X1978 +
                              cropland_1978 + cropland_1978^2 + X1978^2 |
                              statefip,
                            data = countylevel_spline,
                            vcov_conley(cutoff = 100),
                            weights =~ weight_1978)
plot_slopes(spline_10_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_10_1978_2012.png")

spline_13_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                               bs(norm_growth_2012_1978, df = 13) + X1978 +
                               cropland_1978 + cropland_1978^2 + X1978^2 |
                               statefip,
                             data = countylevel_spline,
                             vcov_conley(cutoff = 100),
                             weights =~ weight_1978)
plot_slopes(spline_13_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_13_1978_2012.png")

spline_lin_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                               bs(norm_growth_2012_1978, df = 6, degree = 1) + X1978 +
                               cropland_1978 + cropland_1978^2 + X1978^2 |
                               statefip,
                             data = countylevel_spline,
                             vcov_conley(cutoff = 100),
                             weights =~ weight_1978)
plot_slopes(spline_lin_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_lin_1978_2012.png")

spline_quad_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                                bs(norm_growth_2012_1978, df = 7, degree = 2) + X1978 +
                                cropland_1978 + cropland_1978^2 + X1978^2 |
                                statefip,
                              data = countylevel_spline,
                              vcov_conley(cutoff = 100),
                              weights =~ weight_1978)
plot_slopes(spline_quad_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_quad_1978_2012.png")

spline_cubic_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                                bs(norm_growth_2012_1978, df = 8, degree = 3) + X1978 +
                                cropland_1978 + cropland_1978^2 + X1978^2 |
                                statefip,
                              data = countylevel_spline,
                              vcov_conley(cutoff = 100),
                              weights =~ weight_1978)
plot_slopes(spline_cubic_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_cubic_1978_2012.png")

spline_quart_1978_2012 <- feols(norm_croplandgrowth_2012_1978 ~ 
                                bs(norm_growth_2012_1978, df = 9, degree = 4) + X1978 +
                                cropland_1978 + cropland_1978^2 + X1978^2 |
                                statefip,
                              data = countylevel_spline,
                              vcov_conley(cutoff = 100),
                              weights =~ weight_1978)
plot_slopes(spline_quart_1978_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_quart_1978_2012.png")

plot_slopes(fullspec_2012, variables = "norm_growth_2012_1978",
            condition = "norm_growth_2012_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")

## Setting it with different ending years
spline_cubic_1978_2007 <- feols(norm_croplandgrowth_2007_1978 ~ 
                                  bs(norm_growth_2007_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_2007, variables = "norm_growth_2007_1978",
            condition = "norm_growth_2007_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_cubic_1978_2007.png")

spline_cubic_1978_2002 <- feols(norm_croplandgrowth_2002_1978 ~ 
                                  bs(norm_growth_2002_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_2002, variables = "norm_growth_2002_1978",
            condition = "norm_growth_2002_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_cubic_1978_2002.png")

spline_cubic_1978_1997 <- feols(norm_croplandgrowth_1997_1978 ~ 
                                  bs(norm_growth_1997_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1997, variables = "norm_growth_1997_1978",
            condition = "norm_growth_1997_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_cubic_1978_1997.png")

spline_cubic_1978_1992 <- feols(norm_croplandgrowth_1992_1978 ~ 
                                  bs(norm_growth_1992_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1992, variables = "norm_growth_1992_1978",
            condition = "norm_growth_1992_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_cubic_1978_1992.png")

spline_cubic_1978_1987 <- feols(norm_croplandgrowth_1987_1978 ~ 
                                  bs(norm_growth_1987_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1987, variables = "norm_growth_1987_1978",
            condition = "norm_growth_1987_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_cubic_1978_1987.png")

spline_cubic_1978_1982 <- feols(norm_croplandgrowth_1982_1978 ~ 
                                  bs(norm_growth_1982_1978, df = 8, degree = 3) + X1978 +
                                  cropland_1978 + cropland_1978^2 + X1978^2 |
                                  statefip,
                                data = countylevel_spline,
                                vcov_conley(cutoff = 100),
                                weights =~ weight_1978)
plot_slopes(spline_cubic_1978_1982, variables = "norm_growth_1982_1978",
            condition = "norm_growth_1982_1978") + mytheme + labs(x = "Normalized Growth",
                                                                  y = "Marginal Effect")
ggsave("Output/Figures/spline_cubic_1978_1982.png")

##############################
### Interactions
##############################

fullspec_2012_cropint <- feols(norm_croplandgrowth_2012_1978 ~ 
                                 norm_growth_2012_1978*cropland_1978 + `1978` +
                         cropland_1978^2 + `1978`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1978)

fullspec_2012_empint <- feols(norm_croplandgrowth_2012_1978 ~ 
                                norm_growth_2012_1978*`1978` +
                         cropland_1978 + cropland_1978^2 + `1978`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1978)

fullspec_2012_bothint <- feols(norm_croplandgrowth_2012_1978 ~ 
                                 norm_growth_2012_1978*`1978`*cropland_1978 + 
                                 cropland_1978^2 + `1978`^2 |
                         statefip,
                       data = countylevel_all,
                       vcov_conley(cutoff = 100),
                       weights =~ weight_1978)

modelsummary(models = list(fullspec_2012, fullspec_2012_cropint, 
                           fullspec_2012_empint, fullspec_2012_bothint),
             stars = TRUE, fmt = 5,
             coef_map = c("norm_growth_2012_1978" = "Norm. Employment Growth",
                          "norm_growth_2012_1978:cropland_1978" = "Norm. Growth x 1978 Cropland",
                          "norm_growth_2012_1978:1978" = "Norm. Growth x 1978 Employed Pop.",
                          "norm_growth_2012_1978:1978:cropland_1978" = "Norm. Growth x 1978 Cropland x 1978 Employed Pop."),
             gof_function = check_controls(),
             gof_map = gm,
             title = "\\label{crop_cl_het}Normalized Change in Total Cropland, County Level, 1978 to 2012",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  save_tt("Output/Tables/fullspec_countylevel_het.tex", overwrite = TRUE)

## Interacting with the people employed per capita in 1978
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
  theme_tt("resize", width = 0.8) |>
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

check <- countylevel_all |>
  filter(!is.na(precip) & !is.na(norm_growth_2012_1978))

cor(check$norm_growth_2012_1978, check$precip)

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
