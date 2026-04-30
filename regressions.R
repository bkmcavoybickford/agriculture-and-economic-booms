## Running the regressions on the data


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


########################################
### Getting the full specification function
########################################

run_allbases <- function(var) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0(
          "feols(norm_", var, "growth_", j, "_", i,
          " ~ norm_growth_", j, "_", i, " + `", i, "` + ", var, "_", i,
          " + ", var, "_", i, "^2 + `", i, "`^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
        )))
        result[[paste0(i, "_", j)]] <- run
      }
    }
  }
  
  starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
  ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_", var),           result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
}

run_allbases("cropland")



modelsummary(allbases_cropland_ordered,
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
  save_tt("Output/Tables/cropland_countylevel_allbases.tex", overwrite = TRUE)

modelsummary(models = list("1982" = allbases_cropland$`1978_1982`,
                           "1987" = allbases_cropland$`1978_1987`,
                           "1992" = allbases_cropland$`1978_1992`,
                           "1997" = allbases_cropland$`1978_1997`,
                           "2002" = allbases_cropland$`1978_2002`,
                           "2007" = allbases_cropland$`1978_2007`,
                           "2012" = allbases_cropland$`1978_2012`),
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

modelplot(models = allbases_cropland_ordered$`Starting 1978`,
          coef_map = c("norm_growth_2012_1978" = "2012",
                       "norm_growth_2007_1978" = "2007",
                       "norm_growth_2002_1978" = "2002",
                       "norm_growth_1997_1978" = "1997",
                       "norm_growth_1992_1978" = "1992",
                       "norm_growth_1987_1978" = "1987",
                       "norm_growth_1982_1978" = "1982")) +
  labs(x = "Effect of Change in Employed Pop. on Change in Cropland, 1978 Basis") +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none") + geom_vline(xintercept = 0)
ggsave("Output/figures/fullspec_countylevel.png", width = 6, height = 4)

modelsummary(models = list("1997" = allbases_cropland$`1992_1997`,
                           "2002" = allbases_cropland$`1992_2002`,
                           "2007" = allbases_cropland$`1992_2007`,
                           "2012" = allbases_cropland$`1992_2012`),
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

###################################
### Regional heterogeneity
###################################

## Getting it by region for cropland
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
  mutate(Year = case_when(term == "norm_growth_1982_1978" ~ 1982,
                          term == "norm_growth_1987_1978" ~ 1987,
                          term == "norm_growth_1992_1978" ~ 1992,
                          term == "norm_growth_1997_1978" ~ 1997,
                          term == "norm_growth_2002_1978" ~ 2002,
                          term == "norm_growth_2007_1978" ~ 2007,
                          term == "norm_growth_2012_1978" ~ 2012,
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

## And for production value
regionregs_prodval <- list()
for (i in c("Northeast", "Midwest", "South", "West")){
  regionregs_prodval[[paste0("1982_", i)]] <- feols(norm_prodvalgrowth_1982_1978 ~ norm_growth_1982_1978 + `1978` +
                                              prodval_1978 + prodval_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs_prodval[[paste0("1987_", i)]] <- feols(norm_prodvalgrowth_1987_1978 ~ norm_growth_1987_1978 + `1978` +
                                              prodval_1978 + prodval_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs_prodval[[paste0("1992_", i)]] <- feols(norm_prodvalgrowth_1992_1978 ~ norm_growth_1992_1978 + `1978` +
                                              prodval_1978 + prodval_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs_prodval[[paste0("1997_", i)]] <- feols(norm_prodvalgrowth_1997_1978 ~ norm_growth_1997_1978 + `1978` +
                                              prodval_1978 + prodval_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs_prodval[[paste0("2002_", i)]] <- feols(norm_prodvalgrowth_2002_1978 ~ norm_growth_2002_1978 + `1978` +
                                              prodval_1978 + prodval_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs_prodval[[paste0("2007_", i)]] <- feols(norm_prodvalgrowth_2007_1978 ~ norm_growth_2007_1978 + `1978` +
                                              prodval_1978 + prodval_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
  
  regionregs_prodval[[paste0("2012_", i)]] <- feols(norm_prodvalgrowth_2012_1978 ~ norm_growth_2012_1978 + `1978` +
                                              prodval_1978 + prodval_1978^2 + `1978`^2 |
                                              statefip,
                                            data = subset(countylevel_all, region == i),
                                            vcov_conley(cutoff = 100),
                                            weights =~ weight_1978)
}

results <- bind_rows(
  lapply(names(regionregs_prodval), function(nm) {
    broom::tidy(regionregs_prodval[[nm]]) %>%
      mutate(model = nm)
  })
) |>
  mutate(Year = case_when(term == "norm_growth_1982_1978" ~ 1982,
                          term == "norm_growth_1987_1978" ~ 1987,
                          term == "norm_growth_1992_1978" ~ 1992,
                          term == "norm_growth_1997_1978" ~ 1997,
                          term == "norm_growth_2002_1978" ~ 2002,
                          term == "norm_growth_2007_1978" ~ 2007,
                          term == "norm_growth_2012_1978" ~ 2012,
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

names(regionregs_prodval) <- str_sub(names(regionregs_prodval), 6, -1)

p <- modelplot(models = regionregs_prodval,
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
ggsave("Output/figures/fullspec_byreg_prodval.png", width = 6, height = 8)

## Getting the production value by state
stateregs_prodval <- list()
for (i in unique(countylevel_all$statefip)){
  stateregs_prodval[[paste0(i)]] <- feols(norm_prodvalgrowth_2012_1978 ~ norm_growth_2012_1978 + `1978` +
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
  filter(term == "norm_growth_2012_1978") |>
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

## Getting the farm acre value
stateregs_farmacrevalue <- list()
for (i in unique(countylevel_all$statefip)){
  stateregs_farmacrevalue[[paste0(i)]] <- feols(norm_farmacrevaluegrowth_2012_1978 ~ norm_growth_2012_1978 + `1978` +
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
  filter(term == "norm_growth_2012_1978") |>
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

####################################
### Doing the same for the other variables
####################################

run_allbases("farms")

modelsummary(allbases_farms_ordered,
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

run_allbases("farmacrevalue")

modelsummary(allbases_farmacrevalue_ordered,
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

run_allbases("farmland")

modelsummary(allbases_farmland_ordered,
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

run_allbases("irrland")

modelsummary(allbases_irrland_ordered,
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

run_allbases("nonirrland")

modelsummary(allbases_nonirrland_ordered,
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

run_allbases("farmsize")

modelsummary(allbases_farmsize_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{farmsize_allbases}Normalized Change in Farm Size (Acres), County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/farmsize_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("prodval")

modelsummary(allbases_prodval_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases}Normalized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("cropval")

modelsummary(allbases_cropval_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{cropval_allbases}Normalized Change in Crop Production Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/cropval_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("prodvalacre")

modelsummary(allbases_prodvalacre_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodvalacre_allbases}Normalized Change in Agricultural Production Value per Acre Farmland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodvalacre_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("prodvalworker")

modelsummary(allbases_prodvalworker_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodvalworker_allbases}Normalized Change in Agricultural Production Value per Ag Worker, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodvalworker_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("laborintensity")

modelsummary(allbases_laborintensity_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{laborintensity_allbases}Normalized Change in Agricultural Workers per Acre Farmland, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/laborintensity_countylevel_allbases.tex", overwrite = TRUE)

## Getting the agricultural workforce by itself
run_allbases("agemp")

modelsummary(allbases_agemp_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{agemp_allbases}Normalized Change in Agricultural Workers, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/agemp_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("croplandratio")

modelsummary(allbases_croplandratio_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplandratio_allbases}Normalized Change in Cropland to Farmland Ratio, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/croplandratio_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("cropvalratio")

modelsummary(allbases_cropvalratio_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{cropvalratio_allbases}Normalized Change in Crop to Farm Production Value Ratio, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/cropvalratio_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("machinery")

modelsummary(allbases_machinery_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{machinery_allbases}Normalized Change in Total Machinery and Equipment Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/machinery_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("machineryper")

modelsummary(allbases_machineryper_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{machineryper_allbases}Normalized Change in Machinery and Equipment to Product Value Ratio, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/machineryper_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("fertilizer")

modelsummary(allbases_fertilizer_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{fertilizer_allbases}Normalized Change in Total Fertilizer Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fertilizer_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("fertilizerper")

modelsummary(allbases_fertilizerper_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{fertilizerper_allbases}Normalized Change in Fertilizer to Product Value Ratio, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fertilizerper_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("petroleum")

modelsummary(allbases_petroleum_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{petroleum_allbases}Normalized Change in Total Fuel Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/petroleum_countylevel_allbases.tex", overwrite = TRUE)

run_allbases("petroleumper")

modelsummary(allbases_petroleumper_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{petroleumper_allbases}Normalized Change in Fuel to Product Value Ratio, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/petroleumper_countylevel_allbases.tex", overwrite = TRUE)


#########################################
### Splines
#########################################

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
                                                y = "Marginal Effect",
                                                subtitle = paste0("AIC = ", round(AIC(spline_3_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_4_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                          subtitle = paste0("AIC = ", round(AIC(spline_5_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_6_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_7_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_10_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_13_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_lin_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_quad_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_2012), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_quart_1978_2012), 2)))
ggsave("Output/Figures/spline_quart_1978_2012.png")


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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_2007), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_2002), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1997), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1992), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1987), 2)))
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
                                                                  y = "Marginal Effect",
                                                                  subtitle = paste0("AIC = ", round(AIC(spline_cubic_1978_1982), 2)))
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

modelsummary(models = list(allbases_cropland$`1978_2012`, fullspec_2012_cropint, 
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


##########################
### Robustness
##########################

## Alternate growth measures
allbases_nonag <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growthnonag_", j, "_", i, " + `", i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_nonag[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_nonag <- list("Starting 1978" = 
                                     list("1982" = allbases_nonag$`1978_1982`,
                                          "1987" = allbases_nonag$`1978_1987`,
                                          "1992" = allbases_nonag$`1978_1992`,
                                          "1997" = allbases_nonag$`1978_1997`,
                                          "2002" = allbases_nonag$`1978_2002`,
                                          "2007" = allbases_nonag$`1978_2007`,
                                          "2012" = allbases_nonag$`1978_2012`),
                                   "Starting 1982" =
                                     list("1987" = allbases_nonag$`1982_1987`,
                                          "1992" = allbases_nonag$`1982_1992`,
                                          "1997" = allbases_nonag$`1982_1997`,
                                          "2002" = allbases_nonag$`1982_2002`,
                                          "2007" = allbases_nonag$`1982_2007`,
                                          "2012" = allbases_nonag$`1982_2012`),
                                   "Starting 1987" =
                                     list("1992" = allbases_nonag$`1987_1992`,
                                          "1997" = allbases_nonag$`1987_1997`,
                                          "2002" = allbases_nonag$`1987_2002`,
                                          "2007" = allbases_nonag$`1987_2007`,
                                          "2012" = allbases_nonag$`1987_2012`),
                                   "Starting 1992" =
                                     list("1997" = allbases_nonag$`1992_1997`,
                                          "2002" = allbases_nonag$`1992_2002`,
                                          "2007" = allbases_nonag$`1992_2007`,
                                          "2012" = allbases_nonag$`1992_2012`),
                                   "Starting 1997" =
                                     list("2002" = allbases_nonag$`1997_2002`,
                                          "2007" = allbases_nonag$`1997_2007`,
                                          "2012" = allbases_nonag$`1997_2012`),
                                   "Starting 2002" =
                                     list("2007" = allbases_nonag$`2002_2007`,
                                          "2012" = allbases_nonag$`2002_2012`),
                                   "Starting 2007" =
                                     list("2012" = allbases_nonag$`2007_2012`))

modelsummary(allbases_ordered_nonag,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growthnonag_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growthnonag_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_nonag}Normalized Change in Total Cropland, County Level, Incl. More Industries",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases_nonag.tex", overwrite = TRUE)

## No weights
allbases_nowt <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + `", i, "` + cropland_", i,
                                      " + cropland_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100))")))
    allbases_nowt[[paste0(i, "_", j)]] <- run
  }
}

allbases_nowt_ordered <- list("Starting 1978" = 
                           list("1982" = allbases_nowt$`1978_1982`,
                                "1987" = allbases_nowt$`1978_1987`,
                                "1992" = allbases_nowt$`1978_1992`,
                                "1997" = allbases_nowt$`1978_1997`,
                                "2002" = allbases_nowt$`1978_2002`,
                                "2007" = allbases_nowt$`1978_2007`,
                                "2012" = allbases_nowt$`1978_2012`),
                         "Starting 1982" =
                           list("1987" = allbases_nowt$`1982_1987`,
                                "1992" = allbases_nowt$`1982_1992`,
                                "1997" = allbases_nowt$`1982_1997`,
                                "2002" = allbases_nowt$`1982_2002`,
                                "2007" = allbases_nowt$`1982_2007`,
                                "2012" = allbases_nowt$`1982_2012`),
                         "Starting 1987" =
                           list("1992" = allbases_nowt$`1987_1992`,
                                "1997" = allbases_nowt$`1987_1997`,
                                "2002" = allbases_nowt$`1987_2002`,
                                "2007" = allbases_nowt$`1987_2007`,
                                "2012" = allbases_nowt$`1987_2012`),
                         "Starting 1992" =
                           list("1997" = allbases_nowt$`1992_1997`,
                                "2002" = allbases_nowt$`1992_2002`,
                                "2007" = allbases_nowt$`1992_2007`,
                                "2012" = allbases_nowt$`1992_2012`),
                         "Starting 1997" =
                           list("2002" = allbases_nowt$`1997_2002`,
                                "2007" = allbases_nowt$`1997_2007`,
                                "2012" = allbases_nowt$`1997_2012`),
                         "Starting 2002" =
                           list("2007" = allbases_nowt$`2002_2007`,
                                "2012" = allbases_nowt$`2002_2012`),
                         "Starting 2007" =
                           list("2012" = allbases_nowt$`2007_2012`))

modelsummary(allbases_nowt_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{crop_allbases_nowt}Normalized Change in Total Cropland, County Level And Unweighted",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/fullspec_countylevel_allbases_nowt.tex", overwrite = TRUE)

## No weights for production value
allbases_prodval_nowt <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ norm_growth_", j, "_", i, " + `", i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
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
             coef_omit = "^(?!norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_nowt}Normalized Change in Agricultural Production Value, County Level And Unweighted",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_nowt.tex", overwrite = TRUE)


## Percentiles for production value
allbases_prodval_pctl <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                      " ~ pctl_growth_", j, "_", i, " + `", i, "` + prodval_", i,
                                      " + prodval_", i, "^2 + `", i, "`^2 | statefip,
                   data = countylevel_all, vcov_conley(cutoff = 100),
                                      weights =~ weight_", i, ")")))
    allbases_prodval_pctl[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_pctl_ordered <- list("Starting 1978" = 
                                        list("1982" = allbases_prodval_pctl$`1978_1982`,
                                             "1987" = allbases_prodval_pctl$`1978_1987`,
                                             "1992" = allbases_prodval_pctl$`1978_1992`,
                                             "1997" = allbases_prodval_pctl$`1978_1997`,
                                             "2002" = allbases_prodval_pctl$`1978_2002`,
                                             "2007" = allbases_prodval_pctl$`1978_2007`,
                                             "2012" = allbases_prodval_pctl$`1978_2012`),
                                      "Starting 1982" =
                                        list("1987" = allbases_prodval_pctl$`1982_1987`,
                                             "1992" = allbases_prodval_pctl$`1982_1992`,
                                             "1997" = allbases_prodval_pctl$`1982_1997`,
                                             "2002" = allbases_prodval_pctl$`1982_2002`,
                                             "2007" = allbases_prodval_pctl$`1982_2007`,
                                             "2012" = allbases_prodval_pctl$`1982_2012`),
                                      "Starting 1987" =
                                        list("1992" = allbases_prodval_pctl$`1987_1992`,
                                             "1997" = allbases_prodval_pctl$`1987_1997`,
                                             "2002" = allbases_prodval_pctl$`1987_2002`,
                                             "2007" = allbases_prodval_pctl$`1987_2007`,
                                             "2012" = allbases_prodval_pctl$`1987_2012`),
                                      "Starting 1992" =
                                        list("1997" = allbases_prodval_pctl$`1992_1997`,
                                             "2002" = allbases_prodval_pctl$`1992_2002`,
                                             "2007" = allbases_prodval_pctl$`1992_2007`,
                                             "2012" = allbases_prodval_pctl$`1992_2012`),
                                      "Starting 1997" =
                                        list("2002" = allbases_prodval_pctl$`1997_2002`,
                                             "2007" = allbases_prodval_pctl$`1997_2007`,
                                             "2012" = allbases_prodval_pctl$`1997_2012`),
                                      "Starting 2002" =
                                        list("2007" = allbases_prodval_pctl$`2002_2007`,
                                             "2012" = allbases_prodval_pctl$`2002_2012`),
                                      "Starting 2007" =
                                        list("2012" = allbases_prodval_pctl$`2007_2012`))

modelsummary(allbases_prodval_pctl_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!pctl_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^pctl_growth_", x), "Employment Growth, Percentile Rank", x)
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_pctl}Percentile Rank Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_pctl.tex", overwrite = TRUE)

allbases_prodval_simple <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(prodvalgrowth_", j, "_", i, 
                                      " ~ growth_", j, "_", i, ",
                   data = countylevel_all, vcov_conley(cutoff = 100))")))
    allbases_prodval_simple[[paste0(i, "_", j)]] <- run
  }
}

allbases_prodval_simple_ordered <- list("Starting 1978" = 
                                        list("1982" = allbases_prodval_simple$`1978_1982`,
                                             "1987" = allbases_prodval_simple$`1978_1987`,
                                             "1992" = allbases_prodval_simple$`1978_1992`,
                                             "1997" = allbases_prodval_simple$`1978_1997`,
                                             "2002" = allbases_prodval_simple$`1978_2002`,
                                             "2007" = allbases_prodval_simple$`1978_2007`,
                                             "2012" = allbases_prodval_simple$`1978_2012`),
                                      "Starting 1982" =
                                        list("1987" = allbases_prodval_simple$`1982_1987`,
                                             "1992" = allbases_prodval_simple$`1982_1992`,
                                             "1997" = allbases_prodval_simple$`1982_1997`,
                                             "2002" = allbases_prodval_simple$`1982_2002`,
                                             "2007" = allbases_prodval_simple$`1982_2007`,
                                             "2012" = allbases_prodval_simple$`1982_2012`),
                                      "Starting 1987" =
                                        list("1992" = allbases_prodval_simple$`1987_1992`,
                                             "1997" = allbases_prodval_simple$`1987_1997`,
                                             "2002" = allbases_prodval_simple$`1987_2002`,
                                             "2007" = allbases_prodval_simple$`1987_2007`,
                                             "2012" = allbases_prodval_simple$`1987_2012`),
                                      "Starting 1992" =
                                        list("1997" = allbases_prodval_simple$`1992_1997`,
                                             "2002" = allbases_prodval_simple$`1992_2002`,
                                             "2007" = allbases_prodval_simple$`1992_2007`,
                                             "2012" = allbases_prodval_simple$`1992_2012`),
                                      "Starting 1997" =
                                        list("2002" = allbases_prodval_simple$`1997_2002`,
                                             "2007" = allbases_prodval_simple$`1997_2007`,
                                             "2012" = allbases_prodval_simple$`1997_2012`),
                                      "Starting 2002" =
                                        list("2007" = allbases_prodval_simple$`2002_2007`,
                                             "2012" = allbases_prodval_simple$`2002_2012`),
                                      "Starting 2007" =
                                        list("2012" = allbases_prodval_simple$`2007_2012`))

modelsummary(allbases_prodval_simple_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^growth_", x), "Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_simple}Non-Normalized Change in Agricultural Production Value, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_simple.tex", overwrite = TRUE)

## Doing non-ag for production value
allbases_prodval_nonag <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ norm_growthnonag_", j, "_", i, " + `", i, "` + prodval_", i,
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
             coef_omit = "^(?!norm_growthnonag_)",
             coef_rename = function(x) {
               ifelse(grepl("^norm_growthnonag_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{prodval_allbases_nonag}Normalized Change in Agricultural Production Value, County Level, Incl. More Industries",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodval_countylevel_allbases_nonag.tex", overwrite = TRUE)
