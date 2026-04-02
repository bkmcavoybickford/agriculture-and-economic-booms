glance_custom.fixest <- function(x) {
  tibble(
    "Wald" = fitstat(x, "ivwald")[[1]]$stat,
    "First-stage F" = fitstat(x, "ivf")[[1]]$stat
  )
}
gm_inst <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R2", 3,
  "First-stage F", "First-stage F-stat", 3)

###############################
### Doing the shift-share
###############################

allbases_ss <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftshare_", j, "_", i,
                   ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_ss[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_ss <- list("Starting 1978" = 
                           list("1982" = allbases_ss$`1978_1982`,
                                "1987" = allbases_ss$`1978_1987`,
                                "1992" = allbases_ss$`1978_1992`,
                                "1997" = allbases_ss$`1978_1997`,
                                "2002" = allbases_ss$`1978_2002`,
                                "2007" = allbases_ss$`1978_2007`,
                                "2012" = allbases_ss$`1978_2012`),
                         "Starting 1982" =
                           list("1987" = allbases_ss$`1982_1987`,
                                "1992" = allbases_ss$`1982_1992`,
                                "1997" = allbases_ss$`1982_1997`,
                                "2002" = allbases_ss$`1982_2002`,
                                "2007" = allbases_ss$`1982_2007`,
                                "2012" = allbases_ss$`1982_2012`),
                         "Starting 1987" =
                           list("1992" = allbases_ss$`1987_1992`,
                                "1997" = allbases_ss$`1987_1997`,
                                "2002" = allbases_ss$`1987_2002`,
                                "2007" = allbases_ss$`1987_2007`,
                                "2012" = allbases_ss$`1987_2012`),
                         "Starting 1992" =
                           list("1997" = allbases_ss$`1992_1997`,
                                "2002" = allbases_ss$`1992_2002`,
                                "2007" = allbases_ss$`1992_2007`,
                                "2012" = allbases_ss$`1992_2012`),
                         "Starting 1997" =
                           list("2002" = allbases_ss$`1997_2002`,
                                "2007" = allbases_ss$`1997_2007`,
                                "2012" = allbases_ss$`1997_2012`),
                         "Starting 2002" =
                           list("2007" = allbases_ss$`2002_2007`,
                                "2012" = allbases_ss$`2002_2012`),
                         "Starting 2007" =
                           list("2012" = allbases_ss$`2007_2012`))

modelsummary(allbases_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{crop_allbases_ss}Normalized Change in Total Cropland, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_countylevel_allbases.tex", overwrite = TRUE)

## Getting the first stage
allbases_ordered_fs <- list("Starting 1978" = 
                              list("1982" = summary(allbases_ss$`1978_1982`, stage = 1),
                                   "1987" = summary(allbases_ss$`1978_1987`, stage = 1),
                                   "1992" = summary(allbases_ss$`1978_1992`, stage = 1),
                                   "1997" = summary(allbases_ss$`1978_1997`, stage = 1),
                                   "2002" = summary(allbases_ss$`1978_2002`, stage = 1),
                                   "2007" = summary(allbases_ss$`1978_2007`, stage = 1),
                                   "2012" = summary(allbases_ss$`1978_2012`, stage = 1)),
                            "Starting 1982" =
                              list("1987" = summary(allbases_ss$`1982_1987`, stage = 1),
                                   "1992" = summary(allbases_ss$`1982_1992`, stage = 1),
                                   "1997" = summary(allbases_ss$`1982_1997`, stage = 1),
                                   "2002" = summary(allbases_ss$`1982_2002`, stage = 1),
                                   "2007" = summary(allbases_ss$`1982_2007`, stage = 1),
                                   "2012" = summary(allbases_ss$`1982_2012`, stage = 1)),
                            "Starting 1987" =
                              list("1992" = summary(allbases_ss$`1987_1992`, stage = 1),
                                   "1997" = summary(allbases_ss$`1987_1997`, stage = 1),
                                   "2002" = summary(allbases_ss$`1987_2002`, stage = 1),
                                   "2007" = summary(allbases_ss$`1987_2007`, stage = 1),
                                   "2012" = summary(allbases_ss$`1987_2012`, stage = 1)),
                            "Starting 1992" =
                              list("1997" = summary(allbases_ss$`1992_1997`, stage = 1),
                                   "2002" = summary(allbases_ss$`1992_2002`, stage = 1),
                                   "2007" = summary(allbases_ss$`1992_2007`, stage = 1),
                                   "2012" = summary(allbases_ss$`1992_2012`, stage = 1)),
                            "Starting 1997" =
                              list("2002" = summary(allbases_ss$`1997_2002`, stage = 1),
                                   "2007" = summary(allbases_ss$`1997_2007`, stage = 1),
                                   "2012" = summary(allbases_ss$`1997_2012`, stage = 1)),
                            "Starting 2002" =
                              list("2007" = summary(allbases_ss$`2002_2007`, stage = 1),
                                   "2012" = summary(allbases_ss$`2002_2012`, stage = 1)),
                            "Starting 2007" =
                              list("2012" = summary(allbases_ss$`2007_2012`, stage = 1)))

modelsummary(allbases_ordered_fs,
             shape = "rbind",
             stars = TRUE,
             coef_rename = function(x) {
               ifelse(grepl("^norm_shiftshare_", x), "Shift-Share Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{crop_allbases_1s}County-Level Shift-Share, First Stage",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/firststage_countylevel_allbases.tex", overwrite = TRUE)

#######################################
### Getting alternate variables
#######################################

allbases_farms_ss <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_farmsgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftshare_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_farms_ss[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_farms_ss <- list("Starting 1978" = 
                              list("1982" = allbases_farms_ss$`1978_1982`,
                                   "1987" = allbases_farms_ss$`1978_1987`,
                                   "1992" = allbases_farms_ss$`1978_1992`,
                                   "1997" = allbases_farms_ss$`1978_1997`,
                                   "2002" = allbases_farms_ss$`1978_2002`,
                                   "2007" = allbases_farms_ss$`1978_2007`,
                                   "2012" = allbases_farms_ss$`1978_2012`),
                            "Starting 1982" =
                              list("1987" = allbases_farms_ss$`1982_1987`,
                                   "1992" = allbases_farms_ss$`1982_1992`,
                                   "1997" = allbases_farms_ss$`1982_1997`,
                                   "2002" = allbases_farms_ss$`1982_2002`,
                                   "2007" = allbases_farms_ss$`1982_2007`,
                                   "2012" = allbases_farms_ss$`1982_2012`),
                            "Starting 1987" =
                              list("1992" = allbases_farms_ss$`1987_1992`,
                                   "1997" = allbases_farms_ss$`1987_1997`,
                                   "2002" = allbases_farms_ss$`1987_2002`,
                                   "2007" = allbases_farms_ss$`1987_2007`,
                                   "2012" = allbases_farms_ss$`1987_2012`),
                            "Starting 1992" =
                              list("1997" = allbases_farms_ss$`1992_1997`,
                                   "2002" = allbases_farms_ss$`1992_2002`,
                                   "2007" = allbases_farms_ss$`1992_2007`,
                                   "2012" = allbases_farms_ss$`1992_2012`),
                            "Starting 1997" =
                              list("2002" = allbases_farms_ss$`1997_2002`,
                                   "2007" = allbases_farms_ss$`1997_2007`,
                                   "2012" = allbases_farms_ss$`1997_2012`),
                            "Starting 2002" =
                              list("2007" = allbases_farms_ss$`2002_2007`,
                                   "2012" = allbases_farms_ss$`2002_2012`),
                            "Starting 2007" =
                              list("2012" = allbases_farms_ss$`2007_2012`))

modelsummary(allbases_ordered_farms_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{farms_allbases_ss}Normalized Change in Number of Farms, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_farms_countylevel_allbases.tex", overwrite = TRUE)

## Farm acre value
allbases_fav_ss <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_farmacrevaluegrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftshare_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_fav_ss[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_fav_ss <- list("Starting 1978" = 
                                    list("1982" = allbases_fav_ss$`1978_1982`,
                                         "1987" = allbases_fav_ss$`1978_1987`,
                                         "1992" = allbases_fav_ss$`1978_1992`,
                                         "1997" = allbases_fav_ss$`1978_1997`,
                                         "2002" = allbases_fav_ss$`1978_2002`,
                                         "2007" = allbases_fav_ss$`1978_2007`,
                                         "2012" = allbases_fav_ss$`1978_2012`),
                                  "Starting 1982" =
                                    list("1987" = allbases_fav_ss$`1982_1987`,
                                         "1992" = allbases_fav_ss$`1982_1992`,
                                         "1997" = allbases_fav_ss$`1982_1997`,
                                         "2002" = allbases_fav_ss$`1982_2002`,
                                         "2007" = allbases_fav_ss$`1982_2007`,
                                         "2012" = allbases_fav_ss$`1982_2012`),
                                  "Starting 1987" =
                                    list("1992" = allbases_fav_ss$`1987_1992`,
                                         "1997" = allbases_fav_ss$`1987_1997`,
                                         "2002" = allbases_fav_ss$`1987_2002`,
                                         "2007" = allbases_fav_ss$`1987_2007`,
                                         "2012" = allbases_fav_ss$`1987_2012`),
                                  "Starting 1992" =
                                    list("1997" = allbases_fav_ss$`1992_1997`,
                                         "2002" = allbases_fav_ss$`1992_2002`,
                                         "2007" = allbases_fav_ss$`1992_2007`,
                                         "2012" = allbases_fav_ss$`1992_2012`),
                                  "Starting 1997" =
                                    list("2002" = allbases_fav_ss$`1997_2002`,
                                         "2007" = allbases_fav_ss$`1997_2007`,
                                         "2012" = allbases_fav_ss$`1997_2012`),
                                  "Starting 2002" =
                                    list("2007" = allbases_fav_ss$`2002_2007`,
                                         "2012" = allbases_fav_ss$`2002_2012`),
                                  "Starting 2007" =
                                    list("2012" = allbases_fav_ss$`2007_2012`))

modelsummary(allbases_ordered_fav_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{fav_allbases_ss}Normalized Change in Average Farm Acre Value, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_fav_countylevel_allbases.tex", overwrite = TRUE)

## Doing acres of farmland
allbases_farmland_ss <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_farmlandgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftshare_", j, "_", i,
                                      ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
    allbases_farmland_ss[[paste0(i, "_", j)]] <- run
  }
}

allbases_ordered_farmland_ss <- list("Starting 1978" = 
                                    list("1982" = allbases_farmland_ss$`1978_1982`,
                                         "1987" = allbases_farmland_ss$`1978_1987`,
                                         "1992" = allbases_farmland_ss$`1978_1992`,
                                         "1997" = allbases_farmland_ss$`1978_1997`,
                                         "2002" = allbases_farmland_ss$`1978_2002`,
                                         "2007" = allbases_farmland_ss$`1978_2007`,
                                         "2012" = allbases_farmland_ss$`1978_2012`),
                                  "Starting 1982" =
                                    list("1987" = allbases_farmland_ss$`1982_1987`,
                                         "1992" = allbases_farmland_ss$`1982_1992`,
                                         "1997" = allbases_farmland_ss$`1982_1997`,
                                         "2002" = allbases_farmland_ss$`1982_2002`,
                                         "2007" = allbases_farmland_ss$`1982_2007`,
                                         "2012" = allbases_farmland_ss$`1982_2012`),
                                  "Starting 1987" =
                                    list("1992" = allbases_farmland_ss$`1987_1992`,
                                         "1997" = allbases_farmland_ss$`1987_1997`,
                                         "2002" = allbases_farmland_ss$`1987_2002`,
                                         "2007" = allbases_farmland_ss$`1987_2007`,
                                         "2012" = allbases_farmland_ss$`1987_2012`),
                                  "Starting 1992" =
                                    list("1997" = allbases_farmland_ss$`1992_1997`,
                                         "2002" = allbases_farmland_ss$`1992_2002`,
                                         "2007" = allbases_farmland_ss$`1992_2007`,
                                         "2012" = allbases_farmland_ss$`1992_2012`),
                                  "Starting 1997" =
                                    list("2002" = allbases_farmland_ss$`1997_2002`,
                                         "2007" = allbases_farmland_ss$`1997_2007`,
                                         "2012" = allbases_farmland_ss$`1997_2012`),
                                  "Starting 2002" =
                                    list("2007" = allbases_farmland_ss$`2002_2007`,
                                         "2012" = allbases_farmland_ss$`2002_2012`),
                                  "Starting 2007" =
                                    list("2012" = allbases_farmland_ss$`2007_2012`))

modelsummary(allbases_ordered_farmland_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{farmland_allbases_ss}Normalized Change in Acres Farmland, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_farmland_countylevel_allbases.tex", overwrite = TRUE)

#########################################
### Computing the Rotemberg weights
#########################################
modshares_cc <- unique(subset(cbp_mod_shares, year == 1978)$countycode)
master_rot <- countylevel_all |>
  select(norm_growth_2012_1978, norm_croplandgrowth_2012_1978, countycode) |>
  filter(countycode %in% modshares_cc)
master_cc <- unique(master_rot$countycode)

shares_rot <- cbp_mod_shares |>
  ungroup() |>
  filter(year == 1978) |>
  select(countycode, naics4, emppct) |>
  filter(naics4 %in% commonnaics & countycode %in% master_cc) |>
  pivot_wider(values_from = emppct, names_from = naics4) |>
  mutate(across(everything(), ~replace_na(.x, 0)))
Z_rot <- colnames(select(shares_rot, -c(countycode)))
shifts_rot <- cbp_mod_shifts |>
  ungroup() |>
  filter(year == 1978 | year == 2012) |>
  complete(naics4, year, fill = list(emp = 0)) |>
  group_by(naics4) |>
  summarize(growth = if_else(emp[year == 1978] == 0, NA,
                             (emp[year == 2012]-emp[year == 1978])/emp[year == 1978])) |>
  filter(naics4 %in% commonnaics & naics4 %in% Z_rot)

rotweights <- bw(master_rot, "norm_croplandgrowth_2012_1978",
   "norm_growth_2012_1978", controls = NULL, weight = NULL, shares_rot, Z_rot, shifts_rot,
   "growth")

rotweights |>
  select(naics4, alpha) |>
  arrange(desc(alpha)) |>
  head(5) |>
  mutate(Industry = case_when(naics4 == 7225 ~ "Restaurants and Other Eating Places",
                              naics4 == 3152 ~ "Cut and Sew Apparel Manufacturing",
                              naics4 == 7211 ~ "Traveler Accomodation",
                              naics4 == 5613 ~ "Employment Services",
                              naics4 == 3132 ~ "Fabric Mills")) |>
  select("NAICS Code" = naics4, "Rotemberg Weight" = alpha, Industry) |>
  tt(caption = "\\label{rotweights}Rotemberg Weights, 1978-2012") |>
  save_tt("Output/Tables/rotemberg_weights.tex", overwrite = TRUE)

master_rot_nonnorm <- countylevel_all |>
  select(growth_2012_1978, croplandgrowth_2012_1978, countycode) |>
  filter(countycode %in% modshares_cc)

rotweights_nonnorm <- bw(master_rot_nonnorm, "croplandgrowth_2012_1978",
                 "growth_2012_1978", controls = NULL, weight = NULL, shares_rot, Z_rot, shifts_rot,
                 "growth")

rotweights_nonnorm |>
  select(naics4, alpha) |>
  arrange(desc(alpha)) |>
  head(5) |>
  mutate(Industry = case_when(naics4 == 6113 ~ "Colleges, Universities, and Professional Schools",
                              naics4 == 3131 ~ "Fiber, Yarn, and Thread Mills",
                              naics4 == 3159 ~ "Apparel Accessories and Other Apparel Manufacturing",
                              naics4 == 5415 ~ "Computer Systems Design and Related Services",
                              naics4 == 6211 ~ "Offices of Physicians")) |>
  select("NAICS Code" = naics4, "Rotemberg Weight" = alpha, Industry) |>
  tt(caption = "\\label{rotweights_nonnorm}Rotemberg Weights, 1978-2012, Not Normalized") |>
  save_tt("Output/Tables/rotemberg_weights_nonnorm.tex", overwrite = TRUE)

