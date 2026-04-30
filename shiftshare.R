glance_custom.fixest <- function(x) {
  if (!is.null(x$iv)) {
    tibble(
      "Wald" = fitstat(x, "ivwald")[[1]]$stat,
      "First-stage F" = fitstat(x, "ivf")[[1]]$stat
    )
  } else {
    tibble()
  }
}

gm_inst <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "First-stage F", "First-stage F-stat", 3)

###############################
### Doing the shift-share
###############################

run_allbases_ss <- function(var) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0("feols(norm_", var, "growth_", j, "_", i, 
                                        " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                        " ~ norm_shiftshare_", j, "_", i,
                                        ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
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
  
  assign(paste0("allbases_", var, "_ss"),           result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_ss"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
}

run_allbases_ss("cropland")


modelsummary(allbases_cropland_ordered_ss,
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
                              list("1982" = summary(allbases_cropland_ss$`1978_1982`, stage = 1),
                                   "1987" = summary(allbases_cropland_ss$`1978_1987`, stage = 1),
                                   "1992" = summary(allbases_cropland_ss$`1978_1992`, stage = 1),
                                   "1997" = summary(allbases_cropland_ss$`1978_1997`, stage = 1),
                                   "2002" = summary(allbases_cropland_ss$`1978_2002`, stage = 1),
                                   "2007" = summary(allbases_cropland_ss$`1978_2007`, stage = 1),
                                   "2012" = summary(allbases_cropland_ss$`1978_2012`, stage = 1)),
                            "Starting 1982" =
                              list("1987" = summary(allbases_cropland_ss$`1982_1987`, stage = 1),
                                   "1992" = summary(allbases_cropland_ss$`1982_1992`, stage = 1),
                                   "1997" = summary(allbases_cropland_ss$`1982_1997`, stage = 1),
                                   "2002" = summary(allbases_cropland_ss$`1982_2002`, stage = 1),
                                   "2007" = summary(allbases_cropland_ss$`1982_2007`, stage = 1),
                                   "2012" = summary(allbases_cropland_ss$`1982_2012`, stage = 1)),
                            "Starting 1987" =
                              list("1992" = summary(allbases_cropland_ss$`1987_1992`, stage = 1),
                                   "1997" = summary(allbases_cropland_ss$`1987_1997`, stage = 1),
                                   "2002" = summary(allbases_cropland_ss$`1987_2002`, stage = 1),
                                   "2007" = summary(allbases_cropland_ss$`1987_2007`, stage = 1),
                                   "2012" = summary(allbases_cropland_ss$`1987_2012`, stage = 1)),
                            "Starting 1992" =
                              list("1997" = summary(allbases_cropland_ss$`1992_1997`, stage = 1),
                                   "2002" = summary(allbases_cropland_ss$`1992_2002`, stage = 1),
                                   "2007" = summary(allbases_cropland_ss$`1992_2007`, stage = 1),
                                   "2012" = summary(allbases_cropland_ss$`1992_2012`, stage = 1)),
                            "Starting 1997" =
                              list("2002" = summary(allbases_cropland_ss$`1997_2002`, stage = 1),
                                   "2007" = summary(allbases_cropland_ss$`1997_2007`, stage = 1),
                                   "2012" = summary(allbases_cropland_ss$`1997_2012`, stage = 1)),
                            "Starting 2002" =
                              list("2007" = summary(allbases_cropland_ss$`2002_2007`, stage = 1),
                                   "2012" = summary(allbases_cropland_ss$`2002_2012`, stage = 1)),
                            "Starting 2007" =
                              list("2012" = summary(allbases_cropland_ss$`2007_2012`, stage = 1)))

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

run_allbases_ss("farms")

modelsummary(allbases_farms_ordered_ss,
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

run_allbases_ss("farmacrevalue")

modelsummary(allbases_farmacrevalue_ordered_ss,
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
run_allbases_ss("farmland")

modelsummary(allbases_farmland_ordered_ss,
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

## Doing acres of irrigated land
run_allbases_ss("irrland")

modelsummary(allbases_irrland_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{irrland_allbases_ss}Normalized Change in Irrigated Acres, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_irrland_countylevel_allbases.tex", overwrite = TRUE)


## Doing acres of nonirrigated land
run_allbases_ss("nonirrland")

modelsummary(allbases_nonirrland_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{nonirrland_allbases_ss}Normalized Change in Non-Irrigated Acres, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_nonirrland_countylevel_allbases.tex", overwrite = TRUE)

## Doing farm size
run_allbases_ss("farmsize")

modelsummary(allbases_farmsize_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{farmsize_allbases_ss}Normalized Change in Average Farm Size, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_farmsize_countylevel_allbases.tex", overwrite = TRUE)

## Doing production value
run_allbases_ss("prodval")

modelsummary(allbases_prodval_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{prodval_allbases_ss}Normalized Change in Agricultural Production Value, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_prodval_countylevel_allbases.tex", overwrite = TRUE)

## Doing crop production value
run_allbases_ss("cropval")

modelsummary(allbases_cropval_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{cropval_allbases_ss}Normalized Change in Crop Production Value, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_cropval_countylevel_allbases.tex", overwrite = TRUE)



## Doing production value per acre
run_allbases_ss("prodvalacre")

modelsummary(allbases_prodvalacre_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{prodvalacre_allbases_ss}Normalized Change in Agricultural Production Value per Acre, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_prodvalacre_countylevel_allbases.tex", overwrite = TRUE)

## Doing production value per worker
run_allbases_ss("prodvalworker")

modelsummary(allbases_prodvalworker_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{prodvalworker_allbases_ss}Normalized Change in Agricultural Production Value per Worker, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_prodvalworker_countylevel_allbases.tex", overwrite = TRUE)

## Doing production value
run_allbases_ss("laborintensity")

modelsummary(allbases_laborintensity_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{laborintensity_allbases_ss}Normalized Change in Ag Workers per Acre Farmland, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_laborintensity_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("agemp")

modelsummary(allbases_agemp_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{agemp_allbases_ss}Normalized Change in Agricultural Workers, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_agemp_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("croplandratio")

modelsummary(allbases_croplandratio_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{croplandratio_allbases_ss}Normalized Change in Cropland to Farmland Ratio, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_croplandratio_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("cropvalratio")

modelsummary(allbases_cropvalratio_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{cropvalratio_allbases_ss}Normalized Change in Crop to Farm Production Value Ratio, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_cropvalratio_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("machinery")

modelsummary(allbases_machinery_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{machinery_allbases_ss}Normalized Change in Machinery and Equipment Value, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_machinery_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("machineryper")

modelsummary(allbases_machineryper_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{machineryper_allbases_ss}Normalized Change in Machinery and Equipment to Product Value Ratio, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_machineryper_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("fertilizer")

modelsummary(allbases_fertilizer_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{fertilizer_allbases_ss}Normalized Change in Total Fertilizer Value, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_fertilizer_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("fertilizerper")

modelsummary(allbases_fertilizerper_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{fertilizerper_allbases_ss}Normalized Change in Fertilizer to Product Value Ratio, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_fertilizerper_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("petroleumper")

modelsummary(allbases_petroleumper_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{petroleumper_allbases_ss}Normalized Change in Total Fuel to Product Value Ratio, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_petroleumper_countylevel_allbases.tex", overwrite = TRUE)

run_allbases_ss("petroleum")

modelsummary(allbases_petroleum_ordered_ss,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{petroleum_allbases_ss}Normalized Change in Total Fuel etc. Value, County Level, Shift-Share",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_petroleum_countylevel_allbases.tex", overwrite = TRUE)

#########################################
### Computing the Rotemberg weights
#########################################

modshares_cc <- unique(subset(cbp_mod_shares, year == 1978)$countycode)
master_rot <- countylevel_all |>
  select(norm_growth_2012_1978, norm_prodvalgrowth_2012_1978, countycode) |>
  filter(countycode %in% modshares_cc)
master_cc <- unique(master_rot$countycode)

shares_rot <- cbp_mod_shares |>
  ungroup() |>
  filter(year == 1978) |>
  select(countycode, naics4, emppct) |>
  filter(naics4 %in% commonnaics & countycode %in% master_cc) |>
  complete(countycode = master_cc, naics4 = commonnaics,
           fill = list(emppct = 0)) |>
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

rotweights <- bw(master_rot, "norm_prodvalgrowth_2012_1978",
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
                              naics4 == 3132 ~ "Fabric Mills",
                              naics4 == 6241 ~ "Individual and Family Services",
                              naics4 == 3162 ~ "Footwear Manufacturing",
                              naics4 == 6216 ~ "Home Health Care Services",
                              naics4 == 6213 ~ "Offices of Other Health Practitioners")) |>
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

#################################
### Robustness checks
#################################

## For tradables (NAICS code 3) only

allbases_ss_tradable <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftsharetradable_", j, "_", i,
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
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{crop_allbases_ss_tradable}Normalized Change in Total Cropland, County Level, Shift-Share of Manufacturing Only",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_countylevel_allbases_tradable.tex", overwrite = TRUE)

allbases_prodval_ss_tradable <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftsharetradable_", j, "_", i,
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
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{prodval_allbases_ss_tradable}Normalized Change in Agricultural Production Value, County Level, Shift-Share of Manufacturing Only",
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
      run <- eval(parse(text = paste0("feols(norm_farmacrevaluegrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftsharetradable_", j, "_", i,
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
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{farmacrevalue_allbases_ss_tradable}Normalized Change in Value for an Acre of Farmland, County Level, Shift-Share of Manufacturing Only",
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
               ifelse(grepl("^norm_shiftshare", x), "Shift-Share Norm. Employment Growth", x)
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
      run <- eval(parse(text = paste0("feols(norm_croplandgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftsharenaics2_", j, "_", i,
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
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{crop_allbases_ss_naics2}Normalized Change in Total Cropland, County Level, Shift-Share with 2-Digit NAICS Codes",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_countylevel_allbases_naics2.tex", overwrite = TRUE)

allbases_prodval_ss_naics2 <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (i >= j) print("NA") else
      run <- eval(parse(text = paste0("feols(norm_prodvalgrowth_", j, "_", i, 
                                      " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                      " ~ norm_shiftsharenaics2_", j, "_", i,
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
             coef_omit = "^(?!fit_norm_growth_)",
             coef_rename = function(x) {
               ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
             },
             gof_map = gm_inst,
             title = "\\label{prodval_allbases_ss_naics2}Normalized Change in Agricultural Production Value, County Level, Shift-Share with 2-Digit NAICS Codes",
             output = "tinytable",
             escape = FALSE) |>
  style_tt(i = "groupi", bold = TRUE) |>
  theme_tt(theme = "resize", multipage = TRUE) |>
  save_tt("Output/Tables/shiftshare_prodval_countylevel_allbases_naics2.tex", overwrite = TRUE)


###################################
### Doing quintiles
###################################

for (q in 1:5) {
  allbases_farmacrevalue_percap_quint_ss <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) print("NA") else
        run <- eval(parse(text = paste0("feols(norm_farmacrevaluegrowth_", j, "_", i, 
                                        " ~ 1 | statefip | norm_growth_", j, "_", i, 
                                        " ~ norm_shiftshare_", j, "_", i,
                                        ", data = subset(countylevel_all, quintile_emppercap ==", q, 
                                        "), vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
      allbases_farmacrevalue_percap_quint_ss[[paste0(i, "_", j)]] <- run
    }
  }
  
  allbases_farmacrevalue_ordered_percap_quint_ss <- list(
    "Starting 1978" = list("1982" = allbases_farmacrevalue_percap_quint_ss$`1978_1982`,
                           "1987" = allbases_farmacrevalue_percap_quint_ss$`1978_1987`,
                           "1992" = allbases_farmacrevalue_percap_quint_ss$`1978_1992`,
                           "1997" = allbases_farmacrevalue_percap_quint_ss$`1978_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint_ss$`1978_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint_ss$`1978_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint_ss$`1978_2012`),
    "Starting 1982" = list("1987" = allbases_farmacrevalue_percap_quint_ss$`1982_1987`,
                           "1992" = allbases_farmacrevalue_percap_quint_ss$`1982_1992`,
                           "1997" = allbases_farmacrevalue_percap_quint_ss$`1982_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint_ss$`1982_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint_ss$`1982_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint_ss$`1982_2012`),
    "Starting 1987" = list("1992" = allbases_farmacrevalue_percap_quint_ss$`1987_1992`,
                           "1997" = allbases_farmacrevalue_percap_quint_ss$`1987_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint_ss$`1987_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint_ss$`1987_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint_ss$`1987_2012`),
    "Starting 1992" = list("1997" = allbases_farmacrevalue_percap_quint_ss$`1992_1997`,
                           "2002" = allbases_farmacrevalue_percap_quint_ss$`1992_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint_ss$`1992_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint_ss$`1992_2012`),
    "Starting 1997" = list("2002" = allbases_farmacrevalue_percap_quint_ss$`1997_2002`,
                           "2007" = allbases_farmacrevalue_percap_quint_ss$`1997_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint_ss$`1997_2012`),
    "Starting 2002" = list("2007" = allbases_farmacrevalue_percap_quint_ss$`2002_2007`,
                           "2012" = allbases_farmacrevalue_percap_quint_ss$`2002_2012`),
    "Starting 2007" = list("2012" = allbases_farmacrevalue_percap_quint_ss$`2007_2012`))
  
  assign(paste0("allbases_farmacrevalue_ordered_percap_quint_ss",q), allbases_farmacrevalue_ordered_percap_quint_ss)
  
  modelsummary(allbases_farmacrevalue_ordered_percap_quint_ss,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!fit_norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^fit_norm_growth_", x), "Fitted. Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{farmacrevalue_allbases_percap_ss_q", q, 
                              "}Normalized Change in Farm Acre Value, County Level Shift-Share, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/farmacrevalue_countylevel_empquint_ss", q, ".tex"), overwrite = TRUE)
}