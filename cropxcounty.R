## This is for the crop-x-county level regressions



##################################
### Getting them in 1978-2012
##################################

crops_1978_2012 <- list()
for (i in names_all) {
  subset <- acres_growth |>
    filter(crop == i & year_from == 1978 & year_to == 2012)
  if (nrow(subset(subset, !is.na(growth_rate_wins))) <= 60) {print("NA")} else
  {
    crops_1978_2012[[paste0(i)]] <- feols(growth_rate_wins ~ econgrowth_wins + wins_acres_from +
                 wins_acres_from^2 + wins_emp_base + wins_emp_base^2 | fipstate,
               weight =~ weight, data = subset)
  }
}

coefs_df <- map_dfr(crops_1978_2012, tidy, .id = "model", conf.int = TRUE) |>
  filter(term == "econgrowth_wins") |>
  left_join(cropgroups, by = c("model" = "names_all")) |>
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    label = paste0(model, stars),
    label = reorder(label, estimate, decreasing = TRUE)   # sort by coefficient
  )

ggplot(coefs_df, aes(x = estimate, y = label, color = group,
                     xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  labs(x = "",
       y = NULL, color = "") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))
ggsave("Output/figures/cropxcounty_1978_2012_forpaper.png", width = 6, height = 8)

#############################
### Getting them for all years
#############################

year_list <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007, 2012)
year_pairs <- subset(expand.grid(year_from = year_list, year_to = year_list), year_to > year_from)

for (r in seq_len(nrow(year_pairs))) {
  yr_from <- year_pairs$year_from[r]
  yr_to   <- year_pairs$year_to[r]
  
  crops_list <- list()
  for (i in names_all) {
    subset_data <- acres_growth |>
      filter(crop == i & year_from == yr_from & year_to == yr_to)
    if (nrow(subset(subset_data, !is.na(growth_rate_pctl))) <= 60) {
      print("NA")
    } else {
      crops_list[[i]] <- tryCatch(
        feols(
          growth_rate_pctl ~ econgrowth_pctl + acres_from +
            acres_from^2 + emp_base + emp_base^2 | fipstate,
          weight =~ weight, data = subset_data
        ),
        error = function(e) NULL
      )
    }
  }
  crops_list <- Filter(Negate(is.null), crops_list)
  coefs_df <- map_dfr(crops_list, tidy, .id = "model", conf.int = TRUE) |>
    filter(term == "econgrowth_pctl") |>
    left_join(cropgroups, by = c("model" = "names_all")) |>
    mutate(
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ ""
      ),
      label = paste0(model, stars),
      label = reorder(label, estimate, decreasing = TRUE)
    )
  
  ggplot(coefs_df, aes(x = estimate, y = label, color = group,
                       xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange() +
    labs(
      x = paste0("Effect of Change in Employed Pop. on Change in Land per Crop, ",
                 yr_from, "-", yr_to),
      y = NULL, color = ""
    ) +
    theme_minimal() +
    theme(text = element_text(family = "Times New Roman"))
  
  ggsave(
    paste0("Output/figures/cropxcounty_", yr_from, "_", yr_to, ".png"),
    width = 6, height = 8
  )
  rm(crops_list, coefs_df, subset_data)
  gc()
}

run_allbases_bycrop <- function(var) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) {
        print("NA")
      } else {
        subset <- acres_growth |>
          filter(crop == var & year_from == i & year_to == j)
        if (nrow(subset(subset, !is.na(growth_rate_wins))) <= 60) {
          print("NA")
        } else {
         run <- tryCatch(
            feols(
              growth_rate_wins ~ econgrowth_wins + wins_acres_from +
                wins_acres_from^2 + wins_emp_base + wins_emp_base^2 | fipstate,
              weight =~ weight, data = subset
            ),
            error = function(e) NULL
          )
        }
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
  
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    keep <- setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
                     as.character(js))
    keep[!sapply(keep, is.null)]
  })
  names(ordered) <- paste("Starting", starts)
  ordered <- ordered[sapply(ordered, length) > 0] 
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_map = c("econgrowth_wins" = "Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{", tolower(var), "_allbases}Winsorized Change in Acres ", var, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", tolower(var), "_allbases.tex"), overwrite = TRUE)
  
}

for (i in c("Corn (Grain)", "Soybeans", "Wheat", "Alfalfa", "Cotton",
            "Sorghum (grain)", "Oats", "Barley", "Rice", "Sunflower",
            "Peanuts", "Potatoes")) {
  run_allbases_bycrop(i)
}

## Getting another figure just of 1978 to 2012
bigcrops <- list("Corn (Grain)" = `allbases_Corn (Grain)_ordered`$`Starting 1978`,
                 "Soybeans" = `allbases_Soybeans_ordered`$`Starting 1978`,
                 "Wheat" = `allbases_Wheat_ordered`$`Starting 1978`,
                 "Alfalfa" = `allbases_Alfalfa_ordered`$`Starting 1978`,
                 "Cotton" = `allbases_Cotton_ordered`$`Starting 1978`)
modelsummary(bigcrops,
             shape = "rbind",
             stars = TRUE,
             coef_map = c("econgrowth_wins" = "Employment Growth"),
             gof_map = gm_small,
             title = "\\label{bigcrops} Winsorized Change in Acres for Major Crops, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/bigcrops.tex", overwrite = TRUE)

## And doing the grouped version
run_allbases_bycropgroup <- function(var) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) {
        print("NA")
      } else {
        subset <- acresgrouped_growth |>
          filter(group == var & year_from == i & year_to == j)
        run <-  feols(
          growth_rate_wins ~ econgrowth_wins + wins_acres_from +
            wins_acres_from^2 + wins_emp_base + wins_emp_base^2 | fipstate,
          weight =~ weight, data = subset
        )
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
  
  ordered <- get(paste0("allbases_", var, "_ordered"))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_map = c("econgrowth_wins" = "Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{", tolower(var), "_allbases}Winsorized Change in Acres of ", var, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", tolower(var), "_allbases.tex"), overwrite = TRUE)
  
}

for (i in unique(acresgrouped_growth$group)) {
run_allbases_bycropgroup(i)
}

## Subsetting to just the 1978 to 2012 iterations
cropgroups <- list("Grains" = allbases_Grains_ordered$`Starting 1978`,
                   "Legumes" = allbases_Legumes_ordered$`Starting 1978`,
                   "Fruit" = allbases_Fruit_ordered$`Starting 1978`,
                   "Vegetables" = allbases_Vegetables_ordered$`Starting 1978`,
                   "Other" = allbases_Other_ordered$`Starting 1978`)
modelsummary(cropgroups,
             shape = "rbind",
             stars = TRUE,
             coef_map = c("econgrowth_wins" = "Employment Growth"),
             gof_map = gm_small,
             title = "\\label{cropgroups} Winsorized Change in Acres for Crop Groups, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/cropgroups.tex", overwrite = TRUE)

## And grouping by crop production quintile instead
run_allbases_byprod <- function(var) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) {
        print("NA")
      } else {
        subset <- acresbyprod_growth |>
          filter(prodquint == var & year_from == i & year_to == j)
        run <-  feols(
          growth_rate_wins ~ econgrowth_wins + wins_acres_from +
            wins_acres_from^2 + wins_emp_base + wins_emp_base^2 | fipstate,
          weight =~ weight, data = subset
        )
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
  
  ordered <- get(paste0("allbases_", var, "_ordered"))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_map = c("econgrowth_wins" = "Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{prodquint", tolower(var), "_allbases}Winsorized Change in Acres, Crops in Production Quintile ", var, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/prodquint_", var, "_allbases.tex"), overwrite = TRUE)
  
}

for (i in unique(subset(acresbyprod_growth, prodquint != 1)$prodquint)) {
  run_allbases_byprod(i)
}

prodquints <- list("Quintile 2" = allbases_2_ordered$`Starting 1978`,
                   "Quintile 3" = allbases_3_ordered$`Starting 1978`,
                   "Quintile 4" = allbases_4_ordered$`Starting 1978`,
                   "Quintile 5" = allbases_5_ordered$`Starting 1978`)

modelsummary(prodquints,
             shape = "rbind",
             stars = TRUE,
             coef_map = c("econgrowth_wins" = "Employment Growth"),
             gof_map = gm_small,
             title = "\\label{prodquints} Winsorized Change in Acres for Crops by Production Quintile, County Level",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/prodquints.tex", overwrite = TRUE)


#############################
### Getting data at the crop level
#############################

croplevel <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)){
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)){
    if (j <= i) print("NA") else{
      dataset <- cropgrowths2 |>
        filter(year_base == i & year_end == j)
      reg <- feols(cropgrowth_wins ~ econgrowth_wins + wins_acres_base +
                     wins_acres_base^2 + wins_econshare + wins_econshare^2, data = dataset, weights =~ weight)
      croplevel[[paste0(i, "_", j)]] <- reg
    }
  }
}

starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
ordered <- lapply(starts, function(i) {
  js <- ends[ends > i]
  setNames(lapply(js, function(j) croplevel[[paste0(i, "_", j)]]),
           as.character(js))
})
names(ordered) <- paste("Starting", starts)

assign(paste0("croplevel_ordered"), ordered, envir = .GlobalEnv)

modelsummary(croplevel_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!econgrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^econgrowth_", x), "Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel}Winsorized Change in Total Cropland Nationwide by Employment Growth",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/croplevel_allbases.tex", overwrite = TRUE)



## Doing it without weights also
croplevel_nowt <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)){
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)){
    if (j <= i) print("NA") else{
      dataset <- cropgrowths2 |>
        filter(year_base == i & year_end == j)
      reg <- feols(cropgrowth_pctl ~ econgrowth_pctl + acres_base +
                     acres_base^2 + econshare + econshare^2, data = dataset)
      croplevel_nowt[[paste0(i, "_", j)]] <- reg
    }
  }
}

starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
ordered <- lapply(starts, function(i) {
  js <- ends[ends > i]
  setNames(lapply(js, function(j) croplevel_nowt[[paste0(i, "_", j)]]),
           as.character(js))
})
names(ordered) <- paste("Starting", starts)

assign(paste0("croplevel_nowt_ordered"), ordered, envir = .GlobalEnv)

modelsummary(croplevel_nowt_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!econgrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^econgrowth_", x), "Pctl. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel_nowt}Percentile Change in Total Cropland Nationwide by Employment Growth",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/croplevel_nowt_allbases.tex", overwrite = TRUE)

## Doing it for the twelve largest crops
croplevel_small <- list()
croplist <- unique(summarystats_bycrop_small$crop)
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)){
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)){
    if (j <= i) print("NA") else{
      dataset <- cropgrowths2 |>
        filter(year_base == i & year_end == j & crop %in% croplist)
      reg <- feols(cropgrowth_pctl ~ econgrowth_pctl + acres_base +
                     acres_base^2 + econshare + econshare^2, data = dataset, weights =~ weight)
      croplevel_small[[paste0(i, "_", j)]] <- reg
    }
  }
}

starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
ordered <- lapply(starts, function(i) {
  js <- ends[ends > i]
  setNames(lapply(js, function(j) croplevel_small[[paste0(i, "_", j)]]),
           as.character(js))
})
names(ordered) <- paste("Starting", starts)

assign(paste0("croplevel_small_ordered"), ordered, envir = .GlobalEnv)

modelsummary(croplevel_small_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!econgrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^econgrowth_", x), "Pctl. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel_small}Percentile Change in Total Cropland Nationwide, 12 Largest Crops",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/croplevel_small_allbases.tex", overwrite = TRUE)

croplevel_grains <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)){
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)){
    if (j <= i) print("NA") else{
      dataset <- cropgrowths2 |>
        filter(year_base == i & year_end == j & group == "Grains")
      reg <- feols(cropgrowth_pctl ~ econgrowth_pctl + acres_base +
                     acres_base^2 + econshare + econshare^2, data = dataset, weights =~ weight)
      croplevel_grains[[paste0(i, "_", j)]] <- reg
    }
  }
}

starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
ordered <- lapply(starts, function(i) {
  js <- ends[ends > i]
  setNames(lapply(js, function(j) croplevel_grains[[paste0(i, "_", j)]]),
           as.character(js))
})
names(ordered) <- paste("Starting", starts)

assign(paste0("croplevel_grains_ordered"), ordered, envir = .GlobalEnv)

modelsummary(croplevel_grains_ordered,
             shape = "rbind",
             stars = TRUE,
             coef_omit = "^(?!econgrowth_)",
             coef_rename = function(x) {
               ifelse(grepl("^econgrowth_", x), "Pctl. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel_grains}Percentile Change in Total Cropland Nationwide for Grains",
             output = "tinytable",
             escape = FALSE) |>
  theme_tt("resize") |>
  style_tt(i = "groupi", bold = TRUE) |>
  save_tt("Output/Tables/croplevel_grains_allbases.tex", overwrite = TRUE)

#####################################
### Doing all the same stuff for animals
#####################################

for (r in seq_len(nrow(year_pairs))) {
  yr_from <- year_pairs$year_from[r]
  yr_to   <- year_pairs$year_to[r]
  
  animals_list <- list()
  for (i in unique(animals_growth$animal)) {
    subset_data <- animals_growth |>
      filter(animal == i & year_from == yr_from & year_to == yr_to)
    if (nrow(subset(subset_data, !is.na(growth_rate_pctl))) <= 60) {
      print("NA")
    } else {
      animals_list[[i]] <- tryCatch(
        feols(
          growth_rate_pctl ~ econgrowth_pctl + number_from +
            number_from^2 + emp_base + emp_base^2 | fipstate,
          weight =~ weight, data = subset_data
        ),
        error = function(e) NULL
      )
    }
  }
  animals_list <- Filter(Negate(is.null), animals_list)
  coefs_df <- map_dfr(animals_list, tidy, .id = "model", conf.int = TRUE) |>
    filter(term == "econgrowth_pctl") |>
    mutate(
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ ""
      ),
      label = paste0(model, stars),
      label = reorder(label, estimate, decreasing = TRUE)
    )
  
  ggplot(coefs_df, aes(x = estimate, y = label,
                       xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange() +
    labs(
      x = paste0("Effect of Change in Employed Pop. on Change in No. Animals, ",
                 yr_from, "-", yr_to),
      y = NULL
    ) +
    theme_minimal() +
    theme(text = element_text(family = "Times New Roman"))
  
  ggsave(
    paste0("Output/figures/animalxcounty_", yr_from, "_", yr_to, ".png"),
    width = 6, height = 8
  )
  rm(animals_list, coefs_df, subset_data)
  gc()
}

run_allbases_byanimal <- function(var) {
  var2 <- gsub("[[:punct:][:space:]]", "", tolower(var))
  print(var2)
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i >= j) {
        print("NA")
      } else {
        subset <- animals_growth |>
          filter(animal == var & year_from == i & year_to == j)
        run <-  feols(
          growth_rate_pctl ~ econgrowth_pctl + number_from +
            number_from^2 + emp_base + emp_base^2 | fipstate,
          weight =~ weight, data = subset
        )
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
  
  assign(paste0("allbases_", var2),           result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var2, "_ordered"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  print(var2)
  ordered <- get(paste0("allbases_", var2, "_ordered"))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_map = c("econgrowth_pctl" = "Pctl. Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{", var2, "_allbases}Percentile Change in Number of ", var, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var2, "_allbases.tex"), overwrite = TRUE)
}

animallist <- unique(subset(animals_growth, !(animal %in% c("Pheasants", "Quails")))$animal)
for (i in animallist) {
  run_allbases_byanimal(i)
}

###########################
### Making a graph of top crops
###########################

all_models <- c(
  setNames(`allbases_Corn (Grain)_ordered`$`Starting 1978`,
           paste0("Corn (grain)|", names(`allbases_Corn (Grain)_ordered`$`Starting 1978`))),
  setNames(allbases_Soybeans_ordered$`Starting 1978`,
           paste0("Soybeans|", names(allbases_Soybeans_ordered$`Starting 1978`))),
  setNames(allbases_Wheat_ordered$`Starting 1978`,
           paste0("Wheat|", names(allbases_Wheat_ordered$`Starting 1978`))),
  setNames(allbases_Alfalfa_ordered$`Starting 1978`,
           paste0("Alfalfa|", names(allbases_Alfalfa_ordered$`Starting 1978`)))
)

dat <- modelplot(all_models,
                 coef_map = c("econgrowth_pctl" = "Pctl. Economic Growth"),
                 draw = FALSE) |>
  separate(model, into = c("crop", "year"), sep = "\\|")

ggplot(dat, aes(x = estimate, y = year, color = crop)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0) + theme_minimal() + scale_y_discrete(limits = rev) +
  labs(x = "", y = "", color = "") +
  theme(text = element_text(family = "Times New Roman"))

ggsave("Output/Figures/majorcrops.png", width = 6, height = 4)

## And doing the same for quintiles
all_models <- c(setNames(allbases_3_ordered$`Starting 1978`,
           paste0("Quintile 3|", names(allbases_Soybeans_ordered$`Starting 1978`))),
  setNames(allbases_4_ordered$`Starting 1978`,
           paste0("Quintile 4|", names(allbases_Wheat_ordered$`Starting 1978`))),
  setNames(allbases_5_ordered$`Starting 1978`,
           paste0("Quintile 5|", names(allbases_Alfalfa_ordered$`Starting 1978`)))
)

dat <- modelplot(all_models,
                 coef_map = c("econgrowth_pctl" = "Pctl. Economic Growth"),
                 draw = FALSE) |>
  separate(model, into = c("crop", "year"), sep = "\\|")

ggplot(dat, aes(x = estimate, y = year, color = crop)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0) + theme_minimal() + scale_y_discrete(limits = rev) +
  labs(x = "", y = "", color = "") +
  theme(text = element_text(family = "Times New Roman"))

ggsave("Output/Figures/cropquints.png", width = 6, height = 4)

## And the crops model


dat <- modelplot(croplevel_ordered$`Starting 1978`,
                 coef_map = c("econgrowth_pctl" = "Pctl. Economic Growth"),
                 draw = FALSE) |>
  rename(year = model) |>
  filter(year != 1982)

ggplot(dat, aes(x = estimate, y = year, color = year)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none")

ggsave("Output/Figures/croplevel.png", width = 6, height = 4)