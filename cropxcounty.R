## This is for the crop-x-county level regressions



##################################
### Getting them in 1978-2012
##################################

crops_1978_2012 <- list()
for (i in names_all) {
  subset <- acres_growth |>
    filter(crop == i & year_from == 1978 & year_to == 2012)
  if (nrow(subset(subset, !is.na(growth_rate_norm))) <= 60) {print("NA")} else
  {
    crops_1978_2012[[paste0(i)]] <- feols(growth_rate_norm ~ econgrowth_norm + acres_from +
                 acres_from^2 + emp_base + emp_base^2 | fipstate,
               weight =~ weight, data = subset)
  }
}

coefs_df <- map_dfr(crops_1978_2012, tidy, .id = "model", conf.int = TRUE) |>
  filter(term == "econgrowth_norm") |>
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
  labs(x = "Effect of Change in Employed Pop. on Change in Land per Crop, 1978-2012",
       y = NULL, color = "") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))
ggsave("Output/figures/cropxcounty_1978_2012.png", width = 6, height = 8)

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
    if (nrow(subset(subset_data, !is.na(growth_rate_norm))) <= 60) {
      print("NA")
    } else {
      crops_list[[i]] <- tryCatch(
        feols(
          growth_rate_norm ~ econgrowth_norm + acres_from +
            acres_from^2 + emp_base + emp_base^2 | fipstate,
          weight =~ weight, data = subset_data
        ),
        error = function(e) NULL
      )
    }
  }
  crops_list <- Filter(Negate(is.null), crops_list)
  coefs_df <- map_dfr(crops_list, tidy, .id = "model", conf.int = TRUE) |>
    filter(term == "econgrowth_norm") |>
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
       run <-  feols(
          growth_rate_norm ~ econgrowth_norm + acres_from +
            acres_from^2 + emp_base + emp_base^2 | fipstate,
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
               coef_map = c("econgrowth_norm" = "Norm. Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{", tolower(var), "_allbases}Normalized Change in Acres ", var, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", tolower(var), "_allbases.tex"), overwrite = TRUE)
  
}

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
          growth_rate_norm ~ econgrowth_norm + acres_from +
            acres_from^2 + emp_base + emp_base^2 | fipstate,
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
               coef_map = c("econgrowth_norm" = "Norm. Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{", tolower(var), "_allbases}Normalized Change in Acres of ", var, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", tolower(var), "_allbases.tex"), overwrite = TRUE)
  
}

for (i in unique(acresgrouped_growth$group)) {
run_allbases_bycropgroup(i)
}

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
          growth_rate_norm ~ econgrowth_norm + acres_from +
            acres_from^2 + emp_base + emp_base^2 | fipstate,
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
               coef_map = c("econgrowth_norm" = "Norm. Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{prodquint", tolower(var), "_allbases}Normalized Change in Acres, Crops in Production Quintile ", var, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/prodquint_", var, "_allbases.tex"), overwrite = TRUE)
  
}

for (i in unique(subset(acresbyprod_growth, prodquint != 1)$prodquint)) {
  run_allbases_byprod(i)
}

#############################
### Getting data at the crop level
#############################

croplevel <- list()
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)){
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)){
    if (j <= i) print("NA") else{
      dataset <- cropgrowths2 |>
        filter(year_base == i & year_end == j)
      reg <- feols(cropgrowth_norm ~ econgrowth_norm + acres_base +
                     acres_base^2 + econshare + econshare^2, data = dataset, weights =~ weight)
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
               ifelse(grepl("^econgrowth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel}Normalized Change in Total Cropland Nationwide by Employment Growth",
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
      reg <- feols(cropgrowth_norm ~ econgrowth_norm + acres_base +
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
               ifelse(grepl("^econgrowth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel_nowt}Normalized Change in Total Cropland Nationwide by Employment Growth",
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
      reg <- feols(cropgrowth_norm ~ econgrowth_norm + acres_base +
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
               ifelse(grepl("^econgrowth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel_small}Normalized Change in Total Cropland Nationwide, 12 Largest Crops",
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
      reg <- feols(cropgrowth_norm ~ econgrowth_norm + acres_base +
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
               ifelse(grepl("^econgrowth_", x), "Norm. Employment Growth", x)
             },
             gof_map = gm_small,
             title = "\\label{croplevel_grains}Normalized Change in Total Cropland Nationwide for Grains",
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
    if (nrow(subset(subset_data, !is.na(growth_rate_norm))) <= 60) {
      print("NA")
    } else {
      animals_list[[i]] <- tryCatch(
        feols(
          growth_rate_norm ~ econgrowth_norm + number_from +
            number_from^2 + emp_base + emp_base^2 | fipstate,
          weight =~ weight, data = subset_data
        ),
        error = function(e) NULL
      )
    }
  }
  animals_list <- Filter(Negate(is.null), animals_list)
  coefs_df <- map_dfr(animals_list, tidy, .id = "model", conf.int = TRUE) |>
    filter(term == "econgrowth_norm") |>
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
          growth_rate_norm ~ econgrowth_norm + number_from +
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
               coef_map = c("econgrowth_norm" = "Norm. Employment Growth"),
               gof_map = gm_small,
               title = paste0("\\label{", var2, "_allbases}Normalized Change in Number of ", var, ", County Level"),
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