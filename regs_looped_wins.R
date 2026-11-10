run_allbases <- function(var, label, startyear = 1978) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0(
          "feols(wins_", var, "growth_", j, "_", i,
          " ~ wins_growth_", j, "_", i, " + wins_", i, " + wins_", var, "_", i,
          " + wins_", var, "_", i, "^2 + wins_", i, "^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
        )))
        result[[paste0(i, "_", j)]] <- run
      }
    }
  }
  
  starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
  ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_wins_", var),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_wins_", var, "_ordered"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!wins_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^wins_growth_", x), "Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases}Winsorized Change in ", label, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases.tex"), overwrite = TRUE)
}

run_allbases_ss <- function(var, label, startyear = 1978) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0("feols(wins_", var, "growth_", j, "_", i, 
                                        " ~ 1 | statefip | wins_growth_", j, "_", i, 
                                        " ~ wins_shiftshare_", j, "_", i,
                                        ", data = countylevel_all, vcov_conley(cutoff = 100),
                   weights =~ weight_", i, ")")))
        result[[paste0(i, "_", j)]] <- run
      }
    }
  }
  
  starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
  ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_", var, "_ss"),           result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_ss"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!fit_wins_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^fit_wins_growth_", x), "Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_ss}Winsorized Change in ", label, ", County Level Shift-Share"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/shiftshare_", var, "_countylevel_allbases.tex"), overwrite = TRUE)
}

run_allspecs_min <- function(var, label, startyear = 1978) {
  run_allbases(var, label, startyear)
  run_allbases_ss(var, label, startyear)
}

run_allspecs_min("cropland", "Total Cropland")
run_allspecs_min("farms", "Number of Farms")
run_allspecs_min("farmacrevalue", "Average Value of Acre Farmland")
run_allspecs_min("farmland", "Total Farmland")
run_allspecs_min("farmsize", "Average Farm Size")
run_allspecs_min("farmvalue", "Average Farm Value")
run_allspecs_min("irrland", "Total Irrigated Land")
run_allspecs_min("nonirrland", "Total Non-Irrigated Cropland")
run_allspecs_min("prodval", "Agricultural Production Value")
run_allspecs_min("cropval", "Crop Production Value")
run_allspecs_min("prodvalacre", "Production Value per Acre Farmland")
run_allspecs_min("prodvalworker", "Production Value per Worker")
run_allspecs_min("laborintensity", "Workers per Acre Farmland", startyear = 1992)
run_allspecs_min("agemp", "Workers in CBP Agriculture-Related Industries")
run_allspecs_min("croplandratio", "Cropland to Farmland Ratio")
run_allspecs_min("cropvalratio", "Crop to Total Production Ratio")
run_allspecs_min("machinery", "Total Machinery and Equipment Value")
run_allspecs_min("machineryper", "Machinery to Production Value Ratio")
run_allspecs_min("fertilizer", "Total Fertilizer Value")
run_allspecs_min("fertilizerper", "Fertilizer to Production Value Ratio")
run_allspecs_min("petroleum", "Total Fuel Value")
run_allspecs_min("petroleumper", "Fuel to Production Value Ratio")
run_allspecs_min("netreturn", "Total Net Profit", startyear = 1987)
run_allspecs_min("profitmargin", "Profit Margin", startyear = 1987)
run_allspecs_min("netlossfarms", "Number of Farms with Net Losses", startyear = 1987)
run_allspecs_min("netlosspct", "Percent of Farms with Net Losses", startyear = 1987)
run_allspecs_min("tractors", "Number of Tractors")
run_allspecs_min("tractorsper", "Tractors to Production Value Ratio")
run_allspecs_min("cccloans", "CCC Loans", startyear = 1982)
run_allspecs_min("govtpay", "Government Payments", startyear = 1987)
run_allspecs_min("family", "Acres in Individual or Family Sole Proprietorship")
run_allspecs_min("partnership", "Acres in Partnership")
run_allspecs_min("familycorp", "Acres in Family-Held Corporations")
run_allspecs_min("corp", "Acres in Non-Family-Held Corporations")
run_allspecs_min("otherorg", "Acres in Other Structures")
run_allspecs_min("directsales", "Value of Direct-to-Consumer Sales", startyear = 1992)
run_allspecs_min("emplabor", "Value of Hired and Contracted Labor")
run_allspecs_min("propertytax", "Property Tax Paid", startyear = 1987)
run_allspecs_min("empcount", "Number of Full-Time Operators and Hired Laborers", startyear = 1992)
run_allspecs_min("mainops", "Number of Full-Time Operators")
run_allspecs_min("hiredcount", "Number of Hired Laborers, Full and Part Time", startyear = 1992)
run_allspecs_min("familyfarmpct", "Percent of Acres in Family Farms")
run_allspecs_min("majorcroppct", "Percent of Acres in Corn, Wheat, and Soybeans")

###################################
### Specs only for some regressions
###################################

## And the simple version
run_allbases_simple <- function(var, label, startyear = 1978) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0(
          "feols(", var, "growth_", j, "_", i,
          " ~ growth_", j, "_", i, ",
          data = countylevel_all, vcov_conley(cutoff = 100))"
        )))
        result[[paste0(i, "_", j)]] <- run
      }
    }
  }
  
  starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
  ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_", var, "_simple"),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_simple"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^growth_", x), "Raw Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_simple}Change in ", label, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_simple.tex"), overwrite = TRUE)
}

## And the quintiles version
run_allbases_percapquint <- function(var, label, startyear = 1978) {
  qbasis_all <- list()
  for (q in 1:5) {
    result <- list()
    for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
      for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
        if (i < startyear || j < startyear) {
          next
        } else if (i >= j) {
          print("NA")
        } else {
          run <- eval(parse(text = paste0("feols(wins_", var, "growth_", j, "_", i, 
                                          " ~ wins_growth_", j, "_", i, 
                                          " + wins_", i, " + wins_", var, "_", i,
                                          " + wins_", var, "_", i, "^2 + wins_", i, "^2 | statefip,
                     data = subset(countylevel_all, quintile_emppercap == ", q, " ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
          result[[paste0(i, "_", j)]] <- run
        }
      }}
    
    starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
    ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
    starts <- starts[starts >= startyear]
    ends   <- ends[ends >= startyear]
    ordered <- lapply(starts, function(i) {
      js <- ends[ends > i]
      setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
               as.character(js))
    })
    names(ordered) <- paste("Starting", starts)
    
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      mod <- result[[paste0(startyear, "_", j)]]
      if (!is.null(mod)) {
        qbasis_all[[paste0("Q", q, " :: ", j)]] <- mod
      }
    }
    
    assign(paste0("allbases_", var, "_percap_q", q),            result,  envir = .GlobalEnv)
    assign(paste0("allbases_", var, "_ordered_percap_q", q), ordered, envir = .GlobalEnv)
    invisible(list(raw = result, ordered = ordered))}
  
  nested <- split(qbasis_all, sub(" :: .*$", "", names(qbasis_all)))
  nested <- lapply(nested, function(grp) {
    names(grp) <- sub("^Q\\d+ :: ", "", names(grp))
    grp
  })
    
  modelsummary(nested,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!wins_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^wins_growth_", x), "Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "percap", 
                              "}Winsorized Change in ", label, 
                              ", County Level, Employment/Area Quintiles, Beginning in ", startyear),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_percap.tex"), overwrite = TRUE) 

    
  pd <- modelplot(qbasis_all,
                  coef_omit = "^(?!wins_growth_)",
                  draw = FALSE)
  
  pd$quintile <- sub("^(Q\\d+) :: .*$", "\\1", pd$model)
  pd$end_year <- factor(sub("^Q\\d+ :: ", "", pd$model),
                        levels = as.character(sort(unique(
                          as.integer(sub("^Q\\d+ :: ", "", pd$model))))))
  
  p <- ggplot(pd, aes(x = estimate, y = end_year, color = quintile)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge2(width = 0.6, reverse = TRUE)) +
    labs(x = "",
         y = "",
         color = "Quintile") +
    theme_minimal()  + theme(text = element_text(family = "Times New Roman")) +
    scale_color_viridis_d() + scale_y_discrete(limits = rev)
  
  ggsave(paste0("Output/Figures/", var, "_countylevel_percap_", startyear, ".png"),
         p, width = 6, height = 4)
  
  
}

## Percentiles
run_allbases_pctl <- function(var, label, startyear = 1978) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0(
          "feols(pctl_", var, "growth_", j, "_", i,
          " ~ pctl_growth_", j, "_", i, " + `", i, "` + ", var, "_", i,
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
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_pctl_", var),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_pctl_", var, "_ordered"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Percentile Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_pctl}Percentile Change in ", label, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_pctl.tex"), overwrite = TRUE)
}

## Manufacturing only
run_allbases_manuf <- function(var, label, startyear = 1978) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0(
          "feols(wins_", var, "growth_", j, "_", i,
          " ~ wins_growth_manuf_", j, "_", i, " + wins_", i, " + wins_", var, "_", i,
          " + wins_", var, "_", i, "^2 + wins_", i, "^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
        )))
        result[[paste0(i, "_", j)]] <- run
      }
    }
  }
  
  starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
  ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_", var, "_manuf"),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_manuf"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!wins_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^wins_growth_", x), "Manufacturing Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_manuf}Winsorized Change in ", label, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_manuf.tex"), overwrite = TRUE)
}

## Regions
run_allbases_region <- function(var, label, startyear = 1978) {
  qbasis_all <- list()
  for (region in c("Northeast", "Midwest", "South", "West")) {
    result <- list()
    for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
      for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
        if (i < startyear || j < startyear) {
          next
        } else if (i >= j) {
          print("NA")
        } else {
          run <- eval(parse(text = paste0("feols(wins_", var, "growth_", j, "_", i, 
                                          " ~ wins_growth_", j, "_", i, 
                                          " + wins_", i, " + wins_", var, "_", i,
                                          " + wins_", var, "_", i, "^2 + wins_", i, "^2 | statefip,
                     data = subset(countylevel_all, region == '", region, "' ), 
                     vcov_conley(cutoff = 100), weights =~ weight_", i, ")")))
          result[[paste0(i, "_", j)]] <- run
        }
      }}
    
    starts <- c(1978, 1982, 1987, 1992, 1997, 2002, 2007)
    ends   <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012)
    starts <- starts[starts >= startyear]
    ends   <- ends[ends >= startyear]
    ordered <- lapply(starts, function(i) {
      js <- ends[ends > i]
      setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
               as.character(js))
    })
    names(ordered) <- paste("Starting", starts)
    
    assign(paste0("allbases_", var, "_region_", region),            result,  envir = .GlobalEnv)
    assign(paste0("allbases_", var, "_ordered_region_", region), ordered, envir = .GlobalEnv)
    invisible(list(raw = result, ordered = ordered))
    
    
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      mod <- result[[paste0(startyear, "_", j)]]
      if (!is.null(mod)) {
        qbasis_all[[paste0("", region, " :: ", j)]] <- mod
      }
    }
  }
 
  nested <- split(qbasis_all, sub(" :: .*$", "", names(qbasis_all)))
  nested <- lapply(nested, function(grp) {
    names(grp) <- sub("^.* :: ", "", names(grp))
    grp
  })
  
  assign(paste0("nested_", var, "_regions"), nested,  envir = .GlobalEnv)

  
  modelsummary(nested,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!wins_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^wins_growth_", x), "Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "regions", 
                              "}Winsorized Change in ", label, 
                              ", County Level, Regions, Beginning in ", startyear),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_regions.tex"), overwrite = TRUE) 
  
  pd <- modelplot(qbasis_all,
                  coef_omit = "^(?!wins_growth_)",
                  draw = FALSE)
  
  pd$region   <- sub("^(.*) :: .*$", "\\1", pd$model)
  pd$end_year <- factor(sub("^.* :: ", "", pd$model),
                        levels = as.character(sort(unique(
                          as.integer(sub("^.* :: ", "", pd$model))))))
  
  p <- ggplot(pd, aes(x = estimate, y = end_year, color = region)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge2(width = 0.6, reverse = TRUE)) +
    labs(x = "",
         y = "",
         color = "Region") +
    theme_minimal() + theme(text = element_text(family = "Times New Roman")) +
    scale_color_viridis_d() + scale_y_discrete(limits = rev)
  
  ggsave(paste0("Output/Figures/", var, "_countylevel_regions", startyear, ".png"),
         p, width = 6, height = 4)
}

## Growth in working age population
run_allbases_totpop <- function(var, label, startyear = 1992) {
  result <- list()
  for (i in c(1992, 1997, 2002, 2007)) {
    for (j in c(1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0(
          "feols(wins_", var, "growth_", j, "_", i,
          " ~ wins_totpopgrowth_", j, "_", i, " + wins_", i, " + wins_", var, "_", i,
          " + wins_", var, "_", i, "^2 + wins_", i, "^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
        )))
        result[[paste0(i, "_", j)]] <- run
      }
    }
  }
  
  starts <- c(1992, 1997, 2002, 2007)
  ends   <- c(1997, 2002, 2007, 2012)
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_wins_totpop_", var),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_wins_totpop_", var, "_ordered"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!wins_totpopgrowth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^wins_totpopgrowth_", x), "Population Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_totpop}Winsorized Change in ", label, ", County Level, by Working-Age Population Growth"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_totpop.tex"), overwrite = TRUE)
}

run_allbases_unemp <- function(var, label, startyear = 1992) {
  result <- list()
  for (i in c(1992, 1997, 2002, 2007)) {
    for (j in c(1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
        print("NA")
      } else {
        run <- eval(parse(text = paste0(
          "feols(wins_", var, "growth_", j, "_", i,
          " ~ wins_unemprategrowth_", j, "_", i, " + wins_", i, " + wins_", var, "_", i,
          " + wins_", var, "_", i, "^2 + wins_", i, "^2 | statefip,
          data = countylevel_all, vcov_conley(cutoff = 100),
          weights =~ weight_", i, ")"
        )))
        result[[paste0(i, "_", j)]] <- run
      }
    }
  }
  
  starts <- c(1992, 1997, 2002, 2007)
  ends   <- c(1997, 2002, 2007, 2012)
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_wins_unemp_", var),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_wins_unemp_", var, "_ordered"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!wins_unemprategrowth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^wins_unemprategrowth_", x), "Unemployment Rate Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_unemprate}Winsorized Change in ", label, ", County Level, by Unemployment Rate Growth"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_unemprate.tex"), overwrite = TRUE)
}



run_allbases_percapquint("prodval", "Agricultural Production Value")
run_allbases_percapquint("cropland", "Total Cropland")
run_allbases_pctl("prodval", "Total Production Value")
run_allbases_simple("prodval", "Total Production Value")
run_allbases_manuf("prodval", "Total Production Value")
run_allbases_region("prodval", "Total Production Value")
run_allbases_region("prodvalnondirect", "Production Value Besides Direct-to-Consumer Sales", startyear = 1992)
run_allbases_simple("cropland", "Total Cropland")
run_allbases_manuf("cropland", "Total Cropland")
run_allbases_region("cropland", "Total Cropland")
run_allbases_totpop("prodval", "Total Production Value")
run_allbases_unemp("prodval", "Total Production Value")
run_allbases_totpop("cropland", "Total Cropland")
run_allbases_unemp("cropland", "Total Cropland")
run_allbases_unemp("empcount", "Number of Full-Time Operators and Hired Laborers")
run_allbases_totpop("empcount", "Number of Full-Time Operators and Hired Laborers")
run_allbases_unemp("farmacrevalue", "Average Value of Acre Farmland")
run_allbases_totpop("farmacrevalue", "Average Value of Acre Farmland")
