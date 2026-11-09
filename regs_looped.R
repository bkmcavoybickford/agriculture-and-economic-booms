## Running the regressions on the data

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

gm_inst <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "First-stage F", "First-stage F-stat", 3)

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

## Running the main specification

run_allbases_norm <- function(var, label, startyear = 1978) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
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
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_", var, "_norm"),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_norm"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
 
   modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^norm_growth_", x), "Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_norm}Normalized Change in ", label, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_norm.tex"), overwrite = TRUE)
}

run_allbases_ss_norm <- function(var, label, startyear = 1978) {
  result <- list()
  for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      if (i < startyear || j < startyear) {
        next
      } else if (i >= j) {
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
  starts <- starts[starts >= startyear]
  ends   <- ends[ends >= startyear]
  ordered <- lapply(starts, function(i) {
    js <- ends[ends > i]
    setNames(lapply(js, function(j) result[[paste0(i, "_", j)]]),
             as.character(js))
  })
  names(ordered) <- paste("Starting", starts)
  
  assign(paste0("allbases_", var, "_ss_norm"),           result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_ss_norm"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!fit_norm_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^fit_norm_growth_", x), "Fitted Norm. Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_ss_norm}Normalized Change in ", label, ", County Level Shift-Share"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/shiftshare_", var, "_countylevel_allbases_norm.tex"), overwrite = TRUE)
}

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
  
  assign(paste0("allbases_", var),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Percentile Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases}Percentile Change in ", label, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases.tex"), overwrite = TRUE)
}

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
          "feols(pctl_", var, "growth_", j, "_", i,
          " ~ pctl_growth_manuf_", j, "_", i, " + `", i, "` + ", var, "_", i,
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
  
  assign(paste0("allbases_", var, "_manuf"),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_manuf"), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Percentile Manufacturing Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_manuf}Percentile Change in ", label, ", County Level"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_manuf.tex"), overwrite = TRUE)
}

## Doing a just winsorized version
run_allbases_wins <- function(var, label, startyear = 1978) {
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
    save_tt(paste0("Output/Tables/", var, "_countylevel_allbases_wins.tex"), overwrite = TRUE)
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
        run <- eval(parse(text = paste0("feols(pctl_", var, "growth_", j, "_", i, 
                                        " ~ 1 | statefip | pctl_growth_", j, "_", i, 
                                        " ~ pctl_shiftshare_", j, "_", i,
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
               coef_omit = "^(?!fit_pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^fit_pctl_growth_", x), "Fitted Percentile Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "_allbases_ss}Percentile Change in ", label, ", County Level Shift-Share"),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/shiftshare_", var, "_countylevel_allbases.tex"), overwrite = TRUE)
}

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
        run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                        " ~ pctl_growth_", j, "_", i, 
                                        " + `", i, "` + prodval_", i,
                                        " + prodval_", i, "^2 + `", i, "`^2 | statefip,
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
    invisible(list(raw = result, ordered = ordered))
    
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Percentile Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "percap_q", q, 
                              "}Percentile Change in ", label, 
                              ", County Level, Employment/Area Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_percap", q, ".tex"), overwrite = TRUE) }
 
  pd <- modelplot(qbasis_all,
                  coef_omit = "^(?!pctl_growth_)",
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
          run <- eval(parse(text = paste0("feols(pctl_", var, "growth_", j, "_", i, 
                                          " ~ pctl_growth_", j, "_", i, 
                                          " + `", i, "` + ", var, "_", i,
                                          " + ", var, "_", i, "^2 + `", i, "`^2 | statefip,
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
    
    modelsummary(ordered,
                 shape = "rbind",
                 stars = TRUE,
                 coef_omit = "^(?!pctl_growth_)",
                 coef_rename = function(x) {
                   ifelse(grepl("^pctl_growth_", x), "Percentile Employment Growth", x)
                 },
                 gof_map = gm_small,
                 title = paste0("\\label{", var, "_", region, 
                                "}Percentile Change in ", label, 
                                ", County Level, ", region),
                 output = "tinytable",
                 escape = FALSE) |>
      theme_tt("resize") |>
      style_tt(i = "groupi", bold = TRUE) |>
      save_tt(paste0("Output/Tables/", var, "_countylevel_region", region, ".tex"), overwrite = TRUE)
    
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      mod <- result[[paste0(startyear, "_", j)]]
      if (!is.null(mod)) {
        qbasis_all[[paste0("", region, " :: ", j)]] <- mod
      }
    }
  }
    
    pd <- modelplot(qbasis_all,
                    coef_omit = "^(?!pctl_growth_)",
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

run_allbases_agsharequint <- function(var, label, startyear = 1978) {
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
          run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                          " ~ pctl_growth_", j, "_", i, 
                                          " + `", i, "` + prodval_", i,
                                          " + prodval_", i, "^2 + `", i, "`^2 | statefip,
                     data = subset(countylevel_all, quintile_agshare == ", q, " ), 
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
   
  
  assign(paste0("allbases_", var, "_agshare_q", q),            result,  envir = .GlobalEnv)
  assign(paste0("allbases_", var, "_ordered_agshare_q", q), ordered, envir = .GlobalEnv)
  invisible(list(raw = result, ordered = ordered))
  
  modelsummary(ordered,
               shape = "rbind",
               stars = TRUE,
               coef_omit = "^(?!pctl_growth_)",
               coef_rename = function(x) {
                 ifelse(grepl("^pctl_growth_", x), "Percentile Employment Growth", x)
               },
               gof_map = gm_small,
               title = paste0("\\label{", var, "agshare_q", q, 
                              "}Percentile Change in ", label, 
                              ", County Level, Agricultural Workforce Share Quintile ", q),
               output = "tinytable",
               escape = FALSE) |>
    theme_tt("resize") |>
    style_tt(i = "groupi", bold = TRUE) |>
    save_tt(paste0("Output/Tables/", var, "_countylevel_agshare", q, ".tex"), overwrite = TRUE)}
  
  pd <- modelplot(qbasis_all,
                  coef_omit = "^(?!pctl_growth_)",
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
  
  ggsave(paste0("Output/Figures/", var, "_countylevel_agshare_", startyear, ".png"),
         p, width = 6, height = 4)
}

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
          run <- eval(parse(text = paste0("feols(pctl_prodvalgrowth_", j, "_", i, 
                                          " ~ pctl_growth_", j, "_", i, 
                                          " + `", i, "` + prodval_", i,
                                          " + prodval_", i, "^2 + `", i, "`^2 | statefip,
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
    
    modelsummary(ordered,
                 shape = "rbind",
                 stars = TRUE,
                 coef_omit = "^(?!pctl_growth_)",
                 coef_rename = function(x) {
                   ifelse(grepl("^pctl_growth_", x), "Percentile Employment Growth", x)
                 },
                 gof_map = gm_small,
                 title = paste0("\\label{", var, "_", region, 
                                "}Percentile Change in ", label, 
                                ", County Level, ", region),
                 output = "tinytable",
                 escape = FALSE) |>
      theme_tt("resize") |>
      style_tt(i = "groupi", bold = TRUE) |>
      save_tt(paste0("Output/Tables/", var, "_countylevel_region", region, ".tex"), overwrite = TRUE)
    
    for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
      mod <- result[[paste0(startyear, "_", j)]]
      if (!is.null(mod)) {
        qbasis_all[[paste0("", region, " :: ", j)]] <- mod
      }
    }
  }
    
    pd <- modelplot(qbasis_all,
                    coef_omit = "^(?!pctl_growth_)",
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


run_allspecs <- function(var, label, startyear = 1978) {
  run_allbases(var, label, startyear)
  run_allbases_ss(var, label, startyear)
  run_allbases_norm(var, label, startyear)
  run_allbases_ss_norm(var, label, startyear)
  run_allbases_simple(var, label, startyear)
  run_allbases_percapquint(var, label, startyear)
  run_allbases_region(var, label, startyear)
  run_allbases_agsharequint(var, label, startyear)
  run_allbases_manuf(var, label, startyear)
}

run_allspecs("cropland", "Total Cropland")
run_allspecs("farms", "Number of Farms")
run_allspecs("farmacrevalue", "Average Value of Acre Farmland")
run_allspecs("farmland", "Total Farmland")
run_allspecs("farmsize", "Average Farm Size")
run_allspecs("farmvalue", "Average Farm Value")
run_allspecs("irrland", "Total Irrigated Land")
run_allspecs("nonirrland", "Total Non-Irrigated Cropland")
run_allspecs("prodval", "Agricultural Production Value")
run_allspecs("cropval", "Crop Production Value")
run_allspecs("prodvalacre", "Production Value per Acre Farmland")
run_allspecs("prodvalworker", "Production Value per Worker")
run_allspecs("laborintensity", "Workers per Acre Farmland", startyear = 1992)
run_allspecs("agemp", "Workers in CBP Agriculture-Related Industries")
run_allspecs("croplandratio", "Cropland to Farmland Ratio")
run_allspecs("cropvalratio", "Crop to Total Production Ratio")
run_allspecs("machinery", "Total Machinery and Equipment Value")
run_allspecs("machineryper", "Machinery to Production Value Ratio")
run_allspecs("fertilizer", "Total Fertilizer Value")
run_allspecs("fertilizerper", "Fertilizer to Production Value Ratio")
run_allspecs("petroleum", "Total Fuel Value")
run_allspecs("petroleumper", "Fuel to Production Value Ratio")
run_allspecs("netreturn", "Total Net Profit", startyear = 1987)
run_allspecs("profitmargin", "Profit Margin", startyear = 1987)
run_allspecs("netlossfarms", "Number of Farms with Net Losses", startyear = 1987)
run_allspecs("netlosspct", "Percent of Farms with Net Losses", startyear = 1987)
run_allspecs("tractors", "Number of Tractors")
run_allspecs("tractorsper", "Tractors to Production Value Ratio")
run_allspecs("cccloans", "CCC Loans", startyear = 1982)
run_allspecs("govtpay", "Government Payments", startyear = 1987)
run_allspecs("family", "Acres in Individual or Family Sole Proprietorship")
run_allspecs("partnership", "Acres in Partnership")
run_allspecs("familycorp", "Acres in Family-Held Corporations")
run_allspecs("corp", "Acres in Non-Family-Held Corporations")
run_allspecs("otherorg", "Acres in Other Structures")
run_allspecs("directsales", "Value of Direct-to-Consumer Sales", startyear = 1992)
run_allspecs("emplabor", "Value of Hired and Contracted Labor")
run_allspecs("propertytax", "Property Tax Paid", startyear = 1987)
run_allspecs("empcount", "Number of Full-Time Operators and Hired Laborers", startyear = 1992)
run_allspecs("mainops", "Number of Full-Time Operators")
run_allspecs("hiredcount", "Number of Hired Laborers, Full and Part Time", startyear = 1992)

run_allbases("prodvalnondirect", "Production Value Besides Direct-to-Consumer Sales", startyear = 1992)
run_allbases_ss("prodvalnondirect", "Production Value Besides Direct-to-Consumer Sales", startyear = 1992)
run_allbases_region("prodvalnondirect", "Production Value Besides Direct-to-Consumer Sales", startyear = 1992)
run_allbases_wins("prodval", "Agricultural Production Value")
run_allbases_wins("cropland", "Total Cropland")