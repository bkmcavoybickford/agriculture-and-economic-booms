## Loading all the necessary libraries
library(tidyverse)
library(readr)
library(here)
library(bartik.weight)
library(Matrix)
library(sf)
library(maps)
library(reshape2)
library(ggrepel)
library(fixest)
library(modelsummary)
library(tinytable)
library(splines)
library(marginaleffects)
library(broom)
library(purrr)
library(tigris)
library(scales)
library(BLSloadR)
Sys.setenv(BLS_USER_AGENT="bmcavoyb@stanford.edu")

options(scipen = 999) #I don't like scientific notation

## Getting a winsorization function
winsorize <- function(x, threshold = 0.05) {
  q <- quantile(x, c(threshold, 1 - threshold), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

## Actually running the code
source(file.path("Code/cbpdata.R"))
source(file.path("Code/agcensus.R"))
source(file.path("Code/combineddata.R"))
source(file.path("Code/summarystats.R"))
source(file.path("Code/regs_looped_wins.R"))
source(file.path("Code/regs_nonloop.R"))
source(file.path("Code/cropxcounty.R"))
source(file.path("Code/mechanisms.R"))