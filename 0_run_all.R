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

options(scipen = 999) #I don't like scientific notation


## Actually running the code
source(file.path("Code/cbpdata.R"))
source(file.path("Code/agcensus.R"))
source(file.path("Code/combineddata.R"))
source(file.path("Code/summarystats.R"))
source(file.path("Code/regressions.R"))
source(file.path("Code/cropxcounty.R"))
source(file.path("Code/shiftshare.R"))
source(file.path("Code/mechanisms.R"))