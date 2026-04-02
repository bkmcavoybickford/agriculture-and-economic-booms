## This file downloads and analyzes the county business patterns data

## Loading all necessary libraries
library(tidyverse)
library(readr)
library(here)
library(bartik.weight)
options(scipen = 999)

####################################
### Getting the county industry data
####################################

## Getting the actual data
cbp_harmonized <- read_csv("Data/efsy_panel_naics.csv")

## Modifying the data for our purposes
cbp_mod <- cbp_harmonized |>
  mutate(n1 = str_sub(naics12, 1, 1),
         n2 = str_sub(naics12, 1, 2),
         n3 = str_sub(naics12, 1, 3),
         n4 = str_sub(naics12, 1, 4)) |>
  filter(!(n1 %in% c("1")),
         !(n2 %in% c("21")),
         !(n4 %in% c("3253","3331","4244","4245")),
         !(n3 %in% c("311", "312","924"))) |>
  select(-n1, -n2, -n3, -n4) |>
  #filtering out agriculture, fertilizer, groceries, warehouses, surveying, and ag-based manufacturing and equipment
  group_by(fipstate, fipscty, year) |>
  mutate(emp_total = sum(emp, na.rm = TRUE)) |>
  ungroup() |>
  mutate(emppct = emp/emp_total,
         countycode = paste(fipstate, fipscty)) |>
  filter(!(fipstate %in% c(02, 15, 11)))

##############################
### Getting the CBP info for later years
##############################

cbp_tot_1982 <- cbp_mod |>
  filter(year == 1982 | year == 1978) |>
  group_by(naics12, year) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = emp) |>
  mutate(econgrowth = (`1982`-`1978`)/`1978`) |>
  select("industry" = naics12, econgrowth)

cbp_tot_1987 <- cbp_mod |>
  filter(year == 1987 | year == 1978) |>
  group_by(naics12, year) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = emp) |>
  mutate(econgrowth = (`1987`-`1978`)/`1978`) |>
  select("industry" = naics12, econgrowth)

cbp_tot_1992 <- cbp_mod |>
  filter(year == 1992 | year == 1978) |>
  group_by(naics12, year) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = emp) |>
  mutate(econgrowth = (`1992`-`1978`)/`1978`) |>
  select("industry" = naics12, econgrowth)

cbp_tot_1997 <- cbp_mod |>
  filter(year == 1997 | year == 1978) |>
  group_by(naics12, year) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = emp) |>
  mutate(econgrowth = (`1997`-`1978`)/`1978`) |>
  select("industry" = naics12, econgrowth)

cbp_bycounty <- cbp_mod |>
  filter(year %in% c(1978, 1982, 1987, 1992, 1997, 2002, 2007, 2012)) |>
  group_by(year, fipstate, fipscty) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  mutate(countycode = paste(fipstate, fipscty))

emp_wide <- cbp_bycounty |>
  pivot_wider(names_from = year, values_from = emp)

growth_rates <- cbp_bycounty |>
  inner_join(cbp_bycounty, 
             by = c("fipstate", "fipscty"), 
             suffix = c("_base", "_end")) |>
  filter(year_end > year_base) |>
  mutate(growth = 100 * (emp_end - emp_base) / emp_base,
         pair = paste0("growth_", year_end, "_", year_base)) |>
  select(fipstate, fipscty, pair, growth) |>
  pivot_wider(names_from = pair, values_from = growth) |>
  left_join(emp_wide, by = c("fipstate", "fipscty")) |>
  mutate(countycode = paste(fipstate, fipscty),
         weight_1978 = log(`1978`),
         weight_1982 = log(`1982`),
         weight_1987 = log(`1987`),
         weight_1992 = log(`1992`),
         weight_1997 = log(`1997`),
         weight_2002 = log(`2002`),
         weight_2007 = log(`2007`),
         region = case_when(
           fipstate %in% c(09, 23, 25, 33, 44, 50, 34, 36, 42) ~ "Northeast",
           fipstate %in% c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46) ~ "Midwest",
           fipstate %in% c(10, 11, 12, 13, 24, 37, 45, 51, 54, 01, 21, 28, 47, 05, 22, 40, 48) ~ "South",
           fipstate %in% c(04, 08, 16, 30, 32, 35, 49, 56, 02, 06, 15, 41, 53) ~ "West"
         )) |>
  filter(`1978` > 1) |>
  mutate(across(starts_with("growth"), ~ as.numeric(scale(.)), 
                       .names = "norm_{.col}"))
  

cbp_19801990 <- cbp_mod |>
  filter(year %in% c(1980, 1990)) |>
  group_by(year, fipstate, fipscty) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = emp) |>
  mutate(empgrowth = (`1990`-`1980`)/`1980`,
         countycode = paste(fipstate, fipscty)) |>
  filter(empgrowth != Inf)

#####################################
### Doing a split by polluting vs. less polluting industries
#####################################

cbp_bypollute <- cbp_mod |>
  filter(year %in% c(1978, 1982, 1987, 1992, 1997, 2002, 2007, 2012)) |>
  mutate(pollute = if_else(str_sub(naics12, 1, 3) %in% c(311, 312, 313, 314,
                                                         315, 316, 321, 322, 323, 324,
                                                         325, 326, 327, 331, 332,
                                                         333, 334, 335, 336, 337,
                                                         339) |
                             str_sub(naics12, 1, 4) %in% c(5131, 1119, 1133, 2111,
                                                           2123, 4883, 5122, 5162,
                                                           5192, 5417, 8114, 2111,
                                                           2121, 2122, 2211, 2213,
                                                           4246, 4247, 4251, 5621,
                                                           5622, 5629),
                           1, 0))

cbp_nopollute <- cbp_bypollute |>
  filter(pollute == 0) |>
  group_by(year, fipstate, fipscty) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  mutate(countycode = paste(fipstate, fipscty))

cbp_pollute <- cbp_bypollute |>
  filter(pollute == 1) |>
  group_by(year, fipstate, fipscty) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  mutate(countycode = paste(fipstate, fipscty))

growth_rates_nopollute <- cbp_nopollute |>
  inner_join(cbp_nopollute, 
             by = c("fipstate", "fipscty"), 
             suffix = c("_base", "_end")) |>
  filter(year_end > year_base) |>
  mutate(growth = 100 * (emp_end - emp_base) / emp_base,
         pair = paste0("nopollute_growth_", year_end, "_", year_base)) |>
  select(fipstate, fipscty, pair, growth) |>
  pivot_wider(names_from = pair, values_from = growth) |>
  mutate(countycode = paste(fipstate, fipscty)) |>
  mutate(across(starts_with("nopollute_growth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}"))

growth_rates_pollute <- cbp_pollute |>
  inner_join(cbp_pollute, 
             by = c("fipstate", "fipscty"), 
             suffix = c("_base", "_end")) |>
  filter(year_end > year_base) |>
  mutate(growth = 100 * (emp_end - emp_base) / emp_base,
         pair = paste0("pollute_growth_", year_end, "_", year_base)) |>
  select(fipstate, fipscty, pair, growth) |>
  pivot_wider(names_from = pair, values_from = growth) |>
  mutate(countycode = paste(fipstate, fipscty)) |>
  mutate(across(starts_with("pollute_growth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}"))

##########################
### Getting expected county growth by industry
##########################

cbp_indgrowth <- cbp_mod |>
  filter(year == 1978 | year == 1997) |>
  group_by(naics12, year) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = emp) |>
  filter(!is.na(`1997`) & !is.na(`1978`)) |>
  mutate(growth = (`1997`-`1978`)/`1978`) |>
  select(naics12, growth)

cbp_weirdind <- cbp_mod |>
  filter(year == 1978 | year == 1997) |>
  group_by(naics12, year) |>
  summarize(emp = sum(emp, na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = emp) |>
  filter(is.na(`1997`) | is.na(`1978`))
cbp_weirdind_list <- cbp_weirdind$naics12

cbp_county_1997 <- cbp_mod |>
  filter(year == 1997 & !(naics12 %in% cbp_weirdind_list)) |>
  mutate(countycode = paste(fipstate, fipscty)) |>
  group_by(countycode) |>
  summarize(emp_1997 = sum(emp))

cbp_county_est <- cbp_mod |>
  filter(year == 1978) |>
  mutate(countycode = paste(fipstate, fipscty)) |>
  select(countycode, naics12, emp) |>
  left_join(cbp_indgrowth, by = "naics12") |>
  filter(!is.na(growth)) |>
  mutate(emp_est_1997 = emp*growth) |>
  group_by(countycode) |>
  summarize(emp_est_1997 = sum(emp_est_1997),
            emp = sum(emp)) |>
  left_join(cbp_county_1997, by = "countycode") |>
  filter(!is.na(emp_1997)) |>
  mutate(change_emp = emp_1997 - emp,
         est_change_emp = emp_est_1997 - emp)

cor(cbp_county_est$emp, cbp_county_est$emp_1997)
cor(cbp_county_est$emp_est_1997, cbp_county_est$emp_1997)
cor(cbp_county_est$change_emp, cbp_county_est$est_change_emp)

########################
### Getting the shift-share instrument
########################

## The shares portion
cbp_mod_shares <- cbp_mod |>
  filter(year %in% c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) |>
  select(naics12, countycode, year, emppct) |>
  filter(!is.na(emppct)) |>
  mutate(naicstrimmed = str_replace_all(naics12, "[/-]", ""),
         naics4 = str_sub(naics12, 1, 4)) |>
  group_by(naics4, countycode, year) |>
  summarize(emppct = sum(emppct, na.rm = TRUE))

## The shifts portion
cbp_mod_shifts <- cbp_mod |>
  mutate(naicstrimmed = str_replace_all(naics12, "[/-]", ""),
         naics4 = str_sub(naics12, 1, 4)) |>
  filter(year %in% c(1978, 1982, 1987, 1992, 1997, 2002, 2007, 2012)) |>
  group_by(naics4, year) |>
  summarize(emp = sum(emp, na.rm = TRUE))

## Doing it by hand
shiftshare_all <- as.data.frame(unique(cbp_mod_shares$countycode)) |>
  select("countycode" = 1)
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (j <= i) print("NA") else{
shares <- cbp_mod_shares |>
        ungroup() |>
        filter(year == i) |>
        select(countycode, naics4, emppct) |>
        pivot_wider(values_from = emppct, names_from = countycode)
shifts <- cbp_mod_shifts |>
      ungroup() |>
      filter(year == i | year == j) |>
      complete(naics4, year, fill = list(emp = 0)) |>
      group_by(naics4) |>
      summarize(growth = if_else(emp[year == i] == 0, NA,
                                 (emp[year == j]-emp[year == i])/emp[year == i]))

commonnaics <- intersect(unique(shares$naics4), unique(shifts$naics4))

shares_mat <- shares |>
  filter(naics4 %in% commonnaics) |>
        arrange(naics4) |>
        mutate(across(everything(), ~replace_na(.x, 0))) |>
        select(-naics4) |>
        as.matrix()
shifts_mat <- shifts |>
  filter(naics4 %in% commonnaics) |>
  arrange(naics4) |>
  ungroup() |>
  select(growth) |>
  as.matrix()
shiftshare <- t(shares_mat)%*%shifts_mat |>
  as.data.frame() |>
  rownames_to_column("countycode") |>
  rename(!!paste0("shiftshare_", j, "_", i) := 2)
shiftshare_all <- left_join(shiftshare_all, shiftshare,
                            by = "countycode")
  }}
}