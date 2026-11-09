

########################
### Importing geographical data
########################

shapes1970 <- read_sf("Data/1970_shapefiles") |>
  select(X_CENTROID, Y_CENTROID, NHGISST, NHGISCTY, SHAPE_AREA) |>
  mutate(statefip = as.double(str_sub(NHGISST, 1, 2)),
         counfip = as.double(str_sub(NHGISCTY, 1, 3)),
         countycode = paste(statefip, counfip),
         pointcentroid = st_sfc(
           map2(X_CENTROID, Y_CENTROID, ~ st_point(c(.x, .y))),
           crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
         ),
         pointcentroid = st_transform(pointcentroid, crs = 4269),
         longitude = st_coordinates(pointcentroid)[, 1],
         latitude  = st_coordinates(pointcentroid)[, 2]) |>
  select(-c(statefip, counfip))

## Joining it up with the acre-level statistics
summarystats <- acres_1978_mod2 |>
  left_join(shapes1970, by = c("countycode")) |>
  filter(!is.na(geometry)) |>
  st_as_sf()

################################
### Getting data on unemployment and regional stuff
################################

laus_unemp <- get_laus(geography = "county", monthly_only = FALSE)
laus_unemp <- laus_unemp |>
  filter(period == "M13" & measure_code == "03") |>
  mutate(statefips = str_sub(area_code, 3, 4),
         counfips = str_sub(area_code, 5, 7),
         countycode = paste(statefips, counfips)) |>
  select(countycode, unemprate = "value", year) |>
  filter(year %in% c(1992, 1997, 2002, 2007, 2012))
unempgrowth <- laus_unemp |>
  inner_join(laus_unemp, by = c("countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(growth := 100 * (unemprate_to - unemprate_from) / unemprate_from,
         growth_name = paste0("unemprategrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, all_of("growth")) |>
  pivot_wider(id_cols = countycode,
              names_from = growth_name, values_from = all_of("growth"))

totalpop_workage <- read.delim("Data/us.1969_2024.20ages.adjusted.txt", header= FALSE) |>
  mutate(year = str_sub(V1, 1, 4),
         statefips = str_sub(V1, 7, 8),
         counfips = str_sub(V1, 9, 11),
         age = str_sub(V1, 17, 18),
         pop = as.numeric(str_sub(V1, 19, 26)),
         countycode = paste(statefips, counfips)) |>
  select(countycode, pop, age, year) |>
  filter(age %in% c("05", "06", "07", "08", "09", "10",
                    "11", "12", "13")) |>
  group_by(countycode, year) |>
  summarize(pop = sum(pop, na.rm = TRUE)) |>
  filter(year %in% c(1992, 1997, 2002, 2007, 2012))

totpopgrowth <- totalpop_workage |>
  inner_join(totalpop_workage, by = c("countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(growth := 100 * (pop_to - pop_from) / pop_from,
         growth_name = paste0("totpopgrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, all_of("growth")) |>
  pivot_wider(id_cols = countycode,
              names_from = growth_name, values_from = all_of("growth"))

###############################
### Getting the drought and precipitation data
###############################

## Drought
drought <- read.csv("Data/palmer_drought.csv") |>
  mutate(countyfips = as.numeric(str_sub(countyfips, -3, -1)),
         countycode = paste(statefips, countyfips)) |>
  group_by(countycode, year) |>
  summarize(pdsi = mean(pdsi, na.rm = TRUE))
drought_1978 <- drought |>
  filter(year >= 1978) |>
  group_by(countycode) |>
  arrange(year) |>
  mutate(pdsi = cummean(pdsi)) |>
  filter(year %in% c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) |>
  pivot_wider(names_from = year, names_prefix = "drought_1978_", values_from = pdsi)
drought_1982 <- drought |>
  filter(year >= 1982) |>
  group_by(countycode) |>
  arrange(year) |>
  mutate(pdsi = cummean(pdsi)) |>
  filter(year %in% c(1987, 1992, 1997, 2002, 2007, 2012)) |>
  pivot_wider(names_from = year, names_prefix = "drought_1982_", values_from = pdsi)
drought_1987 <- drought |>
  filter(year >= 1987) |>
  group_by(countycode) |>
  arrange(year) |>
  mutate(pdsi = cummean(pdsi)) |>
  filter(year %in% c(1992, 1997, 2002, 2007, 2012)) |>
  pivot_wider(names_from = year, names_prefix = "drought_1987_", values_from = pdsi)
drought_1992 <- drought |>
  filter(year >= 1992) |>
  group_by(countycode) |>
  arrange(year) |>
  mutate(pdsi = cummean(pdsi)) |>
  filter(year %in% c(1997, 2002, 2007, 2012)) |>
  pivot_wider(names_from = year, names_prefix = "drought_1992_", values_from = pdsi)
drought_1997 <- drought |>
  filter(year >= 1997) |>
  group_by(countycode) |>
  arrange(year) |>
  mutate(pdsi = cummean(pdsi)) |>
  filter(year %in% c(2002, 2007, 2012)) |>
  pivot_wider(names_from = year, names_prefix = "drought_1997_", values_from = pdsi)
drought_2002 <- drought |>
  filter(year >= 2002) |>
  group_by(countycode) |>
  arrange(year) |>
  mutate(pdsi = cummean(pdsi)) |>
  filter(year %in% c(2007, 2012)) |>
  pivot_wider(names_from = year, names_prefix = "drought_2002_", values_from = pdsi)
drought_2007 <- drought |>
  filter(year >= 2007) |>
  group_by(countycode) |>
  arrange(year) |>
  mutate(pdsi = cummean(pdsi)) |>
  filter(year %in% c(2012)) |>
  pivot_wider(names_from = year, names_prefix = "drought_2007_", values_from = pdsi)
drought_all <- left_join(drought_1978, drought_1982, by = "countycode") |>
  left_join(drought_1987, by = "countycode") |>
  left_join(drought_1992, by = "countycode") |>
  left_join(drought_1997, by = "countycode") |>
  left_join(drought_2002, by = "countycode") |>
  left_join(drought_2007, by = "countycode")

## A function to get state codes
convert_to_fips <- function(x) {
  case_match(as.integer(x),
        1  ~ "1",  # Alabama
        2  ~ "4",  # Arizona
        3  ~ "5",  # Arkansas
        4  ~ "6",  # California
        5  ~ "8",  # Colorado
        6  ~ "9",  # Connecticut
        7  ~ "10",  # Delaware
        8  ~ "12",  # Florida
        9  ~ "13",  # Georgia
        10 ~ "16",  # Idaho
        11 ~ "17",  # Illinois
        12 ~ "18",  # Indiana
        13 ~ "19",  # Iowa
        14 ~ "20",  # Kansas
        15 ~ "21",  # Kentucky
        16 ~ "22",  # Louisiana
        17 ~ "23",  # Maine
        18 ~ "24",  # Maryland
        19 ~ "25",  # Massachusetts
        20 ~ "26",  # Michigan
        21 ~ "27",  # Minnesota
        22 ~ "28",  # Mississippi
        23 ~ "29",  # Missouri
        24 ~ "30",  # Montana
        25 ~ "31",  # Nebraska
        26 ~ "32",  # Nevada
        27 ~ "33",  # New Hampshire
        28 ~ "34",  # New Jersey
        29 ~ "35",  # New Mexico
        30 ~ "36",  # New York
        31 ~ "37",  # North Carolina
        32 ~ "38",  # North Dakota
        33 ~ "39",  # Ohio
        34 ~ "40",  # Oklahoma
        35 ~ "41",  # Oregon
        36 ~ "42",  # Pennsylvania
        37 ~ "44",  # Rhode Island
        38 ~ "45",  # South Carolina
        39 ~ "46",  # South Dakota
        40 ~ "47",  # Tennessee
        41 ~ "48",  # Texas
        42 ~ "49",  # Utah
        43 ~ "50",  # Vermont
        44 ~ "51",  # Virginia
        45 ~ "53",  # Washington
        46 ~ "54",  # West Virginia
        47 ~ "55",  # Wisconsin
        48 ~ "56",  # Wyoming
        49 ~ "15",  # Hawaii
        50 ~ "2",  # Alaska
        .default = NA_character_
      )
}

## Precipitation
precip <- read.csv("Data/precip.csv") |>
  filter(Year %in% c(1948:1977)) |>
  drop_na() |>
  mutate(yeartot = Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct +
           Nov + Dec,
         State = convert_to_fips(State),
         countycode = paste(State, County)) |>
  group_by(countycode) |>
  summarize(precip = mean(yeartot, na.rm = TRUE))

#################################
### Getting the census unemployment data
#################################

unemp_1980 <- read.csv("Data/1980_employment_nhgis.csv") |>
  mutate(unemp_rate_1980 = (DR0003/DR0002)*100) |>
  mutate(countycode = paste(STATEA, COUNTYA)) |>
  select(countycode, unemp_rate_1980)

###############################
### Merging county-level data
###############################

countylevel_all <- left_join(countyprodall, growth_rates,
                             by = "countycode") |>
  left_join(countycroplandall, by = "countycode") |>
  left_join(countyfarmsall, by = "countycode") |>
  left_join(countyfarmacrevalueall, by = "countycode") |>
  left_join(countyfarmlandall, by = "countycode") |>
  left_join(countyfarmsizeall, by = "countycode") |>
  left_join(countyfarmvalueall, by = "countycode") |>
  left_join(countyirrlandall, by = "countycode") |>
  left_join(countynonirrlandall, by = "countycode") |>
  left_join(countyprodvalall, by = "countycode") |>
  left_join(countycropvalall, by = "countycode") |>
  left_join(countyprodvalacreall, by = "countycode") |>
  left_join(countyprodvalworkerall, by = "countycode") |>
  left_join(countylaborintensityall, by = "countycode") |>
  left_join(countyagempall, by = "countycode") |>
  left_join(totpopgrowth, by = "countycode") |>
  left_join(unempgrowth, by = "countycode") |>
  left_join(countycroplandratioall, by = "countycode") |>
  left_join(countycropvalratioall, by = "countycode") |>
  left_join(countymachineryall, by = "countycode") |>
  left_join(countymachineryperall, by = "countycode") |>
  left_join(countyfertilizerall, by = "countycode") |>
  left_join(countyfertilizerperall, by = "countycode") |>
  left_join(countypetroleumall, by = "countycode") |>
  left_join(countypetroleumperall, by = "countycode") |>
  left_join(shiftshare_all, by = "countycode") |>
  left_join(shiftshare_all_naics2, by = "countycode") |>
  left_join(shiftshare_all_census, by = "countycode") |>
  left_join(shiftshare_tradable_all, by = "countycode") |>
  left_join(drought_all, by = "countycode") |>
  left_join(precip, by = "countycode") |>
  left_join(growth_rates_census, by = "countycode") |>
  left_join(growth_rates_nopollute, by = "countycode") |>
  left_join(growth_rates_pollute, by = "countycode") |>
  left_join(growth_rates_nonag, by = "countycode") |>
  left_join(growth_rates_manuf, by = "countycode") |>
  left_join(countynetreturnall, by = "countycode") |>
  left_join(countyprofitmarginall, by = "countycode") |>
  left_join(countynetlossfarmsall, by = "countycode") |>
  left_join(countynetlosspctall, by = "countycode") |>
  left_join(countytractorsall, by = "countycode") |>
  left_join(countytractorsperall, by = "countycode") |>
  left_join(countycccloansall, by = "countycode") |>
  left_join(countypropertytaxall, by = "countycode") |>
  left_join(countygovtpayall, by = "countycode") |>
  left_join(countyfamilyall, by = "countycode") |>
  left_join(countyfamilyfarmpctall, by = "countycode") |>
  left_join(countymajorcroppctall, by = "countycode") |>
  left_join(countyprodvalnondirectall, by = "countycode") |>
  left_join(countymainopsall, by = "countycode") |>
  left_join(countyempcountall, by = "countycode") |>
  left_join(countyhiredcountall, by = "countycode") |>
  left_join(countypartnershipall, by = "countycode") |>
  left_join(countyfamilycorpall, by = "countycode") |>
  left_join(countycorpall, by = "countycode") |>
  left_join(countyotherorgall, by = "countycode") |>
  left_join(countydirectsalesall, by = "countycode") |>
  left_join(countyemplaborall, by = "countycode") |>
  left_join(countycensusempall, by = "countycode") |>
  left_join(shapes1970, by = "countycode") |>
  left_join(unemp_1980, by = "countycode") |>
  left_join(cbp_agshares, by = "countycode") |>
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA))) |>
  mutate(across(where(is.numeric), ~ winsorize(as.numeric(scale(.)),
                                                           threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(where(is.numeric) & !starts_with("norm"), ~ percent_rank(.)*100,
         .names = "pctl_{.col}")) |> ## Adding in some place variables
  mutate(across(where(is.numeric) & !starts_with("norm") & !starts_with("pctl"),
                ~ winsorize(.,
                threshold = 0.05),
         .names = "wins_{.col}")) |>
  mutate(emppercap_1978 = `1978`/SHAPE_AREA,
         emppercap_1982 = `1982`/SHAPE_AREA,
         emppercap_1987 = `1987`/SHAPE_AREA,
         emppercap_1992 = `1992`/SHAPE_AREA,
         emppercap_1997 = `1997`/SHAPE_AREA,
         emppercap_2002 = `2002`/SHAPE_AREA,
         emppercap_2007 = `2007`/SHAPE_AREA,
         quintile_emppercap = ntile(emppercap_1978, 5),
         quintile_agshare = ntile(agshare_1978, 5),
         quintile_precip = ntile(precip, 5),
         normgrowthdiff_1978_2012 = norm_growth_2012_1978 - norm_shiftshare_2012_1978)

################################
### Getting total stats instead
################################

## For cropland
cropland <- countylevel_all |>
  select(countycode, cropland_1978, cropland_1982, cropland_1987, cropland_1992, cropland_1997,
         cropland_2002, cropland_2007, cropland_2012) |>
  pivot_longer(-countycode, names_to = "year", values_to = "acres") |>
  group_by(year) |>
  summarize(totalcropland = sum(acres, na.rm = TRUE)) |>
  mutate(year = as.integer(str_sub(year, 10, -1))) |>
  ungroup()

## For number of farms
totfarms <- countylevel_all |>
  select(countycode, farms_1978, farms_1982, farms_1987, farms_1992, farms_1997,
         farms_2002, farms_2007, farms_2012) |>
  pivot_longer(-countycode, names_to = "year", values_to = "farms") |>
  group_by(year) |>
  summarize(totalfarms = sum(farms, na.rm = TRUE)) |>
  mutate(year = as.integer(str_sub(year, 7, -1))) |>
  ungroup()

## And production value
totprodval <- countylevel_all |>
  select(countycode, prodval_1978, prodval_1982, prodval_1987, prodval_1992, prodval_1997,
         prodval_2002, prodval_2007, prodval_2012) |>
  pivot_longer(-countycode, names_to = "year", values_to = "prodval") |>
  group_by(year) |>
  summarize(totalprodval = sum(prodval, na.rm = TRUE)) |>
  mutate(year = as.integer(str_sub(year, 9, -1))) |>
  ungroup()

## Getting some stats for kinds of ownership
ownershipstats <- countylevel_all |>
  select(countycode, family_1978, family_1982, family_1987, family_1992, family_1997,
         family_2002, family_2007, family_2012, partnership_1978, partnership_1982,
         partnership_1987, partnership_1992, partnership_1997, partnership_2002,
         partnership_2007, partnership_2012, familycorp_1978,
         familycorp_1982, familycorp_1987, familycorp_1992, familycorp_1997,
         familycorp_2002, familycorp_2007, familycorp_2012, corp_1978, corp_1982,
         corp_1987, corp_1992, corp_1997, corp_2002, corp_2007, corp_2012, 
         otherorg_1978, otherorg_1982, otherorg_1987, otherorg_1992, otherorg_1997,
         otherorg_2002, otherorg_2007, otherorg_2012) |>
  pivot_longer(-countycode, names_to = "name", values_to = "value") |>
  separate_wider_delim(name, delim = "_", names = c("Type", "Year")) |>
  group_by(Type, Year) |>
  summarize(total = sum(value, na.rm = TRUE)) |>
  mutate(Type = factor(case_when(Type == "corp" ~ "Corporation",
                          Type == "family" ~ "Indiv/Family Proprietor",
                          Type == "familycorp" ~ "Family Corp.",
                          Type == "otherorg" ~ "Other",
                          Type == "partnership" ~ "Partnership"), 
                       levels = c("Other", "Corporation", "Family Corp.",
                                  "Partnership", "Indiv/Family Proprietor")))

##############################
### Getting the crop-by-county level
##############################

acres_growth <- acres_all |>
  inner_join(acres_all, by = c("countycode", "crop"),
             suffix = c("_from", "_to"),
             relationship = "many-to-many") %>%
  filter(year_from < year_to) %>%
  mutate(growth_rate = if_else(acres_from == 0, NA, 
                               (acres_to - acres_from)*100 / acres_from)) |>
  ungroup() |>
  group_by(crop, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                                  threshold = 0.05),
         growth_rate_pctl = percent_rank(growth_rate)*100,
         growth_rate_wins = winsorize(growth_rate)) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, crop) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05),
         econgrowth_pctl = percent_rank(econgrowth)*100,
         econgrowth_wins = winsorize(econgrowth),
         wins_acres_from = winsorize(acres_from),
         wins_emp_base = winsorize(emp_base)) |>
  ungroup() |>
  filter(!is.na(growth_rate))

acresgrouped_all <- acres_all |>
  left_join(cropgroups, by = c("crop" = "names_all")) |>
  group_by(countycode, year, group) |>
  summarize(acres = sum(acres, na.rm = TRUE))

acresgrouped_growth <- acresgrouped_all |>
  inner_join(acresgrouped_all, by = c("countycode", "group"),
             suffix = c("_from", "_to"),
             relationship = "many-to-many") %>%
  filter(year_from < year_to) %>%
  mutate(growth_rate = if_else(acres_from == 0, NA, 
                               (acres_to - acres_from)*100 / acres_from)) |>
  ungroup() |>
  group_by(group, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                      threshold = 0.05),
         growth_rate_pctl = percent_rank(growth_rate)*100,
         growth_rate_wins = winsorize(growth_rate)) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, group) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05),
         econgrowth_pctl = percent_rank(econgrowth)*100,
         econgrowth_wins = winsorize(econgrowth),
         wins_emp_base = winsorize(emp_base),
         wins_acres_from = winsorize(acres_from)) |>
  ungroup() |>
  filter(!is.na(growth_rate))

## Getting crops by production quintile
acresprodquint <- acres_all |>
  filter(year == 1978) |>
  group_by(crop) |>
  summarize(totalprod = sum(acres, na.rm = TRUE)) |>
  mutate(prodquint = ntile(totalprod, 5))

acresbyprod_all <- acres_all |>
  left_join(acresprodquint, by = "crop") |>
  group_by(countycode, year, prodquint) |>
  summarize(acres = sum(acres, na.rm = TRUE))

acresbyprod_growth <- acresbyprod_all |>
  inner_join(acresbyprod_all, by = c("countycode", "prodquint"),
             suffix = c("_from", "_to"),
             relationship = "many-to-many") %>%
  filter(year_from < year_to) %>%
  mutate(growth_rate = if_else(acres_from == 0, NA, 
                               (acres_to - acres_from)*100 / acres_from)) |>
  ungroup() |>
  group_by(prodquint, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                      threshold = 0.05),
         growth_rate_pctl = percent_rank(growth_rate)*100,
         growth_rate_wins = winsorize(growth_rate)) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, prodquint) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05),
         econgrowth_pctl = percent_rank(econgrowth)*100,
         econgrowth_wins = winsorize(econgrowth),
         wins_emp_base = winsorize(emp_base),
         wins_acres_from = winsorize(acres_from)) |>
  ungroup() |>
  filter(!is.na(growth_rate))

## And the animal-by-county level
animals_growth <- animals_all |>
  inner_join(animals_all, by = c("countycode", "animal"),
             suffix = c("_from", "_to"),
             relationship = "many-to-many") %>%
  filter(year_from < year_to) %>%
  mutate(growth_rate = if_else(number_from == 0, NA, 
                               (number_to - number_from)*100 / number_from)) |>
  ungroup() |>
  group_by(animal, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                      threshold = 0.05),
         growth_rate_pctl = percent_rank(growth_rate)*100) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, animal) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05),
         econgrowth_pctl = percent_rank(econgrowth)*100) |>
  ungroup() |>
  filter(!is.na(growth_rate))

##############################
### Getting a measure of how much each crop's initial counties grow
##############################

cropgrowths <- as.data.frame(cropgroups) |>
  rename("crop" = names_all)

for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (j <= i) print("NA") else{
  acres_wide <- acres_all |>
    filter(year == i) |>
    group_by(year, crop) |>
    mutate(totcrop = sum(acres, na.rm = TRUE),
           cropshare = acres/totcrop) |>
    ungroup() |>
    select(countycode, crop, cropshare) |>
    pivot_wider(names_from = crop, values_from = cropshare)
  col <- paste0("growth_", j, "_", i)
  growth_rates_forhere <- growth_rates_wins |>
    filter(!is.na(.data[[col]]) & is.finite(.data[[col]]))
  counties1 <- unique(acres_wide$countycode)
  counties2 <- unique(growth_rates_forhere$countycode)
  countiestouse <- intersect(counties1, counties2)
  cropshifts <- growth_rates_forhere |>
    filter(countycode %in% countiestouse) |>
    ungroup() |>
    arrange(countycode) |>
    select(paste0("growth_", j, "_", i)) |>
    as.matrix()
  
  cropbase <- growth_rates_forhere |>
    filter(countycode %in% countiestouse) |>
    ungroup() |>
    arrange(countycode) |>
    select(paste0(i)) |>
    as.matrix()
  
  acres_mat <- acres_wide |>
    filter(countycode %in% countiestouse) |>
    arrange(countycode) |>
    select(-c(countycode)) |>
    as.matrix()
  
  cwg <- (t(acres_mat)%*%cropshifts) |>
    as.data.frame() |>
    rownames_to_column("crop")
  
  cropgrowths <- cropgrowths |>
    left_join(cwg, by = "crop") 
    } } 
  cws <- (t(acres_mat)%*%cropbase) |>
    as.data.frame() |>
    rownames_to_column("crop")
  cropgrowths <- cropgrowths |>
    left_join(cws, by = "crop")
  }

cropgrowths2 <- cropgrowths |>
  pivot_longer(cols = -c(crop, group, `1978`, `1982`, `1987`, `1992`, `1997`, `2002`, `2007`),
               names_to = "name",
               values_to = "econgrowth") |>
  pivot_longer(cols = c(`1978`, `1982`, `1987`, `1992`, `1997`, `2002`, `2007`),
               names_to = "year_base2",
               values_to = "econshare") |>
  mutate(year_end = as.numeric(str_sub(name, 8, 11)),
         year_base = as.numeric(str_sub(name, 13, 16))) |>
  filter(year_base == year_base2) |>
  left_join(cropchanges, by = c("crop", "year_end", "year_base")) |>
  group_by(year_base, year_end) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05),
         cropgrowth_norm = winsorize(as.numeric(scale(cropgrowth)),
                                     threshold = 0.05),
         econgrowth_pctl = percent_rank(econgrowth)*100,
         cropgrowth_pctl = percent_rank(cropgrowth)*100,
         econgrowth_wins = winsorize(econgrowth),
         cropgrowth_wins = winsorize(cropgrowth)*100,
         wins_acres_base = winsorize(acres_base),
         wins_econshare = winsorize(econshare)) |>
  ungroup() |>
  mutate(weight = log(econshare)) |>
  filter(weight > 0)

## And doing it grouped by state instead
cropgrowths_bystate <- as.data.frame(cropgroups) |>
  rename("crop" = names_all) |>
  cross_join(tibble(state = unique(countylevel_all$statefip)))
results <- list()
for (st in unique(countylevel_all$statefip)) {
for (i in c(1978, 1982, 1987, 1992, 1997, 2002, 2007)) {
  for (j in c(1982, 1987, 1992, 1997, 2002, 2007, 2012)) {
    if (j <= i) print("NA") else{
      acres_wide <- acres_all |>
        mutate(state = sub(" .*", "", countycode)) |>
        filter(year == i & state == st) |>
        group_by(year, crop) |>
        mutate(totcrop = sum(acres, na.rm = TRUE),
               cropshare = acres/totcrop) |>
        ungroup() |>
        select(countycode, crop, cropshare) |>
        pivot_wider(names_from = crop, values_from = cropshare)
      col <- paste0("growth_", j, "_", i)
      growth_rates_forhere <- growth_rates |>
        filter(!is.na(.data[[col]]) & is.finite(.data[[col]]) & fipstate == st)
      counties1 <- unique(acres_wide$countycode)
      counties2 <- unique(growth_rates_forhere$countycode)
      countiestouse <- intersect(counties1, counties2)
      cropshifts <- growth_rates_forhere |>
        filter(countycode %in% countiestouse) |>
        ungroup() |>
        arrange(countycode) |>
        select(paste0("growth_", j, "_", i)) |>
        as.matrix()
      
      cropbase <- growth_rates_forhere |>
        filter(countycode %in% countiestouse) |>
        ungroup() |>
        arrange(countycode) |>
        select(paste0(i)) |>
        as.matrix()
      
      acres_mat <- acres_wide |>
        filter(countycode %in% countiestouse) |>
        arrange(countycode) |>
        select(-c(countycode)) |>
        as.matrix()
      
      cwg <- (t(acres_mat)%*%cropshifts) |>
        as.data.frame() |>
        rownames_to_column("crop") |>
        mutate(state = st) |>
        pivot_longer(-c(crop, state), names_to = "measure", values_to = "value")
      
      results[[length(results) + 1]] <- cwg
    } }  
  cws <- (t(acres_mat)%*%cropbase) |>
    as.data.frame() |>
    rownames_to_column("crop") |>
    mutate(state = st) |>
    pivot_longer(-c(crop, state), names_to = "measure", values_to = "value")
  results[[length(results) + 1]] <- cws
} }

cropgrowths_bystate <- cropgrowths_bystate |>
  left_join(
    bind_rows(results) |> pivot_wider(names_from = measure, values_from = value),
    by = c("crop", "state")
  ) |>
  pivot_longer(cols = -c(crop, group, state, `1978`, `1982`, `1987`, `1992`, `1997`, `2002`, `2007`),
               names_to = "name",
               values_to = "econgrowth") |>
  pivot_longer(cols = c(`1978`, `1982`, `1987`, `1992`, `1997`, `2002`, `2007`),
               names_to = "year_base2",
               values_to = "econshare") |>
  mutate(year_end = as.numeric(str_sub(name, 8, 11)),
         year_base = as.numeric(str_sub(name, 13, 16))) |>
  filter(year_base == year_base2) |>
  left_join(cropchanges, by = c("crop", "year_end", "year_base")) |>
  group_by(year_base, year_end) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05),
         cropgrowth_norm = winsorize(as.numeric(scale(cropgrowth)),
                                     threshold = 0.05),
         econgrowth_pctl = percent_rank(econgrowth)*100,
         cropgrowth_pctl = percent_rank(cropgrowth)*100) |>
  ungroup() |>
  mutate(weight = log(econshare)) |>
  filter(weight > 0)

############################
### Getting data on state-to-state trade
############################

cfs2012 <- read.csv("Data/2012_cfs/bts_cfs_2012_pumf.csv")

cfsstat <- cfs2012 |>
  filter(EXPORT_YN == "N") |>
  mutate(sctgcode = if_else(SCTG %in% c("01", "02", "03", "04", "05", "01-05"), 
                            "Ag", "Non-Ag")) |>
  mutate(samearea = if_else(ORIG_CFS_AREA == DEST_CFS_AREA, 1, 0),
         samestate = if_else(ORIG_STATE == DEST_STATE, 1, 0)) |>
  group_by(sctgcode, samearea, samestate) |>
  summarize(value = sum(SHIPMT_VALUE*WGT_FACTOR))

