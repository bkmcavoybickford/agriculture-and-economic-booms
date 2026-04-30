

## Getting a winsorization function
winsorize <- function(x, threshold = 0.05) {
  q <- quantile(x, c(threshold, 1 - threshold), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

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

countylevel_merged <- left_join(countyprodall, growth_rates,
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
  left_join(shiftshare_tradable_all, by = "countycode") |>
  left_join(drought_all, by = "countycode") |>
  left_join(precip, by = "countycode") |>
  left_join(growth_rates_nopollute, by = "countycode") |>
  left_join(growth_rates_pollute, by = "countycode") |>
  left_join(growth_rates_nonag, by = "countycode") |>
  filter(cropland_1978 != 0 & cropland_1982 != 0 & cropland_1987 != 0 &
           cropland_1992 != 0 & cropland_1997 != 0 & cropland_2002 != 0 &
           cropland_2007 != 0 & cropland_2012 != 0) |>
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA))) |>
  mutate(across(starts_with("croplandgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                           threshold = 0.05), 
                .names = "norm_{.col}")) |> ## I do the winsorizing after I merge it, on the set of counties that I actually use
  mutate(across(starts_with("farmsgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                        threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmacrevaluegrowth"), ~ winsorize(as.numeric(scale(.)),
                                                                threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmlandgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                           threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmsizegrowth"), ~ winsorize(as.numeric(scale(.)),
                                                           threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmvaluegrowth"), ~ winsorize(as.numeric(scale(.)),
                                                            threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("irrlandgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                          threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("nonirrlandgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                             threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("prodvalgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                          threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("cropvalgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                          threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("prodvalacregrowth"), ~ winsorize(as.numeric(scale(.)),
                                                          threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("prodvalworkergrowth"), ~ winsorize(as.numeric(scale(.)),
                                                              threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("laborintensitygrowth"), ~ winsorize(as.numeric(scale(.)),
                                                              threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("agempgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                                 threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("shiftshare"), ~ winsorize(as.numeric(scale(.)),
                                                       threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("growth"), ~ winsorize(as.numeric(scale(.)),
                                                   threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("nopollute_growth"), ~ winsorize(as.numeric(scale(.)),
                                                             threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("pollute_growth"), ~ winsorize(as.numeric(scale(.)),
                                                           threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("growthnonag"), ~ winsorize(as.numeric(scale(.)),
                                                           threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("croplandratiogrowth"), ~ winsorize(as.numeric(scale(.)),
                                                        threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("cropvalratiogrowth"), ~ winsorize(as.numeric(scale(.)),
                                                        threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("machinerygrowth"), ~ winsorize(as.numeric(scale(.)),
                                                         threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("machinerypergrowth"), ~ winsorize(as.numeric(scale(.)),
                                                         threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("fertilizergrowth"), ~ winsorize(as.numeric(scale(.)),
                                                         threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("fertilizerpergrowth"), ~ winsorize(as.numeric(scale(.)),
                                                         threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("petroleumgrowth"), ~ winsorize(as.numeric(scale(.)),
                                                         threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("petroleumpergrowth"), ~ winsorize(as.numeric(scale(.)),
                                                         threshold = 0.05), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("prodvalgrowth"), ~ percent_rank(.)*100, 
                .names = "pctl_{.col}")) |>
  mutate(across(starts_with("growth"), ~ percent_rank(.)*100, 
                .names = "pctl_{.col}"))

countylevel_all <- countylevel_merged |> ## Adding in some place variables
  left_join(shapes1970, by = "countycode") |>
  left_join(unemp_1980, by = "countycode") |>
  left_join(cbp_agshares, by = "countycode") |>
  mutate(emppercap_1978 = `1978`/SHAPE_AREA,
         emppercap_1982 = `1982`/SHAPE_AREA,
         emppercap_1987 = `1987`/SHAPE_AREA,
         emppercap_1992 = `1992`/SHAPE_AREA,
         emppercap_1997 = `1997`/SHAPE_AREA,
         emppercap_2002 = `2002`/SHAPE_AREA,
         emppercap_2007 = `2007`/SHAPE_AREA,
         quintile_emppercap = ntile(emppercap_1978, 5),
         quintile_agshare = ntile(agshare_1978, 5))

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

##############################
### Getting the crop-by-county level
##############################

acres_growth <- acres_all |>
  inner_join(acres_all, by = c("countycode", "crop"),
             suffix = c("_from", "_to"),
             relationship = "many-to-many") %>%
  filter(year_from < year_to) %>%
  mutate(growth_rate = if_else(acres_from == 0, NA, 
                               (acres_to - acres_from) / acres_from)) |>
  ungroup() |>
  group_by(crop, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                                  threshold = 0.05)) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, crop) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05)) |>
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
                               (acres_to - acres_from) / acres_from)) |>
  ungroup() |>
  group_by(group, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                      threshold = 0.05)) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, group) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05)) |>
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
                               (acres_to - acres_from) / acres_from)) |>
  ungroup() |>
  group_by(prodquint, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                      threshold = 0.05)) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, prodquint) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05)) |>
  ungroup() |>
  filter(!is.na(growth_rate))

## And the animal-by-county level
animals_growth <- animals_all |>
  inner_join(animals_all, by = c("countycode", "animal"),
             suffix = c("_from", "_to"),
             relationship = "many-to-many") %>%
  filter(year_from < year_to) %>%
  mutate(growth_rate = if_else(number_from == 0, NA, 
                               (number_to - number_from) / number_from)) |>
  ungroup() |>
  group_by(animal, year_from, year_to) |>
  mutate(growth_rate_norm = winsorize(as.numeric(scale(growth_rate)),
                                      threshold = 0.05)) |>
  ungroup() |>
  left_join(growth_rates_nopivot, by = c("countycode", "year_from" = "year_base",
                                         "year_to" = "year_end")) |>
  group_by(year_from, year_to, animal) |>
  mutate(econgrowth_norm = winsorize(as.numeric(scale(econgrowth)),
                                     threshold = 0.05)) |>
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
  growth_rates_forhere <- growth_rates |>
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
  
  cwg <- t(acres_mat)%*%cropshifts |>
    as.data.frame() |>
    rownames_to_column("crop")
  
  cropgrowths <- cropgrowths |>
    left_join(cwg, by = "crop") 
    } } 
  cws <- t(acres_mat)%*%cropbase |>
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
                                     threshold = 0.05)) |>
  ungroup() |>
  mutate(weight = log(econshare)) |>
  filter(weight > 0)