## Loading all necessary libraries
library(Matrix)
library(sf)

#############################
### Writing a function of what crops are in which category
#############################

crop_cat <- function(x){case_when(
  x %in% c("alfalfa", "bahiagrassseed", "birdsfootseed", "bluegrassseed",
           "bromegrassseed", "crimsoncloverseed", "fescueseed",
           "lespedezaseed", "orchardgrassseed", "redcloverseed",
           "redtopseed", "ryegrassseed", "sweetcloverseed",
           "sudangrassseed", "timothyseed", "vetchseed",
           "wheatgrassseed", "winterpeasseed") ~ "hay_seed",
  x %in% c("apples", "apricots", "avocados", "cherries",
           "figs", "grapefruit", "honeytangerines", "kumquats",
           "lemons", "limes", "mangoes", "nectarines", "oranges",
           "papayas", "peaches", "pears", "persimmons", "plums",
           "tangelos", "tangerines") ~ "fruit_trees",
  x %in% c("asparagus", "broccoli", "brusselssprouts", "cauliflower",
           "celery", "cucumbers", "eggplant", "hotpeppers", "okra",
           "pimentos", "popcorn", "pumpkins", "squash", "sweetcorn",
           "sweetpeppers", "tomatoes") ~ "vegetables_misc",
  x %in% c("beets", "carrots", "garlic", "greenonions", "onions",
           "potatoes", "radishes", "sweetpotatoes", "turnips") ~ "root_vegetables",
  x %in% c("bananas", "cantaloups", "grapes", "honeydew", "kiwifruit",
           "rhubarb", "watermelons") ~ "fruit_misc",
  x %in% c("collards", "endive", "escarole", "headcabbage", "kale",
           "lettuce", "mustardgreens", "parsley", "spinach",
           "turnipgreens") ~ "leaves",
  x %in% c("barley", "buckwheat", "emmerspelt", "graincorn", "oats",
           "proso_millet", "rice", "rye", "triticale", "wheat",
           "wildrice") ~ "grains",
  x %in% c("blackberries", "blueberries", "cranberries", "raspberries",
           "strawberries", "wildblueberries") ~ "berries",
  x %in% c("drybeans", "drycowpeas", "drypeas", "greencowpeas",
           "greenlima", "greenpeas", "lentils", "mungbeans", "snapbeans",
           "soybeans", "peanuts") ~ "legumes",
  x %in% c("hazelnuts", "pecans", "pistachios", "walnuts") ~ "nuts",
  x %in% c("cotton", "flaxseed", "guar", "mintoil", "mustardseed",
           "rapeseed", "safflower", "sugarbeets", "sugarcane", "sunflower",
           "tobacco", "sorghum") ~ "misc",
  .default = NA
)}

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
summarystats <- acres_1978_mod |>
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
library(dplyr)

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
  mutate(across(numeric(), ~ na_if(.x, -9.99))) |>
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
  left_join(countyprodgrowth, by = "countycode") |>
  left_join(countyprodfarmsall, by = "countycode") |>
  left_join(countyprodfarmacrevalueall, by = "countycode") |>
  left_join(countyprodfarmlandall, by = "countycode") |>
  left_join(countyprodfarmsizeall, by = "countycode") |>
  left_join(countyprodfarmvalueall, by = "countycode") |>
  left_join(countyprodirrlandall, by = "countycode") |>
  left_join(countyprodnonirrlandall, by = "countycode") |>
  left_join(countyprodvalall, by = "countycode") |>
  left_join(shiftshare_all, by = "countycode") |>
  left_join(drought_all, by = "countycode") |>
  left_join(precip, by = "countycode") |>
  left_join(growth_rates_nopollute, by = "countycode") |>
  left_join(growth_rates_pollute, by = "countycode") |>
  filter(cropland_1978 != 0 & cropland_1982 != 0 & cropland_1987 != 0 &
           cropland_1992 != 0 & cropland_1997 != 0 & cropland_2002 != 0 &
           cropland_2007 != 0 & cropland_2012 != 0) |>
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA))) |>
  mutate(across(starts_with("croplandgrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmsgrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmacrevaluegrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmlandgrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmsizegrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("farmvaluegrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("irrlandgrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("nonirrlandgrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("prodvalgrowth"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  mutate(across(starts_with("shiftshare"), ~ as.numeric(scale(.)), 
                .names = "norm_{.col}")) |>
  left_join(shapes1970, by = "countycode") |>
  left_join(unemp_1980, by = "countycode") |>
  mutate(emppercap_1978 = `1978`/SHAPE_AREA,
         emppercap_1982 = `1982`/SHAPE_AREA,
         emppercap_1987 = `1987`/SHAPE_AREA,
         emppercap_1992 = `1992`/SHAPE_AREA,
         emppercap_1997 = `1997`/SHAPE_AREA,
         emppercap_2002 = `2002`/SHAPE_AREA,
         emppercap_2007 = `2007`/SHAPE_AREA)

################################
### Getting total stats instead
################################

cropland <- countylevel_all |>
  select(countycode, cropland_1978, cropland_1982, cropland_1987, cropland_1992, cropland_1997,
         cropland_2002, cropland_2007, cropland_2012) |>
  pivot_longer(-countycode, names_to = "year", values_to = "acres") |>
  group_by(year) |>
  summarize(totalcropland = sum(acres, na.rm = TRUE)) |>
  mutate(year = as.integer(str_sub(year, 10, -1))) |>
  ungroup()

totfarms <- countylevel_all |>
  select(countycode, farms_1978, farms_1982, farms_1987, farms_1992, farms_1997,
         farms_2002, farms_2007, farms_2012) |>
  pivot_longer(-countycode, names_to = "year", values_to = "farms") |>
  group_by(year) |>
  summarize(totalfarms = sum(farms, na.rm = TRUE)) |>
  mutate(year = as.integer(str_sub(year, 7, -1))) |>
  ungroup()