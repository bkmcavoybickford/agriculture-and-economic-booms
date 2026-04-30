## This file loads all the agricultural census data and performs the first stages of datacleaning

## Setting a function to label the csv files
agcensus_label <- function(x){
  y <- x |>
    select(statefip, counfip, level, name, "Corn (Grain)" = item24002, "Sorghum (grain)" = item24007,
           "Wheat" = item24012, "Barley" = item24017, "Buckwheat" = item24022,
           "Emmer and spelt" = item24027, "Flaxseed" = item24032, "Mustard seed" = item24042,
           "Oats" = item24047, "Popcorn" = item24052, "Proso millet" = item24057,
           "Rice" = item24062, "Rye" = item24067, "Safflower" = item24072, 
           "Sunflower" = item24077, "Triticale" = item24082, "Wild rice" = item24087,
           "Cotton" = item25002, "Tobacco" = item25007, "Soybeans" = item25012,
           "Dry beans" = item25017, "Dry lima beans" = item25022, "Dry peas" = item25027,
           "Dry cowpeas" = item25032, "Lentils" = item25037, "Potatoes" = item25042,
           "Sweet potatoes" = item25047, "sugarbeets1" = item25052, 
           "sugarbeets2" = item25057, "sugarcane1" = item25062, "sugarcane2" = item25067,
           "sugarcane3" = item25072, "sugarcane4" = item25077, "Peanuts" = item25082,
           "alfalfaseed" = item26007, "Alsike clover seed" = item26012, 
           "Winter peas seed" = item26017, "Bahia grass seed" = item26022, 
           "Bentgrass seed" = item26027, "Bermuda grass seed" = item26032,
           "Birdsfoot trefoil seed" = item26037, "Bromegrass seed" = item26042, 
           "Crimson clover seed" = item26047, "Fescue seed" = item26052, 
           "Foxtail millet seed" = item26057, "Bluegrass seed" = item26062,
           "Ladino clover seed" = item26067, "Lespedeza seed" = item26072, 
           "Orchard grass seed" = item26077, "Red clover seed" = item26082, 
           "Redtop grass seed" = item26087, "Ryegrass seed" = item26092,
           "Sudan grass seed" = item26097, "Sweet clover seed" = item26102,
           "Timothy seed" = item26107, "Vetch seed" = item26112, "Wheatgrass seed" = item26117,
           "White clover seed" = item26122, "alfalfahay" = item26137, "sorghumhay" = item26167,
           "sorghumsilage" = item26172, "Artichokes" = item27006, "Asparagus" = item27010,
           "Green lima beans" = item27014, "Snap beans" = item27018, "Beets" = item27022,
           "Broccoli" = item27026, "Brussels sprouts" = item27030, 
           "Chinese cabbage" = item27034, "Cabbages (Head)" = item27038, 
           "Mustard cabbage" = item27042, "Cantaloupes" = item27046, "Carrots" = item27050,
           "Cauliflower" = item27054, "Celery" = item27058, "Chicory" = item27062,
           "Chinese peas" = item27066, "Collards" = item27070, "Green cowpeas" = item27074,
           "Cucumbers" = item27078, "Daikon" = item27082, "Eggplant" = item27086,
           "Endive" = item27090, "escarole" = item27094, "Garlic" = item27098,
           "Honeydew melon" = item27102, "Kale" = item27106, "Lettuce" = item27110,
           "Mustard greens" = item27114, "Onions" = item27118, "Green onions" = item27122,
           "Okra" = item27126, "Parsley" = item27130, "Green peas" = item27134,
           "hotpeppers" = item27138, "Bell peppers" = item27142, "pimentos" = item27146,
           "Pumpkins" = item27150, "Radishes" = item27154, "Rhubarb" = item27158,
           "Shallots" = item27162, "Spinach" = item27166, "Squash" = item27170,
           "Sweet corn" = item27174, "Tomatoes" = item27178, "Turnips" = item27182,
           "Turnip greens" = item27186, "Watercress" = item27194, "Watermelons" = item27198,
           "Apples" = item28011, "Apricots" = item28020, "Avocados" = item28029,
           "Bananas" = item28038, "Cherries" = item28047, "Coffee" = item28083,
           "Dates" = item28092, "Figs" = item28101, "Grapes" = item28110,
           "Guavas" = item28119, "Kiwis" = item28128, "Mangoes" = item28137,
           "Nectarines" = item28146, "Olives" = item28155, "Papayas" = item28164,
           "Passion fruit" = item28173, "Peaches" = item28182, "Pears" = item28191,
           "Persimmons" = item28200, "Plums" = item28209, "Pomegranates" = item28218,
           "Grapefruits" = item28245, "Kumquats" = item28254, "Lemons" = item28263,
           "Limes" = item28272, "Oranges" = item28281, "Tangelos" = item28290,
           "honeytangerines" = item28299, "Tangerines" = item28308, "Almonds" = item28326,
           "Hazelnuts" = item28335, "Macadamia nuts" = item28344, "Pecans" = item28353,
           "Pistachios" = item28362, "Walnuts" = item28371, "Blackberries" = item29007,
           "Blueberries" = item29012, "Wild blueberries" = item29017, 
           "Boysenberries" = item29022, "Cranberries" = item29027, "Currants" = item29032,
           "Loganberries" = item29037, "Raspberries" = item29042, 
           "Strawberries" = item29047, "Mushrooms" = item30035, "Dill for oil" = item31007,
           "Ginger" = item31012, "Guar" = item31017, "Hops" = item31027,
           "jojoba" = item31032, "Lotus" = item31042, 
           "Mint for oil" = item31047, "pineapples2" = item31062,
           "mungbeans" = item31052, "pineapples1" = item31057, "rapeseed" = item31067,
           "salthay" = item31072, "sweetsorghum" = item31077, "Taro" = item31092) |>
    filter(level == 1) |>
    mutate(across(everything(), ~replace_na(., 0))) |>
    mutate(Sorghum = sorghumhay + sorghumsilage,
           Alfalfa = alfalfahay + alfalfaseed,
           `Sugar cane` = sugarcane1 + sugarcane2 + sugarcane3 + sugarcane4,
           `Sugar beets` = sugarbeets1 + sugarbeets2,
           `Other peppers` = hotpeppers + pimentos,
           Pineapples = pineapples1 + pineapples2) |>
    select(-c(sorghumhay, sorghumsilage, alfalfahay, alfalfaseed, sugarcane1,
              sugarcane2, sugarcane3, sugarcane4, sugarbeets1, sugarbeets2, 
              pineapples1, pineapples2, hotpeppers, pimentos))
return(y)
}

#########################
### Loading the actual data
#########################

## 1978
acres_1978 <- read_delim("Data/ICPSR_35206/DS0039/35206-0039-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  agcensus_label()

## 1982
acres_1982 <- read_delim("Data/ICPSR_35206/DS0040/35206-0040-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  agcensus_label()

## 1987
acres_1987 <- read_delim("Data/ICPSR_35206/DS0041/35206-0041-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  agcensus_label()

## 1992
acres_1992 <- read_delim("Data/ICPSR_35206/DS0042/35206-0042-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  select(statefip, counfip, level, name, "Corn (Grain)" = item260002, "Sorghum (grain)" = item260007,
         "Wheat" = item260012, "Barley" = item260042, "Buckwheat" = item260047,
         "Emmer and spelt" = item260067, "Flaxseed" = item260072, "Mustard seed" = item260077,
         "Oats" = item260082, "Popcorn" = item260087, "Proso millet" = item260092,
         "Rice" = item260097, "Rye" = item260102, "Safflower" = item260107, 
         "Sunflower" = item260112, "Triticale" = item260117, "Wild rice" = item260122,
         "Cotton" = item270002, "Tobacco" = item270007, "Soybeans" = item270012,
         "Dry beans" = item270017, "Dry lima beans" = item270022, "Dry peas" = item270027,
         "Dry cowpeas" = item270032, "Lentils" = item270037, "Potatoes" = item270042,
         "Sweet potatoes" = item270047, "sugarbeets1" = item270052, 
         "sugarbeets2" = item270057, "sugarcane1" = item270062, "sugarcane2" = item270067,
         "sugarcane3" = item270072, "Peanuts" = item270077,
         "alfalfaseed" = item280007,
         "Winter peas seed" = item280012, "Bahia grass seed" = item280017, 
         "Bentgrass seed" = item280022, "Bermuda grass seed" = item280027,
         "Birdsfoot trefoil seed" = item280032, "Bromegrass seed" = item280037, 
         "Crimson clover seed" = item280042, "Fescue seed" = item280047, 
         "Foxtail millet seed" = item280052, "Bluegrass seed" = item280057,
         "Ladino clover seed" = item280062, "Lespedeza seed" = item280067, 
         "Orchard grass seed" = item280072, "Red clover seed" = item280077, 
         "Redtop grass seed" = item280082, "Ryegrass seed" = item280087,
         "Sudan grass seed" = item280092, "Sweet clover seed" = item280097,
         "Timothy seed" = item280102, "Vetch seed" = item280107, "Wheatgrass seed" = item280112,
         "White clover seed" = item280117, "alfalfahay" = item280132, "sorghumhay" = item280162,
         "sorghumsilage" = item280167, "Artichokes" = item290010, "Asparagus" = item290014,
         "Green lima beans" = item290018, "Snap beans" = item290022, "Beets" = item290026,
         "Broccoli" = item290030, "Brussels sprouts" = item290034, 
         "Chinese cabbage" = item290038, "Cabbages (Head)" = item290042, 
         "Mustard cabbage" = item290046, "Cantaloupes" = item290050, "Carrots" = item290054,
         "Cauliflower" = item290058, "Celery" = item290062, "Chicory" = item290066,
         "Chinese peas" = item290070, "Collards" = item290074, "Green cowpeas" = item290078,
         "Cucumbers" = item290082, "Daikon" = item290086, "Eggplant" = item290090,
         "Endive" = item290094, "escarole" = item290098, "Garlic" = item290102,
         "Honeydew melon" = item290110, "Kale" = item290114, "Lettuce" = item290118,
         "Mustard greens" = item290122, "Onions" = item290126, "Green onions" = item290130,
         "Okra" = item290134, "Parsley" = item290138, "Green peas" = item290142,
         "hotpeppers" = item290146, "Bell peppers" = item290150, "pimentos" = item290154,
         "Pumpkins" = item290158, "Radishes" = item290162, "Rhubarb" = item290166,
         "Spinach" = item290170, "Squash" = item290174,
         "Sweet corn" = item290178, "Tomatoes" = item290182, "Turnips" = item290186,
         "Turnip greens" = item290190, "Watercress" = item290198, "Watermelons" = item290202,
         "Apples" = item310002, "Apricots" = item310011, "Avocados" = item310020,
         "Bananas" = item310029, "Cherries" = item310038, "Coffee" = item310074,
         "Dates" = item310083, "Figs" = item310092, "Grapes" = item310101,
         "Guavas" = item310110, "Kiwis" = item310119, "Mangoes" = item310128,
         "Nectarines" = item310137, "Olives" = item310146, "Papayas" = item310155,
         "Passion fruit" = item310164, "Peaches" = item310173, "Pears" = item310182,
         "Persimmons" = item310191, "Plums" = item310200, "Pomegranates" = item310209,
         "Grapefruits" = item310236, "Kumquats" = item310245, "Lemons" = item310254,
         "Limes" = item310263, "Oranges" = item310272, "Tangelos" = item310281,
         "honeytangerines" = item310290, "Tangerines" = item310299, "Almonds" = item310317,
         "Hazelnuts" = item310326, "Macadamia nuts" = item310335, "Pecans" = item310344,
         "Pistachios" = item310353, "Walnuts" = item310362, "Blackberries" = item320007,
         "Blueberries" = item320012, "Wild blueberries" = item320017, 
         "Boysenberries" = item320022, "Cranberries" = item320027, "Currants" = item320032,
         "Loganberries" = item320037, "Raspberries" = item320042, 
         "Strawberries" = item320047, "Mushrooms" = item330035, "Dill for oil" = item340006,
         "Ginger" = item340012, "Guar" = item340022, "Hops" = item340037,
         "Jojoba" = item340042, "Lotus" = item340052, "Mint for oil" = item340057,
         "pineapples2" = item340072,
         "mungbeans" = item340062, "pineapples1" = item340067, "rapeseed" = item260052,
         "salthay" = item340077, "sweetsorghum" = item340082, "Taro" = item340097) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(Sorghum = sorghumhay + sorghumsilage,
         Alfalfa = alfalfahay + alfalfaseed,
         `Sugar cane` = sugarcane1 + sugarcane2 + sugarcane3,
         `Sugar beets` = sugarbeets1 + sugarbeets2,
         Pineapples = pineapples1 + pineapples2,
         `Other peppers` = hotpeppers + pimentos) |>
  select(-c(sorghumhay, sorghumsilage, alfalfahay, alfalfaseed, sugarcane1,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2, pineapples1,
            pineapples2, hotpeppers, pimentos))

## 1997
acres_1997 <- read_delim("Data/ICPSR_35206/DS0043/35206-0043-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  select("statefip" = STATEFIP, "counfip" = COUNFIP, "level" = LEVEL, "name" = NAME,
         "Corn (Grain)" = ITEM26002, "Sorghum (grain)" = ITEM26007,
         "Wheat" = ITEM26012, "Barley" = ITEM26042, "Buckwheat" = ITEM26047,
         "Emmer and spelt" = ITEM26067, "Flaxseed" = ITEM26072, "Mustard seed" = ITEM26077,
         "Oats" = ITEM26082, "Popcorn" = ITEM26087, "Proso millet" = ITEM26092,
         "Rice" = ITEM26097, "Rye" = ITEM26102, "Safflower" = ITEM26107, 
         "Sunflower" = ITEM26112, "Triticale" = ITEM26117, "Wild rice" = ITEM26122,
         "Cotton" = ITEM27002, "Tobacco" = ITEM27007, "Soybeans" = ITEM27012,
         "Dry beans" = ITEM27017, "Dry lima beans" = ITEM27022, "Dry peas" = ITEM27027,
         "Dry cowpeas" = ITEM27032, "Lentils" = ITEM27037, "Potatoes" = ITEM27042,
         "Sweet potatoes" = ITEM27047, "sugarbeets1" = ITEM27052, 
         "sugarbeets2" = ITEM27057, "sugarcane1" = ITEM27062, "sugarcane2" = ITEM27067,
         "sugarcane3" = ITEM27072, "Peanuts" = ITEM27077,
         "alfalfaseed" = ITEM28007,
         "Winter peas seed" = ITEM28012, "Bahia grass seed" = ITEM28017, 
         "Bentgrass seed" = ITEM28022, "Bermuda grass seed" = ITEM28027,
         "Birdsfoot trefoil seed" = ITEM28032, "Bromegrass seed" = ITEM28037, 
         "Crimson clover seed" = ITEM28042, "Fescue seed" = ITEM28047, 
         "Foxtail millet seed" = ITEM28052, "Bluegrass seed" = ITEM28057,
         "Ladino clover seed" = ITEM28062, "Lespedeza seed" = ITEM28067, 
         "Orchard grass seed" = ITEM28072, "Red clover seed" = ITEM28077, 
         "Redtop grass seed" = ITEM28082, "Ryegrass seed" = ITEM28087,
         "Sudan grass seed" = ITEM28092, "Sweet clover seed" = ITEM28097,
         "Timothy seed" = ITEM28102, "Vetch seed" = ITEM28107, "Wheatgrass seed" = ITEM28112,
         "White clover seed" = ITEM28117, "alfalfahay" = ITEM28132, "sorghumhay" = ITEM28162,
         "sorghumsilage" = ITEM28167, "Artichokes" = ITEM29010, "Asparagus" = ITEM29014,
         "Green lima beans" = ITEM29018, "Snap beans" = ITEM29022, "Beets" = ITEM29026,
         "Broccoli" = ITEM29030, "Brussels sprouts" = ITEM29034, 
         "Chinese cabbage" = ITEM29038, "Cabbages (Head)" = ITEM29042, 
         "Mustard cabbage" = ITEM29046, "Cantaloupes" = ITEM29050, "Carrots" = ITEM29054,
         "Cauliflower" = ITEM29058, "Celery" = ITEM29062, "Chicory" = ITEM29066,
         "Chinese peas" = ITEM29070, "Collards" = ITEM29074, "Green cowpeas" = ITEM29078,
         "Cucumbers" = ITEM29082, "Daikon" = ITEM29086, "Eggplant" = ITEM29090,
         "Endive" = ITEM29094, "escarole" = ITEM29098, "Garlic" = ITEM29102,
         "Honeydew melon" = ITEM29110, "Kale" = ITEM29114, "Lettuce" = ITEM29118,
         "Mustard greens" = ITEM29122, "Onions" = ITEM29126, "Green onions" = ITEM29130,
         "Okra" = ITEM29134, "Parsley" = ITEM29138, "Green peas" = ITEM29142,
         "hotpeppers" = ITEM29146, "Bell peppers" = ITEM29150, "pimentos" = ITEM29154,
         "Pumpkins" = ITEM29158, "Radishes" = ITEM29162, "Rhubarb" = ITEM29166,
         "Spinach" = ITEM29170, "Squash" = ITEM29174,
         "Sweet corn" = ITEM29178, "Tomatoes" = ITEM29182, "Turnips" = ITEM29186,
         "Turnip greens" = ITEM29190, "Watercress" = ITEM29198, "Watermelons" = ITEM29202,
         "Apples" = ITEM31002, "Apricots" = ITEM31011, "Avocados" = ITEM31020,
         "Bananas" = ITEM31029, "Cherries" = ITEM31038, "Coffee" = ITEM31074,
         "Dates" = ITEM31083, "Figs" = ITEM31092, "Grapes" = ITEM31101,
         "Guavas" = ITEM31110, "Kiwis" = ITEM31119, "Mangoes" = ITEM31128,
         "Nectarines" = ITEM31137, "Olives" = ITEM31146, "Papayas" = ITEM31155,
         "Passion fruit" = ITEM31164, "Peaches" = ITEM31173, "Pears" = ITEM31182,
         "Persimmons" = ITEM31191, "Plums" = ITEM31200, "Pomegranates" = ITEM31209,
         "Grapefruits" = ITEM31236, "Kumquats" = ITEM31245, "Lemons" = ITEM31254,
         "Limes" = ITEM31263, "Oranges" = ITEM31272, "Tangelos" = ITEM31281,
         "honeytangerines" = ITEM31290, "Tangerines" = ITEM31299, "Almonds" = ITEM31317,
         "Hazelnuts" = ITEM31326, "Macadamia nuts" = ITEM31335, "Pecans" = ITEM31344,
         "Pistachios" = ITEM31353, "Walnuts" = ITEM31362, "Blackberries" = ITEM32007,
         "Blueberries" = ITEM32012, "Wild blueberries" = ITEM32017, 
         "Boysenberries" = ITEM32022, "Cranberries" = ITEM32027, "Currants" = ITEM32032,
         "Loganberries" = ITEM32037, "Raspberries" = ITEM32042, 
         "Strawberries" = ITEM32047, "Mushrooms" = ITEM33035, "Dill for oil" = ITEM34006,
         "Ginger" = ITEM34012, "Guar" = ITEM34022, "Hops" = ITEM34037,
         "Jojoba" = ITEM34042, "Lotus" = ITEM34052, "Mint for oil" = ITEM34062,
         "mungbeans" = ITEM34067, "pineapples1" = ITEM34072, "rapeseed" = ITEM26052,
         "salthay" = ITEM34082, "sweetsorghum" = ITEM34087, "Taro" = ITEM34102,
         "pineapples2" = ITEM34077) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(Sorghum = sorghumhay + sorghumsilage,
         Alfalfa = alfalfahay + alfalfaseed,
         `Sugar cane` = sugarcane1 + sugarcane2 + sugarcane3,
         `Sugar beets` = sugarbeets1 + sugarbeets2,
         Pineapples = pineapples1 + pineapples2,
         `Other peppers` = hotpeppers + pimentos) |>
  select(-c(sorghumhay, sorghumsilage, alfalfahay, alfalfaseed, sugarcane1,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2, hotpeppers, pimentos,
            pineapples1, pineapples2))

## 2002
acres_2002 <- read_delim("Data/ICPSR_35206/DS0044/35206-0044-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  select(statefip, counfip, level, name,
         "Corn (Grain)" = item24002, "Sorghum (grain)" = item24012,
         "Wheat" = item24022, "Barley" = item24062, "Buckwheat" = item24072,
         "Emmer and spelt" = item24102, "Flaxseed" = item24112, "Mustard seed" = item24122,
         "Oats" = item24132, "Popcorn" = item24142, "Proso millet" = item24152,
         "Rice" = item24162, "Rye" = item24172, "Safflower" = item24182, 
         "Sunflower" = item24192, "Triticale" = item24222, "Wild rice" = item24232,
         "Cotton" = item25002, "Tobacco" = item25032, "Soybeans" = item25042,
         "Dry beans" = item25052, "Dry lima beans" = item25062, "Dry peas" = item25072,
         "Dry cowpeas" = item25082, "Lentils" = item25092, "Potatoes" = item25102,
         "Sweet potatoes" = item25112, "sugarbeets1" = item25122, 
         "sugarbeets2" = item25132, "sugarcane1" = item25142, "sugarcane2" = item25152,
         "sugarcane3" = item25162, "Peanuts" = item25172,
         "alfalfaseed" = item26012,
         "Winter peas seed" = item26022, "Bahia grass seed" = item26032, 
         "Bentgrass seed" = item26042, "Bermuda grass seed" = item26052,
         "Birdsfoot trefoil seed" = item26062, "Bromegrass seed" = item26072, 
         "Crimson clover seed" = item26082, "Fescue seed" = item26092, 
         "Foxtail millet seed" = item26102, "Bluegrass seed" = item26112,
         "Ladino clover seed" = item26122, "Lespedeza seed" = item26132, 
         "Orchard grass seed" = item26142, "Red clover seed" = item26152, 
         "Redtop grass seed" = item26162, "Ryegrass seed" = item26172,
         "Sudan grass seed" = item26182, "Sweet clover seed" = item26192,
         "Timothy seed" = item26202, "Vetch seed" = item26212, "Wheatgrass seed" = item26222,
         "White clover seed" = item26232, "alfalfahay" = item26262,
         "Artichokes" = item29008, "Asparagus" = item29014,
         "Green lima beans" = item29020, "Snap beans" = item29026, "Beets" = item29032,
         "Broccoli" = item29038, "Brussels sprouts" = item29044, 
         "Chinese cabbage" = item29050, "Cabbages (Head)" = item29056, 
         "Mustard cabbage" = item29062, "Cantaloupes" = item29068, "Carrots" = item29074,
         "Cauliflower" = item29080, "Celery" = item29086, "Chicory" = item29092,
         "Chinese peas" = item29206, "Collards" = item29098, "Green cowpeas" = item29218,
         "Cucumbers" = item29104, "Daikon" = item29110, "Eggplant" = item29116,
         "Garlic" = item29128,
         "Honeydew melon" = item29140, "Kale" = item29146, "Lettuce" = item29152,
         "Mustard greens" = item29176, "Onions" = item29188, "Green onions" = item29194,
         "Okra" = item29182, "Parsley" = item29200, "Green peas" = item29212,
         "hotpeppers" = item29230, "Bell peppers" = item29224, "pimentos" = item29236,
         "Pumpkins" = item29242, "Radishes" = item29248, "Rhubarb" = item29254,
         "Spinach" = item29260, "Squash" = item29266,
         "Sweet corn" = item29272, "Tomatoes" = item29278, "Turnips" = item29284,
         "Turnip greens" = item29290, "Watercress" = item29308, "Watermelons" = item29314,
         "Apples" = item31002, "Apricots" = item31014, "Avocados" = item31026,
         "Bananas" = item31038, "Cherries" = item31050, "Coffee" = item31074,
         "Dates" = item31086, "Figs" = item31098, "Grapes" = item31110,
         "Guavas" = item31122, "Kiwis" = item31134, "Mangoes" = item31146,
         "Nectarines" = item31158, "Olives" = item31170, "Papayas" = item31182,
         "Passion fruit" = item31194, "Peaches" = item31206, "Pears" = item31242,
         "Persimmons" = item31278, "Plums" = item31290, "Pomegranates" = item31326,
         "Grapefruits" = item31362, "Kumquats" = item31386, "Lemons" = item31398,
         "Limes" = item31410, "Oranges" = item31422, "Tangelos" = item31458,
         "Tangerines" = item31470, "Almonds" = item31506,
         "Hazelnuts" = item31518, "Macadamia nuts" = item31530, "Pecans" = item31542,
         "Pistachios" = item31554, "Walnuts" = item31566, "Blackberries" = item33002,
         "Blueberries" = item33006, "Wild blueberries" = item33010, 
         "Boysenberries" = item33014, "Cranberries" = item33018, "Currants" = item33022,
         "Loganberries" = item33026, "Raspberries" = item33030, 
         "Strawberries" = item33042, "Dill for oil" = item27022,
         "Ginger" = item27032, "Guar" = item27052, "Hops" = item27072, 
         "Jojoba" = item27082, "Lotus" = item27102, "Mint for oil" = item27112,
         "mungbeans" = item27142, "pineapples1" = item27152, "pineapples2" = item27162,
         "salthay" = item27182, "sweetsorghum" = item27202, "Taro" = item27222) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(Alfalfa = alfalfahay + alfalfaseed,
         `Sugar cane` = sugarcane1 + sugarcane2 + sugarcane3,
         `Sugar beets` = sugarbeets1 + sugarbeets2,
         Pineapples = pineapples1 + pineapples2,
         `Other peppers` = hotpeppers + pimentos) |>
  select(-c(alfalfahay, alfalfaseed, sugarcane1, pineapples1, hotpeppers, pimentos,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2, pineapples2))

## 2007
acres_2007 <- read_delim("Data/ICPSR_35206/DS0045/35206-0045-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  select(statefip, "counfip" = countyfip, level, name,
         "Corn (Grain)" = data26_250, "Sorghum (grain)" = data26_2250,
         "Wheat" = data26_2380, "Barley" = data26_220, "Buckwheat" = data26_230,
         "Emmer and spelt" = data26_2130, "Flaxseed" = data26_2140, "Mustard seed" = data26_2160,
         "Oats" = data26_2170, "Popcorn" = data26_2190, "Proso millet" = data26_2200,
         "Rice" = data26_2220, "Rye" = data26_2230, "Safflower" = data26_2240, 
         "Sunflower" = data26_2320, "Triticale" = data26_2360, "Wild rice" = data26_2370,
         "Cotton" = data26_260, "Tobacco" = data26_2350, "Soybeans" = data26_2260,
         "Dry beans" = data26_290, "Dry lima beans" = data26_2100, "Dry peas" = data26_2110,
         "Dry cowpeas" = data26_2120, "Lentils" = data26_2150, "Potatoes" = data30_2420,
         "Sweet potatoes" = data30_2510, "sugarbeets1" = data26_2270, 
         "sugarbeets2" = data26_2280, "sugarcane1" = data26_2290, "sugarcane2" = data26_2300,
         "sugarcane3" = data26_2310, "Peanuts" = data26_2180,
         "alfalfaseed" = data27_220, "Bahia grass seed" = data27_230, 
         "Bentgrass seed" = data27_240, "Bermuda grass seed" = data27_250,
         "Birdsfoot trefoil seed" = data27_260, "Bromegrass seed" = data27_270, 
         "Crimson clover seed" = data27_280, "Fescue seed" = data27_290, 
         "Bluegrass seed" = data27_2100, "Winter peas seed" = data26_210,
         "Ladino clover seed" = data27_2110, "Lespedeza seed" = data27_2120, 
         "Orchard grass seed" = data27_2130, "Red clover seed" = data27_2140, 
         "Ryegrass seed" = data27_2150,
         "Sudan grass seed" = data27_2160,
         "Timothy seed" = data27_2170, "Vetch seed" = data27_2180, 
         "Wheatgrass seed" = data27_2190,
         "White clover seed" = data27_2200, "alfalfahay" = data27_2240,
         "Artichokes" = data30_220, "Asparagus" = data30_230,
         "Green lima beans" = data30_240, "Snap beans" = data30_250, "Beets" = data30_260,
         "Broccoli" = data30_270, "Brussels sprouts" = data30_280, 
         "Chinese cabbage" = data30_290, "Cabbages (Head)" = data30_2100, 
         "Mustard cabbage" = data30_2110, "Cantaloupes" = data30_2120, "Carrots" = data30_2130,
         "Cauliflower" = data30_2140, "Celery" = data30_2150, "Chicory" = data30_2160,
         "Chinese peas" = data30_2370, "Collards" = data30_2170, "Green cowpeas" = data30_2390,
         "Cucumbers" = data30_2180, "Daikon" = data30_2190, "Eggplant" = data30_2200,
         "Garlic" = data30_2220,
         "Honeydew melon" = data30_2250, "Kale" = data30_2270, "Lettuce" = data30_2280,
         "Mustard greens" = data30_2320, "Onions" = data30_2340, "Green onions" = data30_2350,
         "Okra" = data30_2330, "Parsley" = data30_2360, "Green peas" = data30_2380,
         "Other peppers" = data30_2410, "Bell peppers" = data30_2400,
         "Pumpkins" = data30_2430, "Radishes" = data30_2440, "Rhubarb" = data30_2450,
         "Spinach" = data30_2460, "Squash" = data30_2470,
         "Sweet corn" = data30_2500, "Tomatoes" = data30_2520, "Turnips" = data30_2530,
         "Turnip greens" = data30_2540, "Watercress" = data30_2550, "Watermelons" = data30_2560,
         "Apples" = data32_220, "Apricots" = data32_230, "Avocados" = data32_240,
         "Bananas" = data32_250, "cherries1" = data32_260,
         "cherries2" = data32_270, "Coffee" = data32_280,
         "Dates" = data32_290, "Figs" = data32_2100, "Grapes" = data32_2110,
         "Guavas" = data32_2120, "Kiwis" = data32_2130, "Mangoes" = data32_2140,
         "Nectarines" = data32_2150, "Olives" = data32_2160, "Papayas" = data32_2170,
         "Passion fruit" = data32_2180, "Peaches" = data32_2190, "Pears" = data32_2220,
         "Persimmons" = data32_2250, "Plums" = data32_2270, "Pomegranates" = data32_2300,
         "Grapefruits" = data32_2330, "Kumquats" = data32_2340, "Lemons" = data32_2350,
         "Limes" = data32_2360, "Oranges" = data32_2370, "Tangelos" = data32_2400,
         "Tangerines" = data32_2410, "Almonds" = data32_2450,
         "Hazelnuts" = data32_2470, "Macadamia nuts" = data32_2480, "Pecans" = data32_2490,
         "Pistachios" = data32_2520, "Walnuts" = data32_2530, "Blackberries" = data34_410,
         "Blueberries" = data34_420, "Wild blueberries" = data34_430, 
         "Boysenberries" = data34_440, "Cranberries" = data34_450, "Currants" = data34_460,
         "Loganberries" = data34_470, "Raspberries" = data34_480, 
         "Strawberries" = data34_4120, "Dill for oil" = data28_220,
         "Ginger" = data28_230, "Guar" = data28_240, "Hops" = data28_260, 
         "Jojoba" = data28_270, "Mint for oil" = data28_280,
         "pineapples1" = data28_2110, "pineapples2" = data28_2120,
         "sweetsorghum" = data28_2140, "Taro" = data28_2160) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(Alfalfa = alfalfahay + alfalfaseed,
         `Sugar cane` = sugarcane1 + sugarcane2 + sugarcane3,
         `Sugar beets` = sugarbeets1 + sugarbeets2,
         Pineapples = pineapples1 + pineapples2,
         Cherries = cherries1 + cherries2) |>
  select(-c(alfalfahay, alfalfaseed, sugarcane1, pineapples1,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2, pineapples2,
            cherries1, cherries2))

acres_2012 <- read_delim("Data/ICPSR_35206/DS0047/35206-0047-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  select("statefip" = STATEFIP, "counfip" = COFIPS, "level" = LEVEL, "name" = NAME,
         "Corn (Grain)" = DATA25_26, "Sorghum (grain)" = DATA25_227,
         "Wheat" = DATA25_240, "Barley" = DATA25_22, "Buckwheat" = DATA25_23,
         "Emmer and spelt" = DATA25_213, "Flaxseed" = DATA25_214, "Mustard seed" = DATA25_216,
         "Oats" = DATA25_217, "Popcorn" = DATA25_221, "Proso millet" = DATA25_222,
         "Rice" = DATA25_224, "Rye" = DATA25_225, "Safflower" = DATA25_226, 
         "Sunflower" = DATA25_234, "Triticale" = DATA25_238, "Wild rice" = DATA25_241,
         "Cotton" = DATA25_27, "Tobacco" = DATA25_237, "Soybeans" = DATA25_228,
         "Dry beans" = DATA25_28, "Dry lima beans" = DATA25_210, "Dry peas" = DATA25_29,
         "Dry cowpeas" = DATA25_211, "Lentils" = DATA25_215, "Potatoes" = DATA29_242,
         "Sweet potatoes" = DATA29_251, "sugarbeets1" = DATA25_229, 
         "sugarbeets2" = DATA25_230, "sugarcane1" = DATA25_231, "sugarcane2" = DATA25_232,
         "sugarcane3" = DATA25_233, "Peanuts" = DATA25_219,
         "alfalfaseed" = DATA26_22,
         "Winter peas seed" = DATA25_21, "Bahia grass seed" = DATA26_24, 
         "Bentgrass seed" = DATA26_25, "Bermuda grass seed" = DATA26_26,
         "Birdsfoot trefoil seed" = DATA26_27, "Bromegrass seed" = DATA26_28, 
         "Crimson clover seed" = DATA26_210, "Fescue seed" = DATA26_211, 
         "Bluegrass seed" = DATA26_216,
         "Ladino clover seed" = DATA26_217, "Lespedeza seed" = DATA26_218, 
         "Orchard grass seed" = DATA26_219, "Red clover seed" = DATA26_223, 
         "Ryegrass seed" = DATA26_224, "Sudan grass seed" = DATA26_227,
         "Timothy seed" = DATA26_228, "Vetch seed" = DATA26_229, 
         "Wheatgrass seed" = DATA26_230,
         "White clover seed" = DATA26_231, "alfalfahay" = DATA26_21,
         "Artichokes" = DATA29_21, "Asparagus" = DATA29_22,
         "Green lima beans" = DATA29_23, "Snap beans" = DATA29_24, "Beets" = DATA29_25,
         "Broccoli" = DATA29_26, "Brussels sprouts" = DATA29_27, 
         "Chinese cabbage" = DATA29_28, "Cabbages (Head)" = DATA29_29, 
         "Mustard cabbage" = DATA29_210, "Cantaloupes" = DATA29_211, 
         "Carrots" = DATA29_212,
         "Cauliflower" = DATA29_213, "Celery" = DATA29_214, "Chicory" = DATA29_215,
         "Chinese peas" = DATA29_237, "Collards" = DATA29_216, "Green cowpeas" = DATA29_239,
         "Cucumbers" = DATA29_217, "Daikon" = DATA29_218, "Eggplant" = DATA29_219,
         "Garlic" = DATA29_221,
         "Honeydew melon" = DATA29_224, "Kale" = DATA29_226, "Lettuce" = DATA29_227,
         "Mustard greens" = DATA29_231, "Onions" = DATA29_233, "Green onions" = DATA29_234,
         "Okra" = DATA29_232, "Parsley" = DATA29_236, "Green peas" = DATA29_238,
         "Other peppers" = DATA29_240, "Bell peppers" = DATA29_241,
         "Pumpkins" = DATA29_243, "Radishes" = DATA29_244, "Rhubarb" = DATA29_245,
         "Spinach" = DATA29_246, "Squash" = DATA29_247,
         "Sweet corn" = DATA29_250, "Tomatoes" = DATA29_252, "Turnips" = DATA29_254,
         "Turnip greens" = DATA29_253, "Watercress" = DATA29_256, "Watermelons" = DATA29_257,
         "Apples" = DATA31_23, "Apricots" = DATA31_25, "Avocados" = DATA31_27,
         "Bananas" = DATA31_29, "cherries1" = DATA31_211, "cherries2" = DATA31_213,
         "Coffee" = DATA31_219,
         "Dates" = DATA31_221, "Figs" = DATA31_223, "Grapes" = DATA31_227,
         "Guavas" = DATA31_229, "Kiwis" = DATA31_233, "Mangoes" = DATA31_243,
         "Nectarines" = DATA31_245, "Olives" = DATA31_251, "Papayas" = DATA31_263,
         "Passion fruit" = DATA31_265, "Peaches" = DATA31_267, "Pears" = DATA31_273,
         "Persimmons" = DATA31_285, "Plums" = DATA31_293, "Pomegranates" = DATA31_295,
         "Grapefruits" = DATA31_225, "Kumquats" = DATA31_235, "Lemons" = DATA31_237,
         "Limes" = DATA31_239, "Oranges" = DATA31_253, "Tangelos" = DATA31_299,
         "Tangerines" = DATA31_2101, "Almonds" = DATA31_21,
         "Hazelnuts" = DATA31_231, "Macadamia nuts" = DATA31_241, "Pecans" = DATA31_279,
         "Pistachios" = DATA31_287, "Walnuts" = DATA31_2107, "Blackberries" = DATA33_41,
         "Blueberries" = DATA33_43, "Wild blueberries" = DATA33_45, 
         "Boysenberries" = DATA33_47, "Cranberries" = DATA33_49, "Currants" = DATA33_411,
         "Loganberries" = DATA33_413, "Raspberries" = DATA33_417, 
         "Strawberries" = DATA33_423, "Dill for oil" = DATA27_21,
         "Ginger" = DATA27_22, "Guar" = DATA27_23, "Hops" = DATA27_25, 
         "Jojoba" = DATA27_26, "Mint for oil" = DATA27_27,
         "pineapples1" = DATA27_213, "pineapples2" = DATA27_214,
         "sweetsorghum" = DATA27_216, "Taro" = DATA27_219) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(Alfalfa = alfalfahay + alfalfaseed,
         `Sugar cane` = sugarcane1 + sugarcane2 + sugarcane3,
         `Sugar beets` = sugarbeets1 + sugarbeets2,
         Cherries = cherries1 + cherries2,
         Pineapples = pineapples1 + pineapples2) |>
  select(-c(alfalfahay, alfalfaseed, sugarcane1, pineapples1,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2, pineapples2,
            cherries1, cherries2))

###########################
### Modifying them for the format I'll generally use
###########################

acres_1978_mod <- acres_1978 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 1978) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

acres_1978_mod2 <- acres_1978 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 1978)

acres_1982_mod <- acres_1982 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 1982) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

acres_1987_mod <- acres_1987 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 1987) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

acres_1992_mod <- acres_1992 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 1992) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

acres_1997_mod <- acres_1997 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 1997) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

acres_2002_mod <- acres_2002 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 2002) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

acres_2007_mod <- acres_2007 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 2007) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

acres_2012_mod <- acres_2012 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(countycode = paste(statefip, counfip),
         year = 2012) |>
  select(-c(level, name, statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "crop", values_to = "acres")

names_1978 <- colnames(acres_1978)
names_1982 <- colnames(acres_1982)
names_1987 <- colnames(acres_1987)
names_1992 <- colnames(acres_1992)
names_1997 <- colnames(acres_1997)
names_2002 <- colnames(acres_2002)
names_2007 <- colnames(acres_2007)
names_2012 <- colnames(acres_2012)
names_all <- Reduce(intersect, list(names_1978, names_1982, names_1987, names_1992, names_1997,
                                    names_2002, names_2007, names_2012))

## Getting a list of all counties in common
common_counties <- intersect(cbp_bycounty$countycode, acres_1978_mod2$countycode)
anticommon_counties <- setdiff(subset(acres_1978_mod2, counfip != 998)$countycode, cbp_bycounty$countycode)
anticommon_counties2 <- setdiff(subset(cbp_bycounty, fipscty != 999)$countycode, acres_1978_mod2$countycode)


## Combining all the countries
acres_all <- rbind(acres_1978_mod, acres_1982_mod, acres_1987_mod, acres_1992_mod,
                   acres_1997_mod, acres_2002_mod, acres_2007_mod, acres_2012_mod) |>
  filter(crop %in% names_all & countycode %in% common_counties)

## Getting weights
##acres_weights <- acres_all |>
  ##mutate(weight = if_else(acres < 1, NA, log(acres))) |>
  ##select(-c(acres))

## Getting total crop numbers
cropprod <- acres_all |>
  group_by(crop, year) |>
  summarize(acres = sum(acres, na.rm = TRUE))

## Getting growths
cropchanges <- inner_join(cropprod, cropprod, by = "crop",
                          suffix = c("_base", "_end"),
                          relationship = "many-to-many") |>
  filter(year_base < year_end) %>%
  mutate(cropgrowth = if_else(acres_base == 0, NA, 
                               (acres_end - acres_base) / acres_base))

#################################
### Getting county-level total production
#################################

## 1978
countyprod_1978 <- read_delim("Data/ICPSR_35206/DS0039/35206-0039-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1978" = item01002,
         "farms_1978" = item01001, "farmsize_1978" = item01003,
         "farmvalue_1978" = item01004, "farmacrevalue_1978" = item01005,
         "cropland_1978" = item01014, "irrland_1978" = item01018,
         "prodval_1978" = item01019, "cropval_1978" = item01021,
         "nonmainops_1978" = item01031, "machinerypf_1978" = item01006,
         "fertilizer_1978" = item03013, "petroleum_1978" = item03017,
         "propertytax_1978" = item03045)

countyprod_1978_alt <- countyprod_1978 |>
  mutate(countycode = paste(statefip, counfip))

## 1982
countyprod_1982 <- read_delim("Data/ICPSR_35206/DS0040/35206-0040-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1982" = item01002,
         "farms_1982" = item01001, "farmsize_1982" = item01003,
         "farmvalue_1982" = item01004, "farmacrevalue_1982" = item01005,
         "cropland_1982" = item01014, "irrland_1982" = item01018,
         "prodval_1982" = item01019, "cropval_1982" = item01021, 
         "nonmainops_1982" = item01031, "machinerypf_1982" = item01006,
         "fertilizer_1982" = item03013, "petroleum_1982" = item03017,
         "propertytax_1982" = item03045)

## 1987
countyprod_1987 <- read_delim("Data/ICPSR_35206/DS0041/35206-0041-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1987" = item01002,
         "farms_1987" = item01001, "farmsize_1987" = item01003,
         "farmvalue_1987" = item01004, "farmacrevalue_1987" = item01005,
         "cropland_1987" = item01014, "irrland_1987" = item01018,
         "prodval_1987" = item01019, "cropval_1987" = item01021, 
         "nonmainops_1987" = item01031, "machinerypf_1987" = item01006,
         "fertilizer_1987" = item03013, "petroleum_1987" = item03017,
         "propertytax_1987" = item03045)

## 1992
countyprod_1992 <- read_delim("Data/ICPSR_35206/DS0042/35206-0042-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1992" = item010002,
         "farms_1992" = item010001, "farmsize_1992" = item010003,
         "farmvalue_1992" = item010004, "farmacrevalue_1992" = item010005,
         "cropland_1992" = item010014, "irrland_1992" = item010018,
         "prodval_1992" = item010019, "cropval_1992" = item010021,
         "nonmainops_1992" = item010036, "machinerypf_1992" = item010006,
         "fertilizer_1992" = item030013, "petroleum_1992" = item030017,
         "propertytax_1992" = item030045)

## 1997
countyprod_1997 <- read_delim("Data/ICPSR_35206/DS0043/35206-0043-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(LEVEL == 1) |>
  select("counfip" = COUNFIP, "statefip" = STATEFIP, "farmland_1997" = ITEM01002,
         "farms_1997" = ITEM01001, "farmsize_1997" = ITEM01003, 
         "farmvalue_1997" = ITEM01005, "farmacrevalue_1997" = ITEM01006,
         "cropland_1997" = ITEM01015, "irrland_1997" = ITEM01019,
         "prodval_1997" = ITEM01020, "cropval_1997" = ITEM01022, 
         "nonmainops_1997" = ITEM01037, "machinerypf_1997" = ITEM01007,
         "fertilizer_1997" = ITEM03013, "petroleum_1997" = ITEM03017,
         "propertytax_1997" = ITEM03045) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)

## 2002
countyprod_2002 <- read_delim("Data/ICPSR_35206/DS0044/35206-0044-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_2002" = item01002,
         "farms_2002" = item01001, "farmsize_2002" = item01003, 
         "farmvalue_2002" = item01005, "farmacrevalue_2002" = item01006,
         "cropland_2002" = item01015, "irrland_2002" = item01019,
         "prodval_2002" = item01020, "cropval_2002" = item01022, 
         "machinerypf_2002" = item01007, "fertilizer_2002" = item03009, 
         "petroleum_2002" = item03037, "propertytax_2002" = item03081) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)

## 2007
countyprod_2007 <- read_delim("Data/ICPSR_35206/DS0045/35206-0045-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(level == 1) |>
  select("counfip" = countyfip, statefip, "farmland_2007" = data1_2,
         "farms_2007" = data1_1, "farmsize_2007" = data1_3, 
         "farmvalue_2007" = data1_5, "farmacrevalue_2007" = data1_6,
         "cropland_2007" = data1_15, "irrland_2007" = data1_19,
         "prodval_2007" = data1_20, "cropval_2007" = data1_22,
         "machinerypf_2007" = data1_7, "fertilizer_2007" = data3_9, 
         "petroleum_2007" = data3_37, "propertytax_2007" = data3_81) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)

## 2012
countyprod_2012 <- read_delim("Data/ICPSR_35206/DS0047/35206-0047-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(LEVEL == 1) |>
  select("counfip" = COFIPS, "statefip" = STATEFIP, "farmland_2012" = DATA1_2,
         "farms_2012" = DATA1_1, "farmsize_2012" = DATA1_3, 
         "farmvalue_2012" = DATA1_5, "farmacrevalue_2012" = DATA1_6,
         "cropland_2012" = DATA1_16, "irrland_2012" = DATA1_20,
         "prodval_2012" = DATA1_21, "cropval_2012" = DATA1_23,
         "machinerypf_2012" = DATA1_8, "fertilizer_2012" = DATA3_9, 
         "petroleum_2012" = DATA3_37, "propertytax_2012" = DATA3_81) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)


## Joining them all together
countyprodall <- left_join(countyprod_1978, countyprod_1982, by = c("counfip", "statefip")) |>
  left_join(countyprod_1987, by = c("counfip", "statefip")) |>
  left_join(countyprod_1992, by = c("counfip", "statefip")) |>
  left_join(countyprod_1997, by = c("counfip", "statefip")) |>
  left_join(countyprod_2002, by = c("counfip", "statefip")) |>
  left_join(countyprod_2007, by = c("counfip", "statefip")) |>
  left_join(countyprod_2012, by = c("counfip", "statefip")) |>
  mutate(countycode = paste(statefip, counfip)) |>
  left_join(cbp_ag, by = "countycode") |>
  filter(countycode %in% common_counties) |>
  filter(!is.na(cropland_1978)) |>
  mutate(nonirrland_1978 = if_else(is.na(irrland_1978), cropland_1978, cropland_1978 - irrland_1978),
         nonirrland_1982 = if_else(is.na(irrland_1982), cropland_1982, cropland_1982 - irrland_1982),
         nonirrland_1987 = if_else(is.na(irrland_1987), cropland_1987, cropland_1987 - irrland_1987),
         nonirrland_1992 = if_else(is.na(irrland_1992), cropland_1992, cropland_1992 - irrland_1992),
         nonirrland_1997 = if_else(is.na(irrland_1997), cropland_1997, cropland_1997 - irrland_1997),
         nonirrland_2002 = if_else(is.na(irrland_2002), cropland_2002, cropland_2002 - irrland_2002),
         nonirrland_2007 = if_else(is.na(irrland_2007), cropland_2007, cropland_2007 - irrland_2007),
         nonirrland_2012 = if_else(is.na(irrland_2012), cropland_2012, cropland_2012 - irrland_2012),
         prodvalacre_1978 = if_else(is.na(farmland_1978), NA, prodval_1978/farmland_1978),
         prodvalacre_1982 = if_else(is.na(farmland_1982), NA, prodval_1982/farmland_1982),
         prodvalacre_1987 = if_else(is.na(farmland_1987), NA, prodval_1987/farmland_1987),
         prodvalacre_1992 = if_else(is.na(farmland_1992), NA, prodval_1992/farmland_1992),
         prodvalacre_1997 = if_else(is.na(farmland_1997), NA, prodval_1997/farmland_1997),
         prodvalacre_2002 = if_else(is.na(farmland_2002), NA, prodval_2002/farmland_2002),
         prodvalacre_2007 = if_else(is.na(farmland_2007), NA, prodval_2007/farmland_2007),
         prodvalacre_2012 = if_else(is.na(farmland_2012), NA, prodval_2012/farmland_2012),
         prodvalworker_1978 = if_else(is.na(agemp_1978), NA, prodval_1978/agemp_1978),
         prodvalworker_1982 = if_else(is.na(agemp_1982), NA, prodval_1982/agemp_1982),
         prodvalworker_1987 = if_else(is.na(agemp_1987), NA, prodval_1987/agemp_1987),
         prodvalworker_1992 = if_else(is.na(agemp_1992), NA, prodval_1992/agemp_1992),
         prodvalworker_1997 = if_else(is.na(agemp_1997), NA, prodval_1997/agemp_1997),
         prodvalworker_2002 = if_else(is.na(agemp_2002), NA, prodval_2002/agemp_2002),
         prodvalworker_2007 = if_else(is.na(agemp_2007), NA, prodval_2007/agemp_2007),
         prodvalworker_2012 = if_else(is.na(agemp_2012), NA, prodval_2012/agemp_2012),
         laborintensity_1978 = if_else(is.na(farmland_1978), NA, agemp_1978/farmland_1978),
         laborintensity_1982 = if_else(is.na(farmland_1982), NA, agemp_1982/farmland_1982),
         laborintensity_1987 = if_else(is.na(farmland_1987), NA, agemp_1987/farmland_1987),
         laborintensity_1992 = if_else(is.na(farmland_1992), NA, agemp_1992/farmland_1992),
         laborintensity_1997 = if_else(is.na(farmland_1997), NA, agemp_1997/farmland_1997),
         laborintensity_2002 = if_else(is.na(farmland_2002), NA, agemp_2002/farmland_2002),
         laborintensity_2007 = if_else(is.na(farmland_2007), NA, agemp_2007/farmland_2007),
         laborintensity_2012 = if_else(is.na(farmland_2012), NA, agemp_2012/farmland_2012),
         croplandratio_1978 = if_else(is.na(farmland_1978), NA, cropland_1978/farmland_1978),
         croplandratio_1982 = if_else(is.na(farmland_1982), NA, cropland_1982/farmland_1982),
         croplandratio_1987 = if_else(is.na(farmland_1987), NA, cropland_1987/farmland_1987),
         croplandratio_1992 = if_else(is.na(farmland_1992), NA, cropland_1992/farmland_1992),
         croplandratio_1997 = if_else(is.na(farmland_1997), NA, cropland_1997/farmland_1997),
         croplandratio_2002 = if_else(is.na(farmland_2002), NA, cropland_2002/farmland_2002),
         croplandratio_2007 = if_else(is.na(farmland_2007), NA, cropland_2007/farmland_2007),
         croplandratio_2012 = if_else(is.na(farmland_2012), NA, cropland_2012/farmland_2012),
         cropvalratio_1978 = if_else(is.na(prodval_1978), NA, cropval_1978/prodval_1978),
         cropvalratio_1982 = if_else(is.na(prodval_1982), NA, cropval_1982/prodval_1982),
         cropvalratio_1987 = if_else(is.na(prodval_1987), NA, cropval_1987/prodval_1987),
         cropvalratio_1992 = if_else(is.na(prodval_1992), NA, cropval_1992/prodval_1992),
         cropvalratio_1997 = if_else(is.na(prodval_1997), NA, cropval_1997/prodval_1997),
         cropvalratio_2002 = if_else(is.na(prodval_2002), NA, cropval_2002/prodval_2002),
         cropvalratio_2007 = if_else(is.na(prodval_2007), NA, cropval_2007/prodval_2007),
         cropvalratio_2012 = if_else(is.na(prodval_2012), NA, cropval_2012/prodval_2012),
         machinery_1978 = machinerypf_1978*farms_1978,
         machinery_1982 = machinerypf_1982*farms_1982,
         machinery_1987 = machinerypf_1987*farms_1987,
         machinery_1992 = machinerypf_1992*farms_1992,
         machinery_1997 = machinerypf_1997*farms_1997,
         machinery_2002 = machinerypf_2002*farms_2002,
         machinery_2007 = machinerypf_2007*farms_2007,
         machinery_2012 = machinerypf_2012*farms_2012,
         machineryper_1978 = if_else(is.na(prodval_1978), NA, machinery_1978/prodval_1978),
         machineryper_1982 = if_else(is.na(prodval_1982), NA, machinery_1982/prodval_1982),
         machineryper_1987 = if_else(is.na(prodval_1987), NA, machinery_1987/prodval_1987),
         machineryper_1992 = if_else(is.na(prodval_1992), NA, machinery_1992/prodval_1992),
         machineryper_1997 = if_else(is.na(prodval_1997), NA, machinery_1997/prodval_1997),
         machineryper_2002 = if_else(is.na(prodval_2002), NA, machinery_2002/prodval_2002),
         machineryper_2007 = if_else(is.na(prodval_2007), NA, machinery_2007/prodval_2007),
         machineryper_2012 = if_else(is.na(prodval_2012), NA, machinery_2012/prodval_2012),
         fertilizerper_1978 = if_else(is.na(prodval_1978), NA, fertilizer_1978/prodval_1978),
         fertilizerper_1982 = if_else(is.na(prodval_1982), NA, fertilizer_1982/prodval_1982),
         fertilizerper_1987 = if_else(is.na(prodval_1987), NA, fertilizer_1987/prodval_1987),
         fertilizerper_1992 = if_else(is.na(prodval_1992), NA, fertilizer_1992/prodval_1992),
         fertilizerper_1997 = if_else(is.na(prodval_1997), NA, fertilizer_1997/prodval_1997),
         fertilizerper_2002 = if_else(is.na(prodval_2002), NA, fertilizer_2002/prodval_2002),
         fertilizerper_2007 = if_else(is.na(prodval_2007), NA, fertilizer_2007/prodval_2007),
         fertilizerper_2012 = if_else(is.na(prodval_2012), NA, fertilizer_2012/prodval_2012),
         petroleumper_1978 = if_else(is.na(prodval_1978), NA, petroleum_1978/prodval_1978),
         petroleumper_1982 = if_else(is.na(prodval_1982), NA, petroleum_1982/prodval_1982),
         petroleumper_1987 = if_else(is.na(prodval_1987), NA, petroleum_1987/prodval_1987),
         petroleumper_1992 = if_else(is.na(prodval_1992), NA, petroleum_1992/prodval_1992),
         petroleumper_1997 = if_else(is.na(prodval_1997), NA, petroleum_1997/prodval_1997),
         petroleumper_2002 = if_else(is.na(prodval_2002), NA, petroleum_2002/prodval_2002),
         petroleumper_2007 = if_else(is.na(prodval_2007), NA, petroleum_2007/prodval_2007),
         petroleumper_2012 = if_else(is.na(prodval_2012), NA, petroleum_2012/prodval_2012))

## Getting a function that converts these variables to growth
compute_growth <- function(var) {
  var_prefix   <- paste0(var, "_")
  var_to       <- paste0(var, "_to")
  var_from     <- paste0(var, "_from")
  growth_var   <- paste0(var, "growth")
  growth_pref  <- paste0(growth_var, "_")
  
  county_long <- countyprodall |>
    pivot_longer(cols = starts_with(var_prefix),
                 names_to = "year",
                 names_prefix = var_prefix,
                 values_to = var) |>
    mutate(year = as.integer(year))
  
  result <- county_long |>
    inner_join(county_long, by = c("counfip", "statefip", "countycode"),
               suffix = c("_to", "_from"),
               relationship = "many-to-many") |>
    filter(year_to > year_from) |>
    mutate(!!growth_var := 100 * (.data[[var_to]] - .data[[var_from]]) / .data[[var_from]],
           growth_name = paste0(growth_pref, year_to, "_", year_from)) |>
    select(countycode, growth_name, all_of(growth_var)) |>
    pivot_wider(id_cols = countycode,
                names_from = growth_name, values_from = all_of(growth_var))
  
  assign(paste0("county", var, "all"), result, envir = .GlobalEnv)
}

compute_growth("cropland")
compute_growth("farms")
compute_growth("farmsize")
compute_growth("farmland")
compute_growth("farmvalue")
compute_growth("farmacrevalue")
compute_growth("irrland")
compute_growth("nonirrland")
compute_growth("prodval")
compute_growth("cropval")
compute_growth("prodvalacre")
compute_growth("prodvalworker")
compute_growth("laborintensity")
compute_growth("agemp")
compute_growth("cropvalratio")
compute_growth("croplandratio")
compute_growth("machinery")
compute_growth("machineryper")
compute_growth("fertilizer")
compute_growth("fertilizerper")
compute_growth("petroleum")
compute_growth("petroleumper")


###################################
### Getting the other data from the same source
###################################

stats_1980_1990 <- read_delim("Data/ICPSR_35206/DS0046/35206-0046-Data.tsv", 
                                            delim = "\t", escape_double = FALSE, 
                                            trim_ws = TRUE) |>
  select(statefip, fips, level, "pop1980" = age010180d, "pop1990" = age010190d,
         "estabs1980" = bza010180d, "estabs1990" = bza010190d,
         "emp1980bza" = bza110180d, "emp1990bza" = bza110190d,
         "payroll1980" = bza210180d, "payroll1990" = bza210190d,
         "households1980" = hsd010180d, "households1990" = hsd010190d,
         "housing1980" = hsg040180d, "housing1990" = hsg040190d,
         "inc1980" = inc910179d, "inc1990" = inc910189d) |>
  filter(level == 1) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  mutate(counfip = as.numeric(str_sub(fips, -3, -1)),
         countycode = paste(statefip, counfip),
         popgrowth = (pop1990-pop1980)/pop1980,
         estabsgrowth = (estabs1990-estabs1980)/estabs1980,
         empgrowthbza = (emp1990bza-emp1980bza)/emp1980bza,
         payrollgrowth = (payroll1990-payroll1980)/payroll1980,
         householdgrowth = (households1990-households1980)/households1980,
         housinggrowth = (housing1990-housing1980)/housing1980,
         incgrowth = (inc1990-inc1980)/inc1980) |>
  left_join(cbp_19801990, by = "countycode") |>
  filter(!is.na(empgrowth) & countycode %in% common_counties)

## Grouping crops
cropgroups <- data.frame(names_all) |>
  filter(!(names_all %in% c("counfip", "statefip", "level", "name"))) |>
  mutate(group = case_when(names_all %in% c("Corn (Grain)", "Sorghum (grain)",
                                            "Wheat", "Barley", "Buckwheat", "Emmer and spelt", "Flaxseed",
                                            "Oats", "Popcorn", "Proso millet", "Rice", "Rye",
                                            "Triticale", "Wild rice") ~ "Grains",
                           names_all %in% c("Soybeans", "Dry beans", "Dry lima beans",
                                            "Dry peas", "Dry cowpeas", "Lentils",
                                            "Peanuts", "Green lima beans", 
                                            "Snap beans", "Green cowpeas") ~ "Legumes",
                           names_all %in% c("Artichokes", "Asparagus", "Beets", "Broccoli",
                                            "Brussels sprouts", "Chinese cabbage", "Cabbages (Head)",
                                            "Mustard cabbage", "Carrots", "Cauliflower",
                                            "Celery", "Chicory", "Chinese peas", "Collards",
                                            "Cucumbers", "Daikon", "Eggplant", "Garlic", 
                                            "Kale", "Lettuce", "Mustard greens", "Onions",
                                            "Green onions", "Okra", "Parsley",
                                            "Green peas", "Bell peppers", "Pumpkins",
                                            "Radishes", "Spinach", "Squash", "Sweet corn",
                                            "Tomatoes", "Turnips", "Turnip greens",
                                            "Watercress", "Taro", "Other peppers") ~ "Vegetables",
                           names_all %in% c("Cantaloupes", "Honeydew melon", "Rhubarb", "Watermelons",
                                            "Apples", "Apricots", "Avocados", "Bananas", "Cherries",
                                            "Dates", "Figs", "Grapes", "Guavas", "Kiwis",
                                            "Mangoes", "Nectarines", "Papayas", "Passion fruit",
                                            "Peaches", "Pears", "Persimmons", "Plums",
                                            "Pomegranates", "Grapefruits", "Kumquats",
                                            "Lemons", "Limes", "Oranges", "Tangelos", "Tangerines",
                                            "Blackberries", "Blueberries", "Wild blueberries",
                                            "Boysenberries", "Cranberries", "Currants",
                                            "Loganberries", "Raspberries", "Strawberries",
                                            "Pineapples") ~ "Fruit",
                           .default = "Other"
  ))

###########################
### Getting indicators of production in specific animals
###########################

## 1978
animals_1978 <- read_delim("Data/ICPSR_35206/DS0039/35206-0039-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "Beef cows" = item11020, "Milk cows" = item11036,
         "Pigs" = item12002, "Sheep" = item13002, "Horses" = item13018,
         "Laying chickens" = item14005, "Meat chickens" = item14020,
         "Turkeys" = item14022, "Milk goats" = item17002, "Angora goats" = item18002,
         "Mink" = item19002, "Bee colonies" = item20002, "Catfish" = item21004,
         "Trout" = item21010, "Ducks" = item22002, "Geese" = item22006,
         "Pigeons" = item22010, "Pheasants" = item22014, "Quails" = item22018,
         "Mules, Donkeys, Burros" = item23002, "Other goats" = item23012,
         "Rabbits" = item23017) |>
  mutate(year = 1978) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")



## 1982
animals_1982 <- read_delim("Data/ICPSR_35206/DS0040/35206-0040-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "Beef cows" = item11020, "Milk cows" = item11036,
         "Pigs" = item12002, "Sheep" = item13002, "Horses" = item13018,
         "Laying chickens" = item14005, "Meat chickens" = item14020,
         "Turkeys" = item14022, "Milk goats" = item17002, "Angora goats" = item18002,
         "Mink" = item19002, "Bee colonies" = item20002, "Catfish" = item21004,
         "Trout" = item21010, "Ducks" = item22002, "Geese" = item22006,
         "Pigeons" = item22010, "Pheasants" = item22014, "Quails" = item22018,
         "Mules, Donkeys, Burros" = item23002, "Other goats" = item23012,
         "Rabbits" = item23017) |>
  mutate(year = 1982) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")

## 1987
animals_1987 <- read_delim("Data/ICPSR_35206/DS0041/35206-0041-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "Beef cows" = item11020, "Milk cows" = item11036,
         "Pigs" = item12002, "Sheep" = item13002, "Horses" = item13018,
         "Laying chickens" = item14005, "Meat chickens" = item14020,
         "Turkeys" = item14022, "Milk goats" = item17002, "Angora goats" = item18002,
         "Mink" = item19002, "Bee colonies" = item20002, "Catfish" = item21004,
         "Trout" = item21010, "Ducks" = item22002, "Geese" = item22006,
         "Pigeons" = item22010, "Pheasants" = item22014, "Quails" = item22018,
         "Mules, Donkeys, Burros" = item23002, "Other goats" = item23012,
         "Rabbits" = item23017) |>
  mutate(year = 1987) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")

## 1992
animals_1992 <- read_delim("Data/ICPSR_35206/DS0042/35206-0042-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "Beef cows" = item140020, "Milk cows" = item140036,
         "Pigs" = item150002, "Sheep" = item170002, "Horses" = item180002,
         "Laying chickens" = item160005, "Meat chickens" = item160020,
         "Turkeys" = item160022, "Milk goats" = item190002, "Angora goats" = item200002,
         "Mink" = item210002, "Bee colonies" = item220002, "Catfish" = item230004,
         "Trout" = item230010, "Ducks" = item240002, "Geese" = item240006,
         "Pigeons" = item240010, "Pheasants" = item240014, "Quails" = item240018,
         "Mules, Donkeys, Burros" = item250002, "Other goats" = item250012,
         "Rabbits" = item250017) |>
  mutate(year = 1992) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")

## 1997
animals_1997 <- read_delim("Data/ICPSR_35206/DS0043/35206-0043-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(LEVEL == 1) |>
  filter(STATEFIP != 02 & STATEFIP != 15 & STATEFIP != 11) |>
  select("counfip" = COUNFIP, "statefip" = STATEFIP, 
         "Beef cows" = ITEM14020, "Milk cows" = ITEM14036,
         "Pigs" = ITEM15002, "Sheep" = ITEM17002, "Horses" = ITEM18002,
         "Laying chickens" = ITEM16005, "Meat chickens" = ITEM16020,
         "Turkeys" = ITEM16022, "Milk goats" = ITEM19002, "Angora goats" = ITEM20002,
         "Mink" = ITEM21002, "Bee colonies" = ITEM22002, "Catfish" = ITEM23004,
         "Trout" = ITEM23010, "Ducks" = ITEM24002, "Geese" = ITEM24006,
         "Pigeons" = ITEM24010, "Pheasants" = ITEM24014, "Quails" = ITEM24018,
         "Mules, Donkeys, Burros" = ITEM25002, "Other goats" = ITEM25012,
         "Rabbits" = ITEM25017) |>
           mutate(year = 1997) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")

## 2002
animals_2002 <- read_delim("Data/ICPSR_35206/DS0044/35206-0044-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(level == 1) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  select(counfip, statefip, "Beef cows" = item11039, "Milk cows" = item11057,
         "Pigs" = item12003, "Sheep" = item16003, "Horses" = item15002,
         "Laying chickens" = item13005, "Meat chickens" = item13022,
         "Turkeys" = item13026, "Milk goats" = item17002, "Angora goats" = item18002,
         "Mink" = item20002, "Bee colonies" = item19002, "Catfish" = item21005,
         "Trout" = item21015, "Ducks" = item14002, "Geese" = item14018,
         "Pigeons" = item14042, "Pheasants" = item14034, "Quails" = item14050,
         "Mules, Donkeys, Burros" = item22050, "Other goats" = item22034,
         "Rabbits" = item22058) |>
  mutate(year = 2002) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")

## 2007
animals_2007 <- read_delim("Data/ICPSR_35206/DS0045/35206-0045-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(level == 1) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  select("counfip" = countyfip, statefip, "Beef cows" = data11_39, "Milk cows" = data11_57,
         "Pigs" = data12_3, "Sheep" = data16_3, "Horses" = data15_210,
         "Laying chickens" = data13_5, "Meat chickens" = data13_22,
         "Turkeys" = data13_26, "Milk goats" = data18_210, "Angora goats" = data19_210,
         "Mink" = data22_410, "Bee colonies" = data21_210, "Catfish" = data23_610,
         "Trout" = data23_620, "Ducks" = data14_210, "Geese" = data14_230,
         "Pigeons" = data14_260, "Pheasants" = data14_250, "Quails" = data14_270,
         "Mules, Donkeys, Burros" = data24_260, "Other goats" = data20_210,
         "Rabbits" = data24_270) |>
  mutate(year = 2007) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")

## 2012
animals_2012 <- read_delim("Data/ICPSR_35206/DS0047/35206-0047-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(LEVEL == 1) |>
  filter(STATEFIP != 02 & STATEFIP != 15 & STATEFIP != 11) |>
  select("counfip" = COFIPS, "statefip" = STATEFIP, 
         "Beef cows" = DATA11_39, "Milk cows" = DATA11_57,
         "Pigs" = DATA12_3, "Sheep" = DATA13_3, "Horses" = DATA18_2,
         "Laying chickens" = DATA19_5, "Meat chickens" = DATA19_22,
         "Turkeys" = DATA19_26, "Milk goats" = DATA15_2, "Angora goats" = DATA16_2,
         "Bee colonies" = DATA21_21, "Ducks" = DATA20_23, "Geese" = DATA20_27,
         "Pigeons" = DATA20_221, "Pheasants" = DATA20_219, "Quails" = DATA20_225,
         "Mules, Donkeys, Burros" = DATA18_22, "Other goats" = DATA17_2,
         "Rabbits" = DATA23_215) |>
  mutate(year = 2012) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(countycode = paste(statefip, counfip)) |>
  select(-c(statefip, counfip)) |>
  pivot_longer(-c(countycode, year), names_to = "animal", values_to = "number")

## Getting all the values
animals_all <- rbind(animals_1978, animals_1982, animals_1987, animals_1992, 
                     animals_1997, animals_2002, animals_2007, animals_2012) |>
  filter(countycode %in% common_counties & animal %in% 
           c("Beef cows", "Milk cows", "Pigs", "Sheep", "Horses",
             "Laying chickens", "Meat chickens", "Turkeys", "Milk goats",
             "Angora goats", "Bee colonies", "Ducks", "Geese", "Pigeons",
             "Pheasants", "Quails", "Mules, Donkeys, Burros", "Other goats",
             "Rabbits"))