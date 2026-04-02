## This file loads all the agricultural census data and performs the first stages of datacleaning

## Setting a function to label the csv files
agcensus_label <- function(x){
  y <- x |>
    select(statefip, counfip, level, name, "graincorn" = item24002, "sorghum" = item24007,
           "wheat" = item24012, "barley" = item24017, "buckwheat" = item24022,
           "emmerspelt" = item24027, "flaxseed" = item24032, "mustardseed" = item24042,
           "oats" = item24047, "popcorn" = item24052, "proso_millet" = item24057,
           "rice" = item24062, "rye" = item24067, "safflower" = item24072, 
           "sunflower" = item24077, "triticale" = item24082, "wildrice" = item24087,
           "cotton" = item25002, "tobacco" = item25007, "soybeans" = item25012,
           "drybeans" = item25017, "drylimas" = item25022, "drypeas" = item25027,
           "drycowpeas" = item25032, "lentils" = item25037, "potatoes" = item25042,
           "sweetpotatoes" = item25047, "sugarbeets1" = item25052, 
           "sugarbeets2" = item25057, "sugarcane1" = item25062, "sugarcane2" = item25067,
           "sugarcane3" = item25072, "sugarcane4" = item25077, "peanuts" = item25082,
           "alfalfaseed" = item26007, "alsikecloverseed" = item26012, 
           "winterpeasseed" = item26017, "bahiagrassseed" = item26022, 
           "bentgrassseed" = item26027, "bermudagrassseed" = item26032,
           "birdsfootseed" = item26037, "bromegrassseed" = item26042, 
           "crimsoncloverseed" = item26047, "fescueseed" = item26052, 
           "foxtailmilletseed" = item26057, "bluegrassseed" = item26062,
           "ladinocloverseed" = item26067, "lespedezaseed" = item26072, 
           "orchardgrassseed" = item26077, "redcloverseed" = item26082, 
           "redtopseed" = item26087, "ryegrassseed" = item26092,
           "sudangrassseed" = item26097, "sweetcloverseed" = item26102,
           "timothyseed" = item26107, "vetchseed" = item26112, "wheatgrassseed" = item26117,
           "whitecloverseed" = item26122, "alfalfahay" = item26137, "sorghumhay" = item26167,
           "sorghumsilage" = item26172, "artichokes" = item27006, "asparagus" = item27010,
           "greenlima" = item27014, "snapbeans" = item27018, "beets" = item27022,
           "broccoli" = item27026, "brusselssprouts" = item27030, 
           "chinesecabbage" = item27034, "headcabbage" = item27038, 
           "mustardcabbage" = item27042, "cantaloups" = item27046, "carrots" = item27050,
           "cauliflower" = item27054, "celery" = item27058, "chicory" = item27062,
           "chinesepeas" = item27066, "collards" = item27070, "greencowpeas" = item27074,
           "cucumbers" = item27078, "daikon" = item27082, "eggplant" = item27086,
           "endive" = item27090, "escarole" = item27094, "garlic" = item27098,
           "honeydew" = item27102, "kale" = item27106, "lettuce" = item27110,
           "mustardgreens" = item27114, "onions" = item27118, "greenonions" = item27122,
           "okra" = item27126, "parsley" = item27130, "greenpeas" = item27134,
           "hotpeppers" = item27138, "sweetpeppers" = item27142, "pimentos" = item27146,
           "pumpkins" = item27150, "radishes" = item27154, "rhubarb" = item27158,
           "shallots" = item27162, "spinach" = item27166, "squash" = item27170,
           "sweetcorn" = item27174, "tomatoes" = item27178, "turnips" = item27182,
           "turnipgreens" = item27186, "watercress" = item27194, "watermelons" = item27198,
           "apples" = item28011, "apricots" = item28020, "avocados" = item28029,
           "bananas" = item28038, "cherries" = item28047, "coffee" = item28083,
           "dates" = item28092, "figs" = item28101, "grapes" = item28110,
           "guavas" = item28119, "kiwifruit" = item28128, "mangoes" = item28137,
           "nectarines" = item28146, "olives" = item28155, "papayas" = item28164,
           "passionfruit" = item28173, "peaches" = item28182, "pears" = item28191,
           "persimmons" = item28200, "plums" = item28209, "pomegranates" = item28218,
           "grapefruit" = item28245, "kumquats" = item28254, "lemons" = item28263,
           "limes" = item28272, "oranges" = item28281, "tangelos" = item28290,
           "honeytangerines" = item28299, "tangerines" = item28308, "almonds" = item28326,
           "hazelnuts" = item28335, "macadamia" = item28344, "pecans" = item28353,
           "pistachios" = item28362, "walnuts" = item28371, "blackberries" = item29007,
           "blueberries" = item29012, "wildblueberries" = item29017, 
           "boysenberries" = item29022, "cranberries" = item29027, "currants" = item29032,
           "loganberries" = item29037, "raspberries" = item29042, 
           "strawberries" = item29047, "mushrooms" = item30035, "dilloil" = item31007,
           "ginger" = item31012, "guar" = item31017, "hops" = item31027,
           "jojoba" = item31032, "lotus" = item31042, "mintoil" = item31047,
           "mungbeans" = item31052, "pineapples" = item31057, "rapeseed" = item31067,
           "salthay" = item31072, "sweetsorghum" = item31077, "taro" = item31092) |>
    filter(level == 1) |>
    mutate(across(everything(), ~replace_na(., 0))) |>
    mutate(sorghum = sorghumhay + sorghumsilage,
           alfalfa = alfalfahay + alfalfaseed,
           sugarcane = sugarcane1 + sugarcane2 + sugarcane3 + sugarcane4,
           sugarbeets = sugarbeets1 + sugarbeets2) |>
    select(-c(sorghumhay, sorghumsilage, alfalfahay, alfalfaseed, sugarcane1,
              sugarcane2, sugarcane3, sugarcane4, sugarbeets1, sugarbeets2))
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
  select(statefip, counfip, level, name, "graincorn" = item260002, "sorghum" = item260007,
         "wheat" = item260012, "barley" = item260042, "buckwheat" = item260047,
         "emmerspelt" = item260067, "flaxseed" = item260072, "mustardseed" = item260077,
         "oats" = item260082, "popcorn" = item260087, "proso_millet" = item260092,
         "rice" = item260097, "rye" = item260102, "safflower" = item260107, 
         "sunflower" = item260112, "triticale" = item260117, "wildrice" = item260122,
         "cotton" = item270002, "tobacco" = item270007, "soybeans" = item270012,
         "drybeans" = item270017, "drylimas" = item270022, "drypeas" = item270027,
         "drycowpeas" = item270032, "lentils" = item270037, "potatoes" = item270042,
         "sweetpotatoes" = item270047, "sugarbeets1" = item270052, 
         "sugarbeets2" = item270057, "sugarcane1" = item270062, "sugarcane2" = item270067,
         "sugarcane3" = item270072, "peanuts" = item270077,
         "alfalfaseed" = item280007,
         "winterpeasseed" = item280012, "bahiagrassseed" = item280017, 
         "bentgrassseed" = item280022, "bermudagrassseed" = item280027,
         "birdsfootseed" = item280032, "bromegrassseed" = item280037, 
         "crimsoncloverseed" = item280042, "fescueseed" = item280047, 
         "foxtailmilletseed" = item280052, "bluegrassseed" = item280057,
         "ladinocloverseed" = item280062, "lespedezaseed" = item280067, 
         "orchardgrassseed" = item280072, "redcloverseed" = item280077, 
         "redtopseed" = item280082, "ryegrassseed" = item280087,
         "sudangrassseed" = item280092, "sweetcloverseed" = item280097,
         "timothyseed" = item280102, "vetchseed" = item280107, "wheatgrassseed" = item280112,
         "whitecloverseed" = item280117, "alfalfahay" = item280132, "sorghumhay" = item280162,
         "sorghumsilage" = item280167, "artichokes" = item290010, "asparagus" = item290014,
         "greenlima" = item290018, "snapbeans" = item290022, "beets" = item290026,
         "broccoli" = item290030, "brusselssprouts" = item290034, 
         "chinesecabbage" = item290038, "headcabbage" = item290042, 
         "mustardcabbage" = item290046, "cantaloups" = item290050, "carrots" = item290054,
         "cauliflower" = item290058, "celery" = item290062, "chicory" = item290066,
         "chinesepeas" = item290070, "collards" = item290074, "greencowpeas" = item290078,
         "cucumbers" = item290082, "daikon" = item290086, "eggplant" = item290090,
         "endive" = item290094, "escarole" = item290098, "garlic" = item290102,
         "honeydew" = item290110, "kale" = item290114, "lettuce" = item290118,
         "mustardgreens" = item290122, "onions" = item290126, "greenonions" = item290130,
         "okra" = item290134, "parsley" = item290138, "greenpeas" = item290142,
         "hotpeppers" = item290146, "sweetpeppers" = item290150, "pimentos" = item290154,
         "pumpkins" = item290158, "radishes" = item290162, "rhubarb" = item290166,
         "spinach" = item290170, "squash" = item290174,
         "sweetcorn" = item290178, "tomatoes" = item290182, "turnips" = item290186,
         "turnipgreens" = item290190, "watercress" = item290198, "watermelons" = item290202,
         "apples" = item310002, "apricots" = item310011, "avocados" = item310020,
         "bananas" = item310029, "cherries" = item310038, "coffee" = item310074,
         "dates" = item310083, "figs" = item310092, "grapes" = item310101,
         "guavas" = item310110, "kiwifruit" = item310119, "mangoes" = item310128,
         "nectarines" = item310137, "olives" = item310146, "papayas" = item310155,
         "passionfruit" = item310164, "peaches" = item310173, "pears" = item310182,
         "persimmons" = item310191, "plums" = item310200, "pomegranates" = item310209,
         "grapefruit" = item310236, "kumquats" = item310245, "lemons" = item310254,
         "limes" = item310263, "oranges" = item310272, "tangelos" = item310281,
         "honeytangerines" = item310290, "tangerines" = item310299, "almonds" = item310317,
         "hazelnuts" = item310326, "macadamia" = item310335, "pecans" = item310344,
         "pistachios" = item310353, "walnuts" = item310362, "blackberries" = item320007,
         "blueberries" = item320012, "wildblueberries" = item320017, 
         "boysenberries" = item320022, "cranberries" = item320027, "currants" = item320032,
         "loganberries" = item320037, "raspberries" = item320042, 
         "strawberries" = item320047, "mushrooms" = item330035, "dilloil" = item340006,
         "ginger" = item340012, "guar" = item340022, "hops" = item340037,
         "jojoba" = item340042, "lotus" = item340052, "mintoil" = item340057,
         "mungbeans" = item340062, "pineapples" = item340067, "rapeseed" = item260052,
         "salthay" = item340077, "sweetsorghum" = item340082, "taro" = item340097) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(sorghum = sorghumhay + sorghumsilage,
         alfalfa = alfalfahay + alfalfaseed,
         sugarcane = sugarcane1 + sugarcane2 + sugarcane3,
         sugarbeets = sugarbeets1 + sugarbeets2) |>
  select(-c(sorghumhay, sorghumsilage, alfalfahay, alfalfaseed, sugarcane1,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2))

## 1997
acres_1997 <- read_delim("Data/ICPSR_35206/DS0043/35206-0043-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  select("statefip" = STATEFIP, "counfip" = COUNFIP, "level" = LEVEL, "name" = NAME,
         "graincorn" = ITEM26002, "sorghum" = ITEM26007,
         "wheat" = ITEM26012, "barley" = ITEM26042, "buckwheat" = ITEM26047,
         "emmerspelt" = ITEM26067, "flaxseed" = ITEM26072, "mustardseed" = ITEM26077,
         "oats" = ITEM26082, "popcorn" = ITEM26087, "proso_millet" = ITEM26092,
         "rice" = ITEM26097, "rye" = ITEM26102, "safflower" = ITEM26107, 
         "sunflower" = ITEM26112, "triticale" = ITEM26017, "wildrice" = ITEM26122,
         "cotton" = ITEM27002, "tobacco" = ITEM27007, "soybeans" = ITEM27012,
         "drybeans" = ITEM27017, "drylimas" = ITEM27022, "drypeas" = ITEM27027,
         "drycowpeas" = ITEM27032, "lentils" = ITEM27037, "potatoes" = ITEM27042,
         "sweetpotatoes" = ITEM27047, "sugarbeets1" = ITEM27052, 
         "sugarbeets2" = ITEM27057, "sugarcane1" = ITEM27062, "sugarcane2" = ITEM27067,
         "sugarcane3" = ITEM27072, "peanuts" = ITEM27077,
         "alfalfaseed" = ITEM28007,
         "winterpeasseed" = ITEM28012, "bahiagrassseed" = ITEM28017, 
         "bentgrassseed" = ITEM28022, "bermudagrassseed" = ITEM28027,
         "birdsfootseed" = ITEM28032, "bromegrassseed" = ITEM28037, 
         "crimsoncloverseed" = ITEM28042, "fescueseed" = ITEM28047, 
         "foxtailmilletseed" = ITEM28052, "bluegrassseed" = ITEM28057,
         "ladinocloverseed" = ITEM28062, "lespedezaseed" = ITEM28067, 
         "orchardgrassseed" = ITEM28072, "redcloverseed" = ITEM28077, 
         "redtopseed" = ITEM28082, "ryegrassseed" = ITEM28087,
         "sudangrassseed" = ITEM28092, "sweetcloverseed" = ITEM28097,
         "timothyseed" = ITEM28102, "vetchseed" = ITEM28107, "wheatgrassseed" = ITEM28112,
         "whitecloverseed" = ITEM28117, "alfalfahay" = ITEM28132, "sorghumhay" = ITEM28162,
         "sorghumsilage" = ITEM28167, "artichokes" = ITEM29010, "asparagus" = ITEM29014,
         "greenlima" = ITEM29018, "snapbeans" = ITEM29022, "beets" = ITEM29026,
         "broccoli" = ITEM29030, "brusselssprouts" = ITEM29034, 
         "chinesecabbage" = ITEM29038, "headcabbage" = ITEM29042, 
         "mustardcabbage" = ITEM29046, "cantaloups" = ITEM29050, "carrots" = ITEM29054,
         "cauliflower" = ITEM29058, "celery" = ITEM29062, "chicory" = ITEM29066,
         "chinesepeas" = ITEM29070, "collards" = ITEM29074, "greencowpeas" = ITEM29078,
         "cucumbers" = ITEM29082, "daikon" = ITEM29086, "eggplant" = ITEM29090,
         "endive" = ITEM29094, "escarole" = ITEM29098, "garlic" = ITEM29102,
         "honeydew" = ITEM29110, "kale" = ITEM29114, "lettuce" = ITEM29118,
         "mustardgreens" = ITEM29122, "onions" = ITEM29126, "greenonions" = ITEM29130,
         "okra" = ITEM29134, "parsley" = ITEM29138, "greenpeas" = ITEM29142,
         "hotpeppers" = ITEM29146, "sweetpeppers" = ITEM29150, "pimentos" = ITEM29154,
         "pumpkins" = ITEM29158, "radishes" = ITEM29162, "rhubarb" = ITEM29166,
         "spinach" = ITEM29170, "squash" = ITEM29174,
         "sweetcorn" = ITEM29178, "tomatoes" = ITEM29182, "turnips" = ITEM29186,
         "turnipgreens" = ITEM29190, "watercress" = ITEM29198, "watermelons" = ITEM29202,
         "apples" = ITEM31002, "apricots" = ITEM31011, "avocados" = ITEM31020,
         "bananas" = ITEM31029, "cherries" = ITEM31038, "coffee" = ITEM31074,
         "dates" = ITEM31083, "figs" = ITEM31092, "grapes" = ITEM31101,
         "guavas" = ITEM31110, "kiwifruit" = ITEM31119, "mangoes" = ITEM31128,
         "nectarines" = ITEM31137, "olives" = ITEM31146, "papayas" = ITEM31155,
         "passionfruit" = ITEM31164, "peaches" = ITEM31173, "pears" = ITEM31182,
         "persimmons" = ITEM31191, "plums" = ITEM31200, "pomegranates" = ITEM31209,
         "grapefruit" = ITEM31236, "kumquats" = ITEM31245, "lemons" = ITEM31254,
         "limes" = ITEM31263, "oranges" = ITEM31272, "tangelos" = ITEM31281,
         "honeytangerines" = ITEM31290, "tangerines" = ITEM31299, "almonds" = ITEM31317,
         "hazelnuts" = ITEM31326, "macadamia" = ITEM31335, "pecans" = ITEM31344,
         "pistachios" = ITEM31353, "walnuts" = ITEM31362, "blackberries" = ITEM32007,
         "blueberries" = ITEM32012, "wildblueberries" = ITEM32017, 
         "boysenberries" = ITEM32022, "cranberries" = ITEM32027, "currants" = ITEM32032,
         "loganberries" = ITEM32037, "raspberries" = ITEM32042, 
         "strawberries" = ITEM32047, "mushrooms" = ITEM33035, "dilloil" = ITEM34006,
         "ginger" = ITEM34012, "guar" = ITEM34022, "hops" = ITEM34037,
         "jojoba" = ITEM34042, "lotus" = ITEM34052, "mintoil" = ITEM34062,
         "mungbeans" = ITEM34067, "pineapples" = ITEM34072, "rapeseed" = ITEM26052,
         "salthay" = ITEM34082, "sweetsorghum" = ITEM34087, "taro" = ITEM34102) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(sorghum = sorghumhay + sorghumsilage,
         alfalfa = alfalfahay + alfalfaseed,
         sugarcane = sugarcane1 + sugarcane2 + sugarcane3,
         sugarbeets = sugarbeets1 + sugarbeets2) |>
  select(-c(sorghumhay, sorghumsilage, alfalfahay, alfalfaseed, sugarcane1,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2))

## 2002
acres_2002 <- read_delim("Data/ICPSR_35206/DS0044/35206-0044-Data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) |>
  select(statefip, counfip, level, name,
         "graincorn" = item24002, "sorghum" = item24013,
         "wheat" = item24022, "barley" = item24062, "buckwheat" = item24072,
         "emmerspelt" = item24102, "flaxseed" = item24112, "mustardseed" = item24122,
         "oats" = item24132, "popcorn" = item24142, "proso_millet" = item24152,
         "rice" = item24162, "rye" = item24172, "safflower" = item24182, 
         "sunflower" = item24192, "triticale" = item24222, "wildrice" = item24232,
         "cotton" = item25002, "tobacco" = item25032, "soybeans" = item25042,
         "drybeans" = item25052, "drylimas" = item25062, "drypeas" = item25072,
         "drycowpeas" = item25082, "lentils" = item25092, "potatoes" = item25102,
         "sweetpotatoes" = item25112, "sugarbeets1" = item25122, 
         "sugarbeets2" = item25132, "sugarcane1" = item25142, "sugarcane2" = item25152,
         "sugarcane3" = item25162, "peanuts" = item25172,
         "alfalfaseed" = item26012,
         "winterpeasseed" = item26022, "bahiagrassseed" = item26032, 
         "bentgrassseed" = item26042, "bermudagrassseed" = item26052,
         "birdsfootseed" = item26062, "bromegrassseed" = item26072, 
         "crimsoncloverseed" = item26082, "fescueseed" = item26092, 
         "foxtailmilletseed" = item26102, "bluegrassseed" = item26112,
         "ladinocloverseed" = item26122, "lespedezaseed" = item26132, 
         "orchardgrassseed" = item26142, "redcloverseed" = item26152, 
         "redtopseed" = item26162, "ryegrassseed" = item26172,
         "sudangrassseed" = item26182, "sweetcloverseed" = item26192,
         "timothyseed" = item26202, "vetchseed" = item26212, "wheatgrassseed" = item26222,
         "whitecloverseed" = item26232, "alfalfahay" = item26262,
         "artichokes" = item29008, "asparagus" = item29014,
         "greenlima" = item29020, "snapbeans" = item29026, "beets" = item29032,
         "broccoli" = item29038, "brusselssprouts" = item29044, 
         "chinesecabbage" = item29050, "headcabbage" = item29056, 
         "mustardcabbage" = item29062, "cantaloups" = item29068, "carrots" = item29074,
         "cauliflower" = item29080, "celery" = item29086, "chicory" = item29092,
         "chinesepeas" = item29206, "collards" = item29098, "greencowpeas" = item29218,
         "cucumbers" = item29104, "daikon" = item29110, "eggplant" = item29116,
         "garlic" = item29128,
         "honeydew" = item29140, "kale" = item29146, "lettuce" = item29152,
         "mustardgreens" = item29176, "onions" = item29188, "greenonions" = item29194,
         "okra" = item29182, "parsley" = item29200, "greenpeas" = item29212,
         "hotpeppers" = item29230, "sweetpeppers" = item29224, "pimentos" = item29236,
         "pumpkins" = item29242, "radishes" = item29248, "rhubarb" = item29254,
         "spinach" = item29260, "squash" = item29266,
         "sweetcorn" = item29272, "tomatoes" = item29278, "turnips" = item29284,
         "turnipgreens" = item29290, "watercress" = item29308, "watermelons" = item29314,
         "apples" = item31002, "apricots" = item31014, "avocados" = item31026,
         "bananas" = item31038, "cherries" = item31050, "coffee" = item31074,
         "dates" = item31086, "figs" = item31098, "grapes" = item31110,
         "guavas" = item31122, "kiwifruit" = item31134, "mangoes" = item31146,
         "nectarines" = item31158, "olives" = item31170, "papayas" = item31182,
         "passionfruit" = item31194, "peaches" = item31206, "pears" = item31242,
         "persimmons" = item31278, "plums" = item31290, "pomegranates" = item31326,
         "grapefruit" = item31362, "kumquats" = item31386, "lemons" = item31398,
         "limes" = item31410, "oranges" = item31422, "tangelos" = item31458,
         "tangerines" = item31470, "almonds" = item31506,
         "hazelnuts" = item31518, "macadamia" = item31530, "pecans" = item31542,
         "pistachios" = item31554, "walnuts" = item31566, "blackberries" = item33002,
         "blueberries" = item33006, "wildblueberries" = item33010, 
         "boysenberries" = item33014, "cranberries" = item33018, "currants" = item33022,
         "loganberries" = item33026, "raspberries" = item33030, 
         "strawberries" = item33042, "dilloil" = item27022,
         "ginger" = item27032, "guar" = item27052, "hops" = item27072,
         "jojoba" = item27082, "lotus" = item27102, "mintoil" = item27112,
         "mungbeans" = item27142, "pineapples" = item27152, 
         "salthay" = item27182, "sweetsorghum" = item27202, "taro" = item27222) |>
  filter(level == 1) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(alfalfa = alfalfahay + alfalfaseed,
         sugarcane = sugarcane1 + sugarcane2 + sugarcane3,
         sugarbeets = sugarbeets1 + sugarbeets2) |>
  select(-c(alfalfahay, alfalfaseed, sugarcane1,
            sugarcane2, sugarcane3, sugarbeets1, sugarbeets2))

###########################
### Modifying them for the format I'll generally use
###########################

acres_1978_mod <- acres_1978 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  select(-c(level, name)) |>
  mutate(countycode = paste(statefip, counfip))

acres_1982_mod <- acres_1982 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  select(-c(level, name)) |>
  mutate(countycode = paste(statefip, counfip))

acres_1987_mod <- acres_1987 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  select(-c(level, name)) |>
  mutate(countycode = paste(statefip, counfip))

acres_1992_mod <- acres_1992 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  select(-c(level, name)) |>
  mutate(countycode = paste(statefip, counfip))

acres_1997_mod <- acres_1997 |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  select(-c(level, name)) |>
  mutate(countycode = paste(statefip, counfip))

#########################
### Converting them to a long format
#########################

## Getting a list of all counties in common
common_counties <- intersect(cbp_bycounty$countycode, acres_1978_mod$countycode)
anticommon_counties <- setdiff(subset(acres_1978_mod, counfip != 998)$countycode, cbp_bycounty$countycode)
anticommon_counties2 <- setdiff(subset(cbp_bycounty, fipscty != 999)$countycode, acres_1978_mod$countycode)


cropprod_1978 <- acres_1978_mod |>
  filter(countycode %in% common_counties) |>
  pivot_longer(-c(statefip, counfip, countycode), names_to = "crop", values_to = "value") |>
  group_by(crop) |>
  summarize(totalprod_1978 = sum(value))

cropprod_1982 <- acres_1982_mod |>
  filter(countycode %in% common_counties) |>
  pivot_longer(-c(statefip, counfip, countycode), names_to = "crop", values_to = "value") |>
  group_by(crop) |>
  summarize(totalprod_1982 = sum(value))

cropprod_1987 <- acres_1987_mod |>
  filter(countycode %in% common_counties) |>
  pivot_longer(-c(statefip, counfip, countycode), names_to = "crop", values_to = "value") |>
  group_by(crop) |>
  summarize(totalprod_1987 = sum(value))

cropprod_1992 <- acres_1992_mod |>
  filter(countycode %in% common_counties) |>
  pivot_longer(-c(statefip, counfip, countycode), names_to = "crop", values_to = "value") |>
  group_by(crop) |>
  summarize(totalprod_1992 = sum(value))

cropprod_1997 <- acres_1997_mod |>
  filter(countycode %in% common_counties) |>
  pivot_longer(-c(statefip, counfip, countycode), names_to = "crop", values_to = "value") |>
  group_by(crop) |>
  summarize(totalprod_1997 = sum(value))

## And getting the change in crop production over time
cropprod_1978_1982 <- left_join(cropprod_1978, cropprod_1982, by = "crop") |>
  mutate(cropgrowth = ((totalprod_1982-totalprod_1978)/totalprod_1978)*100)

cropprod_1978_1987 <- left_join(cropprod_1978, cropprod_1987, by = "crop") |>
  mutate(cropgrowth = ((totalprod_1987-totalprod_1978)/totalprod_1978)*100)

cropprod_1978_1992 <- left_join(cropprod_1978, cropprod_1992, by = "crop") |>
  mutate(cropgrowth = ((totalprod_1992-totalprod_1978)/totalprod_1978)*100)

cropprod_1978_1997 <- left_join(cropprod_1978, cropprod_1997, by = "crop") |>
  mutate(cropgrowth = ((totalprod_1997-totalprod_1978)/totalprod_1978)*100)

#################################
### Getting county-level total production
#################################

countyprod_1978 <- read_delim("Data/ICPSR_35206/DS0039/35206-0039-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1978" = item01002,
         "farms_1978" = item01001, "farmsize_1978" = item01003,
         "farmvalue_1978" = item01004, "farmacrevalue_1978" = item01005,
         "cropland_1978" = item01014, "irrland_1978" = item01018,
         "prodval_1978" = item01019, "nonmainops_1978" = item01031)

countyprod_1982 <- read_delim("Data/ICPSR_35206/DS0040/35206-0040-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1982" = item01002,
         "farms_1982" = item01001, "farmsize_1982" = item01003,
         "farmvalue_1982" = item01004, "farmacrevalue_1982" = item01005,
         "cropland_1982" = item01014, "irrland_1982" = item01018,
         "prodval_1982" = item01019, "nonmainops_1982" = item01031)

countyprod_1987 <- read_delim("Data/ICPSR_35206/DS0041/35206-0041-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1987" = item01002,
         "farms_1987" = item01001, "farmsize_1987" = item01003,
         "farmvalue_1987" = item01004, "farmacrevalue_1987" = item01005,
         "cropland_1987" = item01014, "irrland_1987" = item01018,
         "prodval_1987" = item01019, "nonmainops_1987" = item01031)

countyprod_1992 <- read_delim("Data/ICPSR_35206/DS0042/35206-0042-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_1992" = item010002,
         "farms_1992" = item010001, "farmsize_1992" = item010003,
         "farmvalue_1992" = item010004, "farmacrevalue_1992" = item010005,
         "cropland_1992" = item010014, "irrland_1992" = item010018,
         "prodval_1992" = item010019, "nonmainops_1992" = item010036)

countyprod_1997 <- read_delim("Data/ICPSR_35206/DS0043/35206-0043-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(LEVEL == 1) |>
  select("counfip" = COUNFIP, "statefip" = STATEFIP, "farmland_1997" = ITEM01002,
         "farms_1997" = ITEM01001, "farmsize_1997" = ITEM01003, 
         "farmvalue_1997" = ITEM01005, "farmacrevalue_1997" = ITEM01006,
         "cropland_1997" = ITEM01015, "irrland_1997" = ITEM01019,
         "prodval_1997" = ITEM01020, "nonmainops_1997" = ITEM01037) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)

countyprod_2002 <- read_delim("Data/ICPSR_35206/DS0044/35206-0044-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(level == 1) |>
  select(counfip, statefip, "farmland_2002" = item01002,
         "farms_2002" = item01001, "farmsize_2002" = item01003, 
         "farmvalue_2002" = item01005, "farmacrevalue_2002" = item01006,
         "cropland_2002" = item01015, "irrland_2002" = item01019,
         "prodval_2002" = item01020) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)

countyprod_2007 <- read_delim("Data/ICPSR_35206/DS0045/35206-0045-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(level == 1) |>
  select("counfip" = countyfip, statefip, "farmland_2007" = data1_2,
         "farms_2007" = data1_1, "farmsize_2007" = data1_3, 
         "farmvalue_2007" = data1_5, "farmacrevalue_2007" = data1_6,
         "cropland_2007" = data1_15, "irrland_2007" = data1_19,
         "prodval_2007" = data1_20) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)

countyprod_2012 <- read_delim("Data/ICPSR_35206/DS0047/35206-0047-Data.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) |>
  filter(LEVEL == 1) |>
  select("counfip" = COFIPS, "statefip" = STATEFIP, "farmland_2012" = DATA1_2,
         "farms_2012" = DATA1_1, "farmsize_2012" = DATA1_3, 
         "farmvalue_2012" = DATA1_5, "farmacrevalue_2012" = DATA1_6,
         "cropland_2012" = DATA1_16, "irrland_2012" = DATA1_20,
         "prodval_2012" = DATA1_21) |>
  filter(statefip != 02 & statefip != 15 & statefip != 11)

countyprodall <- left_join(countyprod_1978, countyprod_1982, by = c("counfip", "statefip")) |>
  left_join(countyprod_1987, by = c("counfip", "statefip")) |>
  left_join(countyprod_1992, by = c("counfip", "statefip")) |>
  left_join(countyprod_1997, by = c("counfip", "statefip")) |>
  left_join(countyprod_2002, by = c("counfip", "statefip")) |>
  left_join(countyprod_2007, by = c("counfip", "statefip")) |>
  left_join(countyprod_2012, by = c("counfip", "statefip")) |>
  mutate(countycode = paste(statefip, counfip)) |>
  filter(countycode %in% common_counties) |>
  filter(!is.na(cropland_1978)) |>
  mutate(nonirrland_1978 = if_else(is.na(irrland_1978), cropland_1978, cropland_1978 - irrland_1978),
         nonirrland_1982 = if_else(is.na(irrland_1982), cropland_1982, cropland_1982 - irrland_1982),
         nonirrland_1987 = if_else(is.na(irrland_1987), cropland_1987, cropland_1987 - irrland_1987),
         nonirrland_1992 = if_else(is.na(irrland_1992), cropland_1992, cropland_1992 - irrland_1992),
         nonirrland_1997 = if_else(is.na(irrland_1997), cropland_1997, cropland_1997 - irrland_1997),
         nonirrland_2002 = if_else(is.na(irrland_2002), cropland_2002, cropland_2002 - irrland_2002),
         nonirrland_2007 = if_else(is.na(irrland_2007), cropland_2007, cropland_2007 - irrland_2007),
         nonirrland_2012 = if_else(is.na(irrland_2012), cropland_2012, cropland_2012 - irrland_2012))

countyprod2 <- countyprodall |>
  pivot_longer(cols = starts_with("cropland_"),
               names_to = "year",
               names_prefix = "cropland_",
               values_to = "cropland") |>
  mutate(year = as.integer(year))

countyprodgrowth <- countyprod2 |>
  inner_join(countyprod2, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(croplandgrowth = 100 * (cropland_to - cropland_from) / cropland_from,
         growth_name = paste0("croplandgrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, croplandgrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = croplandgrowth)

countyprodfarms <- countyprodall |>
  pivot_longer(cols = starts_with("farms_"),
               names_to = "year",
               names_prefix = "farms_",
               values_to = "farms") |>
  mutate(year = as.integer(year))

countyprodfarmsall <- countyprodfarms |>
  inner_join(countyprodfarms, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(farmsgrowth = 100 * (farms_to - farms_from) / farms_from,
         growth_name = paste0("farmsgrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, farmsgrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = farmsgrowth)

countyprodfarmsize <- countyprodall |>
  pivot_longer(cols = starts_with("farmsize_"),
               names_to = "year",
               names_prefix = "farmsize_",
               values_to = "farmsize") |>
  mutate(year = as.integer(year))

countyprodfarmsizeall <- countyprodfarmsize |>
  inner_join(countyprodfarmsize, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(farmsizegrowth = 100 * (farmsize_to - farmsize_from) / farmsize_from,
         growth_name = paste0("farmsizegrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, farmsizegrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = farmsizegrowth)

countyprodfarmland <- countyprodall |>
  pivot_longer(cols = starts_with("farmland_"),
               names_to = "year",
               names_prefix = "farmland_",
               values_to = "farmland") |>
  mutate(year = as.integer(year))

countyprodfarmlandall <- countyprodfarmland |>
  inner_join(countyprodfarmland, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(farmlandgrowth = 100 * (farmland_to - farmland_from) / farmland_from,
         growth_name = paste0("farmlandgrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, farmlandgrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = farmlandgrowth)

countyprodfarmvalue <- countyprodall |>
  pivot_longer(cols = starts_with("farmvalue_"),
               names_to = "year",
               names_prefix = "farmvalue_",
               values_to = "farmvalue") |>
  mutate(year = as.integer(year))

countyprodfarmvalueall <- countyprodfarmvalue |>
  inner_join(countyprodfarmvalue, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(farmvaluegrowth = 100 * (farmvalue_to - farmvalue_from) / farmvalue_from,
         growth_name = paste0("farmvaluegrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, farmvaluegrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = farmvaluegrowth)

countyprodfarmacrevalue <- countyprodall |>
  pivot_longer(cols = starts_with("farmacrevalue_"),
               names_to = "year",
               names_prefix = "farmacrevalue_",
               values_to = "farmacrevalue") |>
  mutate(year = as.integer(year))

countyprodfarmacrevalueall <- countyprodfarmacrevalue |>
  inner_join(countyprodfarmacrevalue, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(farmacrevaluegrowth = 100 * (farmacrevalue_to - farmacrevalue_from) / farmacrevalue_from,
         growth_name = paste0("farmacrevaluegrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, farmacrevaluegrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = farmacrevaluegrowth)

countyprodirrland <- countyprodall |>
  pivot_longer(cols = starts_with("irrland_"),
               names_to = "year",
               names_prefix = "irrland_",
               values_to = "irrland") |>
  mutate(year = as.integer(year))

countyprodirrlandall <- countyprodirrland |>
  inner_join(countyprodirrland, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(irrlandgrowth = 100 * (irrland_to - irrland_from) / irrland_from,
         growth_name = paste0("irrlandgrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, irrlandgrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = irrlandgrowth)

countyprodnonirrland <- countyprodall |>
  pivot_longer(cols = starts_with("nonirrland_"),
               names_to = "year",
               names_prefix = "nonirrland_",
               values_to = "nonirrland") |>
  mutate(year = as.integer(year))

countyprodnonirrlandall <- countyprodnonirrland |>
  inner_join(countyprodnonirrland, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(nonirrlandgrowth = 100 * (nonirrland_to - nonirrland_from) / nonirrland_from,
         growth_name = paste0("nonirrlandgrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, nonirrlandgrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = nonirrlandgrowth)


countyprodval <- countyprodall |>
  pivot_longer(cols = starts_with("prodval_"),
               names_to = "year",
               names_prefix = "prodval_",
               values_to = "prodval") |>
  mutate(year = as.integer(year))

countyprodvalall <- countyprodval |>
  inner_join(countyprodval, by = c("counfip", "statefip", "countycode"),
             suffix = c("_to", "_from"),
             relationship = "many-to-many") |>
  filter(year_to > year_from) |>
  mutate(prodvalgrowth = 100 * (prodval_to - prodval_from) / prodval_from,
         growth_name = paste0("prodvalgrowth_", year_to, "_", year_from)) |>
  select(countycode, growth_name, prodvalgrowth) |>
  pivot_wider(id_cols = countycode, 
              names_from = growth_name, values_from = prodvalgrowth)
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
