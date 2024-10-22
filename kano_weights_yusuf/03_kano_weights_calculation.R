
rm(list=ls())

source("loadpath.R")

###Household Listed into 
kano_hh_listed_00<- readxl::read_excel(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_with_new_ward_2023-12-01.xlsx"))
kano_hh_listed_01 <- readxl::read_excel(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_HH_Listing_2023-12-01 (1).xlsx"))


kano_hh_listed <- rbind(kano_hh_listed_00, kano_hh_listed_01) 

kano_hh_listed <- kano_hh_listed %>%
  mutate(ea_serial_number_new = gsub("-","",ea_serial_number) 
  )

kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2102] <- "GOBIRAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2180] <- "GOBIRAWAB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2382] <- "GOBIRAWAA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2122] <- "GOBIRAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2282] <- "GOBIRAWAB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2154] <- "GOBIRAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2116] <- "GOBIRAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2168] <- "G/CIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2298] <- "GOBIRAWAB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2194] <- "GOBIRAWAB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2252] <- "G/CIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2374] <- "GOBIRAWAA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2152] <- "G/UNGUWARHASSAN"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2276] <- "G/YAMMA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2244] <- "BURHANA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2222] <- "G/CIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2432] <- "G/SHAGOTARA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2058] <- "G/CIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2220] <- "G/CIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2216] <- "G/CIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2166] <- "FILINDURUMI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2332] <- "G/DUKAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2162] <- "GOBIRAWAB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2324] <- "G/DUKAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2402] <- "G/SHAGOTARA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2092] <- "G/CIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2440] <- "G/SHAGOTARA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2424] <- "G/SHAGOTARA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2144] <- "GOBIRAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2198] <- "GOBIRAWAB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2460] <- "GOBIRAWAA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2138] <- "GOBIRAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2062] <- "GOBIRAWAA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2286] <- "GOBIRAWAB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2406] <- "G/SHAGOTARA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2410] <- "G/SHAGOTARA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2032] <- "GOBIRAWAA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2492] <- "G/KURNAA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2188] <- "G/KURNA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 2484] <- "KURNAA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1366] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1378] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1386] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1390] <- "FORESTRY"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1392] <- "FORESTRY"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1394] <- "FORESTRY"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1396] <- "YAMADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1408] <- "YAMADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1412] <- "YAMADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1420] <- "FORESTRY"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1422] <- "JAENJIGAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1428] <- "DORAYI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1384] <- "YAMADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1438] <- "UNGUWARWAMBAI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1426] <- "UNGUWARJAKADA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1376] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1416] <- "UNGUWARJAKADA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1398] <- "DORAYIKARAMA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1380] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1436] <- "UNGUWARWAMBAI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1362] <- "YAMADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1382] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1402] <- "DORAYIKARAMA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1400] <- "DORAYIKARAMA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1434] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1404] <- "DORAYIKARAMA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1370] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1410] <- "YAMADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1406] <- "DORAYIKARAMA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1388] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1414] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1368] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1432] <- "UNGUWARJAKADA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1418] <- "UNGUWARJAKADA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1364] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1424] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1374] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1358] <- "YAMADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1372] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1360] <- "DORAYIBABBA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 316] <- "KASUWARMATA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 324] <- "POLICESTATION"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 326] <- "POLICESTATION"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 328] <- "FRANCEROAD"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 330] <- "MASALLACINWAJE"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 336] <- "KASUWARMATA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 348] <- "TRIUMPHPUBLISHINGCOMPANY"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 318] <- "FAGGED2"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 320] <- "WAPA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 322] <- "WAPAD2"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 332] <- "WAPA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 334] <- "FAGGED2"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 338] <- "ILLOROAD"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 340] <- "KASUWARMATA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 342] <- "GURUZAROAD"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 344] <- "GURUZAROAD"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 346] <- "HAJHAUWAMAISAKAROAD"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 350] <- "HAJHAUWAMAISAKAROAD"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 352] <- "FAGGED2"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 354] <- "FAGGED2"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 356] <- "CHIKALAROAD"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 358] <- "TRIUMPHPUBLISHINGCOMPANY"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 314] <- "WAPA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1470] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1452] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1454] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1456] <- "KOFARMATADYEPITS"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1458] <- "FILINIDI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1460] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1462] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1464] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1466] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1468] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1472] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1474] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1476] <- "MURTALAMUHAMMADSPECIALISTHOSPITAL1"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1478] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1480] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1482] <- "MURTALAMUHAMMADSPECIALISTHOSPITAL2"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1484] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1486] <- "ZANGO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1450] <- "FILINIDI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 1448] <- "FILINIDI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 3] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 4] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 8] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 9] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 10] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 11] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 12] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 13] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 14] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 15] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 16] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 17] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 18] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 19] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 21] <- "NASSARAWAGRA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 23] <- "KAWO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 24] <- "KAWOCIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 25] <- "KAWOCIKINGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 26] <- "KAWOKUDU"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 27] <- "KAWOKUDU"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 28] <- "KAWOMAIGARI"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 29] <- "HOTORO"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 31] <- "GIGINYUB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 32] <- "GIGINYUB"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 37] <- "BADAWA"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 46] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 47] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 48] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 49] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 50] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 51] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 52] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 53] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 54] <- "BADAWALAYOUT"
kano_hh_listed$ea_name_new[kano_hh_listed$ea_serial_number_new == 55] <- "BADAWALAYOUT"



#Reading the sampled HH file

kano_hh_sampled <- readxl::read_excel(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN Sampled HHs_2024.xlsx"))
kano_hh_sampled <- kano_hh_sampled %>%
  mutate(ea_serial_number_new = gsub("-","",easerialnumber) 
  )

kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2102] <- "GOBIRAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2180] <- "GOBIRAWAB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2382] <- "GOBIRAWAA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2122] <- "GOBIRAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2282] <- "GOBIRAWAB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2154] <- "GOBIRAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2116] <- "GOBIRAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2168] <- "G/CIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2298] <- "GOBIRAWAB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2194] <- "GOBIRAWAB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2252] <- "G/CIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2374] <- "GOBIRAWAA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2152] <- "G/UNGUWARHASSAN"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2276] <- "G/YAMMA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2244] <- "BURHANA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2222] <- "G/CIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2432] <- "G/SHAGOTARA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2058] <- "G/CIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2220] <- "G/CIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2216] <- "G/CIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2166] <- "FILINDURUMI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2332] <- "G/DUKAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2162] <- "GOBIRAWAB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2324] <- "G/DUKAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2402] <- "G/SHAGOTARA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2092] <- "G/CIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2440] <- "G/SHAGOTARA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2424] <- "G/SHAGOTARA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2144] <- "GOBIRAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2198] <- "GOBIRAWAB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2460] <- "GOBIRAWAA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2138] <- "GOBIRAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2062] <- "GOBIRAWAA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2286] <- "GOBIRAWAB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2406] <- "G/SHAGOTARA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2410] <- "G/SHAGOTARA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2032] <- "GOBIRAWAA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2492] <- "G/KURNAA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2188] <- "G/KURNA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 2484] <- "KURNAA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1366] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1378] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1386] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1390] <- "FORESTRY"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1392] <- "FORESTRY"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1394] <- "FORESTRY"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1396] <- "YAMADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1408] <- "YAMADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1412] <- "YAMADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1420] <- "FORESTRY"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1422] <- "JAENJIGAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1428] <- "DORAYI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1384] <- "YAMADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1438] <- "UNGUWARWAMBAI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1426] <- "UNGUWARJAKADA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1376] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1416] <- "UNGUWARJAKADA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1398] <- "DORAYIKARAMA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1380] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1436] <- "UNGUWARWAMBAI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1362] <- "YAMADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1382] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1402] <- "DORAYIKARAMA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1400] <- "DORAYIKARAMA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1434] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1404] <- "DORAYIKARAMA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1370] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1410] <- "YAMADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1406] <- "DORAYIKARAMA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1388] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1414] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1368] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1432] <- "UNGUWARJAKADA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1418] <- "UNGUWARJAKADA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1364] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1424] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1374] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1358] <- "YAMADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1372] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1360] <- "DORAYIBABBA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 316] <- "KASUWARMATA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 324] <- "POLICESTATION"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 326] <- "POLICESTATION"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 328] <- "FRANCEROAD"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 330] <- "MASALLACINWAJE"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 336] <- "KASUWARMATA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 348] <- "TRIUMPHPUBLISHINGCOMPANY"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 318] <- "FAGGED2"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 320] <- "WAPA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 322] <- "WAPA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 332] <- "WAPA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 334] <- "FAGGED2"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 338] <- "ILLOROAD"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 340] <- "KASUWARMATA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 342] <- "GURUZAROAD"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 344] <- "GURUZAROAD"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 346] <- "HAJHAUWAMAISAKAROAD"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 350] <- "HAJHAUWAMAISAKAROAD"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 352] <- "FAGGED2"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 354] <- "FAGGED2"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 356] <- "CHIKALAROAD"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 358] <- "TRIUMPHPUBLISHINGCOMPANY"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 314] <- "WAPA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1470] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1452] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1454] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1456] <- "KOFARMATADYEPITS"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1458] <- "FILINIDI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1460] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1462] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1464] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1466] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1468] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1472] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1474] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1476] <- "MURTALAMUHAMMADSPECIALISTHOSPITAL1"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1478] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1480] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1482] <- "MURTALAMUHAMMADSPECIALISTHOSPITAL2"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1484] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1486] <- "ZANGO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1450] <- "FILINIDI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 1448] <- "FILINIDI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 3] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 4] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 8] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 9] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 10] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 11] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 12] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 13] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 14] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 15] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 16] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 17] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 18] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 19] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 21] <- "NASSARAWAGRA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 23] <- "KAWO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 24] <- "KAWOCIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 25] <- "KAWOCIKINGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 26] <- "KAWOKUDU"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 27] <- "KAWOKUDU"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 28] <- "KAWOMAIGARI"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 29] <- "HOTORO"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 31] <- "GIGINYUB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 32] <- "GIGINYUB"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 37] <- "BADAWA"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 46] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 47] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 48] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 49] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 50] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 51] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 52] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 53] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 54] <- "BADAWALAYOUT"
kano_hh_sampled$ea_name_new[kano_hh_sampled$ea_serial_number_new == 55] <- "BADAWALAYOUT"


kano_hh_sampled <- kano_hh_sampled %>% 
  mutate(enumeration_area = paste0(toupper(ea_name_new), "/", ea_serial_number_new), 
         enumeration_area = str_replace_all(enumeration_area, "[ ,]", ""))


kano_hh_sampled <- kano_hh_sampled %>%
  group_by(enumeration_area) %>%  # Group by the Enumeration Area
  mutate(
    # Assign total households in the same EA to 'tota_hh_ea', only if 'tota_hh_ea' is NA
    tota_hh_ea = ifelse(is.na(tota_hh_ea), n(), tota_hh_ea),  
    
    # Assign serial number within the EA, only if 'hh_serial_no_in_ea' is NA
    hh_serial_no_in_ea = ifelse(is.na(hh_serial_no_in_ea), row_number(), hh_serial_no_in_ea)
  ) %>%
  ungroup()


names(kano_hh_sampled)

#Reading corrected EA names
newcorrected_EAS <- read_xlsx(file.path(dhsDir,"nigeria/kano_ibadan_epi/EA_data/Kano Selected EAs.xlsx"))%>%
  mutate(capital_ea_name = toupper(name), 
         ea_name = paste0(capital_ea_name,"/",eas),
        ea_name = gsub(" ","",ea_name)
         )


information <- datahhkn %>%
 dplyr::select(1,6,121,122,10,120,12,13,14,15 )
write_dta(information, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/HH_Details_request.dta")


rdtkn_data <- read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_data_with_EA_names_cleaned_to_114.dta") )

gpsmissing <- haven::read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano_missing_lon_lat_add_totals.dta"))%>%
  left_join(rdtkn_data, by=c("serial_number" = "sn"),relationship = "many-to-many")%>%
  dplyr::select(1:34,46,47,153:155)


###Household Sampled
kano_hh_listed <- as.data.frame(kano_hh_listed)

names(kano_hh_listed) <- c("start", "end", "location", "latitude", "longitude", 
                           "altitude", "precision", "date_time", "city", "ward",
                           "enumeration_area", "ea_serial_number", "settlement", 
                           "lister_name", "phone_number", "structure_serial_number", 
                           "address", "residential_structure", "hh_serial_number", 
                           "visit_one", "notes_one", "save_one", "visit_two", 
                           "notes_two", "save_two", "visit_three", "hh_gender", "number_in_hh", 
                           "final_completion", "gratitude", "id", "uuid", "submission",
                           "validation_status", "notes_three", "status", "submitted",
                           "version",  "tags", "index","ea_serial_number_new", "ea_name_new" )


listed_households <- kano_hh_listed %>% 
  mutate(
   enumeration_area = paste0(toupper(ea_name_new), "/", ea_serial_number_new), 
           enumeration_area = str_replace_all(enumeration_area, "[ ,]", ""),
    prob_selected_ward = case_when(ward == "Zango"~ 1,
                                        ward == "Dorayi"~ 1/2,
                                        ward == "Tudun Wazurchi"~ 1,
                                        ward == "Gobirawa"~ 1, 
                                        ward == "Giginyu"~ 1,
                                        ward == "Fagge"~ 1)) %>%
  group_by(ea_serial_number_new, ward, hh_serial_number, structure_serial_number) %>%
  distinct() %>% 
  mutate(total_hh_listed_structure = n())


selected_household <- kano_hh_sampled %>% 
  mutate(enumeration_area  = enumeration_area,
    prob_selected_eas_settlement = case_when(
      ward == "Zango" & settlement == "Informal" ~ 1,
      ward == "Zango" & settlement == "Slum" ~ 1,
      ward == "Zango" & settlement == "Formal" ~ 1,
      
      ward == "Gobirawa" & settlement == "Formal" ~ 3/14,
      ward == "Gobirawa" & settlement == "Informal" ~ 37/228,
      ward == "Gobirawa" & settlement == "Slum" ~ 0,
      
      ward == "Giginyu" & settlement == "Formal" ~ 1,
      ward == "Giginyu" & settlement == "Informal" ~ 1,
      ward == "Giginyu" & settlement == "Slum" ~ 0,
      
      ward == "Fagge" & settlement == "Formal" ~ 1,
      ward == "Fagge" & settlement == "Informal" ~ 1,
      ward == "Fagge" & settlement == "Slum" ~ 0,
      
      ward == "Dorayi" & settlement == "Formal" ~ 1,
      ward == "Dorayi" & settlement == "Informal" ~ 27/29,
      ward == "Dorayi" & settlement == "Slum" ~ 0,
      
      TRUE ~ NA_real_  # If none of the conditions match, keep the original 'Value'
    )
  ) %>%
  group_by(enumeration_area, ea_serial_number_new, ward, ea_name_new ,structure_serial_number) %>% 
  dplyr::distinct() %>% 
  mutate(total_hh_selected_structure = n(), ) %>% 
  ungroup()


naeas <- selected_household %>% filter(is.na(prob_selected_eas_settlement))




all_eas <- selected_household %>% 
  dplyr::select(ward, enumeration_area, ea_serial_number_new, settlement) %>% 
  group_by(ward, enumeration_area, ea_serial_number_new, settlement) %>% 
  summarise(total = n(), 
            ea_name = paste0(enumeration_area[1], "/", ea_serial_number_new[1]))



all_selected_hh <- inner_join(selected_household, 
                             kano_hh_sampled , 
                              by = c( "index" = "_index"), relationship = "many-to-many") %>% 
  mutate(prob_selected_hh_structure = total_hh_selected_structure/total_hh_listed_structure)



weights_data <- all_selected_hh %>% 
  inner_join(kano_hh_sampled_eanames, by= c("enumeration_area.x"="enumeration_area"), relationship = "many-to-many") %>% 
  dplyr::select(longitude, latitude, ward = ward.x, index,
                enumeration_area.x, ea_name_new.x, ea_serial_number_new.x,
                hh_serial_number, 
                structure_serial_number,prob_selected_ward, 
                prob_selected_eas_settlement,
                prob_selected_hh_structure)%>%
                distinct()



write.csv(weights_data, file.path(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/kano_weights_data.csv")))

