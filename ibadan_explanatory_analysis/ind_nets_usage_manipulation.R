rm(list = ls())


metropolis_name <- "Ibadan"

source("load_paths.R")
LuDir <- file.path(Drive,"Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data")

library(dplyr);library(tidyr);library(stringr)



all_data <- haven::read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/ibadan_long_wetseason_household_members.dta"))
ib_household_net_insp <- read_dta(file.path(LuDir, "240922_Ibadan_latest_data", "IB Wet season hhold net inspection.dta" ))

max_splits <- max(str_count(ib_household_net_insp$nh114a, "\\s*(,|AND|and|/|&)\\s*") + 1)


# Separate the columns nh114 and nh114a directly

# Process the data with corrected separate statements
ib_household_slept_net <- ib_household_net_insp %>%
  dplyr::select(sn, redcap_repeat_instrument, redcap_repeat_instance, nh113, nh113a, nh114, nh114a) %>%
  mutate(nh114a = ifelse(sn == 11010, "1", nh114a),
         nh114a = ifelse(nh114a == "83", "3", nh114a)) %>% 
  separate(nh114, into = paste0("nh114_part", 1:max_splits), sep = ",", fill = "right", extra = "drop") %>%
  separate(nh114a, into = paste0("nh114a_part", 1:max_splits), sep = "\\s*( |,|AND|and|/|&)\\s*", fill = "right", extra = "drop") %>%
  dplyr::select(-c(nh114_part1, nh114_part2, nh114_part3, nh114_part4, nh114_part5))

ib_household_slept_net_melted <- melt(ib_household_slept_net, id.vars = c("sn", "redcap_repeat_instrument", 
                                          "redcap_repeat_instance", "nh113", "nh113a")) 


ib_household_slept_net_melted$new_line_number <- as.numeric(ib_household_slept_net_melted$value) 


ib_household_slept_net_melted <- ib_household_slept_net_melted %>% 
  mutate(slept_under_net = ifelse(!is.na(new_line_number), 1, 0)) %>% 
  drop_na(nh113)


net_sleeping_columns <- left_join(all_data, ib_household_slept_net_melted, by = c("sn" = "sn", 
                                                                                  "redcap_repeat_instance" ="new_line_number"))



#cleaning names for saving 
net_sleeping_columns_filtered <- net_sleeping_columns[, !grepl("\\.y", names(net_sleeping_columns))]

names(net_sleeping_columns_filtered)[grepl("\\.x", names(net_sleeping_columns_filtered))] <- 
  gsub("\\.x", "", names(net_sleeping_columns_filtered)[grepl("\\.x", names(net_sleeping_columns_filtered))])

names(net_sleeping_columns_filtered) <- gsub("[^[:alnum:]]", "_", names(net_sleeping_columns_filtered))


write_dta(net_sleeping_columns_filtered, file.path(dhsDir, "data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/ibadan_long_wetseason_household_members_with_ind_nets.dta") ) 
 





