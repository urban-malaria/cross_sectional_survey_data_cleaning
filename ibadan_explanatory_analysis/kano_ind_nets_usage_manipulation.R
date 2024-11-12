rm(list = ls())


metropolis_name <- "Ibadan"

source("loadpath.R")

library(dplyr);library(tidyr);library(stringr)



all_data <- haven::read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/long_wetseason_household_membersV2_678_cols.dta"))
#ib_household_net_insp <- read_dta(file.path(dhsDir, "nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/IB Wet season hhold net inspection.dta" ))

max_splits <- max(str_count(kn_household_net_insp$nh114a, "\\s*(,|AND|and|/|&)\\s*") + 1)


# Separate the columns nh114 and nh114a directly

# Process the data with corrected separate statements
kn_household_slept_net <- kn_household_net_insp %>%
  select(sn, redcap_repeat_instrument, redcap_repeat_instance, nh113, nh113a, nh114, nh114a) %>%
  mutate(nh114a = ifelse(sn == 11010, "1", nh114a),
         nh114a = ifelse(nh114a == "83", "3", nh114a)) %>% 
  separate(nh114, into = paste0("nh114_part", 1:max_splits), sep = ",", fill = "right", extra = "drop") %>%
  separate(nh114a, into = paste0("nh114a_part", 1:max_splits), sep = "\\s*( |,|AND|and|/|&)\\s*", fill = "right", extra = "drop") %>%
  select(-c(nh114_part1, nh114_part2, nh114_part3, nh114_part4, nh114_part5))



kn_household_slept_net_melted <- melt(kn_household_slept_net, id.vars = c("sn", "redcap_repeat_instrument", 
                                          "redcap_repeat_instance", "nh113", "nh113a")) 


kn_household_slept_net_melted$new_line_number <- as.numeric(kn_household_slept_net_melted$value) 


kn_household_slept_net_melted <- kn_household_slept_net_melted %>% 
  mutate(slept_under_net = ifelse(!is.na(new_line_number), 1, 0)) %>% 
  drop_na(nh113)


net_sleeping_columns <- left_join(all_data, kn_household_slept_net_melted, by = c("sn" = "sn", 
                                                                                  "redcap_repeat_instance" ="new_line_number"))



#cleaning names for saving 
net_sleeping_columns_filtered <- net_sleeping_columns[, !grepl("\\.y", names(net_sleeping_columns))]

names(net_sleeping_columns_filtered)[grepl("\\.x", names(net_sleeping_columns_filtered))] <- 
  gsub("\\.x", "", names(net_sleeping_columns_filtered)[grepl("\\.x", names(net_sleeping_columns_filtered))])

names(net_sleeping_columns_filtered) <- gsub("[^[:alnum:]]", "_", names(net_sleeping_columns_filtered))


write_dta(net_sleeping_columns_filtered, file.path(dhsDir, "data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_long_wetseason_household_members_with_ind_nets.dta") ) 
 





