rm(list=ls())

metropolis_name <- "Kano"

source("load_paths.R")


 # Kano_data_malaria_screening <- read_dta(file.path(dropbox, "KN Wet season household malaria screening.dta"))
 # Kano_data_hh_individuals00 <- read_dta(file.path(dropbox, "KN wet season household list wt xteristics.dta"))
 # 
 # 
 #selected_names <- names(Kano_data_hh_individuals)

Kano_data_malaria_screening <- read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN wet season hhold  RDT results_290924.dta"))


rdt_data <- read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/HH_Details_request.dta"))



Kano_data_hh_individuals <- Kano_data_malaria_screening   %>% 
  left_join(rdt_data , by = c("sn" = "sn") ) %>% 
  distinct()


names(Kano_data_hh_individuals) <-c("serial_number", "repeat_instrument", "repeat_instance",
          "line_number01", "household_residents", "relationship_head_household",
          "gender", "age", "dob", "mother_present", "marital_status", "rdt_eligibility", 
         "complete02",  "request_consent",   "consent_rdt", "rdt_test_result" ,   
         "dried_blood_sample" ,"dbs_code" , "lga", "ward", "settlement_type","community_name",
         "enumaration_area", "hh_number", "longitude", "latidude", "name_household_head")


kano_all_data  <-  Kano_data_hh_individuals





combined_data_corrected_eas <- kano_all_data %>% 
  mutate(line_number00 = repeat_instance, 
         unique_id = paste0(serial_number, "_", line_number00), 
         agebin = cut(age, c(0, 5, 10, 17, 30, 100), include.lowest = T)) %>%
  #inner_join(Kano_data_malaria_screening_cleaned, by = c("serial_number", "unique_id")) %>% 
  group_by(ward) %>%
  mutate(ward_total = n()) %>% 
  ungroup() %>% 
  group_by(ward, enumaration_area) %>% 
  mutate(ea_total = n()) %>%
  ungroup() %>% 
  group_by(ward, enumaration_area, serial_number) %>% 
  mutate(hh_total = n()) %>%
  ungroup() %>% 
  mutate(agebin = cut(age, c(0,5,10,17,30, 122), include.lowest = T)) %>% 
  group_by(ward, enumaration_area, serial_number, agebin) %>%
  mutate(age_total = n(), 
         longitude = as.numeric(longitude)
         ,
         latitude = as.numeric(latidude)
         ) 
  


missing_add_totals <- combined_data_corrected_eas %>% 
  filter(is.na(latidude))  

write_dta(missing_add_totals, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano_missing_lon_lat_add_totals.dta"))



add_totals <- combined_data_corrected_eas %>% 
  filter(!is.na(latidude))



# write.csv(combined_data_corrected_eas, file.path(cleaned_data, metropolis_name,"combined_data_corrected_eas.csv"))



##########################################################################################################
# ANALYSIS

# kano_weights <- read.csv(file.path(dropbox, "kano_weights_data_v01.csv")) %>%
#   mutate(longitude, latitude, ward, enumeration_area,ea_serial_number,
#          hh_serial_number, structure_serial_number,
#          ward_weight = 1/prob_selected_ward,
#          ea_settlement_weight = 1/prob_selected_eas_settlement,
#          hhs_weights = 1/prob_selected_hh_structure) %>% 
#   distinct()


kano_weights <- read.csv(file.path(dropbox, "kano_weights_data_v01.csv")) %>%
  mutate(longitude, latitude, ward, ea_serial_number,
         hh_serial_number, structure_serial_number,
         ward_weight = 1/prob_selected_ward,
         ea_settlement_weight = 1/prob_selected_eas_settlement,
         hhs_weights = 1/prob_selected_hh_structure) %>% 
  distinct()


# newdata <- read_xlsx("C:/Users/laure/Downloads/EA Names Missing weights (1).xlsx")


add_totals_sf <- sf::st_as_sf(add_totals, coords = c("longitude", "latitude"), crs = 4326)
kano_weight_sf <- sf::st_as_sf(kano_weights, coords = c("longitude", "latitude"), crs = 4326) 

# merged_dataset <- sf::st_join(add_totals_sf, kano_weight_sf) 

nearest_indices <- sf::st_nearest_feature(add_totals_sf, kano_weight_sf)

merged_dataset <- cbind(add_totals_sf, kano_weight_sf[nearest_indices, ])



missing_weights <- merged_dataset %>%
  filter(is.na(hhs_weights))



modified_merged_dataset <- merged_dataset %>% 
  mutate(enumeration_area = enumaration_area) %>% 
  group_by(ward) %>%
  mutate(ward_weight = ifelse(is.na(ward_weight)==T, mean(ward_weight, na.rm = T), ward_weight)) %>% 
  ungroup() %>% 
  group_by(ward,  settlement_type, ea_settlement_weight) %>% 
  mutate(ea_settlement_weight = ifelse(is.na(ea_settlement_weight)==T, mean(ea_settlement_weight, na.rm = T), ea_settlement_weight)) %>% 
  ungroup() %>% 
  group_by(ward, enumeration_area ,settlement_type) %>% 
  mutate(hhs_weights = ifelse(is.na(hhs_weights)==T, 
                              mean(hhs_weights, na.rm = T), 
                              hhs_weights)) 



coords <- sf::st_coordinates(modified_merged_dataset)


modified_merged_dataset$longitude <- coords[, 'X']
modified_merged_dataset$latitude <- coords[, 'Y']

modified_merged_dataset_mod <- modified_merged_dataset %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-c(X, ward.1, geometry.1)) %>% 
  filter(is.na(hhs_weights))


modified_merged_dataset_updated <- modified_merg
ed_dataset_mod %>% 
  group_by(ward, settlement_type, enumaration_area, hh_number, agebin) %>% 
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() 



missin_eas_weights <- modified_merged_dataset_updated %>% 
  dplyr::filter(is.na(overall_hh_weight))


# write.csv(modified_merged_dataset_mod, file.path(cleaned_data_path, metropolis_name,"kano_malaria_weighted_information_v00.csv")) 

haven::write_dta(modified_merged_dataset_mod, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano_rdt_with_weights.dta"))

