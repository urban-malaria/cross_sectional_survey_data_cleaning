


hh_survey_data <- file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_HH data_with_RDT Results.dta")  # Replace with the path to your .dta file
datahhkn <- haven::read_dta(hh_survey_data)

weights_data <- read_csv(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/kano_weights_data.csv"))
  



missingeas_field_data <- datahhkn %>%
  mutate(ward_y = ifelse(ward_y == "Fagge D2","Fagge",ward_y)
         )%>%
  filter(!ea_cluster %in% unique(weights_data$enumeration_area) )%>%
  dplyr::select(ward = ward_y, longitude = bi7_long , latitude = bi7_lat, enumeration_area = ea_cluster)%>%
  mutate(...1 = NA, index = NA, ea_serial_number = NA, hh_serial_number = NA, structure_serial_number = NA, prob_selected_ward=NA, prob_selected_eas_settlement=NA, prob_selected_hh_structure=NA)%>%
  distinct()%>%
  dplyr::select(names(weights_data))

new_weight_data <- bind_rows(weights_data,missingeas_field_data )%>%
  group_by(ward)%>%
  mutate(prob_selected_ward=ifelse(is.na(prob_selected_ward), mean(prob_selected_ward, na.rm = T), prob_selected_ward), 
         prob_selected_eas_settlement=ifelse(is.na(prob_selected_eas_settlement), mean(prob_selected_eas_settlement, na.rm = T), prob_selected_eas_settlement), 
         prob_selected_hh_structure=ifelse(is.na(prob_selected_hh_structure), mean(prob_selected_hh_structure, na.rm = T), prob_selected_hh_structure),
         prob_selected_ward = ifelse(ward == "Fagge",1,prob_selected_ward),
         prob_selected_hh_structure = ifelse(prob_selected_hh_structure > 1,1,prob_selected_hh_structure)
          )
#write the new weights out
write.csv(new_weight_data, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/final_weight_calculated_kano.csv"))

unique(missingeas_field_data$ea_cluster)


write.csv(missingeas_field_data, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/missing_ea_data.csv"))