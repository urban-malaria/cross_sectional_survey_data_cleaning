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

write.csv(missing_add_totals, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano_missing_lon_lat_add_totals.dta"))



add_totals <- combined_data_corrected_eas %>% 
  filter(!is.na(latidude))



# write.csv(combined_data_corrected_eas, file.path(cleaned_data, metropolis_name,"combined_data_corrected_eas.csv"))



##########################################################################################################
# ANALYSIS


kano_weights <- read.csv(file.path(dropbox, "kano_weights_data.csv")) %>%
  mutate(longitude, latitude, ward, enumeration_area,ea_serial_number,
         hh_serial_number, structure_serial_number,
         ward_weight = 1/prob_selected_ward,
         ea_settlement_weight = 1/prob_selected_eas_settlement,
         hhs_weights = 1/prob_selected_hh_structure) %>% 
  distinct()

# kano_weights00 <- read.csv(file.path(dropbox, "kano_weights_data.csv")) %>% 
#   mutate(longitude, latitude, ward, enumeration_area,ea_serial_number, 
#            hh_serial_number, structure_serial_number, 
#            ward_weight = 1/prob_selected_ward, 
#            ea_settlement_weight = 1/prob_selected_eas_settlement, 
#            hhs_weights = 1/prob_selected_hh_structure) %>% 
#   dplyr::select(ward, enumeration_area,ea_serial_number, ward_weight, ea_settlement_weight) %>% 
#   distinct()

# newdata <- read_xlsx("C:/Users/laure/Downloads/EA Names Missing weights (1).xlsx")


add_totals_sf <- sf::st_as_sf(add_totals, coords = c("longitude", "latitude"), crs = 4326)
kano_weight_sf <- sf::st_as_sf(kano_weights, coords = c("longitude", "latitude"), crs = 4326) 

merged_dataset <- sf::st_join(add_totals_sf, kano_weight_sf) 

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



coords <- sf::st_coordinates(modified_merged_dataset_updated)


modified_merged_dataset_updated$longitude <- coords[, 'X']
modified_merged_dataset_updated$latitude <- coords[, 'Y']

modified_merged_dataset_mod <- modified_merged_dataset_updated %>% 
  sf::st_drop_geometry()

modified_merged_dataset_mod <- modified_merged_dataset_mod[!duplicated(modified_merged_dataset_mod$unique_id, fromLast = TRUE), ]

write.csv(modified_merged_dataset_mod, file.path(cleaned_data_path, metropolis_name,"kano_malaria_weighted_information_v00.csv")) 

coordinates <- modified_merged_dataset_mod %>% 
  dplyr::select(Ward = ward.x, longitude, latitude) %>% 
  distinct()

write.csv(coordinates, file.path(cleaned_data_path, metropolis_name,"coordinates.csv")) 


#######################################ANALYSIS###################################################


kano_data_malaria_data <- read.csv(file.path(cleaned_data_path, metropolis_name,"kano_malaria_weighted_information_v00.csv")) %>% 
  group_by(ward.x, settlement_type,  hh_number, ea_names) %>% 
  mutate(members_tested_hh = n()) %>% 
  ungroup() %>% 
  group_by(ward.x, settlement_type, ea_names) %>% 
  mutate(members_tested_ea = n()) %>% 
  distinct() 



weight_adjusted_tpr <- kano_data_malaria_data %>%
  drop_na(rdt_test_result, settlement_type) %>% 
  mutate(malaria_test = ifelse(rdt_test_result == 1, 1, 0)) %>%
  group_by(settlement_type) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum(malaria_test * overall_hh_weight, na.rm = T) / sum(overall_hh_weight, na.rm = T) * 100, 3),
            compliment = 100 - tpr)




new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type"))

names(new_data) <- c("settlement_type", "result", "value")



labels_new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_new_data) <- c("settlement_type", "result", "percentage")

plotting_data <- inner_join(new_data, labels_new_data) %>% 
  mutate(plot_position = cumsum(value) - ( value))


ggplot(data = plotting_data) +
  geom_bar(aes(x = settlement_type, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = settlement_type, y = value, label = paste(percentage, "(%)")),  
            color = "black",
            nudge_y = 10, size = 8) +
  scale_fill_manual(values = c("negative" = "#FFE7E7", "positive" = "#944E63")) +
  labs(title = "",
       x = "settlement Type",
       y = "number of people tested for malaria",
       fill = "malaria RDT result") +
  theme_bw(base_size = 20, base_family = "")


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_02.pdf"), 
       dpi = 400, width = 15,
       height = 10,)


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_02.png"), 
       dpi = 400, width = 15,
       height = 10,)

# box plot 

EA_weight_adjusted_tpr <- kano_data_malaria_data %>%
  drop_na(rdt_test_result, settlement_type) %>% 
  # st_drop_geometry() %>%
  mutate(malaria_test = ifelse(rdt_test_result == 1, 1, 0)) %>%
  group_by(ward.x, settlement_type, ea_names, members_tested_ea) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum((malaria_test * overall_hh_weight), na.rm = T) / sum(overall_hh_weight,na.rm = T) * 100, 3),
            compliment = 100 - tpr) %>% 
  mutate(settlement_type  = ifelse(settlement_type == 1, "Formal", 
                                   ifelse(settlement_type == 2, "Informal", "Slum")))






less_than_5 = EA_weight_adjusted_tpr %>% filter(!is.nan(tpr)) %>% 
  mutate(target = ifelse(tpr < 5, "less than 5%", 
                         "greater than 5%")) %>% 
  group_by(target) %>% 
  summarise(totals = sum(total))



less_than_1 = EA_weight_adjusted_tpr %>% filter(!is.nan(tpr)) %>% 
  mutate(target = ifelse(tpr < 1, "less than 1%", 
                         "greater than 1%")) %>% 
  group_by(target) %>% 
  summarise(totals = sum(total))


duplicated <- EA_weight_adjusted_tpr %>% 
  group_by(ward.x, ea_names, settlement_type) %>% 
  summarise(total = n()) %>% 
  mutate(ward = case_when(ward.x == 1 ~ "ZANGO", 
                          ward.x == 2 ~ "DORAYI",
                          ward.x == 4 ~ "FAGGE D2", 
                          ward.x == 5 ~ "GOBIRAWA", 
                          ward.x == 6 ~ "GIGINYU"))
  
# %>% 
#   filter(total >1)

write.csv(duplicated, file.path(cleaned_data_path, metropolis_name,"alleas.ea_names.csv"), row.names = F)  

plot(sf::st_read("C:/Users/lml6626/Downloads/GEOPODE_GEOMETRY_EXPORT (6)/boundary_wards_export/boundary_wards_export.shp"))

# write.csv(EA_weight_adjusted_tpr, file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr.csv"), row.names = F)  



ggplot(EA_weight_adjusted_tpr, aes(x = settlement_type, y = tpr),  fill = settlement_type_new ) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = settlement_type, size = members_tested_ea), width = 0.08, alpha = 0.5)+
  # scale_color_manual(values=c("#FFE7E7",  "#F2A6A2", "#B47B84")) +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  labs(title = "",
       x = "settlement type",
       y = "enumaration area test positivity rate", 
       color ="settlement type", 
       size = "number tested") +
  #theme_manuscript()+ 
  theme(legend.position = "none") +
  theme_bw(base_size = 20, base_family = "") 



ggsave(file.path(results, metropolis_name, "kano_tpr_wardlevel00.pdf"), 
       dpi = 300, width = 12,
       height = 10,)

ggsave(file.path(results, metropolis_name, "kano_tpr_wardlevel00.png"), 
       dpi = 400, width = 15,
       height = 8,)


# write.csv(EA_weight_adjusted_tpr, file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr.csv"), row.names = F)  

# estimate the prevalence by ward and settlement type 

weight_adjusted_ward_tpr <- Ibadan_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
  mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
  group_by(settlement_type_new, Ward) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum(malaria_test * overall_hh_weight, na.rm = T) / sum(overall_hh_weight, na.rm = T) * 100, 3),
            compliment = 100 - tpr)



new_ward_data <- weight_adjusted_ward_tpr %>% 
  dplyr::select(settlement_type_new, Ward, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type_new", "Ward"))


names(new_ward_data) <- c("settlement_type", "Ward", "result", "value")



labels_ward_new_data <- weight_adjusted_ward_tpr %>% 
  dplyr::select(settlement_type_new, Ward, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type_new", "Ward")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_ward_new_data) <- c("settlement_type", "Ward", "result", "percentage")

plotting_data <- inner_join(new_ward_data, labels_ward_new_data) %>% 
  mutate(plot_position = cumsum(value) - ( value))


ggplot(data = plotting_data) +
  geom_bar(aes(x = settlement_type, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = settlement_type, y = value, label = paste(percentage, "(%)")),  
            color = "black",
            size = 3.5, size = 3.5, nudge_y = 10) +
  facet_wrap(~Ward, ncol = 2)+
  scale_fill_manual(values = c("negative" = "#FFE7E7", "positive" = "#944E63")) +
  labs(title = "Malaria test results by settlement type",
       x = "Settlement Type",
       y = "Number of people tested for malaria",
       fill = "malaria RDT result") +
  theme_bw(base_size = 12, base_family = "")


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_ward_02.pdf"), 
       dpi = 300, width = 12,
       height = 10)


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_ward_02.png"), 
       dpi = 400, width = 12,
       height = 10)



age_adjusted_tpr <- Ibadan_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
  # st_drop_geometry() %>%
  mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
  group_by(settlement_type_new, agebin ) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum((malaria_test * overall_hh_weight), na.rm = T) / sum(overall_hh_weight,na.rm = T) * 100, 3),
            compliment = 100 - tpr)


new_ward_data <- age_adjusted_tpr %>% 
  dplyr::select(agebin, settlement_type_new, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type_new", "agebin"))


names(new_ward_data) <- c("settlement_type", "agebin", "result", "value")



labels_ward_new_data <- age_adjusted_tpr %>% 
  dplyr::select(settlement_type_new, agebin, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type_new", "agebin")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_ward_new_data) <- c("settlement_type", "agebin", "result", "percentage")

plotting_data <- inner_join(new_ward_data, labels_ward_new_data) %>% 
  mutate(plot_position = cumsum(value) - ( value))%>% 
  mutate(age_bin = factor(agebin, levels = c("[0,5]", "(5,10]", "(10,17]", "(17,30]", "(30,122]")))



ggplot(data = plotting_data) +
  geom_bar(aes(x = age_bin, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = age_bin, y = value, label = paste(round(percentage, 1), "(%)")),  
            color = "black",
            size = 3.5,  nudge_y = 10) +
  facet_wrap(~settlement_type)+
  scale_fill_manual(values = c("negative" = "#FFE7E7", "positive" = "#944E63")) +
  labs(x = "age groups",
       y = "number of people tested for malaria",
       fill = "") +
  theme_bw(base_size = 12, base_family = "")


ggsave(file.path(results, metropolis_name, "malaria_burden_age_and_settlement_type.pdf"), 
       dpi = 300, width = 12,
       height = 8)


ggsave(file.path(results, metropolis_name, "malaria_burden_age_and_settlement_type.png"), 
       dpi = 400, width = 12,
       height = 8)







##########################################################################################################

newdata <- kano_all_data %>% 
  mutate(agebin = cut(age, c(0,5,10,15,20,30,40,50, 60, 70, 122), include.lowest = T))

ggplot(newdata, aes(x = agebin, fill = as.factor(gender)))+
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#944E63"), 
                        labels = c("males", "females"))+
  labs(title = "Kano age sex distribution", 
       x = "age group", y = "Frequency", fill = "gender")
  

newdata %>% 
  # filter() %>% 
  ggplot(aes(x = agebin, fill = as.factor(settlement_type)))+
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#B47B84", "3" = "#944E63"), 
                    labels = c("formal", "informal","slums"))+
  labs(title = "Kano age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "settlement type")


newdata %>% 
  filter(!is.na(ward)) %>% 
  ggplot(aes(x = agebin, fill = as.factor(settlement_type)))+
  geom_bar() +
  facet_wrap(~ward,  labeller = labeller(ward = c("1" = "Zango", "2" = "Dorayi", "3" = "Tundun Wazurchi", 
,                        "4" = "Fagge 2", "5" = "Gobirawa", "6" = "Others")))+
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#B47B84", "3" = "#944E63"), 
                    labels = c("formal", "informal","slums"))+
  labs(title = "Kano age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "settlement type")



newdata$ward <- factor(newdata$ward, levels = c("1", "2", "3", "4", "5", "6"))


newdata %>% 
  filter(!is.na(settlement_type),!is.na(rdt_test_result), 
         !is.na(ward), ward != "3") %>% 
  ggplot(aes(x = ward, fill = as.factor(rdt_test_result)))+
  geom_bar() +
  facet_wrap(~settlement_type,  labeller = labeller(settlement_type = c("1" = "formal", "2" = "informal", "3" = "slums")))+
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#B47B84", "3" = "#944E63"), 
                    labels = c("positive", "negative", "undeterminate"))+
  scale_x_discrete(labels = c("1"= "Zango", "2" = "Dorayi", # "3" = "Tundun Wazurchi", 
  "4" = "Fagge 2", "5" = "Gobirawa", "6" = "Giginyu"))+
  labs(title = "Kano age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "test result")


# extract the covariates from Kano raster file 
# plot the data collection points on the respective shape files 
# create a geospatial model for the data area level and one that incoporates Krigging 
# fits a smooth surface over the data points 
