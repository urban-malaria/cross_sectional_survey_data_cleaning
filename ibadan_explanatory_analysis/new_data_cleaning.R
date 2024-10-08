

# Ibadan_data <- read.csv(file.path(cleaned_data_path, metropolis_name, "ibadan_malaria_individual_information.csv")) #ibadan_malaria_individual_hh_individual_information.csv


Ibadan_data <- read_dta("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/IB Wet season household members RDT_160924.dta")


modified_merged_dataset_mod <- read.csv(file.path("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Field data/cleaned_data", metropolis_name,"corrected_eas_ibadan_malaria_data00.csv")) %>% 
  # please check this file with the file that you say you cleaned 
  # @ this point i am reading the data that I previously 
  # cleaned and I have 127 EAs and I think the difference may be explained by an EA called Agugu, 
  # NA, and "", that I think I saw in you data wen we spoke.
  # take a moment to look at the code and see if it works for 
  # you and if the files are in alignment you can then proceed 
  # with the analysis you are required to do 
  dplyr::select(serial_number, ward = Ward, enumaration_area = ea_numbers_new ,settlement_type_new, 
               prob_selected_ward, prob_selected_eas_settlement,
               prob_selected_hh_structure, ward_weight, ea_settlement_weight, 
               hhs_weights, hh_number) %>% 
  group_by(serial_number) %>% 
  mutate(tot = 1:n()) %>% 
  filter(tot == 1) %>% 
  distinct()


individual_information_weights <- left_join(Ibadan_data, modified_merged_dataset_mod, 
                                             #in the merge I gained an additional
                                             #i need the ward, EAs information the 
                                             #for the merge to be successful 
                                             by = c("sn" = "serial_number"))  %>% 
  mutate(overall_total = n(), 
         ages = as.numeric(difftime(as.Date("2023-06-01"),as.Date(hl6), units = "days")/365.2422)) %>% 
  group_by(ward) %>% 
  mutate(ward_total = n()) %>% 
  ungroup() %>% 
  group_by(ward, enumaration_area) %>% 
  mutate(ea_total = n()) %>%
  ungroup() %>% 
  group_by(ward, enumaration_area, sn) %>% 
  mutate(hh_total = n()) %>%
  ungroup() %>% 
  mutate(agebin = cut(ages, c(0,5,10,17,30, 122), include.lowest = T)) %>% 
  group_by(ward, enumaration_area, sn, agebin) %>%
  mutate(age_total = n())


# add the overall individual weights 



final_individual_data_with_weights <- individual_information_weights %>% 
  group_by(ward, settlement_type_new, enumaration_area, hh_number, agebin) %>% 
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() %>%
  # the missing 146 house holds are currently assigned an overall mean weight of the survey
  mutate(overall_hh_weight =  replace_na(overall_hh_weight, mean(overall_hh_weight, na.rm = TRUE)))


write_dta(final_individual_data_with_weights, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/wetseason_household_members_with_weights.dta")  





# save the file and share with Bernand and Yusuf  and
# point them to all the other respective dataset they should 
# be merging with to get all the respective variables for their descriptives 

Ibadan_data <- read_dta("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/wetseason_household_members_with_weights.dta")


serial_numbers = sort(unique(Ibadan_data$sn)) %in% sort(unique(modified_merged_dataset_mod$serial_number))
indices = which(serial_numbers == FALSE)
which(serial_numbers == FALSE)

sort(unique(Ibadan_data$sn))[indices]
