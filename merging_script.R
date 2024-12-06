LuDir <- file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024")
Maldir <- file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/last_upload_Akinyemi/Kano Wet Household questionnaire/")
IbDir <- file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/")  

# data paths 
kano_household_list <- read_dta(file.path(LuDir,"kano_malaria_weighted_information_final.dta") )
kn_household_net_insp <- read_dta(file.path(Maldir, "5. KN Wet season household net inspection.dta" ))
kn_household_travelers <- read_dta(file.path(Maldir, "3. KN Wet season household travellers.dta" ))
kn_household_visitors <- read_dta(file.path(Maldir, "2. KN Wet season household visitors.dta" ))
kn_women_survey <- read_dta(file.path(LuDir, "KN Wet season women survey_edited_290924.dta" ))
kn_men_survey <- read_dta(file.path(LuDir, "KN Wet season men survey_edited 290924.dta" ))


#
wide_kano_data <- kano_household_list %>%
  # dplyr::select(-c(redcap_repeat_instrument_x, redcap_repeat_instance_x)) %>% 
  pivot_wider(names_from = hl1, 
              values_from = c(hl2:q304))


wide_kn_household_visitors <- kn_household_visitors %>%
  dplyr::select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = redcap_repeat_instance, 
              values_from = c(v1:visitors_complete))

wide_kn_household_travelers <- kn_household_travelers %>%
  dplyr::select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = c(redcap_repeat_instance, c1ii ), 
              values_from = c(c1iii:tfhi_complete))


stata_format <- function(name) {
  
  name <- gsub("[\\(\\)]", "", name)  # Remove parentheses
  name <- gsub("[^[:alnum:]_]", "_", name)  # Replace special characters with underscores
  name <- tolower(name)  # Convert to lowercase
  
  # Ensure names are no longer than 32 characters
  name <- substr(name, 1, 32)
  
  # Make sure the name starts with a letter or underscore
  if (!grepl("^[a-zA-Z_]", name)) {
    name <- paste0("v_", name)  
  }
  
  return(name)
}


names(wide_kn_household_travelers) <- sapply(names(wide_kn_household_travelers), stata_format)




wide_kn_household_net_insp <- kn_household_net_insp %>%
  dplyr::select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = c(redcap_repeat_instance), 
              values_from = c(nh108:net_inspection_complete))


wide_data_together <- left_join(wide_kano_data) %>% 
  left_join(kn_men_survey, by = ("sn")) %>% 
  left_join(kn_women_survey, by = ("sn")) %>% 
  left_join(wide_kn_household_net_insp, by = ("sn")) %>% 
  left_join(wide_kn_household_travelers, by = ("sn")) %>% 
  left_join(wide_kn_household_visitors, by = ("sn"))

#cleaning names for saving 
wide_data_together_filtered <- wide_data_together[, !grepl("\\.y", names(wide_data_together))]

names(wide_data_together_filtered)[grepl("\\.x", names(wide_data_together_filtered))] <- gsub("\\.x", "", names(wide_data_together_filtered)[grepl("\\.x", names(wide_data_together_filtered))])

names(wide_data_together_filtered) <- gsub("[^[:alnum:]]", "_", names(wide_data_together_filtered))



##################################
#long data 
##################################
allnames_householdlist <- names(kano_household_list)



# removing duplicated columns before merging 
women_common_names <- which(names(kn_women_survey) %in% allnames_householdlist)[-1]

kn_women_survey <- kn_women_survey %>% 
  dplyr::select(-c(463:485))



men_common_names <- which(names(kn_men_survey) %in% allnames_householdlist)[-1]

kn_men_survey <- kn_men_survey %>% 
  dplyr::select(-c(men_common_names))


net_insp_common_names <- which(names(kn_household_net_insp) %in% allnames_householdlist)[-c(1, 3)]

kn_household_net_insp <- kn_household_net_insp %>% 
  dplyr::select(-c(net_insp_common_names))



kn_household_net_insp_mod_hh <- kn_household_net_insp %>% 
  group_by(sn) %>% 
  # mutate(number_nets = max(nh108)) %>%
  mutate(number_nets = max(redcap_repeat_instance )) %>% 
  transmute(number_nets,  #redcap_repeat_instance,
            nh108a_yes = sum(ifelse(nh108a == 1, 1, 0), na.rm = T), 
            nh108a_no = sum(ifelse(nh108a == 2, 1, 0), na.rm = T), 
            number_nets = ifelse((nh108a_yes + nh108a_no)!= number_nets,
                                 (nh108a_yes + nh108a_no), number_nets ), 
            nh109_yes = sum(ifelse(nh109 == 1, 1, 0), na.rm = T), 
            nh109_no = sum(ifelse(nh109 == 2, 1, 0), na.rm = T), 
            nh110_yes = sum(ifelse(nh110 == 1, 1, 0), na.rm = T), 
            nh110_no = sum(ifelse(nh110 == 2, 1, 0), na.rm = T),
            nh111_cotton = sum(ifelse(nh111 == 1, 1, 0), na.rm = T), 
            nh111_synthetic = sum(ifelse(nh111 == 2, 1, 0), na.rm = T), 
            nh111_others= sum(ifelse(nh111 == 3, 1, 0), na.rm = T), 
            nh113_yes = sum(ifelse(nh113 == 1, 1, 0), na.rm = T), 
            nh113_no = sum(ifelse(nh113== 2, 1, 0), na.rm = T), ) %>% 
  # pivot_wider(names_from = nh108, 
  #             values_from = c(nh108a:nh113a, nh115:number_nets)) %>% 
  distinct() %>% 
  mutate(count = n())



# ib_household_net_insp_mod_ind <- ib_household_net_insp %>% 
#   # to be corrected later after adding unique ids 
#   group_by(sn) %>% 
#   mutate(unique_id = paste0(sn,"/",)) %>% 
#   pivot_wider(names_from = nh108, 
#               values_from = c(nh108a:n113, n115:number_nets))



travelers_common_names <- which(names(kn_household_travelers) %in% allnames_householdlist)[-1]

kn_household_travelers <- kn_household_travelers %>% 
  dplyr::select(-c(travelers_common_names))






visitors_common_names <- which(names(kn_household_visitors) %in% allnames_householdlist)[-1]

kn_household_visitors <- kn_household_visitors %>% 
  dplyr::select(-c(visitors_common_names))

kn_household_visitors_mod <- kn_household_visitors  %>% 
  group_by(sn) %>% 
  mutate(number_visitors = n()) %>% 
  transmute(number_visitors, 
            v1) %>% 
  distinct()



names(kn_women_survey) <- c( "sn", paste0("women_", names(kn_women_survey)[-1]))
names(kn_men_survey) <- c("sn", paste0("men_", names(kn_men_survey)[-1]))



# kn_household_common_names <- which(names(ib_household) %in% allnames_householdlist)[-1]
# 
# ib_household_mod <- ib_household %>% 
#   dplyr::select(-c(ib_household_common_names))
# 
# ib_household <- ib_household%>% 
#   dplyr::select(-c(redcap_repeat_instrument, 
#          redcap_repeat_instance, hl4, ))



long_data_together <-kano_household_list %>% #left_join(
  #, ib_household_mod, by = ("sn")) 
  dplyr::left_join(kn_men_survey, by = ("sn")) %>% 
  left_join(kn_women_survey, by = ("sn" )) %>% 
  left_join(kn_household_net_insp_mod_hh, by = ("sn")) %>% 
  left_join(kn_household_travelers, by = ("sn")) %>%  #Individual and should be put at the end
  left_join(kn_household_visitors_mod, by = ("sn"))


long_data_together <- long_data_together %>%
  dplyr::select(-c(463:485,162:184))

write_dta(long_data_together, file.path(LuDir,"long_wetseason_household_membersV2_678_records.dta"))  