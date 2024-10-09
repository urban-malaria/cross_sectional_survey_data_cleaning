warning(-1)

library(haven);library(labelled)

LuDir <- file.path(Drive,"Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data")

# data paths 
Ibadan_household_list <- haven::read_dta("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/wetseason_household_members_with_weights.dta")
ib_household <- read_dta(file.path(LuDir, "240922_Ibadan_latest_data", "IB Wet season household data_edited_220924 rev.dta" ))
ib_household_net_insp <- read_dta(file.path(LuDir, "240922_Ibadan_latest_data", "IB Wet season hhold net inspection.dta" ))
ib_household_travelers <- read_dta(file.path(LuDir, "240922_Ibadan_latest_data", "IB Wet season hhold travelers.dta" ))
ib_household_visitors <- read_dta(file.path(LuDir, "240922_Ibadan_latest_data", "IB Wet season hhold visitors.dta" ))
ib_women_survey <- read_dta(file.path(LuDir, "240922_Ibadan_latest_data", "IB Wet season Women survey_210924 rev.dta" ))
ib_men_survey <- read_dta(file.path(LuDir, "240922_Ibadan_latest_data", "IB Wet season Men survey_rev 220924.dta" ))


#
wide_Ibadan_data <- Ibadan_household_list %>%
  select(-c(redcap_repeat_instrument, redcap_repeat_instance)) %>% 
  pivot_wider(names_from = hl1, 
              values_from = c(hl2:overall_hh_weight))


wide_ib_household_visitors <- ib_household_visitors %>%
  select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = redcap_repeat_instance, 
              values_from = c(v1:visitors_complete))

wide_ib_household_travelers <- ib_household_travelers %>%
  select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = c(redcap_repeat_instance, c1ii ), 
              values_from = c(c1iii:tfhi_complete))


wide_ib_household_net_insp <- ib_household_net_insp %>%
  select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = c(redcap_repeat_instance), 
              values_from = c(nh108:net_inspection_complete))


wide_data_together <- left_join(wide_Ibadan_data, ib_household) %>% 
  left_join(ib_men_survey, by = ("sn")) %>% 
  left_join(ib_women_survey, by = ("sn")) %>% 
  left_join(wide_ib_household_net_insp, by = ("sn")) %>% 
  left_join(wide_ib_household_travelers, by = ("sn")) %>% 
  left_join(wide_ib_household_visitors, by = ("sn"))

write_dta(wide_data_together, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/all_ibadan_wetseason_dater_wide.dta")  

##################################
#long data 
##################################
allnames_householdlist <- names(Ibadan_household_list)



# removing duplicated columns before merging 
women_common_names <- which(names(ib_women_survey) %in% allnames_householdlist)[-1]

ib_women_survey <- ib_women_survey %>% 
  dplyr::select(-c(women_common_names))



men_common_names <- which(names(ib_men_survey) %in% allnames_householdlist)[-1]

ib_men_survey <- ib_men_survey %>% 
  dplyr::select(-c(men_common_names))


net_insp_common_names <- which(names(ib_household_net_insp) %in% allnames_householdlist)[-1]

ib_household_net_insp <- ib_household_net_insp %>% 
  dplyr::select(-c(net_insp_common_names))



ib_household_net_insp_mod_hh <- ib_household_net_insp %>% 
  group_by(sn) %>% 
  mutate(number_nets = max(nh108)) %>% 
  pivot_wider(names_from = nh108, 
              values_from = c(nh108a:n113a, n115:number_nets))



ib_household_net_insp_mod_ind <- ib_household_net_insp %>% 
  # to be corrected later 
  group_by(sn) %>% 
  mutate(unique_id = paste0(sn,"/",)) %>% 
  pivot_wider(names_from = nh108, 
              values_from = c(nh108a:n113, n115:number_nets))
  


travelers_common_names <- which(names(ib_household_travelers) %in% allnames_householdlist)[-1]

ib_household_travelers <- ib_household_travelers %>% 
  dplyr::select(-c(travelers_common_names))


visitors_common_names <- which(names(ib_household_visitors) %in% allnames_householdlist)[-1]

ib_household_visitors <- ib_household_visitors %>% 
  dplyr::select(-c(visitors_common_names))



names(ib_women_survey) <- c( "sn", paste0("women_", names(ib_women_survey)[-1]))
names(ib_men_survey) <- c("sn", paste0("men_", names(ib_men_survey)[-1]))



long_data_together <- left_join(Ibadan_household_list, ib_household, by = ("sn")) %>% 
  dplyr::left_join(ib_men_survey, by = ("sn")) %>% 
  left_join(ib_women_survey, by = ("sn" )) %>% 
  left_join(ib_household_net_insp, by = ("sn")) %>% 
  left_join(ib_household_travelers, by = ("sn")) %>% 
  left_join(ib_household_visitors, by = ("sn"))


  



