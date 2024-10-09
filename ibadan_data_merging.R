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


net_insp_common_names <- which(names(ib_household_net_insp) %in% allnames_householdlist)[-c(1, 3)]

ib_household_net_insp <- ib_household_net_insp %>% 
  dplyr::select(-c(net_insp_common_names))



ib_household_net_insp_mod_hh <- ib_household_net_insp %>% 
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
  


travelers_common_names <- which(names(ib_household_travelers) %in% allnames_householdlist)[-1]

ib_household_travelers <- ib_household_travelers %>% 
  dplyr::select(-c(travelers_common_names))


  
  


visitors_common_names <- which(names(ib_household_visitors) %in% allnames_householdlist)[-1]

ib_household_visitors <- ib_household_visitors %>% 
  dplyr::select(-c(visitors_common_names))

ib_household_visitors_mod <- ib_household_visitors  %>% 
  group_by(sn) %>% 
  mutate(number_visitors = n()) %>% 
  transmute(number_visitors, 
            v1) %>% 
  distinct()



names(ib_women_survey) <- c( "sn", paste0("women_", names(ib_women_survey)[-1]))
names(ib_men_survey) <- c("sn", paste0("men_", names(ib_men_survey)[-1]))



long_data_together <- left_join(Ibadan_household_list, ib_household, by = ("sn")) %>% 
  dplyr::left_join(ib_men_survey, by = ("sn")) %>% 
  left_join(ib_women_survey, by = ("sn" )) %>% 
  left_join(ib_household_net_insp_mod_hh, by = ("sn")) %>% 
  #left_join(ib_household_travelers, by = ("sn")) %>%  Individual and should be put at the end
  left_join(ib_household_visitors_mod, by = ("sn"))


  



