
library(haven)

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






  



