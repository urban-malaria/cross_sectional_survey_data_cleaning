rm(list=ls())


metropolis_name <- "Ibadan"

source("~/urban_malaria/cross_sectional_survey_data_cleaning/load_paths.R")

wash_performance_index <- read.csv( file.path(cleaned_data, metropolis_name, "household_wash_index.csv")) %>% 
  dplyr::select(unique_id, wash_index = pca)%>%
  mutate(wash_index_class = ifelse(wash_index < 0.2539444, "inadequate", "adequate")) %>% 
  dplyr::distinct(unique_id, .keep_all = TRUE) 



wealthindex <- read.csv( file.path(cleaned_data, metropolis_name, "household_wealth_index.csv")) %>% 
  dplyr::select(unique_id, wealth_index = pca) %>%
  mutate(wealth_index_class = ifelse(wealth_index <= -1.7, "poorest", 
                                   ifelse(wealth_index <= -1.3754918 , "second", 
                                          ifelse(wealth_index <= -0.4837534, "middle", 
                                                 ifelse(wealth_index < 1.0234251, "fourth", 
                                                        ifelse(wealth_index > 1.0234251, "wealthiest", NA)))))) %>% 
  dplyr::distinct(unique_id, .keep_all = TRUE)%>%
  dplyr::inner_join(wash_performance_index)

malaria_cases <- read.csv(file.path(cleaned_data_path, metropolis_name,"ibadan_enviromental_covariates_coded.csv")) %>% 
  inner_join(wealthindex, by = c("unique_id"))


coordinates <- malaria_cases[, c("longitude", "latitude")]

malaria_cases$location <- paste(malaria_cases$latitude, malaria_cases$longitude, sep = "-")

malaria_cases_new <- malaria_cases %>%
  dplyr::select(settlement_type_new,
         agebin,
         gender,
         itn_presence,
         net_use_frequencey,
         wealth_index_class, 
         wash_index_class,
         overall_hh_weight,
         test_result = rdt_test_result)

# predictor_vars <- setdiff(names(malaria_cases_new), "test_result")



malaria_cases_new <- split(malaria_cases_new,
                           malaria_cases_new$settlement_type_new)




columns <- c(2:8)


#######################################################################################
# univariate logistic regression model
#######################################################################################
model_results <- list()
ward_models <- list()


for (index in seq_along(malaria_cases_new)){
  
  model_results <- list()
  
  for(vari in columns){
    

    
    prevalence_data  = malaria_cases_new[[index]]
    variable  = names(prevalence_data)[vari]
    
    prevalence_data$agebin <- factor(prevalence_data$agebin,
                                       levels = c("[0,5]", "(5,10]", "(10,17]",
                                                  "(17,30]", "(30,122]"))
    
    prevalence_data$wealth_index_class <- factor(prevalence_data$wealth_index_class,
                                     levels = c("poorest", "second", "middle",
                                                "fourth", "wealthiest"))
    
    prevalence_data$wash_index_class <- factor(prevalence_data$wash_index_class,
                                           levels = c("adequate", "inadequate"))
    
    formula <- as.formula(paste("test_result ~", variable))
    
    model <- glm(formula, family = binomial(link = "logit"),
                 data = prevalence_data)
    
    model_results[[vari]] <- broom::tidy(model) %>% 
      mutate(ward = names(malaria_cases_new)[index],
             oddsratio = round(exp(estimate), 3),
             ci_low = round(exp(estimate - (1.96 * std.error)), 3),
             ci_high = round(exp(estimate + (1.96 * std.error)), 3))
    
    
  }
  
  ward_models[[index]] <- data.table::rbindlist(model_results[!sapply(model_results, is.null)])
  
}




unlisted_files <- data.table::rbindlist(ward_models) %>%
  filter(term != "(Intercept)",
         term != "overall_hh_weight")
# 
# oldlabels <- unique(unlisted_files$term)
# 
# newlabels <- c( "agebin", "gender (male)" ,"itn presence", "elevation", "population density",  "average EVI 2023", 
#                 "wealth index", "wash performance index") 



unlisted_files <- unlisted_files %>% 
  mutate(covariate = case_when(
    term  == "age" ~ "age",
    term  == "agebin(17,30]" ~ "age group (18-30)", 
    term  == "agebin[0,5]" ~ "age group (0-5)",
    term  == "agebin(5,10]" ~ "age group (6-10)",
    term  == "agebin(10,17]" ~ "age group (11-17)",
    term  == "agebin(30,122]" ~ "age group (30+)",
    term  == "genderMale"     ~ "gender (male)",
    term == "wash_index_classinadequate" ~ "wash index (inadequate)",
    term == "wealth_index_classmiddle" ~ "WI (middle)",
    term == "wealth_index_classfourth" ~ "WI (fourth)",
    term == "wealth_index_classsecond" ~ "WI (second)",
    term == "wealth_index_classwealthiest" ~ "WI (wealthiest)",
    term  == "itn_presence"   ~ "itn presence",
    term  == "net_use_frequencey" ~ "itn usage",
    term  == "elevation" ~"elevation" ,
    term  == "popcount_100m" ~"population density",
    term  == "avgEVI_2023" ~"average EVI 2023 (July - September)",
    term  == "mother_present" ~ "mother present",
    term  == "wealth_index" ~ "wealth index",
    term  == "wash_index" ~ "wash index",
    #Covariate == "road_type" ~ "road type",
    TRUE                     ~ NA_character_  # Default case
  )) %>% 
  drop_na()


ggplot(unlisted_files, aes(x = oddsratio, y = covariate, color = ward)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =.2) +
  geom_point(size = 3.5) +
  #scale_y_discrete()
  facet_wrap(~ward)+ labs(colour  = "")+
 # scale_color_manual(values = c("Formal" = "#c8005f", "Informal" = "#ff4770","Slum" = "#86001e"))+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  theme_bw(base_size = 20, base_family = "") + xlab("odds ratio")+
  theme(panel.grid.minor = element_blank()) 

ggsave(file.path(results, metropolis_name, "Univariate_logistic_regression.pdf"),
       dpi = 400, width = 15,
       height = 10)


ggsave(file.path(results, metropolis_name, "Univariate_logistic_regression.png"),
       dpi = 400, width = 20,
       height = 10,)


######################################################################################
# multivariate logistic regression analysis 
#######################################################################################


malaria_cases_new <- malaria_cases %>%
  dplyr::select(settlement_type_new,
                age,
                agebin,
                gender,
                itn_presence,
                net_use_frequencey,
                overgrown_vegetation,
                clogged_open_drainage,
                garden_farm_incompound, 
                overall_hh_weight,
                wealth_index_class,
                wash_index_class,
                test_result = rdt_test_result)




covariates <- c("agebin", "gender", #"itn_presence", 
                "net_use_frequencey",
                #"elevation", "popcount_100m", "avgEVI_2023",
                "wealth_index_class", "wash_index_class"
               # , #"clogged_open_drainage", 
                #"garden_farm_incompound"
                )


final_model <- data.frame()


for(settlement in unique(malaria_cases_new$settlement_type)) {

  subset_data <- na.omit(malaria_cases_new[malaria_cases_new$settlement_type_new == settlement, ])
  
  subset_data$agebin <- factor(subset_data$agebin,
                                   levels = c("[0,5]", "(5,10]", "(10,17]",
                                              "(17,30]", "(30,122]"))

  for (var in covariates) {
    if (is.factor(subset_data[[var]]) && nlevels(subset_data[[var]]) < 2) {
      subset_data[[var]] <- NULL
      warning(paste("Dropped variable", var, "for settlement", settlement,
                    "because it has less than two levels."))
    }
  }
  
  

  if (ncol(subset_data) > 1) { 
    
    full_model <- glm(test_result ~ ., data = subset_data[,-1], family = "binomial")
    
    
    stepwise_model <- stepAIC(full_model, direction = "both", trace = T)
    
    summary_model <- summary(stepwise_model)
    
    
    significant_covariates <- summary_model$coefficients #[which(summary_model$coefficients[, "Pr(>|z|)"] < 0.05), ]
    
    
    significant_df <- data.frame(
      settlement_type = settlement,
      covariate = rownames(significant_covariates),
      estimate = significant_covariates[, "Estimate"],
      std_error = significant_covariates[, "Std. Error"],
      statistic = significant_covariates[, "z value"],
      p_value = significant_covariates[, "Pr(>|z|)"]
    )
    
    
  } else {
    warning(paste("Not enough covariates for settlement", settlement, "to run a model."))
  }


final_model <- rbind(final_model, significant_df)
}



ward_multivariate_models <- data.frame(final_model) %>% 
  transmute(settlement_tpye = settlement_type,
            Covariate = covariate, PValue = p_value,
            oddsratio = round(exp(as.numeric(estimate)), 3),
            ci_low = round(exp(as.numeric(estimate) - 1.96 *  as.numeric(std_error)), 3),
            ci_high = round(exp(as.numeric(estimate) + 1.96 *  as.numeric(std_error)), 3))



ward_multivariate_models <- ward_multivariate_models %>% 
  mutate(covariates = case_when(
    Covariate == "age" ~ "age",
    Covariate == "agebin(17,30]" ~ "age group (18-30)", 
    Covariate == "agebin[0,5]" ~ "age group (0-5)",
    Covariate == "agebin(5,10]" ~ "age group (6-10)",
    Covariate == "agebin(10,17]" ~ "age group (11-17)",
    Covariate == "agebin(30,122]" ~ "age group (30+)",
    Covariate == "genderMale"     ~ "gender (male)",
    Covariate == "wash_index_classadequate" ~ "wash index (adequate)",
    Covariate == "wealth_index_classmiddle" ~ "WI (middle)",
    Covariate == "wealth_index_classfourth" ~ "WI (fourth)",
    Covariate == "wealth_index_classsecond" ~ "WI (second)",
    Covariate == "wealth_index_classwealthiest" ~ "WI (wealthiest)",
    Covariate == "itn_presence"   ~ "itn presence",
    # Covariate == "itn_usage"   ~ "itn usage",
    Covariate == "net_use_frequencey" ~ "itn usage",
    Covariate == "elevation" ~"elevation" ,
    Covariate == "popcount_100m" ~"population density",
    Covariate == "avgEVI_2023" ~"average EVI 2023 (July - September)",
    Covariate == "mother_present" ~ "mother present",
    Covariate == "wealth_index" ~ "wealth index",
    Covariate == "wash_index" ~ "wash index",
    #Covariate == "road_type" ~ "road type",
    TRUE                     ~ NA_character_  # Default case
  )) %>% 
  drop_na()




ggplot(ward_multivariate_models, aes(x = oddsratio, y = covariates, colour = settlement_tpye)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =.2) +
  geom_point(size = 3.5) +
  facet_wrap(~settlement_tpye)+ xlab("odds ratio")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  theme_bw(base_size = 20, base_family = "") +labs(color ="")+
  theme(panel.grid.minor = element_blank(), ) 


ggsave(file.path(results, metropolis_name, "multivariate_logistic_regression.pdf"),
       dpi = 400, width = 15,
       height = 10)

ggsave(file.path(results, metropolis_name, "multivariate_logistic_regression.png"),
       dpi = 400, width = 12,
       height = 10)




 # multivariate_models <- list()
# ward_multivariate_models<- list()
# 
# 
# wards <- names(malaria_cases_new)
# 
# for (index in seq_along(malaria_cases_new)){
#   
#   
#   prevalence_data  = malaria_cases_new[[index]]
#   
#   prevalence_data$agebin <- factor(prevalence_data$agebin, 
#                                      levels = c("[0,5]", "(5,10]", "(10,17]", 
#                                                 "(17,30]", "(30,122]"))
#   
#   formula <- as.formula(paste("test_result ~", multivariable))
#   
#   model <- glm(formula, family = binomial(link = "logit"), data = prevalence_data)
#   
#   multivariate_models[[index]] <- model
#   
#   ward_multivariate_models[[index]] <- broom::tidy(model) %>% 
#     mutate(ward = wards[index],
#            oddsratio = round(exp(estimate), 3),
#            ci_low = round(exp(estimate - 1.96 * std.error), 3),
#            ci_high = round(exp(estimate + 1.96 * std.error), 3))
# }
# 
# 
# multivariate_unlisted_files <- data.table::rbindlist(ward_multivariate_models) 
# 
# 
# 
# 
# 
# # %>% 
# #   filter(term != "(Intercept)")


ggplot(multivariate_unlisted_files, aes(x = oddsratio, y = term)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =
                   .2, color = "brown") +
  geom_point(size = 3.5, color = "brown") +
  facet_wrap(~ward)+
  theme_bw(base_size = 20, base_family = "") +
  theme_bw(base_size = 20, base_family = "") +
  theme(panel.grid.minor = element_blank()) 


ggsave(file.path(results, metropolis_name, "multivariate_logistic_regression.pdf"),
       dpi = 300, width = 15,
       height = 10,)
