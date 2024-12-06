rm(list=ls())

metropolis_name <- "Kano"

source("load_paths.R")

#hf_df_allw <- read.csv(file.path(dhsDir, "nigeria", "kano_ibadan_epi", "Field data/HF_data/HFS_data_final_wetseason.csv"))

hf_df_allw <- read.csv(file.path(dhsDir, "nigeria", "kano_ibadan_epi", "Field data/HF_data/HF_data_merged_kn_ib_dryseason.csv"))
  
nigeria_data <-  file.path(DriveDir,"data","nigeria")


df_ko = sf::st_read(file.path(nigeria_data, "kano_ibadan/kano_ibadan_shape_files", "Kano_metro_ward_sixLGAs", "Kano_metro_ward_sixLGAs.shp"))

df_ib <- sf::st_read(file.path(nigeria_data, "kano_ibadan/kano_ibadan_shape_files", "Ibadan_metro_ward_fiveLGAs/Ibadan_metro_fiveLGAs.shp"))


##Break data into Kano and Ibadan for Plotting of Location
hf_df_allkn <- hf_df_allw %>%
  dplyr::filter(State == "Kano")



hf_df_allib <- hf_df_allw %>%
  dplyr::filter(State == "Ibadan (Oyo)")

##Kano## Summarize Respondents by Ward
hf_df_allkn_s <- hf_df_allkn %>%
  group_by(Ward) %>%
  summarise(count = n())


## read kano ward shape files

 ggplot(df_ko) +
  geom_sf(fill = "khaki") +
  ggrepel::geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+
  labs(title= "Wards in Kano ")+
  coord_sf()


##Make dataframe with coordinates
hf_kn_dff <- left_join(df_ko, hf_df_allkn_s, by= c ("WardName" = "Ward"))

hf_kn_dff$count <- as.numeric(hf_kn_dff$count)

hf_kn_dffn <- hf_kn_dff %>%
  mutate(count = if_else(WardName == "Zango", 735, count)) %>%
  mutate(count = if_else(WardName == "Dorayi", 1114, count)) %>%
  mutate(count = if_else(WardName == "Fagge D2", 750, count)) %>%
  mutate(count = if_else(WardName == "Gobirawa", 1374, count)) %>%
  mutate(count = if_else(WardName == "Giginyu", 670, count), 
         count_classes = cut(count, c(0, 100, 400, 800, 1200, 1400), include.lowest = T)) 


colors = brewer.pal(5, name = "YlOrRd")
##Plot distribution of Respondents
ggplot(hf_kn_dffn) +
  geom_sf(aes(fill = count_classes)) +
  scale_fill_manual(name = "", values = c("[0,100]" = colors[1],
                                           "(100,400]" =  colors[2], 
                                           "(400,800]" =  colors[3], 
                                           "(800,1.2e+03]"=  colors[4], 
                                           "(1.2e+03,1.4e+03])" = colors[5]),  na.value = "white")+
  ggrepel::geom_text_repel(data = hf_kn_dffn, aes(label =  paste(WardName, "(", count, ")"), 
                                                  geometry = geometry),
                           color ='black', stat = "sf_coordinates", 
                           min.segment.length = 0, size = 4, force = 1)+
  map_theme()+
  labs(title= "", x = "", y = "")+
  coord_sf()


##Ibadan
## Summarize Respondents by Ward

hf_df_allib_s <- hf_df_allib %>%
  group_by(Ward) %>%
  summarise(count = n())

## read kano ward shape files

 ggplot(df_ib) +
  geom_sf(fill = "khaki") +
  ggrepel::geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+
  labs(title= "Wards in Ibadan")+
  coord_sf()
 
 
##Make dataframe with coordinates
 
hf_ib_dff <- left_join(df_ib, hf_df_allib_s, by= c ("WardName" = "Ward"))

hf_ib_dff$count <- as.numeric(hf_ib_dff$count)

hf_ib_dffn <- hf_ib_dff %>%
  mutate(count = if_else(WardName == "Agugu", 1245, count)) %>%
  mutate(count = if_else(WardName == "Bashorun", 650, count)) %>%
  mutate(count = if_else(WardName == "Basorun", 34, count)) %>%
  mutate(count = if_else(WardName == "Challenge", 750, count)) %>%
  mutate(count = if_else(WardName == "Olopomewa", 574, count), 
         count_classes = cut(count, c(0, 100, 400, 800, 1200, 1400), include.lowest = T))



##Create Categorical variable for count
break_pts <- c(0, 10, 20, 30, 50, 100, 1500)
labels <- c("Less than 10", "10-20", "21-30", "31-50", "51-100", "100 and above")
count_cat <- cut(hf_ib_dffn$count, breaks = break_pts, labels = labels, na.rm = TRUE)
count_cat <- data.frame(count_cat)
hf_ib_dffnc <- cbind(hf_ib_dffn, count_cat)

##Plot distribution of Respondents


ggplot(hf_ib_dffnc) +
  geom_sf(aes(fill = count_classes)) +
  scale_fill_manual(name = "", values = c("[0,100]" = colors[1],
                                          "(100,400]" =  colors[2], 
                                          "(400,800]" =  colors[3], 
                                          "(800,1.2e+03]"=  colors[4], 
                                          "(1.2e+03,1.4e+03]" = colors[5]),  na.value = "white")+
  ggrepel::geom_text_repel(data = hf_ib_dffnc, aes(label =  paste(WardName, "(", count, ")"), 
                                                  geometry = geometry),
                           color ='black', stat = "sf_coordinates", 
                           min.segment.length = 0, size = 4, force = 1)+
  map_theme()+
  labs(title= "", x = "", y = "")+
  coord_sf()


# Naive malaria prevalence 


malaria_test <- hf_df_allw %>% 
  dplyr::select(State, Ward, result = `q503..RESULT`) %>% 
  group_by(State, Ward) %>% 
  summarise(total = n(), 
            postive = sum(ifelse(result == "POSITIVE", 1, 0)), 
            negative = total - postive, 
            proportion = postive/total)



Ibadan_data <- df_ib %>% 
  left_join(malaria_test %>% 
               dplyr::filter(State == "Ibadan (Oyo)"),
             by= c ("WardName" = "Ward" ))


centroids <- sf::st_centroid(Ibadan_data) %>% 
  mutate(proportion_class = cut(proportion, c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 1), include.lowest = T))
  

colors = brewer.pal(6, name = "YlOrRd")


ggplot(Ibadan_data) +
  geom_sf(fill = "white") +
  geom_sf(data = centroids, aes(color = proportion_class, size = proportion_class)) +
  scale_color_manual(name = "", values = c("[0,0.02]" = colors[2],
                                          "(0.02,0.04]" =  colors[3], 
                                          "(0.04,0.06]" =  colors[4], 
                                          "(0.06,0.08]"=  colors[5], 
                                          "(0.08,0.1]" = colors[6], 
                                          "(0.1,1]" = colors[6]),
                                     na.value = "white") +
  ggrepel::geom_text_repel(data = centroids, aes(label =  round(proportion, 3),
                                                   geometry = geometry),
                           color ='black', stat = "sf_coordinates",
                           min.segment.length = 0, size = 4, force = 1)+
  map_theme()+
  labs(title= "", x = "", y = "")+
  coord_sf()

ggsave(file.path(results, "Ibadan", "Ibadan_hf_tpr.pdf"), 
       dpi = 400, width = 15,
       height = 10,)


# Naive malaria prevalence 
Kano_data <- df_ko %>% 
  left_join(malaria_test %>% 
              dplyr::filter(State == "Kano"),
            by= c ("WardName" = "Ward" ))


centroids_kano <- sf::st_centroid(Kano_data) %>% 
  mutate(proportion_class = cut(proportion, 
                                c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 1), 
                                include.lowest = T))


colors = brewer.pal(6, name = "YlOrRd")


ggplot(Kano_data) +
  geom_sf(fill = "white") +
  geom_sf(data = centroids_kano, aes(color = proportion_class, size = proportion_class)) +
  scale_color_manual(name = "", values = c("[0,0.02]" = colors[2],
                                           "(0.02,0.04]" =  colors[3], 
                                           "(0.04,0.06]" =  colors[4], 
                                           "(0.06,0.08]"=  colors[5], 
                                           "(0.08,0.1]" = colors[6], 
                                           "(0.1,1]" = colors[6]),
                     na.value = "white")  +
  ggrepel::geom_text_repel(data = centroids_kano, aes(label =  round(proportion, 3),
                                                 geometry = geometry),
                           color ='black', stat = "sf_coordinates",
                           min.segment.length = 0, size = 4, force = 1)+
  map_theme()+
  labs(title= "", x = "", y = "")+
  coord_sf()


ggsave(file.path(results, "Kano", "Kano_hf_tpr.pdf"), 
       dpi = 400, width = 15,
       height = 10,)


# test for the spatial clusters with the Kano 

hist(Kano_data$proportion)
hist(Ibadan_data$proportion)



centroids_kano <- centroids_kano %>% 
  tidyr::drop_na()


centroids_kano$log_odds <- car::logit(centroids_kano$proportion)
hist(centroids_kano$log_odds, xlab = "Log odds", main = "")

centroids_kano$lon = sf::st_coordinates(centroids_kano)[,1]
centroids_kano$lat = sf::st_coordinates(centroids_kano)[,2]


kan_dists <- as.matrix(dist(cbind(centroids_kano$long, centroids_kano$lat)))


kano_dists_inv <- 1/kan_dists
diag(kano_dists_inv) <- 0 

ape::Moran.I(centroids_kano$log_odds, kano_dists_inv)



# "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins

plot(pgi.cor) 

# statistically significant values (p<0.05) are plotted in red



centroids <- centroids%>% 
  tidyr::drop_na()


centroids$log_odds <- car::logit(centroids$proportion)
hist(Ibadan_data$log_odds, xlab = "Log odds", main = "")



centroids$lon = sf::st_coordinates(centroids)[,1]
centroids$lat = sf::st_coordinates(centroids)[,2]


ibadan_dists <- as.matrix(dist(cbind(centroids$long, centroids$lat)))

ibadan_dists_inv <- 1/ibadan_dists
diag(ibadan_dists_inv) <- 0 

ape::Moran.I(centroids$log_odds, ibadan_dists_inv)


#Option 2 

maxDist<-max(dist(cbind(centroids_kano$lon, centroids_kano$lat)))
maxDist



xy = cbind(centroids_kano$lon, centroids_kano$lat)

pgi.cor <- pgirmess::correlog(coords=xy, z=centroids_kano$log_odds, method="Moran",
                              nbclass = length(custom_breaks) - 1, breaks = custom_breaks)   
# "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins

plot(pgi.cor) 

