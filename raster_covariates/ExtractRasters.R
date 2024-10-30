rm(list=ls())

source("loadpath.R")
install.packages("furrr")
install.packages("progressr")

library(raster)
library(sf)
library(terra)
library(furrr)
library(future)
library(progressr)
library(dplyr)

ShpfilesDir <- file.path(dhsDir,"nigeria/nigeria_ward_boundaries/Nigeria/Nigeria_Wards.shp")
rasterdir <- file.path(dhsDir, "nigeria/Raster_files/NGA_population_v2_1_agesex")

nigeria_shp<- st_read(ShpfilesDir)
nigeria_shp <- nigeria_shp[!st_is_empty(nigeria_shp), ]
nigeria_shp <- as(nigeria_shp, "Spatial")

if (!inherits(nigeria_shp, "Spatial")) {
  nigeria_shp <- as(nigeria_shp, "Spatial")
}


###########################################Creating regional shapefile

northwest <- c("Kano", "Kaduna", "Katsina", "Zamfara", "Sokoto", "Kebbi", "Jigawa")
southwest <- c("Oyo", "Osun", "Ondo", "Ekiti", "Lagos", "Ogun")
nigeria_shp <- nigeria_shp %>%
  mutate(geo_zone = ifelse(State %in% northwest,"Northwest", NA ),
         geo_zone = ifelse(State %in% southwest,"Southwest" ,geo_zone )
         )


nigeria_shp <- nigeria_shp %>%
  mutate(geometry = st_make_valid(geometry))


geo_zone_shapefile <- nigeria_shp %>%
  filter(StateCode %in% c("KN", "OY"))
  
  geo_zone_shapefile <- geo_zone_shapefile %>%
  group_by(geo_zone) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

ggplot(geo_zone_shapefile)+
  geom_sf()

# Save the resulting geopolitical zone shapefile
st_write(geo_zone_shapefile, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/geopolitical_zone_shapefile.shp"))
##################################################

###filtering by Kano and Ibadan Only
geo_zone_shapefile <- nigeria_shp %>%
  filter(StateCode %in% c("KN", "OY"))


start_time = Sys.time()

popcount_rasteru1 <- file.path(rasterdir,"Children_U15", "NGA_population_v2_1_agesex_under1.tif")
popcount_datau1 <- raster(popcount_rasteru1)
popcount_valuesu1 <- raster::extract(popcount_datau1, geo_zone_shapefile,
                                   buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfu1 <- as.data.frame(popcount_valuesu1)

end_time = Sys.time()

run_time = end_time - start_time 


popcount_rasteru5 <- file.path(rasterdir,"Children_U15", "NGA_population_v2_1_agesex_under5.tif")

popcount_datau5 <- raster(popcount_rasteru5)
popcount_valuesu5 <- raster::extract(popcount_datau5, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfu5 <- as.data.frame(popcount_valuesu5)



popcount_raster <- file.path(rasterdir,"Children_U15", "NGA_population_v2_1_agesex_under15.tif")

popcount_data <- raster(popcount_raster)
popcount_values <- raster::extract(popcount_data, geo_zone_shapefile,
                                   buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfu15 <- as.data.frame(popcount_values)





#Male

popcount_rasterm0 <- file.path(rasterdir,"Male", "NGA_population_v2_1_agesex_m0.tif")

popcount_datam0 <- raster(popcount_rasterm0)
popcount_valuesm0 <- raster::extract(popcount_datam0, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm0 <- as.data.frame(popcount_valuesm0)




popcount_rasterm1 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m1.tif")

popcount_datam1 <- raster(popcount_rasterm1)
popcount_valuesm1 <- raster::extract(popcount_datam1, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm1 <- as.data.frame(popcount_valuesm1)



popcount_rasterm5 <- file.path(rasterdir,"Male", "NGA_population_v2_1_agesex_m5.tif")

popcount_datam5 <- raster(popcount_rasterm5)
print(popcount_rasterm5)
popcount_valuesm5 <- raster::extract(popcount_datam5, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm5 <- as.data.frame(popcount_valuesm5)



popcount_rasterm10 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m10.tif")

popcount_datam10 <- raster(popcount_rasterm10)
popcount_valuesm10 <- raster::extract(popcount_datam10, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm10 <- as.data.frame(popcount_valuesm10)





popcount_rasterm15 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m15.tif")

popcount_datam15 <- raster(popcount_rasterm15)
popcount_valuesm15 <- raster::extract(popcount_datam15, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm15 <- as.data.frame(popcount_valuesm15)




popcount_rasterm20 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m20.tif")

popcount_datam20 <- raster(popcount_rasterm20)
popcount_valuesm20 <- raster::extract(popcount_datam20, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm20 <- as.data.frame(popcount_valuesm20)





popcount_rasterm25 <- file.path(rasterdir, "Male", "NGA_population_v2_1_agesex_m25.tif")

popcount_datam25 <- raster(popcount_rasterm25)
popcount_valuesm25 <- raster::extract(popcount_datam25, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm25 <- as.data.frame(popcount_valuesm25)




popcount_rasterm30 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m30.tif")

popcount_datam30 <- raster(popcount_rasterm30)
popcount_valuesm30 <- raster::extract(popcount_datam30, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm30 <- as.data.frame(popcount_valuesm30)



popcount_rasterm35 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m35.tif")

popcount_datam35 <- raster(popcount_rasterm35)
popcount_valuesm35 <- raster::extract(popcount_datam35, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm35 <- as.data.frame(popcount_valuesm35)



popcount_rasterm40 <- file.path(rasterdir, "Male", "NGA_population_v2_1_agesex_m40.tif")

popcount_datam40 <- raster(popcount_rasterm40)
popcount_valuesm40 <- raster::extract(popcount_datam40, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm40 <- as.data.frame(popcount_valuesm40)



popcount_rasterm45 <- file.path(rasterdir, "Male", "NGA_population_v2_1_agesex_m45.tif")

popcount_datam45 <- raster(popcount_rasterm45)
popcount_valuesm45 <- raster::extract(popcount_datam45, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm45 <- as.data.frame(popcount_valuesm45)



popcount_rasterm50 <- file.path(rasterdir, "Male", "NGA_population_v2_1_agesex_m50.tif")

popcount_datam50 <- raster(popcount_rasterm50)
popcount_valuesm50 <- raster::extract(popcount_datam50, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm50 <- as.data.frame(popcount_valuesm50)



popcount_rasterm55 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m55.tif")

popcount_datam55 <- raster(popcount_rasterm55)
popcount_valuesm55 <- raster::extract(popcount_datam55, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm55 <- as.data.frame(popcount_valuesm55)



popcount_rasterm60 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m60.tif")

popcount_datam60 <- raster(popcount_rasterm60)
popcount_valuesm60 <- raster::extract(popcount_datam60, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm60 <- as.data.frame(popcount_valuesm60)



popcount_rasterm65 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m65.tif")

popcount_datam65 <- raster(popcount_rasterm65)
popcount_valuesm65 <- raster::extract(popcount_datam65, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm65 <- as.data.frame(popcount_valuesm65)




popcount_rasterm70 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m70.tif")

popcount_datam70 <- raster(popcount_rasterm70)
popcount_valuesm70 <- raster::extract(popcount_datam70, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm70 <- as.data.frame(popcount_valuesm70)



popcount_rasterm75 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m75.tif")

popcount_datam75 <- raster(popcount_rasterm75)
popcount_valuesm75 <- raster::extract(popcount_datam75, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm75 <- as.data.frame(popcount_valuesm75)



popcount_rasterm80 <- file.path(rasterdir,"Male",  "NGA_population_v2_1_agesex_m80.tif")

popcount_datam80 <- raster(popcount_rasterm80)
popcount_valuesm80 <- raster::extract(popcount_datam80, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dfm80 <- as.data.frame(popcount_valuesm80)



mergedrasters <-  popcount_values_dfu1 %>%
  left_join(popcount_values_dfu5)%>%
  left_join(popcount_values_dfu15)%>%
  left_join(popcount_values_dfm0)%>%
  left_join(popcount_values_dfm1)%>%
  left_join(popcount_values_dfm5)%>%
  left_join(popcount_values_dfm10)%>%
  left_join(popcount_values_dfm15)%>%
  left_join(popcount_values_dfm20)%>%
  left_join(popcount_values_dfm25)%>%
  left_join(popcount_values_dfm30)%>%
  left_join(popcount_values_dfm35)%>%
  left_join(popcount_values_dfm40)%>%
  left_join(popcount_values_dfm45)%>%
  left_join(popcount_values_dfm50)%>%
  left_join(popcount_values_dfm55)%>%
  left_join(popcount_values_dfm60)%>%
  left_join(popcount_values_dfm65)%>%
  left_join(popcount_values_dfm70)%>%
  left_join(popcount_values_dfm75)%>%
  left_join(popcount_values_dfm80)
  
  
write.csv(mergedrasters,file.path(rasterdir,"Male","mergedrasters_children_males.csv"))




#Female

popcount_rasterf0 <- file.path(rasterdir,"Female",  "NGA_population_v2_1_agesex_f0.tif")

popcount_dataf0 <- raster(popcount_rasterf0)
popcount_valuesf0 <- raster::extract(popcount_dataf0, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff0 <- as.data.frame(popcount_valuesf0)





popcount_rasterf1 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f1.tif")

popcount_dataf1 <- raster(popcount_rasterf1)
popcount_valuesf1 <- raster::extract(popcount_dataf1, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff1 <- as.data.frame(popcount_valuesf1)



popcount_rasterf5 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f5.tif")

popcount_dataf5 <- raster(popcount_rasterf5)
popcount_valuesf5 <- raster::extract(popcount_dataf5, geo_zone_shapefile,
                                     buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff5 <- as.data.frame(popcount_valuesf5)




popcount_rasterf10 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f10.tif")

popcount_dataf10 <- raster(popcount_rasterf10)
popcount_valuesf10 <- raster::extract(popcount_dataf10, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff10 <- as.data.frame(popcount_valuesf10)





popcount_rasterf15 <- file.path(rasterdir, "Female","NGA_population_v2_1_agesex_f15.tif")

popcount_dataf15 <- raster(popcount_rasterf15)
popcount_valuesf15 <- raster::extract(popcount_dataf15, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff15 <- as.data.frame(popcount_valuesf15)




popcount_rasterf20 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f20.tif")

popcount_dataf20 <- raster(popcount_rasterf20)
popcount_valuesf20 <- raster::extract(popcount_dataf20, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff20 <- as.data.frame(popcount_valuesf20)





popcount_rasterf25 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f25.tif")

popcount_dataf25 <- raster(popcount_rasterf25)
popcount_valuesf25 <- raster::extract(popcount_dataf25, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff25 <- as.data.frame(popcount_valuesf25)




popcount_rasterf30 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f30.tif")

popcount_dataf30 <- raster(popcount_rasterf30)
popcount_valuesf30 <- raster::extract(popcount_dataf30, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff30 <- as.data.frame(popcount_valuesf30)



popcount_rasterf35 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f35.tif")

popcount_dataf35 <- raster(popcount_rasterf35)
popcount_valuesf35 <- raster::extract(popcount_dataf35, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff35 <- as.data.frame(popcount_valuesf35)



popcount_rasterf50 <- file.path(rasterdir, "NGA_population_v2_1_agesex_f50.tif")

popcount_dataf50 <- raster(popcount_rasterf50)
popcount_valuesf50 <- raster::extract(popcount_dataf50, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff50 <- as.data.frame(popcount_valuesf50)



popcount_rasterf55 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f55.tif")

popcount_dataf55 <- raster(popcount_rasterf55)
popcount_valuesf55 <- raster::extract(popcount_dataf55, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff55 <- as.data.frame(popcount_valuesf55)



popcount_rasterf50 <- file.path(rasterdir, "Female","NGA_population_v2_1_agesex_f50.tif")

popcount_dataf50 <- raster(popcount_rasterf50)
popcount_valuesf50 <- raster::extract(popcount_dataf50, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff50 <- as.data.frame(popcount_valuesf50)



popcount_rasterf55 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f55.tif")

popcount_dataf55 <- raster(popcount_rasterf55)
popcount_valuesf55 <- raster::extract(popcount_dataf55, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff55 <- as.data.frame(popcount_valuesf55)



popcount_rasterf60 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f60.tif")

popcount_dataf60 <- raster(popcount_rasterf60)
popcount_valuesf60 <- raster::extract(popcount_dataf60, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff60 <- as.data.frame(popcount_valuesf60)



popcount_rasterf65 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f65.tif")

popcount_dataf65 <- raster(popcount_rasterf65)
popcount_valuesf65 <- raster::extract(popcount_dataf65, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff65 <- as.data.frame(popcount_valuesf65)




popcount_rasterf70 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f70.tif")

popcount_dataf70 <- raster(popcount_rasterf70)
popcount_valuesf70 <- raster::extract(popcount_dataf70, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff70 <- as.data.frame(popcount_valuesf70)



popcount_rasterf75 <- file.path(rasterdir, "Female","NGA_population_v2_1_agesex_f75.tif")
popcount_dataf75 <- raster(popcount_rasterf75)
popcount_valuesf75 <- raster::extract(popcount_dataf75, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff75 <- as.data.frame(popcount_valuesf75)


popcount_rasterf80 <- file.path(rasterdir,"Female", "NGA_population_v2_1_agesex_f80.tif")

popcount_dataf80 <- raster(popcount_rasterf80)
popcount_valuesf80 <- raster::extract(popcount_dataf80, geo_zone_shapefile,
                                      buffer=buffer, fun=mean, df=TRUE, sp=TRUE)
popcount_values_dff80 <- as.data.frame(popcount_valuesf80)



mergedrastersfemale <-  popcount_values_dfu1 %>%
  left_join(popcount_values_dfu5)%>%
  left_join(popcount_values_dfu15)%>%
  left_join(popcount_values_dff0)%>%
  left_join(popcount_values_dff1)%>%
  left_join(popcount_values_dff5)%>%
  left_join(popcount_values_dff10)%>%
  left_join(popcount_values_dff15)%>%
  left_join(popcount_values_dff20)%>%
  left_join(popcount_values_dff25)%>%
  left_join(popcount_values_dff30)%>%
  left_join(popcount_values_dff35)%>%
  left_join(popcount_values_dff50)%>%
  left_join(popcount_values_dff55)%>%
  left_join(popcount_values_dff50)%>%
  left_join(popcount_values_dff55)%>%
  left_join(popcount_values_dff60)%>%
  left_join(popcount_values_dff65)%>%
  left_join(popcount_values_dff70)%>%
  left_join(popcount_values_dff75)%>%
  left_join(popcount_values_dff80)


write.csv(mergedrastersfemale,file.path(rasterdir,"Male","mergedrasters_females.csv"))

  
