## Get the GRDC zones into the df.



#######################################################################################

###Using ESRI

zone2020_GRDC_bound <- st_read("W:/value_soil_testing_prj/Yield_data/2020/2020_GRDC_zones_test.shp")
zone2019_GRDC_bound <- st_read("W:/value_soil_testing_prj/Yield_data/finished/GIS_Results/All_Zones_2019_GRDC_wgs84.shp")

names(zone2020_GRDC_bound)
names(zone2019_GRDC_bound)

zone2020_GRDC_bound <- zone2020_GRDC_bound %>% 
  dplyr::select(Zone_ID,
                organisati,
                contact,
                farmer,
                paddock,
                Paddock_ID,
                Strip_Type,
                AGROECOLOG)
                
zone2019_GRDC_bound <- zone2019_GRDC_bound %>% 
  dplyr::select(Zone_ID,
                organisati,
                contact,
                farmer,
                paddock,
                AGROECOLOG)
zone2019_GRDC_bound <- as.data.frame(zone2019_GRDC_bound) %>% 
  dplyr::select(- geometry)
zone2020_GRDC_bound <- as.data.frame(zone2020_GRDC_bound) %>% 
  dplyr::select(- geometry)


#### having trouble with this for some reason and under time pressure!

zone2020 <- st_read("W:/value_soil_testing_prj/Yield_data/2020/All_Zones_2020_wgs84.shp")
GRDC_bound <- st_read("W:/Weeds/NVT_Data/GRDC_AgroEcological_zones_boundaries_06_region.shp") 
#plot(GRDC_bound)
#plot(zone2020)

zone2020 <- zone2020 %>%
  dplyr::select(
    Zone_ID ,
    organisati,
    contact,
    farmer,     
    paddock,
    Status,
    Strip_Type,
    geometry 
  )

str(zone2020) #Classes ‘sf’ and 'data.frame'
str(GRDC_bound) #Classes ‘sf’ and 'data.frame'
st_geometry(zone2020 )
st_geometry(GRDC_bound )

st_crs(zone2020) #4326
st_crs(GRDC_bound) #4283

zone2020 <- st_transform(zone2020 , crs = 4283 )

ggplot()+
  geom_sf(data = zone2020, color = "red" , size = 2)+
  geom_sf(data = GRDC_bound, color = "black", fill = NA)

zone2020_GRDC = st_intersection(zone2020, GRDC_bound)
zone2020_GRDC = st_intersection(GRDC_bound, zone2020)
