


zone2020 <- st_read("W:/value_soil_testing_prj/Yield_data/2020/All_Zones_2020_wgs84.shp")


zone2020_df <- data.frame(zone2020)
zone2020_1 <- zone2020_df %>% dplyr::select(-geometry)


## some paddocks are excluded from the analysis
zone2020_df <- zone2020_1 %>% 
  filter(Status != "Excluded from Analysis")
#just keep a few clms
names(zone2020_df)
zone2020_df <- zone2020_df %>% 
  dplyr::select(Zone_ID,
                Strip_Type,
                Status,
                organisati,
                contact, 
                farmer,
                paddock)
rm(zone2020, zone2020_1)


zone2020_unique <- zone2020_df %>% 
  distinct(Zone_ID, Strip_Type, .keep_all = TRUE)

#####################################################################################
#how many zones with N / P strips?
zone2020_count <- zone2020_unique %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>%
  group_by(Strip_Type) %>% 
  summarise(count_zones_by_type = n())


zone2020_count

#####################################################################################
#how many paddocks with N / P strips?

names(zone2020_unique)
#I cant just select paddock names beacuse some have the same name house and creek
paddock_farmer
zone_paddocks <- zone2020_unique %>%
  dplyr::mutate(paddock_farmer = paste0(paddock, "_", farmer)) %>% 
  dplyr::distinct(paddock_farmer, Strip_Type, .keep_all = TRUE)
  
zone_paddocks 
 
zone_paddocks_type %>% 
  group_by(Strip_Type) %>% 
  summarise(count_paddocks_by_type = n())




##########################################################################################
## how many growers?

names(zone2020_unique)
farmers_dist <- zone2020_unique %>% 
  dplyr::distinct(farmer,Strip_Type, .keep_all = TRUE)

farmers_count_type <- farmers_dist %>% 
    group_by(Strip_Type) %>% 
  summarise(count_farmers_by_type = n())

farmers_count <- farmers_dist %>% 
    summarise(count_farmers_by_type = n())

farmers_count



############################################################################################
#### 2019 data ####
############################################################################################



zone2019 <- st_read("W:/value_soil_testing_prj/Yield_data/finished/GIS_Results/All_Zones_2019_wgs84.shp")


zone2019_df <- data.frame(zone2019)
zone2019_1 <- zone2019_df %>% dplyr::select(-geometry)


## some paddocks are excluded from the analysis

#just keep a few clms
names(zone2019_1)
zone2019_df <- zone2019_1 %>% 
  dplyr::select(Zone_ID,
                Strip_Type, # I need to add this in ??
                #Status,
                organisati,
                contact, 
                farmer,
                paddock)
rm(zone2019, zone2019_1)


zone2019_df_unique <- zone2019_df %>% 
  distinct(Zone_ID, Strip_Type, .keep_all = TRUE)

#how many zones with N / P strips?
zone2019_count <- zone2019_df_unique %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>%
  group_by(Strip_Type) %>% 
  summarise(count_zones_by_type = n())

zone2019_count

#####################################################################################
#how many paddocks with N / P strips?

names(zone2019_df_unique)
#I cant just select paddock names beacuse some have the same name house and creek

zone_paddocks2019 <- zone2019_df_unique %>%
  dplyr::mutate(paddock_farmer = paste0(paddock, "_", farmer)) %>% 
  dplyr::distinct(paddock_farmer, Strip_Type, .keep_all = TRUE)

 zone_paddocks2019 %>% 
  group_by(Strip_Type) %>% 
  summarise(count_paddocks_by_type = n())


## how many growers?

names(zone2019_df_unique)
farmers_dist2019 <- zone2019_df_unique %>% 
  dplyr::distinct(farmer,Strip_Type, .keep_all = TRUE)

farmers_count_type2019 <- farmers_dist2019 %>% 
  group_by(Strip_Type) %>% 
  summarise(count_farmers_by_type = n())

farmers_count2019 <- farmers_dist2019 %>% 
  summarise(count_farmers_by_type = n())

farmers_count_type2019
farmers_count2019
