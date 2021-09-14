# ## How do the recommended rates 2020 -2021  season compare in the multiple zones in the paddock
# 
# I have used Sean recommended rates but I need to change this beacuse the rainfall class dont match the ecomonic work.
# 
#### SECTION for R markdown ####

# ```{r stuff around getting recom rates just for our 2020 paddocks, message=TRUE, warning=FALSE, include=FALSE}

#########################################################################################################################################
## step 1 make a list of zone and a list of paddocks that will be included in the analysis 
#1b. zone codes
##########################################################################################################################################


zone2019 <- st_read("W:/value_soil_testing_prj/Yield_data/finished/GIS_Results/All_Zones_2019_wgs84.shp")


zone2019_df <- data.frame(zone2019)
zone2019_1 <- zone2019_df %>% dplyr::select(-geometry)


## some paddocks are excluded from the analysis

#just keep a few clms

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



str(zone2019_df_unique)
zone2019_df_unique$length_zoneID <- nchar(zone2019_df_unique$Zone_ID)
zone2019_df_unique <- zone2019_df_unique %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))

list_of_paddocks_include2019 <- unique(zone2019_df_unique$Paddock_ID)
list_of_zone_include_2019 <- (zone2019_df_unique$Zone_ID)
print(list_of_paddocks_include2019)
rm(zone2019_count, zone2019_df)

##########################################################################################################################################
# 2. Add rainfall data
##########################################################################################################################################
rainfall_fert_2019 <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_step_2019.csv")

#rainfall_fert <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_steps.csv")
names(rainfall_fert_2019)

rainfall_fert_2019 <- rainfall_fert_2019 %>%
  dplyr::select(
    Paddock_ID,
    #Rate,
    GSP	,
    #Strip_Rate,
    #organisati,
    #contact	,
    #farmer	,
    #paddock,
    #Strip_Type,
    av_rain,
    #Total_sum_N_content,
    #Total_sum_P_content,
    #Status
  )


##### filter out the data so I just have what I will use for the economic analysis


rainfall_fert_2019 <- rainfall_fert_2019 %>% 
  filter(Paddock_ID %in% list_of_paddocks_include2019)


### need to remove the Alt GPS from the rainfall data                
rainfall_fert_2019 <- rainfall_fert_2019 %>%   
  filter(is.na(GSP) | GSP == "GSP")


str(zone2020_unique) 
zone2019_df_unique$Paddock_ID <- as.double(zone2019_df_unique$Paddock_ID)
str(rainfall_fert_2019)
rain_zone2019 <- left_join(zone2019_df_unique, rainfall_fert_2019, by = "Paddock_ID")

# some seem to have come through twice ?? remove the

rain_zone2019 <- rain_zone2019 %>%
  distinct(Zone_ID, .keep_all = TRUE)

rain_zone2019 <- rain_zone2019 %>% dplyr::select(-GSP)
# beacuse I used the rainfall data file I have all the strip per zone -
#so I just need to keep unique values for zone ID and strip type

rain_zone2020 <- rain_zone2020 %>% 
  distinct(Zone_ID , Strip_Type, .keep_all = TRUE)

rain_zone2019 <- rain_zone2019 %>% 
  dplyr::mutate(
    rainfall_class = case_when(
      av_rain<=350 ~ "low",
      av_rain >500 ~ "high",
      TRUE ~ "medium"))


rm(rainfall_fert_2019,zone2019_df_unique)

##########################################################################################################################################
#3b. add in the GRDC zones

zone2019_GRDC_bound <- st_read("W:/value_soil_testing_prj/Yield_data/finished/GIS_Results/All_Zones_2019_GRDC_wgs84.shp")

zone2019_GRDC_bound <- zone2019_GRDC_bound %>% 
  dplyr::select(Zone_ID,
                # organisati,
                # contact,
                # farmer,
                # paddock,
                # Paddock_ID,
                # Strip_Type,
                AGROECOLOG)
zone2019_GRDC_bound <- as.data.frame(zone2019_GRDC_bound) %>% 
  dplyr::select(- geometry)
str(zone2019_GRDC_bound)
str(rain_zone2019)

rain_zone2019 <- left_join(rain_zone2019, zone2019_GRDC_bound)

##########################################################################################################################################
#4. Bring in Sean DB
##########################################################################################################################################

recom_rateDB2019 <- read_excel( "W:/value_soil_testing_prj/data_base/downloaded_sep2021/GRDC 2019 Paddock Database_SA_VIC_June11 2021.xlsx")

# select only a few clms with recommedation 
recom_rateDB2019 <- recom_rateDB2019 %>% 
  dplyr::select(Zone_ID =    `Paddock code` ,
                Total_N = `Total N`, 
                p_rec =           `P rec`,
                n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                n_rec_yld_high =  `N Rec (> 5 t/ha)`)
                # SM_comment_Soil_N = `SM comment Soil N`,
                # SM_comment_Soil_P = `SM comment Soil P`,
                # SM_comment_Plant_Tissue = `SM comment Plant Tissue`
  

recom_rateDB2019 <-  dplyr::mutate(recom_rateDB2019,  
                                   maxN = apply(recom_rateDB2019[4:6], 1, max, na.rm = TRUE))



recom_rateDB2019$n_rec_yld_low <- as.double(recom_rateDB2019$n_rec_yld_low)
recom_rateDB2019$n_rec_yld_med <- as.double(recom_rateDB2019$n_rec_yld_med)
recom_rateDB2019$n_rec_yld_high <- as.double(recom_rateDB2019$n_rec_yld_high)




# remove redunant clm and replace inf
recom_rateDB2019 <- recom_rateDB2019 %>% 
  mutate(
    maxN = case_when(
      maxN >= 0 ~ maxN,
      TRUE ~ NA_real_
    )
  )
##make a paddock_ID clm
recom_rateDB2019$length_zoneID <- nchar(recom_rateDB2019$Zone_ID)
recom_rateDB2019 <- recom_rateDB2019 %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
recom_rateDB2019$Paddock_ID <- as.double(recom_rateDB2019$Paddock_ID)

str(recom_rateDB2019) #this has all the zones
str(rain_zone2019) #this has the paddock details
recom_rateDB2019$Zone_ID <- as.double(recom_rateDB2019$Zone_ID)


test <- left_join(rain_zone2019, recom_rateDB2019)




test$p_rec <- as.double(test$p_rec)



#############################################################################################################
### Redo the rec rates for N with my rainfall zone

test <- test %>% 
  mutate(Rec_N_jax = case_when(
    rainfall_class == "low" & Total_N <= 80 ~ ((80 -Total_N)/0.5),
    rainfall_class == "medium" & Total_N <= 160 ~ ((160 -Total_N)/0.5),
    rainfall_class == "high" & Total_N <= 240 ~ ((240 -Total_N)/0.5),
    TRUE                           ~ 0  ))









#just keep the data for the economic analysis
#now use this list to filter out my analysis...
test <- test %>% 
  filter(Zone_ID %in% list_of_zone_include)

test1 <- test %>%
  dplyr::select(
    "Zone_ID" ,
    "p_rec" ,
    "maxN" ,
    "Rec_N_jax", 
    "n_rec_yld_low",
    "n_rec_yld_med" ,
    "n_rec_yld_high",
    # "SM_comment_Soil_N",
    # "SM_comment_Soil_P",
    # "SM_comment_Plant_Tissue",
    "Strip_Type",
    "Paddock_ID",
    "AGROECOLOG",
    rainfall_class
  )     


## make sure I have only one zone ID

test1 <- test1 %>% 
  distinct(Zone_ID, .keep_all=TRUE)


zone_2019_step1 <- test1 

rm(rain_zone2019, recom_rateDB2019, test, test1, zone2019_GRDC_bound,
   list_of_paddocks_include2019, list_of_zone_include_2019)

zone_2019_step1 %>% group_by(Strip_Type) %>% 
  summarise(count= n())

names(zone_2019_step1)

##### SECTION FOR R markdown #####

# For P trials in 2020
# The number of paddocks with either 1, 2,3 zones
# How many paddocks have a P rec rate in zone 1 vs zone 2 that is higher than 5 kg


#```{r rec rates different in zone 2020 P , echo=FALSE, message=TRUE, warning=FALSE}

#### is the recommedations different?  ###

rec_rate_2019_P <- zone_2019_step1 %>%  dplyr::filter(Strip_Type == "P Strip" | Strip_Type == "N Strip, P Strip")
Zone_In_Paddocks_P <- rec_rate_2019_P %>%  group_by(Paddock_ID) %>% 
  summarise(count_zone = n(),
            max_P_rec = max(p_rec, na.rm = FALSE),
            min_P_rec = min(p_rec, na.rm = FALSE))

Zone_In_Paddocks_P <- Zone_In_Paddocks_P %>% 
  mutate(diff_P_rec_rate = max_P_rec - min_P_rec)

Zone_In_Paddocks_P <- Zone_In_Paddocks_P %>% 
  mutate(count_of_diff = case_when(
    count_zone >1 & diff_P_rec_rate >5 ~ 1,
    TRUE ~ 0))
Zone_In_Paddocks_P %>%  group_by(count_zone) %>% 
  summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE))

Zone_In_Paddocks_P %>%  
  summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE),
            count =  n())




zone_2019_step1$Strip_Type <- as.character(zone_2019_step1$Strip_Type)

rec_rate_2019_N <- zone_2019_step1 %>%  dplyr::filter(Strip_Type == "N Strip" | Strip_Type == "N Strip, P Strip" )


Zone_In_Paddocks_N <- rec_rate_2019_N %>%  group_by(Paddock_ID) %>% 
  summarise(count_zone = n(),
            max_N_rec = max(Rec_N_jax, na.rm = FALSE),
            min_N_rec = min(Rec_N_jax, na.rm = FALSE))

Zone_In_Paddocks_N <- Zone_In_Paddocks_N %>% 
  mutate(diff_N_rec_rate = max_N_rec - min_N_rec)

Zone_In_Paddocks_N <- Zone_In_Paddocks_N %>% 
  mutate(count_of_diff = case_when(
    count_zone >1 & diff_N_rec_rate >10 ~ 1,
    TRUE ~ 0))
Zone_In_Paddocks_N %>%  group_by(count_zone) %>% 
  summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE))

Zone_In_Paddocks_N %>%  
  summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE),
            count =  n())

rm(rec_rate_2019_N, rec_rate_2019_P, Zone_In_Paddocks_N, Zone_In_Paddocks_P)
