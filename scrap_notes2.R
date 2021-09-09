

#########################################################################################################################################
## step 1 make a list of zone and a list of paddocks that will be included in the analysis 
#1b. zone codes
##########################################################################################################################################


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



rm(zone2020_df)

str(zone2020_unique)
zone2020_unique$length_zoneID <- nchar(zone2020_unique$Zone_ID)
zone2020_unique <- zone2020_unique %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))

list_of_paddocks_include <- unique(zone2020_unique$Paddock_ID)
list_of_zone_include <- (zone2020_unique$Zone_ID)
print(list_of_paddocks_include)
##########################################################################################################################################
# 2. Add rainfall data
##########################################################################################################################################

rainfall_fert <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_steps.csv")


rainfall_fert <- rainfall_fert %>%
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


rainfall_fert <- rainfall_fert %>% 
  filter(Paddock_ID %in% list_of_paddocks_include)


### need to remove the Alt GPS from the rainfall data                
rainfall_fert <- rainfall_fert %>%   
  filter(is.na(GSP) | GSP == "GSP")


str(zone2020_unique) 
zone2020_unique$Paddock_ID <- as.double(zone2020_unique$Paddock_ID)
str(rainfall_fert)
rain_zone2020 <- left_join(zone2020_unique, rainfall_fert, by = "Paddock_ID")

# some seem to have come through twice ?? remove the

rain_zone2020 <- rain_zone2020 %>%
  filter(Status == "5. Report Complete" )

rain_zone2020 <- rain_zone2020 %>% dplyr::select(-GSP)
# beacuse I used the rainfall data file I have all the strip per zone -
#so I just need to keep unique values for zone ID and strip type

rain_zone2020 <- rain_zone2020 %>% 
  distinct(Zone_ID , Strip_Type, .keep_all = TRUE)

rain_zone2020 <- rain_zone2020 %>% 
  dplyr::mutate(
    rainfall_class = case_when(
      av_rain<=350 ~ "low",
      av_rain >500 ~ "high",
      TRUE ~ "medium"))


#rm(rainfall_fert,zone2020_unique, GS_rates_3a_just_analysis)
##########################################################################################################################################
#4. Bring in Sean DB
##########################################################################################################################################

recom_rateDB <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_May05 2021.xlsx")

# select only a few clms with recommedation 
recom_rateDB <- recom_rateDB %>% 
  dplyr::select(Zone_ID =    `Paddock code` ,
                p_rec =           `P rec`,
                n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                n_rec_yld_high =  `N Rec (> 5 t/ha)`,
                SM_comment_Soil_N = `SM comment Soil N`,
                SM_comment_Soil_P = `SM comment Soil P`,
                SM_comment_Plant_Tissue = `SM comment Plant Tissue`
  ) 

recom_rateDB <-  dplyr::mutate(recom_rateDB,  maxN = apply(recom_rateDB[3:5], 1, max, na.rm = TRUE))


# remove redunant clm and replace inf
recom_rateDB <- recom_rateDB %>% 
  mutate(
    maxN = case_when(
      maxN >= 0 ~ maxN,
      TRUE ~ NA_real_
    )
  )


# recom_rateDB <-recom_rateDB %>%
#   dplyr::select(-n_rec_yld_low,
#                 -n_rec_yld_med,
#                 -n_rec_yld_high)



#just keep the data for the economic analysis
#now use this list to filter out my analysis...
recom_rateDB <- recom_rateDB %>% 
  filter(Zone_ID %in% list_of_zone_include)

recom_rateDB <- recom_rateDB %>%
  dplyr::select(
    "Zone_ID" ,
    "p_rec" ,
    "maxN" ,
    "n_rec_yld_low",
    "n_rec_yld_med" ,
    "n_rec_yld_high",
    "SM_comment_Soil_N",
    "SM_comment_Soil_P",
    "SM_comment_Plant_Tissue"
  )     



## join rec rates with zone rainfall data
str(rain_zone2020)
str(recom_rateDB)
recom_rateDB$Zone_ID <- as.double(recom_rateDB$Zone_ID)

rec_rate_2020 <- left_join(rain_zone2020,recom_rateDB )

rm(rain_zone2020, rainfall_fert, zone2020_unique, recom_rateDB,
   list_of_paddocks_include, list_of_zone_include)


#### is the recommedations different?  ###
str(rec_rate_2020)
rec_rate_2020_P <- rec_rate_2020 %>%  dplyr::filter(Strip_Type == "P Strip" | Strip_Type == "N Strip, P Strip")
Zone_In_Paddocks_P <- rec_rate_2020_P %>%  group_by(Paddock_ID) %>% 
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



#### N ###
str(rec_rate_2020)
unique(rec_rate_2020$Strip_Type)
rec_rate_2020$Strip_Type <- as.character(rec_rate_2020$Strip_Type)

rec_rate_2020_N <- rec_rate_2020 %>%  dplyr::filter(Strip_Type == "N Strip" | Strip_Type == "N Strip, P Strip" )


Zone_In_Paddocks_N <- rec_rate_2020_N %>%  group_by(Paddock_ID) %>% 
  summarise(count_zone = n(),
            max_N_rec = max(maxN, na.rm = FALSE),
            min_N_rec = min(maxN, na.rm = FALSE))

Zone_In_Paddocks_N <- Zone_In_Paddocks_N %>% 
  mutate(diff_N_rec_rate = max_N_rec - min_N_rec)

Zone_In_Paddocks_N <- Zone_In_Paddocks_N %>% 
  mutate(count_of_diff = case_when(
    count_zone >1 & diff_N_rec_rate >10 ~ 1,
    TRUE ~ 0))
Zone_In_Paddocks_N %>%  group_by(count_zone) %>% 
  summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE),
            count =  n())
            
Zone_In_Paddocks_N %>%  
              summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE),
                        count =  n())

