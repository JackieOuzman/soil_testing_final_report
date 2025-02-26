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


rm(rainfall_fert,zone2020_unique)

##########################################################################################################################################
#3b. add in the GRDC zones

zone2020_GRDC_bound <- st_read("W:/value_soil_testing_prj/Yield_data/2020/2020_GRDC_zones_test.shp")
zone2020_GRDC_bound <- zone2020_GRDC_bound %>% 
  dplyr::select(Zone_ID,
                # organisati,
                # contact,
                # farmer,
                # paddock,
                # Paddock_ID,
                # Strip_Type,
                AGROECOLOG)
zone2020_GRDC_bound <- as.data.frame(zone2020_GRDC_bound) %>% 
  dplyr::select(- geometry)
str(zone2020_GRDC_bound)
str(rain_zone2020)

rain_zone2020 <- left_join(rain_zone2020, zone2020_GRDC_bound)

##########################################################################################################################################
#4. Bring in Sean DB
##########################################################################################################################################

recom_rateDB <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_May05 2021.xlsx")

# select only a few clms with recommedation 
recom_rateDB <- recom_rateDB %>% 
  dplyr::select(Zone_ID =    `Paddock code` ,
                Total_N = `Total N`, 
                p_rec =           `P rec`,
                n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                n_rec_yld_high =  `N Rec (> 5 t/ha)`,
                SM_comment_Soil_N = `SM comment Soil N`,
                SM_comment_Soil_P = `SM comment Soil P`,
                SM_comment_Plant_Tissue = `SM comment Plant Tissue`
  ) 

recom_rateDB <-  dplyr::mutate(recom_rateDB,  maxN = apply(recom_rateDB[3:5], 1, max, na.rm = TRUE))



recom_rateDB$n_rec_yld_low <- as.double(recom_rateDB$n_rec_yld_low)
recom_rateDB$n_rec_yld_med <- as.double(recom_rateDB$n_rec_yld_med)
recom_rateDB$n_rec_yld_high <- as.double(recom_rateDB$n_rec_yld_high)




# remove redunant clm and replace inf
recom_rateDB <- recom_rateDB %>% 
  mutate(
    maxN = case_when(
      maxN >= 0 ~ maxN,
      TRUE ~ NA_real_
    )
  )
##make a paddock_ID clm
recom_rateDB$length_zoneID <- nchar(recom_rateDB$Zone_ID)
recom_rateDB <- recom_rateDB %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
recom_rateDB$Paddock_ID <- as.double(recom_rateDB$Paddock_ID)

str(recom_rateDB) #this has all the zones
str(rain_zone2020) #this has the paddock details
recom_rateDB$Zone_ID <- as.double(recom_rateDB$Zone_ID)


test <- left_join(rain_zone2020, recom_rateDB)




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
    "SM_comment_Soil_N",
    "SM_comment_Soil_P",
    "SM_comment_Plant_Tissue",
    "Strip_Type",
    "Paddock_ID",
    "AGROECOLOG"
  )     


## make sure I have only one zone ID

test1 <- test1 %>% 
  distinct(Zone_ID, .keep_all=TRUE)




rm(rain_zone2020, rainfall_fert, zone2020_unique, recom_rateDB,
   list_of_paddocks_include, list_of_zone_include)





##### SECTION FOR R markdown #####

# For P trials in 2020
# The number of paddocks with either 1, 2,3 zones
# How many paddocks have a P rec rate in zone 1 vs zone 2 that is higher than 5 kg


#```{r rec rates different in zone 2020 P , echo=FALSE, message=TRUE, warning=FALSE}

#### is the recommedations different?  ###

rec_rate_2020_P <- test1 %>%  dplyr::filter(Strip_Type == "P Strip" | Strip_Type == "N Strip, P Strip")
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

Zone_In_Paddocks_P %>%  
  summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE),
            count =  n())




test1$Strip_Type <- as.character(test1$Strip_Type)

rec_rate_2020_N <- test1 %>%  dplyr::filter(Strip_Type == "N Strip" | Strip_Type == "N Strip, P Strip" )


Zone_In_Paddocks_N <- rec_rate_2020_N %>%  group_by(Paddock_ID) %>% 
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
str(rec_rate_2020_N)

#Group by argo zone
Zone_In_Paddocks_N_agro <- rec_rate_2020_N %>%  group_by(Paddock_ID, AGROECOLOG) %>% 
  summarise(count_zone = n(),
            max_N_rec = max(Rec_N_jax, na.rm = FALSE),
            min_N_rec = min(Rec_N_jax, na.rm = FALSE))
Zone_In_Paddocks_N_agro <- Zone_In_Paddocks_N_agro %>% 
  mutate(diff_N_rec_rate = max_N_rec - min_N_rec)
Zone_In_Paddocks_N_agro <- Zone_In_Paddocks_N_agro %>% 
  mutate(count_of_diff = case_when(
    count_zone >1 & diff_N_rec_rate >10 ~ 1,
    TRUE ~ 0))
Zone_In_Paddocks_N_agro %>%  group_by(AGROECOLOG) %>% 
  summarise(sum_of_when_diff = sum(count_of_diff, na.rm = TRUE),
            count =  n())
