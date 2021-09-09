###### 2019 #######
### The data for recom rates vs GSP ######



### Get rainfall for sites
#1.bring in shapefile strip data


all_strips2019 <- st_read("W:/value_soil_testing_prj/Yield_data/finished/GIS_Results/All_Strips_2019_wgs84.shp")
str(all_strips2019)
all_strips2019 <- all_strips2019 %>%
  dplyr::select(Paddock_ID, geometry) 

#2.turn polygons into points - centriod

all_strips2019_centroid = st_centroid(all_strips2019)
all_strips2019_centroid <- all_strips2019_centroid %>%  filter(!is.na(Paddock_ID))
###################################################################################################
###################  rainfall data from BOM ########################################################
####################################################################################################

# downloaded gridded rainfall data from BOM

#http://www.bom.gov.au/jsp/ncc/climate_averages/rainfall/index.jsp
#choosing the average rainfall - convert the text file into a grid and extract the gridded values to points

av_rain <- raster::raster("W:/value_soil_testing_prj/Yield_data/2020/processing/rain_grid")
av_rain

##2. extract strips coordinates points from the raster (eg shapefile points and average rainfall grid)
all_strips2019_centroid$av_rain <- raster::extract(av_rain, all_strips2019_centroid)
all_strips2019_centroid
all_strips2019_centroid_df <- as.data.frame(all_strips2019_centroid ) %>%  dplyr::select(-geometry)
str(all_strips2019_centroid_df)

rm(all_strips2019, all_strips2019_centroid, av_rain)

rain_2019 <- all_strips2019_centroid_df %>% 
  dplyr::mutate(
    rainfall_class = case_when(
      av_rain<=350 ~ "low",
      av_rain >500 ~ "high",
      TRUE ~ "medium"))
rain_2019 <- rain_2019 %>%  distinct(Paddock_ID, .keep_all = TRUE)
rm(all_strips2019_centroid_df)

####################################################################################################################

fertiliser_applied2019 <- read.csv("W:/value_soil_testing_prj/Yield_data/processing/step2_fert_app_all_steps_2019.csv")
str(fertiliser_applied2019)

fert_2019 <- fertiliser_applied2019 %>% 
  dplyr::select(Paddock_ID, Rate,GSP,
                Strip_Rate , Strip_Type,
                organisati,
                contact,
                farmer,
                join_field,
                Total_sum_N_content,
                Total_sum_P_content) 

### add in the rainfall
fert_2019_rain <- left_join(fert_2019, rain_2019)
rm(fert_2019, fertiliser_applied2019, rain_2019)
str(fert_2019_rain)

#Using the fert rates I can pull out the GSP paddock code and strip type
fert_2019_rain_GSP <- fert_2019_rain %>%  filter(!is.na( GSP))
str(fert_2019_rain_GSP)
fert_2019_rain_GSP <- fert_2019_rain_GSP %>% dplyr::select(Paddock_ID,
                                                       Strip_Type,
                                                       Total_sum_N_content,
                                                       Total_sum_P_content,
                                                       av_rain,
                                                       rainfall_class)
rm(fert_2019_rain)
####################################################################################################################
## bring in rec rates ##

recom_rateDB2019 <- read_excel( "W:/value_soil_testing_prj/data_base/downloaded_sep2021/GRDC 2019 Paddock Database_SA_VIC_June11 2021.xlsx")
str(recom_rateDB2019)
# select only a few clms with recommedation 
recom_rateDB2019 <- recom_rateDB2019 %>% 
  dplyr::select(Zone_ID =    `Paddock code` ,
                p_rec =           `P rec`,
                n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                n_rec_yld_high =  `N Rec (> 5 t/ha)`
                #SM_comment_Soil_N = `SM comment Soil N`,
                #SM_comment_Soil_P = `SM comment Soil P`,
                #SM_comment_Plant_Tissue = `SM comment Plant Tissue`
  ) 

recom_rateDB2019 <-  dplyr::mutate(recom_rateDB2019,  maxN = apply(recom_rateDB2019[3:5], 1, max, na.rm = TRUE))


# remove redunant clm and replace inf
recom_rateDB2019 <- recom_rateDB2019 %>% 
  mutate(
    maxN = case_when(
      maxN >= 0 ~ maxN,
      TRUE ~ NA_real_
    )
  )

recom_rateDB2019 <- recom_rateDB2019 %>% 
  dplyr::select(Zone_ID  ,
                p_rec ,
                maxN)

##make a paddock_ID clm
recom_rateDB2019$length_zoneID <- nchar(recom_rateDB2019$Zone_ID)
recom_rateDB2019 <- recom_rateDB2019 %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
recom_rateDB2019$Paddock_ID <- as.double(recom_rateDB2019$Paddock_ID)

str(recom_rateDB2019) #this has all the zones
str(fert_2019_rain_GSP) #this has the paddock details

zone_rec_rate_GSP_2019 <- left_join(recom_rateDB2019, fert_2019_rain_GSP)
#if it has no rainfall class we didnt have strip info so chuck it!
zone_rec_rate_GSP_2019 <- zone_rec_rate_GSP_2019 %>% 
  filter(!is.na(rainfall_class))

rm(fert_2019_rain_GSP, recom_rateDB2019)
zone_rec_rate_GSP_2019$Strip_Type <- as.character(zone_rec_rate_GSP_2019$Strip_Type)
zone_rec_rate_GSP_2019$p_rec <- as.double(zone_rec_rate_GSP_2019$p_rec)

str(zone_rec_rate_GSP_2019)
#zone_rec_rate_GSP_2019 %>%  dplyr::filter()
zone_rec_rate_GSP_2019 %>%  group_by(Strip_Type, rainfall_class) %>% 
  summarise(Av_N_content_GSP = mean(Total_sum_N_content, na.rm = TRUE),
            Av_N_rec_rate = mean(maxN, na.rm = TRUE),
            Av_P_content_GSP = mean(Total_sum_P_content, na.rm = TRUE),
            Av_P_rec_rate = mean(p_rec, na.rm = TRUE)
            
            )

