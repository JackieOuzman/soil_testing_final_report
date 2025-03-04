
library(tidyverse)
library(ggplot2)
library(formattable)
library(sf)
library(readxl)
library(readr)


## getting the yield difference and GM difference between GSP and Approx
all_strips2019 <- st_read("W:/value_soil_testing_prj/Yield_data/finished/GIS_Results/All_Strips_2019_wgs84.shp")
all_strips2019 <- all_strips2019 %>%
  dplyr::select(Paddock_ID, geometry) 

#2.turn polygons into points - centriod
all_strips2019_centroid = st_centroid(all_strips2019)
all_strips2019_centroid <- all_strips2019_centroid %>%  filter(!is.na(Paddock_ID))
#3.
av_rain <- raster::raster("W:/value_soil_testing_prj/Yield_data/2020/processing/rain_grid")

##3a. extract strips coordinates points from the raster (eg shapefile points and average rainfall grid)
all_strips2019_centroid$av_rain <- raster::extract(av_rain, all_strips2019_centroid)
all_strips2019_centroid_df <- as.data.frame(all_strips2019_centroid ) %>%  dplyr::select(-geometry)
rm(all_strips2019, all_strips2019_centroid, av_rain)


## add in GRDC zone
zone2019_GRDC_bound <- st_read("W:/value_soil_testing_prj/Yield_data/finished/GIS_Results/All_Zones_2019_GRDC_wgs84.shp")

zone2019_GRDC_bound <- zone2019_GRDC_bound %>% 
  dplyr::select(Zone_ID,
                AGROECOLOG)
zone2019_GRDC_bound <- as.data.frame(zone2019_GRDC_bound) %>% 
  dplyr::select(- geometry)
##make a paddock_ID clm
zone2019_GRDC_bound$length_zoneID <- nchar(zone2019_GRDC_bound$Zone_ID)
zone2019_GRDC_bound <- zone2019_GRDC_bound %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
zone2019_GRDC_bound$Paddock_ID <- as.double(zone2019_GRDC_bound$Paddock_ID)

str(zone2019_GRDC_bound)
str(all_strips2019_centroid_df)

all_strips2019_centroid_df <- left_join(all_strips2019_centroid_df, zone2019_GRDC_bound)
all_strips2019_centroid_df <- all_strips2019_centroid_df %>% 
  dplyr::mutate(
    rainfall_class = case_when(
      av_rain<=350 ~ "low",
      av_rain >500 ~ "high",
      TRUE ~ "medium"))


all_strips2019_centroid_df <- all_strips2019_centroid_df %>% 
  dplyr::select( Paddock_ID, 
                AGROECOLOG, 
                rainfall_class) 
                
### oops I need the rates from the fert df

fertiliser_applied2019 <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_step_2019.csv")
str(fertiliser_applied2019)

fert_2019 <- fertiliser_applied2019 %>% 
  dplyr::select(Paddock_ID, Rate,GSP,Strip_Type,
                Total_sum_N_content,
                Total_sum_P_content) 

rm(fertiliser_applied2019, zone2019_GRDC_bound)
str(all_strips2019_centroid_df)
str(fert_2019)
## add it to the other data

details_2019 <- left_join(fert_2019, all_strips2019_centroid_df)
rm(fert_2019,all_strips2019_centroid_df )

names(details_2019)

## The GSP is coded a bit strange its either a GPS or number - should all be GSP
details_2019 <- details_2019 %>% 
  dplyr::mutate(GSP = case_when(
    GSP =!is.na(GSP) ~ "GSP" 
  ))
str(details_2019)
## remove a couple of problem strips
details_2019 <- details_2019 %>% 
  filter(Paddock_ID != 31712  |  !is.na(Rate))
details_2019 <- details_2019 %>% 
  filter(Paddock_ID != 51521  |  !is.na(Rate))
details_2019 <- details_2019 %>% 
  filter(Paddock_ID != 51522  |  !is.na(Rate))

#######################################################################################

## bring in the t.test data results

set1_2019 <- read.csv("W:/value_soil_testing_prj/Yield_data/finished/complied/use/Landmark_with_soil2020-06-24_For_TM.csv")
set2_2019 <- read.csv("W:/value_soil_testing_prj/Yield_data/finished/complied/use/Non_landmark_results_soil_and_pair_rates2020-06-25_For_TM.csv")
str(set1_2019)
names(set1_2019)
set1_2019 <- set1_2019 %>% 
  dplyr::select(Zone_ID = Paddock.code,
                Organisation,
                Contact,
                Farmer,
                Paddock_tested,
                Rates,
                Yld,
                P_value,Mean_diff,rounded,Significant)

str(set2_2019)
names(set2_2019)

set2_2019 <- set2_2019 %>% 
  dplyr::select(Zone_ID = Paddock.code,
                Organisation,
                Contact,
                Farmer,
                Paddock_tested,
                Rates,
                Yld,
                P_value,Mean_diff,rounded,Significant)
# Pole has rates in shapefile that are different to this, its that same but expressed in different units

set2_2019 <- set2_2019 %>%
  dplyr::mutate(Rates = case_when(
    Paddock_tested == 	"Georges" & Rates ==  0 ~ 0,
    Paddock_tested == 	"Georges" & Rates ==  3 ~ 23,
    Paddock_tested == 	"Georges" & Rates ==  6 ~ 46,
    Paddock_tested == 	"Georges" & Rates ==  9 ~ 69,
    Paddock_tested == 	"Georges" & Rates ==  12 ~ 92,
    Paddock_tested == 	"Georges" & Rates ==  15 ~ 115,
    TRUE ~ as.double(Rates)
  ))

t.test_2019 <- rbind(set2_2019, set1_2019)
rm(set1_2019, set2_2019)

##make a paddock_ID clm
t.test_2019$length_zoneID <- nchar(t.test_2019$Zone_ID)
t.test_2019 <- t.test_2019 %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
t.test_2019$Paddock_ID <- as.double(t.test_2019$Paddock_ID)



## The t test data does not have N or P Strips and there is duplication
str(t.test_2019)
t.test_2019 <- t.test_2019 %>% 
  dplyr::mutate(Strip_Type = case_when(
    Paddock_ID == 31132 ~ "N Strip",
    Paddock_ID == 31231 ~ "N Strip",
    Paddock_ID == 31233 ~ "N Strip",
    Paddock_ID == 31711 ~ "N Strip",
    Paddock_ID == 31712 ~ "N Strip",
    Paddock_ID == 31726 ~ "N Strip",
    Paddock_ID == 51512 ~ "N Strip",
    Paddock_ID == 51713 ~ "N Strip",
    Paddock_ID == 51911 ~ "N Strip",
    Paddock_ID == 51914 ~ "N Strip",
    Paddock_ID == 51915 ~ "N Strip",
    Paddock_ID == 52413 ~ "N Strip",
    Paddock_ID == 52444 ~ "N Strip",
    Paddock_ID == 52453 ~ "N Strip",
    Paddock_ID == 51511 ~ "N Strip",
    Paddock_ID == 33311 ~ "N and P Strip",
    Paddock_ID == 33321 ~ "N and P Strip",
    Paddock_ID == 33331 ~ "N and P Strip",
    TRUE ~ "P Strip"
  ))
## Ann paddocks are a bit of a mess some results are for P and some are for N I have used TB analysis to work out which is which
t.test_2019 <- t.test_2019 %>% 
  dplyr::mutate(Strip_Type = case_when(
    #Schlitz_M2
    Zone_ID == 333110 & Rates == 0 & Yld < 3 ~ "N Strip",
    Zone_ID == 333110 & Rates == 60  ~ "N Strip",
    Zone_ID == 333110 & Rates == 120  ~ "N Strip",
    
    Zone_ID == 333110 & Rates == 0 & Yld > 3 ~ "P Strip",
    Zone_ID == 333110 & Rates == 20  ~ "P Strip",
    Zone_ID == 333110 & Rates == 40  ~ "P Strip",
    Zone_ID == 333110 & Rates == 80  ~ "P Strip",
    
    
    #Schlitz_M2
    Zone_ID == 333111 & Rates == 0 & Yld > 2.69 ~ "N Strip",
    Zone_ID == 333111 & Rates == 60  ~ "N Strip",
    Zone_ID == 333111 & Rates == 120  ~ "N Strip",
    
    Zone_ID == 333111 & Rates == 0 & Yld < 2.69 ~ "P Strip",
    Zone_ID == 333111 & Rates == 20  ~ "P Strip",
    Zone_ID == 333111 & Rates == 40  ~ "P Strip",
    Zone_ID == 333111 & Rates == 80  ~ "P Strip",
    
    #Chamberlain_2
    Zone_ID == 333211 & Rates == 25  ~ "P Strip",
    Zone_ID == 333211 & Rates == 50  ~ "P Strip",
    Zone_ID == 333211 & Rates == 75  ~ "P Strip",
    
    Zone_ID == 333210 & Rates == 25  ~ "P Strip",
    Zone_ID == 333210 & Rates == 50  ~ "P Strip",
    Zone_ID == 333210 & Rates == 75  ~ "P Strip",
    
    Zone_ID == 333211 & Rates == 0  ~ "N Strip",
    Zone_ID == 333211 & Rates == 60  ~ "N Strip",
    Zone_ID == 333211 & Rates == 120  ~ "N Strip",
    
    Zone_ID == 333210 & Rates == 0  ~ "N Strip",
    Zone_ID == 333210 & Rates == 60  ~ "N Strip",
    Zone_ID == 333210 & Rates == 120  ~ "N Strip",
    
    
    #Nelson_3
    Zone_ID == 333311 & Rates == 0 & Yld < 1.5 ~ "P Strip",
    Zone_ID == 333311 & Rates == 25 & Yld > 1.5 ~ "P Strip",
    Zone_ID == 333311 & Rates == 50 & Yld < 1.5 ~ "P Strip",
    Zone_ID == 333311 & Rates == 75  ~ "P Strip",
    
    Zone_ID == 333310 & Rates == 0 & Yld > 1.8 ~ "P Strip",
    Zone_ID == 333310 & Rates == 25 & Yld < 1.7 ~ "P Strip",
    Zone_ID == 333310 & Rates == 50 & Yld > 1.7 ~ "P Strip",
    Zone_ID == 333310 & Rates == 75  ~ "P Strip",
    
    Zone_ID == 333311 & Rates == 0 & Yld < 1.5 ~ "N Strip",
    Zone_ID == 333311 & Rates == 25 & Yld > 1.5 ~ "N Strip",
    Zone_ID == 333311 & Rates == 50 & Yld < 1.5 ~ "N Strip",
    
    
    Zone_ID == 333311 & Rates == 0 & Yld > 1.5 ~ "N Strip",
    Zone_ID == 333311 & Rates == 25 & Yld < 1.5 ~ "N Strip",
    Zone_ID == 333311 & Rates == 50 & Yld > 1.5 ~ "N Strip",
    
    Zone_ID == 333310 & Rates == 0 & Yld < 1.8 ~ "N Strip",
    Zone_ID == 333310 & Rates == 25 & Yld > 1.7 ~ "N Strip",
    Zone_ID == 333310 & Rates == 50 & Yld < 1.7 ~ "N Strip",
    
    TRUE ~ Strip_Type
  ))

# is  paddock is missing code "Landmark	James_Falvey_2	Tim_McClelland_4	Mervyns" but I am not sure it should be included??
names(t.test_2019)
t.test_2019 <- t.test_2019 %>% 
  dplyr::mutate(Zone_ID = case_when(
    Paddock_tested == "Mervyns" ~ 312431,
    TRUE ~ as.double(Zone_ID)
  ) )




## I want to join details_2019 to t.test 

str(t.test_2019)
str(details_2019)

t.test_2019 <- t.test_2019 %>% 
  dplyr::mutate(for_join = paste0(Paddock_ID, Strip_Type, Rates))
details_2019 <- details_2019 %>% 
  dplyr::mutate(for_join = paste0(Paddock_ID, Strip_Type, Rate))

t.test2019_details <- full_join(details_2019, t.test_2019)
names(t.test2019_details)

# I have two problem paddocks Jeff (53621) no rates no analysis and Mervyns non zone 312431 

t.test2019_details <- t.test2019_details %>% 
  filter(Paddock_tested != "Mervyns")
t.test2019_details <- t.test2019_details %>% 
  filter(Paddock_ID != 53621)

t.test2019_details <- t.test2019_details %>%
  dplyr::select(Zone_ID,
                Paddock_ID,
                Strip_Type,
                Rate,
                GSP,
                Strip_Type,
                Total_sum_N_content,
                Total_sum_P_content,
                Yld,
                Organisation,
                Contact,
                Farmer,
                Paddock_tested,
                rainfall_class,
                AGROECOLOG,
                P_value,
                Mean_diff,
                rounded,
                Significant)
#### This is ready for more the recomm rates



####################################################################################################################################################
## bring in rec rates ##

recom_rateDB2019 <- read_excel( "W:/value_soil_testing_prj/data_base/downloaded_sep2021/GRDC 2019 Paddock Database_SA_VIC_June11 2021.xlsx")
str(recom_rateDB2019)
# select only a few clms with recommedation 
recom_rateDB2019 <- recom_rateDB2019 %>% 
  dplyr::select(Zone_ID =    `Paddock code` ,
                Total_N = `Total N`, 
                p_rec =           `P rec`,
                n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                n_rec_yld_high =  `N Rec (> 5 t/ha)`,
                Colwell,
                DGT,
                PBI
                
  ) 

recom_rateDB2019$n_rec_yld_low <- as.double(recom_rateDB2019$n_rec_yld_low)
recom_rateDB2019$n_rec_yld_med <- as.double(recom_rateDB2019$n_rec_yld_med)
recom_rateDB2019$n_rec_yld_high <- as.double(recom_rateDB2019$n_rec_yld_high)
recom_rateDB2019$Colwell <- as.double(recom_rateDB2019$Colwell)
recom_rateDB2019$DGT <- as.double(recom_rateDB2019$DGT)
recom_rateDB2019$PBI <- as.double(recom_rateDB2019$PBI)

recom_rateDB2019 <-  dplyr::mutate(recom_rateDB2019,  maxN = apply(recom_rateDB2019[4:6], 1, max, na.rm = TRUE))


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
                maxN,
                Total_N,
                Colwell,
                DGT,
                PBI)

##make a paddock_ID clm
recom_rateDB2019$length_zoneID <- nchar(recom_rateDB2019$Zone_ID)
recom_rateDB2019 <- recom_rateDB2019 %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
recom_rateDB2019$Paddock_ID <- as.double(recom_rateDB2019$Paddock_ID)

str(recom_rateDB2019) #this has all the zones
str(fert_2019_rain_GSP) #this has the paddock details

rm(details_2019, t.test_2019)

### join the t.test data to the recom rates
str(recom_rateDB2019)
str(t.test2019_details)

t.test_details_rec_rates <- left_join(t.test2019_details, recom_rateDB2019)
rm(recom_rateDB2019, t.test2019_details)
#############################################################################################################
### Redo the rec rates for N with my rainfall zone

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(Rec_N_jax = case_when(
    rainfall_class == "low" & Total_N <= 80 ~ ((80 -Total_N)/0.5),
    rainfall_class == "medium" & Total_N <= 160 ~ ((160 -Total_N)/0.5),
    rainfall_class == "high" & Total_N <= 240 ~ ((240 -Total_N)/0.5),
    TRUE                           ~ 0  ))

str(t.test_details_rec_rates)

# t.test_details_rec_rates <- t.test_details_rec_rates %>% 
#   mutate(critical_colwell = 4.6*( PBI^ (0.393)))
# ## is colwell greater than critical colwell?
# t.test_details_rec_rates <- t.test_details_rec_rates %>% 
#   mutate(colwell_thershold = case_when(
#     Colwell > critical_colwell ~ "adequate",
#     Colwell < critical_colwell ~ "p_required")  )

## if p is required how much extra colwell p is needed to get to critical thershold?
# t.test_details_rec_rates <- t.test_details_rec_rates %>% 
#   mutate(to_reach_col_thershold = case_when(
#     colwell_thershold == "p_required" ~ critical_colwell - Colwell))
# 
# ## what is the recomm P rate?
# t.test_details_rec_rates <- t.test_details_rec_rates %>% 
#   mutate(p_rec_jax = case_when(
#     colwell_thershold == "p_required" ~ ((0.0019*PBI+2.146)*to_reach_col_thershold),
#     colwell_thershold == "adequate" ~ 5
#     ))
# ## clean up extra clms
# 
# names(t.test_details_rec_rates)
# 
# t.test_details_rec_rates <- t.test_details_rec_rates %>%
#   dplyr::select(-"length_zoneID",
#                 - critical_colwell,
#                 - colwell_thershold,
#                 - to_reach_col_thershold)


###################################################################################################################################
#write this out 
write.csv(t.test_details_rec_rates, "W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/t.test_details_rec_rates_2019.csv")
