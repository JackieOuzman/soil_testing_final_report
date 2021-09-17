
library(tidyverse)
library(ggplot2)
library(formattable)
library(sf)
library(readxl)
library(readr)


## getting the yield difference and GM difference between GSP and Approx

all_strips2020 <- st_read("W:/value_soil_testing_prj/Yield_data/2020/All_Strips_2020_wgs84.shp")
#names(all_strips2020)
all_strips2020 <- all_strips2020 %>%
  dplyr::select(Paddock_ID, geometry, 
                Organisation = organisati,
                 Contact = contact,
                 Farmer = farmer,
                 Paddock_tested = paddock,
                Status) 
### remove the paddocks with no reports
#unique(all_strips2020$Status)

all_strips2020 <- all_strips2020 %>%
  dplyr::filter(Status != "Excluded from Analysis")
  
#2.turn polygons into points - centriod
all_strips2020_centroid = st_centroid(all_strips2020)
all_strips2020_centroid <- all_strips2020_centroid %>%  filter(!is.na(Paddock_ID))
#3.
av_rain <- raster::raster("W:/value_soil_testing_prj/Yield_data/2020/processing/rain_grid")

##3a. extract strips coordinates points from the raster (eg shapefile points and average rainfall grid)
all_strips2020_centroid$av_rain <- raster::extract(av_rain, all_strips2020_centroid)
all_strips2020_centroid_df <- as.data.frame(all_strips2020_centroid ) %>%  dplyr::select(-geometry)
rm(all_strips2020, all_strips2020_centroid, av_rain)


## add in GRDC zone
zone2020_GRDC_bound <- st_read("W:/value_soil_testing_prj/Yield_data/2020/2020_GRDC_zones_test.shp")


zone2020_GRDC_bound <- zone2020_GRDC_bound %>% 
  dplyr::select(Zone_ID,
                AGROECOLOG)
zone2020_GRDC_bound <- as.data.frame(zone2020_GRDC_bound) %>% 
  dplyr::select(- geometry)
##make a paddock_ID clm
zone2020_GRDC_bound$length_zoneID <- nchar(zone2020_GRDC_bound$Zone_ID)
zone2020_GRDC_bound <- zone2020_GRDC_bound %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
zone2020_GRDC_bound$Paddock_ID <- as.double(zone2020_GRDC_bound$Paddock_ID)

# str(zone2020_GRDC_bound)
# str(all_strips2020_centroid_df)

all_strips2020_centroid_df <- left_join(all_strips2020_centroid_df, zone2020_GRDC_bound)
all_strips2020_centroid_df <- all_strips2020_centroid_df %>% 
  dplyr::mutate(
    rainfall_class = case_when(
      av_rain<=350 ~ "low",
      av_rain >500 ~ "high",
      TRUE ~ "medium"))

#names(all_strips2020_centroid_df)
all_strips2020_centroid_df <- all_strips2020_centroid_df %>% 
  dplyr::select( Paddock_ID,
                 Organisation,
                 Contact,
                 Farmer,Paddock_tested,
                AGROECOLOG, 
                rainfall_class) 
                
### oops I need the rates from the fert df

fertiliser_applied2020 <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_steps.csv")
#str(fertiliser_applied2020)

fert_2020 <- fertiliser_applied2020 %>% 
  dplyr::select(Paddock_ID, Rate,GSP,Strip_Type,
                Total_sum_N_content,
                Total_sum_P_content) 

rm(fertiliser_applied2020, zone2020_GRDC_bound)
#str(all_strips2020_centroid_df)
#str(fert_2020)
## add it to the other data

details_2020 <- left_join(fert_2020, all_strips2020_centroid_df)
rm(fert_2020,all_strips2020_centroid_df )

#names(details_2020)




## Not sure if there is dupliaction only want paddock ID, strip type and rate
details_2020 <- details_2020 %>% 
  dplyr::distinct(Paddock_ID, Strip_Type, Rate, .keep_all = TRUE)

### fix up some problems paddock 52355 has a rate of 82.5 and should be 82

details_2020 <- details_2020 %>% 
  mutate(Rate = case_when(
    Paddock_ID == 52355 & Rate == 82.5 ~ 82,
    TRUE ~ as.double(Rate )))
# this one only had a mud map made remove from analysis
details_2020 <- details_2020 %>% 
  filter(Paddock_ID != 52341)
## I had some strange label in the shapefile
details_2020 <- details_2020 %>% 
  mutate(Rate = case_when(
    Paddock_ID == 52411 & Rate == 400 ~ 9.6,
    Paddock_ID == 52411 & Rate == 800 ~ 19.2,
    Paddock_ID == 52411 & Rate == 8050 ~ 42.2,
    Paddock_ID == 52411 & Rate == 80150 ~ 88.2,
    TRUE ~ as.double(Rate )))

details_2020 <- details_2020 %>% 
  mutate(Rate = case_when(
    Paddock_ID == 524102 & Rate == 33 ~ 3.3,
    Paddock_ID == 524102 & Rate == 5033 ~ 21.0,
    Paddock_ID == 524102 & Rate == 435033 ~ 40.78,
    Paddock_ID == 524102 & Rate == 875033 ~ 61.02,
    Paddock_ID == 524102 & Rate == 1305033 ~ 80.8,
    TRUE ~ as.double(Rate )))

  #######################################################################################

## bring in the t.test data results

set1_2020 <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/t_test_merged_3a.csv")

#names(set1_2020)
set1_2020 <- set1_2020 %>% 
  dplyr::select(Zone_ID,
                Rates = Rate,
                Yld = yield,
                P_value,
               Mean_diff,
               rounded,Significant,
               paddock_ID_Type)

t.test_2020 <- set1_2020


##make a paddock_ID clm
t.test_2020$length_zoneID <- nchar(t.test_2020$Zone_ID)
t.test_2020 <- t.test_2020 %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
t.test_2020$Paddock_ID <- as.double(t.test_2020$Paddock_ID)

t.test_2020 <- t.test_2020 %>%  
  mutate(part1=str_split_fixed(paddock_ID_Type,"_",2)[,1], Strip_Type=str_split_fixed(paddock_ID_Type,"_",2)[,2])
t.test_2020 <- t.test_2020 %>%
  dplyr::select(- part1)


## I want to join details_2019 to t.test 

# str(t.test_2020)
# str(details_2020)

t.test_2020 <- t.test_2020 %>% 
  dplyr::mutate(for_join = paste0(Paddock_ID, Strip_Type, Rates))

details_2020 <- details_2020 %>% 
  dplyr::mutate(for_join = paste0(Paddock_ID, Strip_Type, Rate))

t.test2020_details <- full_join(details_2020, t.test_2020)
names(t.test2020_details)


## drop the ones that seem to be exluded from analysis ie no conatct
t.test2020_details <- t.test2020_details %>% 
  filter(!is.na(Contact))






t.test2020_details <- t.test2020_details %>%
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


recom_rateDB2020 <- read_excel( "W:/value_soil_testing_prj/data_base/downloaded_sep2021/GRDC 2020 Paddock Database_SA_VIC_June11 2021.xlsx")
#str(recom_rateDB2020)
# select only a few clms with recommedation 
recom_rateDB2020 <- recom_rateDB2020 %>% 
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

recom_rateDB2020$n_rec_yld_low <- as.double(recom_rateDB2020$n_rec_yld_low)
recom_rateDB2020$n_rec_yld_med <- as.double(recom_rateDB2020$n_rec_yld_med)
recom_rateDB2020$n_rec_yld_high <- as.double(recom_rateDB2020$n_rec_yld_high)
recom_rateDB2020$Colwell <- as.double(recom_rateDB2020$Colwell)
recom_rateDB2020$DGT <- as.double(recom_rateDB2020$DGT)
recom_rateDB2020$PBI <- as.double(recom_rateDB2020$PBI)

recom_rateDB2020 <-  dplyr::mutate(recom_rateDB2020,  maxN = apply(recom_rateDB2020[4:6], 1, max, na.rm = TRUE))


# remove redunant clm and replace inf
recom_rateDB2020 <- recom_rateDB2020 %>% 
  mutate(
    maxN = case_when(
      maxN >= 0 ~ maxN,
      TRUE ~ NA_real_
    )
  )

recom_rateDB2020 <- recom_rateDB2020 %>% 
  dplyr::select(Zone_ID  ,
                p_rec ,
                maxN,
                Total_N,
                Colwell,
                DGT,
                PBI)

##make a paddock_ID clm
recom_rateDB2020$length_zoneID <- nchar(v$Zone_ID)
recom_rateDB2020 <- recom_rateDB2020 %>% 
  mutate(Paddock_ID =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))
recom_rateDB2020$Paddock_ID <- as.double(recom_rateDB2020$Paddock_ID)



rm(details_2020, t.test_2020)

### join the t.test data to the recom rates
# str(recom_rateDB2020)
# str(t.test2020_details)
recom_rateDB2020$Zone_ID <- as.double(recom_rateDB2020$Zone_ID)
t.test_details_rec_rates <- left_join(t.test2020_details, recom_rateDB2020)
rm(recom_rateDB2020, t.test2020_details)
#############################################################################################################
### Redo the rec rates for N with my rainfall zone

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(Rec_N_jax = case_when(
    rainfall_class == "low" & Total_N <= 80 ~ ((80 -Total_N)/0.5),
    rainfall_class == "medium" & Total_N <= 160 ~ ((160 -Total_N)/0.5),
    rainfall_class == "high" & Total_N <= 240 ~ ((240 -Total_N)/0.5),
    TRUE                           ~ 0  ))

#str(t.test_details_rec_rates)

# t.test_details_rec_rates <- t.test_details_rec_rates %>% 
#   mutate(critical_colwell = 4.6*( PBI^ (0.393)))
# ## is colwell greater than critical colwell?
# t.test_details_rec_rates <- t.test_details_rec_rates %>% 
#   mutate(colwell_thershold = case_when(
#     Colwell > critical_colwell ~ "adequate",
#     Colwell < critical_colwell ~ "p_required")  )
# 
# ## if p is required how much extra colwell p is needed to get to critical thershold?
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
write.csv(t.test_details_rec_rates, "W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/t.test_details_rec_rates_2020.csv")
