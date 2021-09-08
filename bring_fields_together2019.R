library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(readr)
library(DT)
library(dplyr)

##########################################################################################################################################
## step 1 make a list of zone and a list of paddocks that will be included in the analysis 
#1a. zones
##########################################################################################################################################



##########################################################################################################################################
## step 1 make a list of zone and a list of paddocks that will be included in the analysis 
#1b. paddock codes
##########################################################################################################################################

###################################################################################################################################
# summary of 1a and 1b I have 2 list of zones to include and padocks to include in analysis


##########################################################################################################################################
## step 1 bring in the t.test data 
#1. simple comparision of GSP vs other rates
##########################################################################################################################################

#GS_rates_3a <- read.csv("W:/value_soil_testing_prj/Yield_data/processing/yld_results.csv")
GS_rates_3a <- read_excel("W:/value_soil_testing_prj/Yield_data/processing/All sites partial GM 061120.xlsx", 
                          sheet = "P exclude neg resp plus p")

names(GS_rates_3a)
GS_rates_3a <- GS_rates_3a %>% 
  dplyr::select(

Zone_ID =`Paddock code (Zone ID)`,
Contact,
Farmer,
Paddock_tested,
rainfall_class = `Rainfall Class`,

P_rec = `P rec`,
N_rec_3 = `N Rec (< 3 t/ha)`,
N_rec_3_5 = `N Rec (3-5 t/ha)`,
N_rec_5 = `N Rec (> 5 t/ha)`,

Rate = `Fert Rates kg/ha`,
P_contnet = `Nutrient rate (kg P/ha)`,

GSP = rate_name,

yield = `Yld t/ha`,
GM = `GM ($/ha)`,
)

GS_rates_3a <- GS_rates_3a %>% 
  mutate(Strip_type = "P",
         Zone_ID_rate_type = paste0(Zone_ID, "_",Rate, "_", Strip_type))

## look for duplicates and remove - this tsep might be a problem??

GS_rates_3a <- distinct(GS_rates_3a, Zone_ID_rate_type, .keep_all = TRUE)


### difference between p rec and p applied

GS_rates_3a <- GS_rates_3a %>% 
  mutate(diff_p_rec_applied = abs(P_contnet- P_rec))



closest_match <-GS_rates_3a %>%
   group_by(Zone_ID) %>% 
   summarise(closest_match = min(diff_p_rec_applied))

# closest_match <- closest_match %>% 
#   mutate(closest_match = "closest_match")

#Add this back in
str(test)
GS_rates_3a <- left_join(GS_rates_3a, closest_match)
GS_rates_3a <- GS_rates_3a %>% 
  mutate(rec_rate_approx =
  case_when(
    closest_match == diff_p_rec_applied ~ "approx_rec_rate"))


str(GS_rates_3a)
unique(GS_rates_3a$GSP)

GS_rates_3a <- GS_rates_3a %>% 
  mutate(GSP_Rec_both = case_when(
    GSP == "Grower_rate"  & rec_rate_approx == "approx_rec_rate" ~ "both",
    GSP == "below grower" & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    GSP == "above grower" & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    is.na(GSP)            & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    GSP == "Grower_rate"  & is.na(rec_rate_approx) ~ "GSP",
    TRUE ~ "other"
    
  ))

str(GS_rates_3a)


GS_rates_3a %>%  
  filter( GSP_Rec_both !=  "both" & 
          GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>% 
  ggplot(aes(x = rainfall_class, y = yield, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  #facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "yield t/ha")


GS_rates_3a %>%  
  filter( GSP_Rec_both !=  "both" & GSP_Rec_both != "other") %>% 
  group_by(rainfall_class) %>% 
  summarise(n = n())

GS_rates_3a %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>%  
  group_by(rainfall_class,  GSP_Rec_both) %>% 
  summarise(yld_av = mean(yield),
            yld_median = median(yield))






###################### N ############################################################################


GS_rates_3a_N <- read_excel("W:/value_soil_testing_prj/Yield_data/processing/All sites partial GM 061120.xlsx", 
                          sheet = "nitrogen")

names(GS_rates_3a_N)
GS_rates_3a_N <- GS_rates_3a_N %>% 
  dplyr::select(
    
    Zone_ID =`Paddock code (Zone ID)`,
    Contact,
    Farmer,
    Paddock_tested,
    rainfall_class = `Rainfall Class`,
    
    P_rec = `P rec`,
    N_rec_3 = `N Rec (< 3 t/ha)`,
    N_rec_3_5 = `N Rec (3-5 t/ha)`,
    N_rec_5 = `N Rec (> 5 t/ha)`,
    
    Rate = `Fert Rate`,
    N_contnet = `N applied (kg/ha)`,
    
    GSP = rate_name,
    
    yield = `Yld (t/ha)`,
    GM = `GM ($/ha)`,
  )

GS_rates_3a_N <- GS_rates_3a_N %>% 
  mutate(Strip_type = "N",
         Zone_ID_rate_type = paste0(Zone_ID, "_",Rate, "_", Strip_type))

## look for duplicates and remove - this tsep might be a problem??

GS_rates_3a_N <- distinct(GS_rates_3a_N, Zone_ID_rate_type, .keep_all = TRUE)


## what is the N rec rate


GS_rates_3a_N <-  dplyr::mutate(GS_rates_3a_N,  maxN = apply(GS_rates_3a_N[7: 9], 1, max, na.rm = TRUE))



### difference between n rec and n applied

GS_rates_3a_N <- GS_rates_3a_N %>% 
  mutate(diff_n_rec_applied = abs(N_contnet- maxN))



closest_match_N <-GS_rates_3a_N %>%
  group_by(Zone_ID) %>% 
  summarise(closest_match = min(diff_n_rec_applied))



#Add this back in
str(test)
GS_rates_3a_N <- left_join(GS_rates_3a_N, closest_match_N)
GS_rates_3a_N <- GS_rates_3a_N %>% 
  mutate(rec_rate_approx =
           case_when(
             closest_match == diff_n_rec_applied ~ "approx_rec_rate"))


str(GS_rates_3a_N)
unique(GS_rates_3a_N$GSP)

GS_rates_3a_N <- GS_rates_3a_N %>% 
  mutate(GSP_Rec_both = case_when(
    GSP == "Grower_rate"  & rec_rate_approx == "approx_rec_rate" ~ "both",
    GSP == "below grower" & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    GSP == "above grower" & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    is.na(GSP)            & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    GSP == "Grower_rate"  & is.na(rec_rate_approx) ~ "GSP",
    TRUE ~ "other"
    
  ))

str(GS_rates_3a_N)


GS_rates_3a_N %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>% 
  ggplot(aes(x = rainfall_class, y = yield, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  #facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "yield t/ha")


GS_rates_3a_N %>%  
  filter( GSP_Rec_both !=  "both" & GSP_Rec_both != "other") %>% 
  group_by(rainfall_class) %>% 
  summarise(n = n())

GS_rates_3a_N %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>%  
  group_by(rainfall_class,  GSP_Rec_both) %>% 
  summarise(yld_av = mean(yield),
            yld_median = median(yield))


str(GS_rates_3a)
str(GS_rates_3a_N)

GS_rates_3a_plot <- GS_rates_3a %>% 
  dplyr::select(rainfall_class, Strip_type, GSP_Rec_both, yield , GM)

GS_rates_3a_N_plot <- GS_rates_3a_N %>% 
  dplyr::select(rainfall_class, Strip_type, GSP_Rec_both, yield , GM)


GS_rates_3a_N_P_plot <- rbind(GS_rates_3a_plot, GS_rates_3a_N_plot)


GS_rates_3a_N_P_plot$rainfall_class <- factor(GS_rates_3a_N_P_plot$rainfall_class, levels = c("Low", "Medium", "High"))

GS_rates_3a_N_P_plot %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>% 
  ggplot(aes(x = rainfall_class, y = yield, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  facet_wrap(.~Strip_type)+
  labs(x = "rainfall class", y = "yield t/ha")



GS_rates_3a_N_P_plot %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>%  
  group_by(rainfall_class,  GSP_Rec_both, Strip_type) %>% 
  summarise(yld_av = mean(yield)) %>% 
  arrange(Strip_type)



GS_rates_3a_N_P_plot %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>% 
  ggplot(aes(x = rainfall_class, y = GM, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  facet_wrap(.~Strip_type)+
  labs(x = "rainfall class", y = "GM $/ha")



GS_rates_3a_N_P_plot %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>%  
  group_by(rainfall_class,  GSP_Rec_both, Strip_type) %>% 
  summarise(yld_av = mean(GM)) %>% 
  arrange(Strip_type)





######################################################################################################################################

#### Yld ####

str(GS_rates_3a)

str(GS_rates_3a_N)

GS_rates_3a_yld_gain <- GS_rates_3a %>% 
  dplyr::select(Zone_ID, rainfall_class, 
                Strip_type, GSP_Rec_both, yield , GM)

GS_rates_3a_N_yld_gain <- GS_rates_3a_N %>% 
  dplyr::select(Zone_ID, rainfall_class, 
                Strip_type, GSP_Rec_both, yield , GM)

GS_rates_3a_P_N_yld_gain <- rbind(GS_rates_3a_yld_gain,GS_rates_3a_N_yld_gain)


str(GS_rates_3a_P_N_yld_gain)
high_yld_1 <- GS_rates_3a_P_N_yld_gain %>% 
  select(Zone_ID, rainfall_class,GSP_Rec_both, yield, Strip_type )
high_yld_1 <- high_yld_1 %>% 
  filter( GSP_Rec_both !=  "both" ) 
high_yld_1 <- as.data.frame(high_yld_1)
str(high_yld_1)

high_yld_1 %>% 
  group_by(GSP_Rec_both) %>% 
  summarise(count = n())

# high_yld_1 <-  high_yld_1 %>% 
#   mutate(Zone_ID_GSP_Rec_both = paste0(Zone_ID,GSP_Rec_both ))

## I need to make the data wide again. I dont know why pivot wide isnt working
high_yld_1a <- high_yld_1 %>% 
  filter( GSP_Rec_both == "GSP")
high_yld_1b <- high_yld_1 %>% 
  filter( GSP_Rec_both == "rec_rate")


high_yld_1a <- rename(high_yld_1a, yield_GSP = yield )
high_yld_1b <- rename(high_yld_1b, yield_rec_rate = yield )
str(high_yld_1b)
high_yld_1b <- select(high_yld_1b,Zone_ID, yield_rec_rate )

high_yld_1ab <- left_join(high_yld_1a,high_yld_1b )
high_yld_1ab <- select(high_yld_1ab, -GSP_Rec_both )
str(high_yld_1ab)
## the yield difference between Rec rate and GSP 
high_yld_1ab <- high_yld_1ab %>% 
  mutate(Rec_rate_v_GSP = yield_rec_rate -yield_GSP) 

## create a  clm for yld loss gain

high_yld_1ab <- high_yld_1ab %>% 
  mutate(Yld_loss_gain = case_when(
    Rec_rate_v_GSP <= 0 ~ "loss",
    Rec_rate_v_GSP > 0 ~ "gain"
  ))
str(high_yld_1ab)
high_yld_1ab$Rec_rate_v_GSP <- as.double(high_yld_1ab$Rec_rate_v_GSP)

high_yld_1ab %>% group_by(Yld_loss_gain, rainfall_class, Strip_type ) %>%
  summarise(av_yld = mean(Rec_rate_v_GSP, na.rm = FALSE)) %>% 
  arrange(Strip_type, Yld_loss_gain,rainfall_class )





#### GM ####

str(GS_rates_3a_P_N_yld_gain)
high_GM_1 <- GS_rates_3a_P_N_yld_gain %>% 
  select(Zone_ID, rainfall_class,GSP_Rec_both, GM, Strip_type )
high_GM_1 <- high_GM_1 %>% 
  filter( GSP_Rec_both !=  "both" ) 
high_GM_1 <- as.data.frame(high_GM_1)
str(high_GM_1)

high_GM_1 %>% 
  group_by(GSP_Rec_both) %>% 
  summarise(count = n())



## I need to make the data wide again. I dont know why pivot wide isnt working
high_GM_1a <- high_GM_1 %>% 
  filter( GSP_Rec_both == "GSP")
high_GM_1b <- high_GM_1 %>% 
  filter( GSP_Rec_both == "rec_rate")


high_GM_1a <- rename(high_GM_1a, GM_GSP = GM )
high_GM_1b <- rename(high_GM_1b, GM_rec_rate = GM )
str(high_GM_1b)
high_GM_1b <- select(high_GM_1b,Zone_ID, GM_rec_rate )

high_GM_1ab <- left_join(high_GM_1a,high_GM_1b )
high_GM_1ab <- select(high_GM_1ab, -GSP_Rec_both )
str(high_GM_1ab)
## the yield difference between Rec rate and GSP 
high_GM_1ab <- high_GM_1ab %>% 
  mutate(Rec_rate_v_GSP = GM_rec_rate -GM_GSP) 

## create a  clm for yld loss gain

high_GM_1ab <- high_GM_1ab %>% 
  mutate(GM_loss_gain = case_when(
    Rec_rate_v_GSP <= 0 ~ "loss",
    Rec_rate_v_GSP > 0 ~ "gain"
  ))
str(high_GM_1ab)
high_GM_1ab$Rec_rate_v_GSP <- as.double(high_GM_1ab$Rec_rate_v_GSP)

high_GM_1ab %>% group_by(GM_loss_gain, rainfall_class, Strip_type ) %>%
  summarise(av_GM = mean(Rec_rate_v_GSP, na.rm = FALSE)) %>% 
  arrange(Strip_type, GM_loss_gain,rainfall_class )



