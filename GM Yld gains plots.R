library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(readr)
library(DT)
library(dplyr)

### This script will use the mergered file to plot and tabluate the econmic results.
## the sign values in this file relate to a comaprsion between the GSP and higher or lower rates.

df <- read.csv("W:/value_soil_testing_prj/Economics/2020/GSP_vs_other_withGM.csv")


### check GM

check_GM1 <- df %>%  count(rate_name) #number of zone with analysis done that have GR
check_GM1
check_GM2 <- df %>%
  filter(rate_name == "Grower_rate") %>%
  select(Zone_ID, input_file, paddock_code, Strip_Type, paddock_ID_Type)

check_GM3 <- check_GM2 %>%  distinct(paddock_ID_Type, .keep_all = TRUE)

check_GM3 %>%  count(Strip_Type)
count(check_GM3)


## Ans this question - when growers put on more P than the GSP what do the yield gains look like?


#1. calulate the difference between yld for higher rates vs yld for GSP (high rate - low rates)
#YLD_GSP_rate
#YLD_higher_than_GSP_rate
# df <- df %>% 
#   mutate(yld_diff_higher_rate = YLD_higher_than_GSP_rate-  YLD_GSP_rate,
#          yld_diff_lower_rate =  YLD_GSP_rate - YLD_lower_than_GSP_rate,
#          
#          GM_diff_higher_rate = GM_higher_than_GSP_rate-  GM_GSP_rate,
#          GM_diff_lower_rate = GM_GSP_rate - GM_lower_than_GSP_rate)
# this has heaps of duplication I just want one value per zone (and strip type)

plot_df <- df%>% 
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE)
#beacuse we have removed the zoneID and rate level of detail we should only keep some clm

names(plot_df)

plot_df <- plot_df %>%  dplyr::select(
  #"X" ,
  "Zone_ID" ,
  #"Rate",
  #"rate_name",
  #"GSP"  ,
  "Strip_Type"   ,
  #"Details" ,
  #"P_content" ,
  #"N_content"    ,
  "p_rec" ,
  "n_rec" ,
  "rec_rate_appox"  ,
  "Fld_Join_Approx1"  ,
 # "yield"     ,
  "av_rain"    ,
 # "Significant_practical" ,
  "organisati"  ,
  "contact" ,
  "farmer"  ,
  "paddock"  ,
  "input_file"  ,
  "paddock_code"  ,
  "paddock_ID_Type" ,
  "Status",
  #"rate_name_order"   ,
  #"N_P_content"        ,
  # "rec_rate_label" ,
  # "rec_rate_appox_value"    ,
  #"soil_test_says_P"     ,
  #"soil_test_says_N",
  #"join_zone_ID_Strip_Type" ,
  # "higher_than_GSP_label" ,
  # "the_GSP_label",
  # "lower_than_GSP_label"  ,
  # "high_lower_GSP",
  #"grain_income" ,
  #"cost_test"      ,
  "rainfall_class",
  #"variable_costs" ,
  #"Cost_P_N_dollar_ha" ,
  #"total_cost",
  #"GM"   ,
  "GM_higher_than_GSP_rate" ,
  "GM_GSP_rate",
  "GM_lower_than_GSP_rate",
  "YLD_higher_than_GSP_rate",
  "YLD_GSP_rate" ,
  "YLD_lower_than_GSP_rate" ,
  "cost_NP_higher_than_GSP_rate",
  "cost_NP_GSP_rate",
  "cost_NP_lower_than_GSP_rate",
  "yld_diff_higher_rate",
 "yld_diff_lower_rate",
 "GM_diff_higher_rate",
 "GM_diff_lower_rate"
 
)       


## how best to plot these histograms??
ggplot(plot_df, aes(yld_diff_higher_rate))+
  geom_histogram(bins = 40)+
  facet_wrap(.~Strip_Type)+
  theme_bw()+
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size= 1.0)+
  scale_x_continuous(breaks = seq(-5.0, 5.0, by = .5))+
labs(
  title = "Yield repsonse by applying more fert than the GSP ",
  y = "Count of zones",
  x = "yield t/ha",
  subtitle = "")

ggplot(plot_df, aes(GM_diff_higher_rate))+
  geom_histogram(bins = 40)+
  facet_wrap(.~Strip_Type)+
  theme_bw()+
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size= 1.0)+
  scale_x_continuous(breaks = seq(-300, 600, by = 50))+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))+
  labs(
    title = "GM repsonse by applying more fert than the GSP ",
    y = "Count of zones",
    x = "GM $/ha",
    subtitle = "")
