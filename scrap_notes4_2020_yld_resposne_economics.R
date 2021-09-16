
library(tidyverse)
library(ggplot2)

library(formattable)
library(sf)
library(readxl)



t.test_details_rec_rates <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/t.test_details_rec_rates_2019.csv")

str(t.test_details_rec_rates)


### difference between p rec and p applied

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(diff_p_rec_applied = abs(Total_sum_P_content- p_rec_jax))
closest_match_p <-t.test_details_rec_rates %>%
  group_by(Zone_ID) %>% 
  summarise(closest_match_p = min(diff_p_rec_applied))


#Add this back in

t.test_details_rec_rates <- left_join(t.test_details_rec_rates, closest_match_p)
t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(rec_rate_approx_p =
           case_when(
             closest_match_p == diff_p_rec_applied ~ "approx_rec_rate"))

unique(t.test_details_rec_rates$GSP)
t.test_details_rec_rates$GSP <- as.character(t.test_details_rec_rates$GSP)

str(t.test_details_rec_rates$GSP)

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(GSP_Rec_both_p = case_when(
    GSP == "GSP"  & rec_rate_approx_p == "approx_rec_rate" ~ "both",
    is.na(GSP)            & rec_rate_approx_p == "approx_rec_rate" ~ "rec_rate",
    GSP == "GSP"  & is.na(rec_rate_approx_p) ~ "GSP",
    TRUE ~ "other"
  ))


### difference between N rec and N applied
names(t.test_details_rec_rates)

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(diff_n_rec_applied = abs(Total_sum_N_content- Rec_N_jax))

closest_match_n <-t.test_details_rec_rates %>%
  group_by(Zone_ID) %>% 
  summarise(closest_match_n = min(diff_n_rec_applied))


#Add this back in


t.test_details_rec_rates <- left_join(t.test_details_rec_rates, closest_match_n)
str(t.test_details_rec_rates)
t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(rec_rate_approx_n =
           case_when(
             closest_match_n == diff_n_rec_applied ~ "approx_rec_rate"))



t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(GSP_Rec_both_n = case_when(
    GSP == "GSP"  & rec_rate_approx_n == "approx_rec_rate" ~ "both",
    is.na(GSP)            & rec_rate_approx_n == "approx_rec_rate" ~ "rec_rate",
    GSP == "GSP"  & is.na(rec_rate_approx_n) ~ "GSP",
    TRUE ~ "other"
  ))








#### arrange the data so that I have the order of detail that reflects it.
names(t.test_details_rec_rates)
t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  dplyr::select(

#Tier 1
Paddock_ID,
Strip_Type,
Organisation,
Contact,
Farmer,
Paddock_tested,
rainfall_class,
AGROECOLOG,

#Tier 2
Zone_ID,
Total_N,
Colwell,
DGT,
PBI,
p_rec,
maxN,
Rec_N_jax,
p_rec_jax,
 
#Tier 3
Rate,
GSP,
Total_sum_N_content,
Total_sum_P_content,
Yld,
P_value,
Mean_diff,
rounded,
Significant,

#Tier 3 details# 
diff_p_rec_applied,
closest_match_p,
rec_rate_approx_p,
GSP_Rec_both_p,
diff_n_rec_applied,
closest_match_n,
rec_rate_approx_n,
GSP_Rec_both_n
)    



##############################################################################
### add some GM to yld results 
##############################################################################
# convert the yield to income of grain $/ha
# this assumes that all yield is in t /ha 
# all crops are wheat
# the 5 year wheat average is $286 and is same for all sites.
names(df)
t.test_details_rec_rates <- t.test_details_rec_rates %>% mutate(grain_income = Yld * 286)

##############################################################################
## cost for test $3 per ha for rates the approx rate here some GSP are also approx rec rate labelled as both

names(t.test_details_rec_rates)
unique(t.test_details_rec_rates$GSP_Rec_both_p)
unique(t.test_details_rec_rates$GSP_Rec_both_n)

t.test_details_rec_rates <- t.test_details_rec_rates %>% mutate(cost_test = case_when(
  GSP_Rec_both_p == "rec_rate" ~ 3.00,
  GSP_Rec_both_p == "both" ~ 3.00,
  
  GSP_Rec_both_n == "rec_rate" ~ 3.00,
  GSP_Rec_both_n == "both" ~ 3.00,
  TRUE ~ 0.00))

## cost fert is based on rainfall class - define the rainfall class

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  dplyr::mutate(
    variable_costs = case_when(
      Strip_Type == "P Strip" & rainfall_class == "low" ~     194,
      Strip_Type == "P Strip" & rainfall_class == "medium" ~  358,
      Strip_Type == "P Strip" & rainfall_class == "high" ~    540,
      
      Strip_Type == "N Strip" & rainfall_class == "low" ~     220,
      Strip_Type == "N Strip" & rainfall_class == "medium" ~  340,
      Strip_Type == "N Strip" & rainfall_class == "high" ~    498))

### I don't get this step but it converts N applied from kg/ha to cost of N $ha
str(t.test_details_rec_rates)
t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  dplyr::mutate(
    Cost_P_N_dollar_ha  = case_when(
      Strip_Type == "P Strip"  ~     Total_sum_P_content * 2.9,
      Strip_Type == "N Strip"  ~     Total_sum_N_content * 1.1))     

## GM
#GM = Income grain – cost test – variable cost – cost of N


t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  dplyr::mutate(
    total_cost = cost_test + variable_costs + Cost_P_N_dollar_ha,
    GM  = grain_income - total_cost)

names(t.test_details_rec_rates)
all_step_t.test_details_rec_rates <- t.test_details_rec_rates
neat_t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  dplyr::select(-diff_p_rec_applied,
                -diff_n_rec_applied,
                -closest_match_p,
                -closest_match_n,
                -grain_income,
                -cost_test,
                -variable_costs,
                -Cost_P_N_dollar_ha,
                -total_cost  )

write.csv(neat_t.test_details_rec_rates, "W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/GM_t.test_details_rec_rates2019.csv")



