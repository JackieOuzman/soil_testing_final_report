
t.test_details_rec_rates <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/t.test_details_rec_rates_2019.csv")

str(t.test_details_rec_rates)


### difference between p rec and p applied

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(diff_p_rec_applied = abs(Total_sum_P_content- p_rec_jax))
closest_match <-t.test_details_rec_rates %>%
  group_by(Zone_ID) %>% 
  summarise(closest_match = min(diff_p_rec_applied))


#Add this back in

t.test_details_rec_rates <- left_join(t.test_details_rec_rates, closest_match)
t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(rec_rate_approx =
           case_when(
             closest_match == diff_p_rec_applied ~ "approx_rec_rate"))

unique(t.test_details_rec_rates$GSP)
t.test_details_rec_rates$GSP <- as.character(t.test_details_rec_rates$GSP)

str(t.test_details_rec_rates$GSP)

t.test_details_rec_rates <- t.test_details_rec_rates %>% 
  mutate(GSP_Rec_both = case_when(
    GSP == "GSP"  & rec_rate_approx == "approx_rec_rate" ~ "both",
    #GSP == "NA" & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    is.na(GSP)            & rec_rate_approx == "approx_rec_rate" ~ "rec_rate",
    GSP == "GSP"  & is.na(rec_rate_approx) ~ "GSP",
    TRUE ~ "other"
  ))
#######################################################################################################################################################
#### when the recom rate is different to the GPS then what are the average yields?

t.test_details_rec_rates %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>% 
  ggplot(aes(x = rainfall_class, y = Yld, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  #facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "yield t/ha")



t.test_details_rec_rates %>%  
  filter( GSP_Rec_both !=  "both" & GSP_Rec_both != "other") %>% 
  group_by(rainfall_class) %>% 
  summarise(n = n())

t.test_details_rec_rates %>%  
  filter( GSP_Rec_both !=  "both" & 
            GSP_Rec_both != "other" &
            !is.na(rainfall_class)   ) %>%  
  group_by(rainfall_class,  GSP_Rec_both) %>% 
  summarise(yld_av = mean(Yld),
            yld_median = median(Yld))
