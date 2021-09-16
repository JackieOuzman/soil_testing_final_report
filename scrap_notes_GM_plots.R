### plots that I might need I have looked this up in 2 script file summary_plots_ws_june and GM Yld gains plots
##########################################################################################################################################

GM_t.test_details_rec_rates2019 <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/GM_t.test_details_rec_rates2019.csv")
GM_t.test_details_rec_rates2019 <- GM_t.test_details_rec_rates2019 %>%  dplyr::select(-X)
names(GM_t.test_details_rec_rates2019)

### Duplication may have crept in ?? I only want one zoneID strip type and rate
GM_t.test_details_rec_rates2019 <- GM_t.test_details_rec_rates2019 %>% 
  distinct(Zone_ID, Strip_Type,Rate, .keep_all = TRUE)
### the GM has some negative values lets recode these as zero

min(GM_t.test_details_rec_rates2019$GM)
GM_t.test_details_rec_rates2019 <- GM_t.test_details_rec_rates2019 %>% 
  mutate(GM = case_when(
    GM < 0 ~ 0,
    TRUE ~ GM
  ))


#### recommednation for P in zones that have strips
###########################   P    ###########################################
GM_t.test_details_rec_rates2019_P <- GM_t.test_details_rec_rates2019 %>% 
  dplyr::filter(Strip_Type == "P Strip")
P_zone <- GM_t.test_details_rec_rates2019_P %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>% 
  dplyr::select(Paddock_ID:p_rec_jax)

ggplot(P_zone, aes(x=p_rec_jax)) +
  geom_histogram(aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #facet_wrap(.~ rainfall_class_f)+
  theme_bw() +
  geom_vline(
    xintercept = 5,
    linetype = "dotted",
    color = "red",
    size = 2.0
  ) +
  #scale_x_continuous(breaks = seq(-5.0, 5.0, by = .5))+
  labs(
    title = "Recommendation for P",
    y = "Frequency of zones",
    x = "Soil Test Fertiliser Recommendation (kg P/ha)"
    #subtitle = "only including zones used in economic analysis (n =103)"
  )+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
###########################   N    ###########################################
GM_t.test_details_rec_rates2019_N <- GM_t.test_details_rec_rates2019 %>% 
  dplyr::filter(Strip_Type == "N Strip")
N_zone <- GM_t.test_details_rec_rates2019_N %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>% 
  dplyr::select(Paddock_ID:p_rec_jax)
names(N_zone)

ggplot(N_zone, aes(x=Rec_N_jax)) +
  geom_histogram(aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #facet_wrap(.~ rainfall_class_f)+
  theme_bw() +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    color = "red",
    size = 2.0
  ) +
  #scale_x_continuous(breaks = seq(-5.0, 5.0, by = .5))+
  labs(
    title = "Recommendation for N",
    y = "Frequency of zones",
    x = "Soil Test Fertiliser Recommendation (kg N/ha)")+
  #subtitle = "only including zones used in economic analysis (n =70)"
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
##########################################################################################################################################
### might need something like this ???

names(GM_t.test_details_rec_rates2019)
unique(GM_t.test_details_rec_rates2019$GSP)

GM_t.test_details_rec_rates2019 <- GM_t.test_details_rec_rates2019 %>% mutate(soil_test_says_P = case_when(
  Strip_Type == "P Strip" & GSP == "GSP"& Total_sum_P_content < p_rec_jax ~  "add_more_P",
  Strip_Type == "P Strip" & GSP == "GSP"& Total_sum_P_content == p_rec_jax ~ "no_change",
  Strip_Type == "P Strip" & GSP == "GSP"& Total_sum_P_content > p_rec_jax ~  "less_P",
))

GM_t.test_details_rec_rates2019 <- GM_t.test_details_rec_rates2019 %>% mutate(soil_test_says_N = case_when(
  Strip_Type == "N Strip" & GSP == "GSP"& Total_sum_N_content < Rec_N_jax ~  "add_more_N",
  Strip_Type == "N Strip" & GSP == "GSP"& Total_sum_N_content == Rec_N_jax ~ "no_change",
  Strip_Type == "N Strip" & GSP == "GSP"& Total_sum_N_content > Rec_N_jax ~  "less_N",
))

### for how many zones does the soil suggest more fert is required than current GSP is getting?
P_zones <- GM_t.test_details_rec_rates2019 %>%  filter(Strip_Type == "P Strip") %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>% 
  summarise(count = n())

GM_t.test_details_rec_rates2019 %>%  filter(Strip_Type == "P Strip") %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>% 
  group_by(soil_test_says_P) %>% 
  summarise(count = (n() / P_zones)*100)

N_zones <- GM_t.test_details_rec_rates2019 %>%  filter(Strip_Type == "N Strip") %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>% 
  summarise(count = n())

GM_t.test_details_rec_rates2019 %>%  filter(Strip_Type == "N Strip") %>% 
  distinct(Zone_ID, .keep_all = TRUE) %>% 
  group_by(soil_test_says_N) %>% 
  summarise(count = (n() / N_zones)*100)

##########################################################################################################################################
### GM plots approx rec vs the GSP when the GSP is not equal to the approx rate.
##########################################################################################################################################

#### P #####
### remove all the zones / strips that are classed as other, ie they are not the GSP or the approx rec rate.
unique(GM_t.test_details_rec_rates2019$GSP_Rec_both_p)
P_approx_GSP_zone_strip <- GM_t.test_details_rec_rates2019 %>%  
  filter(Strip_Type == "P Strip") %>% 
  filter(GSP_Rec_both_p !=  "other")
str(P_approx_GSP_zone_strip)

P_approx_GSP_zone_strip$rainfall_class <- factor(P_approx_GSP_zone_strip$rainfall_class, levels = c("low", "medium", "high"))

# P_approx_GSP_zone_strip %>%  
#   filter( GSP_Rec_both_p !=  "both" ) %>% 
#   ggplot(aes(x = rainfall_class, y = GM, color =GSP_Rec_both_p)) +
#   geom_point(position = position_dodge(width=0.75)) +
#   geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both_p)) +
#   theme_bw()+
#   facet_wrap(.~Strip_Type)+
#   labs(x = "rainfall class", y = "GM $/ha")


P_approx_GSP_zone_strip %>%  
  filter( GSP_Rec_both_p !=  "both" ) %>% 
  ggplot(aes(x = rainfall_class, y = GM, fill = GSP_Rec_both_p)) +
  geom_col(position = "dodge")+
  theme_bw()+
  labs(x = "rainfall class", y = "GM $/ha")

#### N #####
N_approx_GSP_zone_strip <- GM_t.test_details_rec_rates2019 %>%  
  filter(Strip_Type == "N Strip") %>% 
  filter(GSP_Rec_both_n !=  "other")


N_approx_GSP_zone_strip$rainfall_class <- factor(N_approx_GSP_zone_strip$rainfall_class, levels = c("low", "medium", "high"))


N_approx_GSP_zone_strip %>%  
  filter( GSP_Rec_both_n !=  "both" ) %>% 
  ggplot(aes(x = rainfall_class, y = GM, fill = GSP_Rec_both_n)) +
  geom_col(position = "dodge")+
  theme_bw()+
  labs(x = "rainfall class", y = "GM $/ha")
