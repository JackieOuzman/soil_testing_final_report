## June 2021 work

library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(readr)
library(DT)
library(dplyr)



df <- read.csv("W:/value_soil_testing_prj/Economics/2020/GSP_vs_other_withGM.csv")


### how many zone are there when the soil test says increase P
df$Strip_Type <- as.character(df$Strip_Type)
df$rec_rate_appox <- as.double(df$rec_rate_appox)

names(df)

plot_soil_test_info <- df %>% 
  dplyr::select("Zone_ID"  = "Zone_ID.x",
         "Strip_Type" = "Strip_Type.y",
         "join_zone_ID_Strip_Type",
         "rec_rate_appox_value", "soil_test_says_P", "soil_test_says_N",
         "p_rec",
         "n_rec")
plot_soil_test_info_N <- plot_soil_test_info %>% 
  filter(!is.na(soil_test_says_N))

plot_soil_test_info_N %>% 
  group_by(soil_test_says_N) %>% 
  summarise(count = n())
count(plot_soil_test_info_N)


plot_soil_test_info_P <- plot_soil_test_info %>% 
  filter(!is.na(soil_test_says_P))

plot_soil_test_info_P %>% 
  group_by(soil_test_says_P) %>% 
  summarise(count = n())
count(plot_soil_test_info_P)


########################################################################################################################

#Plots for recom higher rates

high_low <- read.csv("W:/value_soil_testing_prj/Economics/2020/GSP_vs_high_low_withGM.csv")
#high_low <- GS_high_low_3d
names(high_low)
unique(high_low$comparison)


## just the higher rate comparision
high <- high_low %>% 
  filter(comparison == "GSP_v_high")

high$rainfall_class <- factor(high$rainfall_class, levels = c("low", "medium", "high"))

## summary stats for the higher rate vs GSP

high %>% 
  group_by(Strip_Type, rainfall_class) %>% 
  summarise(count = n())
count(high)

high %>% 
  group_by(Strip_Type, rainfall_class) %>% 
              summarise( 
              mean_yld_GSP = round(mean(the_GSP, na.rm = TRUE),2),
              mean_yld_higher_rate = round(mean(higher_than_GSP, na.rm = TRUE),2))

high <- high %>% 
  mutate(higher_vs_GSP = GSP_vs_higher * -1)


    
high %>% 
  group_by(Strip_Type, rainfall_class) %>% 
     summarise(
       mean_GSP_vs_higher = mean(higher_vs_GSP),
       sd_GSP_vs_higher = sd(higher_vs_GSP),
       n_GSP_vs_higher = n(),
       sderror_GSP_vs_higher = sd_GSP_vs_higher/ sqrt(n_GSP_vs_higher)) %>% 
  arrange(Strip_Type, rainfall_class)
      
## end of summary stats


## as a plot

names(high)
#my cal in GSP_vs_higher clm is yld for GSP rate  - yield for higher rate
#so I need to flip my sign
high_plot <- high %>% 
  mutate(higher_vs_GSP = GSP_vs_higher * -1)
  
#1. boxplot of yield differences
ggplot(high_plot, aes(rainfall_class, higher_vs_GSP))+
  geom_point()+
  geom_boxplot(alpha=0.1)+
  facet_wrap(.~ Strip_Type)+
  theme_bw()+
  labs(x = "rainfall class", y = "yield difference GSP rate - higher fert rate")

names(high_plot)

#2. boxplot of GM differences
ggplot(high_plot, aes(rainfall_class, GM_diff_higher_rate))+
  geom_point()+
  geom_boxplot(alpha=0.1)+
  facet_wrap(.~ Strip_Type)+
  theme_bw()+
  labs(x = "rainfall class", y = "GM difference GSP rate vs higher fert rate")

#3. histogram plot of GM differences
ggplot(high_plot, aes( GM_diff_higher_rate))+
  geom_histogram(aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_histogram()+
  facet_wrap(.~ Strip_Type)+
  theme_bw()+
  labs(x = "GM difference GSP rate vs higher fert rate", y = "zones")+
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    color = "red",
    size = 1.0
  ) 

#### Yld ####

str(high_plot)
high_yld <- high_plot %>% 
  select(rainfall_class,the_GSP, higher_than_GSP, Strip_Type )
str(high_yld)
## put the Yld into one clm to plot
high_yld <- high_yld %>% 
  pivot_longer(cols = c(the_GSP, higher_than_GSP),
               names_to =  "fert_rate", 
               values_to = "yield")

## change the order of fert rate
str(high_yld)
unique(high_yld$fert_rate)
high_yld$fert_rate <- factor(high_yld$fert_rate, levels = c("the_GSP", "higher_than_GSP"))

#4. boxplot of actual yield

high_yld %>%   
  ggplot(aes(x = rainfall_class, y = yield, color =fert_rate)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = fert_rate)) +
  theme_bw()+
  facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "yield t/ha")
  
### GM ##### 
str(high_plot)
high_GM <- high_plot %>% 
  select(rainfall_class,GM_GSP_rate, GM_higher_than_GSP_rate, Strip_Type )
str(high_GM)
## put the Yld into one clm to plot
high_GM <- high_GM %>% 
  pivot_longer(cols = c(GM_GSP_rate, GM_higher_than_GSP_rate),
               names_to =  "fert_rate", 
               values_to = "GM")

 
#5. boxplot of actual GM 
names(high_GM)

high_GM %>%   
  ggplot(aes(x = rainfall_class, y = GM, color =fert_rate)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = fert_rate)) +
  theme_bw()+
  facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "GM $/ha")


 #########################################################################################
## summary stats on what is classified as having a positive yield resposne 
# a positive yield resposne is one that is:
#- yield increases by applying more fert and is significant
#- if yield increases by applying more fert but is not significant then no_response_sig etc..
str(high)
unique(high$yld_response_sig)

high %>% 
  group_by(Strip_Type ) %>% 
  summarise(
    count = n())
high %>% 
  group_by(Strip_Type , yld_response_sig) %>% 
  summarise(
    n_yld_response_sig = n())
## average yield gains recode the negative values to zero 
high <- high %>%
  mutate(
    yld_gains_higher_rate = case_when(
    higher_vs_GSP > 0 ~ higher_vs_GSP,
    TRUE ~ 0))

str(high)
high$GM_diff_higher_rate <- as.double(high$GM_diff_higher_rate)
high <- high %>%
  mutate(
    GM_gains_higher_rate = case_when(
      GM_diff_higher_rate > 0 ~ GM_diff_higher_rate,
      TRUE ~ 0))

high %>% 
  group_by(Strip_Type , yld_response_sig) %>% 
  summarise(mean_yld_gains = mean(yld_gains_higher_rate, na.rm = TRUE))

high %>% 
  group_by(Strip_Type , yld_response_sig) %>% 
  summarise(mean_GM_gains_higher_rate = mean(GM_gains_higher_rate, na.rm = TRUE))

high %>% 
  group_by(Strip_Type , yld_response_sig) %>% 
  summarise(mean_GM_diff_higher_rate = mean(GM_diff_higher_rate, na.rm = TRUE))


###################################################################################
## boxplots for GM or Yld for 2 groups a)the GSP b) the rec rate

## need to do some work to get this.
names(df)
df_subset <- df %>% 
  dplyr::select(Zone_ID = Zone_ID.x,
                Strip_Type,
                join_zone_ID_Strip_Type,
                P_content,
                N_content,
                N_P_content,
                p_rec,
                n_rec,
                rec_rate_label,
                GSP,
                #high_lower_GSP,
                rainfall_class,
                yield,
                GM)
                
## keep only GPS rows and rec_rate_label                
# test <- df_subset %>% 
#   filter(!is.na(GSP) &
#            !is.na(rec_rate_label))
#                 
df_subset %>% replace(is.na(GSP), "no_value")                
str(df_subset)

df_subset$GSP <- as.character(df_subset$GSP)
df_subset$rec_rate_label <- as.character(df_subset$rec_rate_label)
unique(df_subset$GSP)



df_subset <- df_subset %>% 
  mutate(GSP_Rec_both = case_when(
    GSP == "GSP" & rec_rate_label == "closest_match" ~ "both",
    is.na(GSP)  & rec_rate_label == "closest_match" ~ "rec_rate",
    GSP == "GSP"  & is.na(rec_rate_label) ~ "GSP",
    TRUE ~ "other"
    
  ))

#quick chcek on zones - yip this looks good 173
df_subset %>%  
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>% 
  count()
# remove row that arent the GSP or the rec rate               
unique(df_subset$GSP_Rec_both)


df_subset <- df_subset %>%
  filter( GSP_Rec_both !=  "other" )
    


#check
df_subset %>%  
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>% 
  group_by(GSP_Rec_both) %>% 
  summarise(count = n())
names(df_subset)  

### need to check this is correct
df_subset %>%  
  filter( GSP_Rec_both !=  "both" ) %>% 
  count()
check <- df_subset %>%  
  filter( GSP_Rec_both !=  "both" )

df_subset$rainfall_class <- factor(df_subset$rainfall_class, levels = c("low", "medium", "high"))

df_subset %>%  
  filter( GSP_Rec_both !=  "both" ) %>% 
  ggplot(aes(x = rainfall_class, y = yield, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "yield t/ha")

df_subset %>%  
  filter( GSP_Rec_both !=  "both" ) %>% 
  group_by(rainfall_class, Strip_Type, GSP_Rec_both) %>% 
  summarise(yld_av = mean(yield),
            yld_median = median(yield))


df_subset %>%  
  filter( GSP_Rec_both !=  "both" ) %>% 
  ggplot(aes(x = rainfall_class, y = GM, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "GM $/ha")

df_subset %>%  
  filter( GSP_Rec_both !=  "both" ) %>% 
  filter( Strip_Type ==  "N Strip" ) %>% 
  ggplot(aes(x = rainfall_class, y = GM, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  #facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "GM $/ha")

df_subset %>%  
  filter( GSP_Rec_both !=  "both" ) %>% 
  filter( Strip_Type ==  "P Strip" ) %>% 
  ggplot(aes(x = rainfall_class, y = GM, color =GSP_Rec_both)) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
  theme_bw()+
  #facet_wrap(.~Strip_Type)+
  labs(x = "rainfall class", y = "GM $/ha")

df_subset %>%  
  filter( GSP_Rec_both !=  "both" ) %>% 
  group_by(rainfall_class, Strip_Type, GSP_Rec_both) %>% 
  summarise(GM_av = mean(GM))


### what is the What is the N/P content of GSP 

n_p_contnet_GPS <- df_subset %>% 
  filter(GSP == "GSP")

#check
n_p_contnet_GPS %>%  
    count()
names(n_p_contnet_GPS)

n_p_contnet_GPS %>% 
  filter(Strip_Type == "N Strip") %>% 
ggplot( aes( N_P_content))+
  geom_histogram(aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_histogram()+
    theme_bw()+
  labs(x = "N content of GSP", y = "zones")
  
n_p_contnet_GPS %>% 
  filter(Strip_Type == "P Strip") %>% 
  ggplot( aes( N_P_content))+
  geom_histogram(aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_histogram()+
  theme_bw()+
  labs(x = "P content of GSP", y = "zones")


### what is the What is the N/P content of approx rec rate?

names(df_subset)
n_p_contnet_approx_rec <- df_subset %>% 
  filter(rec_rate_label == "closest_match")
#check
n_p_contnet_approx_rec %>%  
  count()


n_p_contnet_approx_rec %>% 
  filter(Strip_Type == "N Strip") %>% 
  ggplot( aes( N_P_content))+
  geom_histogram(aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_histogram()+
  theme_bw()+
  labs(x = "N content of Approx. rec", y = "zones")

n_p_contnet_approx_rec %>% 
  filter(Strip_Type == "P Strip") %>% 
  ggplot( aes( N_P_content))+
  geom_histogram(aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #geom_histogram()+
  theme_bw()+
  labs(x = "P content of Approx. rec", y = "zones")


### what is the difference in P / N contnet between the GSP and the approx rec rate?

## this might not work but now I want to keep 'both' in by data

  n_p_content_GSP_vs_approx <- df_subset
  # df_subset%>%  
  # filter( GSP_Rec_both !=  "both" ) 
names(n_p_content_GSP_vs_approx)
str(n_p_content_GSP_vs_approx)
#3 new clms
#1.
n_p_content_GSP_vs_approx <- n_p_content_GSP_vs_approx %>% 
  mutate(
    GSP_n_p = case_when(
    GSP_Rec_both == "GSP" ~ N_P_content
  ))
#2. 
n_p_content_GSP_vs_approx <- n_p_content_GSP_vs_approx %>% 
  mutate(
    Approx_n_p = case_when(
      GSP_Rec_both == "rec_rate" ~ N_P_content
    ))
#3. 
n_p_content_GSP_vs_approx <- n_p_content_GSP_vs_approx %>% 
  mutate(
    Approx_and_GSP = case_when(
      GSP_Rec_both == "both" ~ N_P_content
    ))

## what is the average unit of N and P per rainfall class for the GSP (when different to the )

str(n_p_content_GSP_vs_approx)

n_p_content_GSP_vs_approx_1 <- n_p_content_GSP_vs_approx %>% 
  filter( GSP_Rec_both ==  "GSP" )
n_p_content_GSP_vs_approx_1 <- n_p_content_GSP_vs_approx_1 %>% 
  distinct(join_zone_ID_Strip_Type, .keep_all= TRUE) %>% 
  select(join_zone_ID_Strip_Type, GSP_n_p)

n_p_content_GSP_vs_approx_2 <- n_p_content_GSP_vs_approx %>% 
  filter( GSP_Rec_both ==  "rec_rate" )
n_p_content_GSP_vs_approx_2 <- n_p_content_GSP_vs_approx_2 %>% 
  distinct(join_zone_ID_Strip_Type, .keep_all= TRUE) %>% 
  select(join_zone_ID_Strip_Type,   Approx_n_p )  

n_p_content_GSP_vs_approx_3 <- n_p_content_GSP_vs_approx %>% 
  filter( GSP_Rec_both ==  "both" )
n_p_content_GSP_vs_approx_3 <- n_p_content_GSP_vs_approx_3 %>% 
  distinct(join_zone_ID_Strip_Type, .keep_all= TRUE) %>% 
  select(join_zone_ID_Strip_Type, Approx_and_GSP )

str(n_p_content_GSP_vs_approx_1) # this is the GSP
str(n_p_content_GSP_vs_approx_2) # this is the approx
str(n_p_content_GSP_vs_approx_3) # this is the approx == GSP

# the zones dont match in the 3 files.
names(n_p_content_GSP_vs_approx)
test <- n_p_content_GSP_vs_approx %>% 
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE ) %>% 
  select(join_zone_ID_Strip_Type, rainfall_class, p_rec, n_rec, Zone_ID,Strip_Type,)

n_p_content_GSP_vs_approx <- left_join(test, n_p_content_GSP_vs_approx_1)
n_p_content_GSP_vs_approx <- left_join(n_p_content_GSP_vs_approx, n_p_content_GSP_vs_approx_3)
n_p_content_GSP_vs_approx <- left_join(n_p_content_GSP_vs_approx, n_p_content_GSP_vs_approx_2)


str(n_p_content_GSP_vs_approx)
n_p_content_GSP_vs_approx <- n_p_content_GSP_vs_approx %>% 
  mutate(diff_GSP_approx_cont = GSP_n_p  - Approx_n_p)


names(n_p_content_GSP_vs_approx)
n_p_content_GSP_vs_approx %>%  
  filter(Strip_Type == "N Strip") %>% 
  ggplot(aes(x = rainfall_class, y = abs(diff_GSP_approx_cont))) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = diff_GSP_approx_cont)) +
  theme_bw()+
  labs(x = "rainfall class", y = "ABS Difference N contnet: GPS - Approx. rec rate")

#### the average P or N content
names(n_p_content_GSP_vs_approx)
View(n_p_content_GSP_vs_approx)
n_p_content_GSP_vs_approx %>%  
  group_by(rainfall_class, Strip_Type) %>% 
  summarise(Approx_n_p_av = mean(Approx_n_p),
            GSP_n_p_av = mean(GSP_n_p)) %>% 
  arrange(Strip_Type,rainfall_class)


n_p_content_GSP_vs_approx %>%  
  filter(Strip_Type == "P Strip") %>% 
  ggplot(aes(x = rainfall_class, y = abs(diff_GSP_approx_cont))) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75,aes(fill = diff_GSP_approx_cont)) +
  theme_bw()+
  labs(x = "rainfall class", y = "ABS Difference P contnet: GPS - Approx. rec rate")



### sometime the P / N contnet for the GSP and the approx rec rate are the same?

n_p_content_same <- 
  df_subset%>%  
  filter( GSP_Rec_both ==  "both" ) 
#check
n_p_content_same %>%  
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>% 
  count()

#check
n_p_content_same %>%  
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>% 
  group_by(Strip_Type) %>% 
  summarise(count = n())


n_p_content_NOTsame <- 
  df_subset%>%  
  filter( GSP_Rec_both !=  "both" ) 
#check
n_p_content_NOTsame %>%  
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>% 
  count()

#check
n_p_content_NOTsame %>%  
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>% 
  group_by(Strip_Type) %>% 
  summarise(count = n())



###########################################################################################

#### Yld ####

str(df_subset)
high_yld_1 <- df_subset %>% 
  select(Zone_ID, rainfall_class,GSP_Rec_both, yield, Strip_Type )
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

high_yld_1ab %>% group_by(Yld_loss_gain, rainfall_class, Strip_Type ) %>%
  summarise(av_yld = mean(Rec_rate_v_GSP, na.rm = FALSE)) %>% 
  arrange(Strip_Type, Yld_loss_gain,rainfall_class )
  
#### GM ####

str(df_subset)
high_GM_1 <- df_subset %>% 
  select(Zone_ID, rainfall_class,GSP_Rec_both, GM, Strip_Type )
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

high_GM_1ab %>% group_by(GM_loss_gain, rainfall_class, Strip_Type ) %>%
  summarise(av_GM = mean(Rec_rate_v_GSP, na.rm = FALSE)) %>% 
  arrange(Strip_Type, GM_loss_gain,rainfall_class )





### For Rick Boardertown slide feedback.
# Question 1) what is the recommed rate from Sean for N and P per rainfall class
# Question 2) what is the approc rec rate for N and P per rainfall class
# Question 3) what is the GPS N and P per rainfall class
# calulate the difference between N/P content for the GPS and N/P content for the rec rate 
# Question 4) In how may zones is the recom rate close to the GPS (within 5-10 units of N / P) 

# slide 32 Rick
#this data frame is all of the zone data by rate analysis, but then I have removed row that are not GPS or the recom rate - I think this is ok
deatils_rec_rate <- df_subset


# Question 1) what is the recommed rate from Sean for N and P per rainfall class
names(deatils_rec_rate)
# obtain one value per zone and strip the cal the average N and P  
deatils_rec_rate %>%
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>%
  group_by(Strip_Type, rainfall_class) %>%
  summarise(
    Av_p_rec = mean(p_rec, na.rm = TRUE),
    Av_n_rec = mean(n_rec, na.rm = TRUE)
  )


# Question 2) what is the approx recommed rate  for N and P per rainfall class
names(deatils_rec_rate)
# filter the data on closest match
deatils_rec_rate_closest_match<- deatils_rec_rate %>%
filter(rec_rate_label == 	"closest_match")
#obtain one value per zone and strip the cal the average N and P  
deatils_rec_rate_closest_match %>%
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>%
  group_by(Strip_Type, rainfall_class) %>%
  summarise(
    Av_P_content = mean(P_content, na.rm = TRUE),
    Av_N_content = mean(N_content, na.rm = TRUE)
  )



# Question 3) what is the GPS N and P per rainfall class
names(deatils_rec_rate)
# filter the data on closest match
deatils_rec_rate_GSP<- deatils_rec_rate %>%
  filter(GSP == 	"GSP")

#obtain one value per zone and strip the cal the average N and P  
deatils_rec_rate_GSP %>%
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>%
  group_by(Strip_Type, rainfall_class) %>%
  summarise(
    Av_P_content = mean(P_content, na.rm = TRUE),
    Av_N_content = mean(N_content, na.rm = TRUE)
  )


# calulate the difference between N/P content for the GPS and N/P content for the rec rate 
# Question 4) In how may zones is the recom rate close to the GPS (within 5-10 units of N / P) 

#Break up my data

#1. This is all the zone and strip type that has GPS 
str(deatils_rec_rate_GSP)
#what is the difference between P contnet and P rec
deatils_rec_rate_GSP <- deatils_rec_rate_GSP %>% 
  mutate(p_rec_P_content = abs( p_rec - P_content ))

deatils_rec_rate_GSP <- deatils_rec_rate_GSP %>% 
  mutate(p_rec_P_content_class = 
           case_when(
             p_rec_P_content <= 10 ~ "close",
             TRUE ~ "far")
           )

#what is the difference between N contnet and N rec
deatils_rec_rate_GSP <- deatils_rec_rate_GSP %>% 
  mutate(n_rec_N_content = abs( n_rec - N_content ))

deatils_rec_rate_GSP <- deatils_rec_rate_GSP %>% 
  mutate(n_rec_N_content_class = 
           case_when(
             n_rec_N_content <= 10 ~ "close",
             TRUE ~ "far")
  )

#obtain one value per zone and strip the cal the average N and P  
deatils_rec_rate_GSP %>%
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>%
  filter(Strip_Type == "P Strip") %>% 
  group_by( rainfall_class, p_rec_P_content_class) %>%
  summarise(
    count = n())

deatils_rec_rate_GSP %>%
  distinct(join_zone_ID_Strip_Type, .keep_all = TRUE) %>%
  filter(Strip_Type == "N Strip") %>% 
  group_by( rainfall_class, n_rec_N_content_class) %>%
  summarise(
    count = n())

names(deatils_rec_rate_GSP)

#### update the graphs so they are not grouped


# df_subset %>%  
#   filter( GSP_Rec_both !=  "both" ) %>% 
#   ggplot(aes(x = rainfall_class, y = GM, color =GSP_Rec_both)) +
#   geom_point(position = position_dodge(width=0.75)) +
#   geom_boxplot(alpha = 0.1, width=0.75,aes(fill = GSP_Rec_both)) +
#   theme_bw()+
#   facet_wrap(.~Strip_Type)+
#   labs(x = "rainfall class", y = "GM $/ha")
