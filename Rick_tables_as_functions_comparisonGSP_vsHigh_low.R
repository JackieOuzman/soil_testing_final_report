library(tidyverse)
library(ggplot2)
library(formattable)
library(sf)
library(readxl)
library(gt)
library(glue)
library(rnaturalearth)
library(readr)
library(DT)
library(plotKML)
library(knitr)
library(png)
library(magick)

function_filter_data <- function(comparison,Strip_type, df ){
  comparison <- quo_name(comparison)
  df <- df %>% 
    dplyr::filter(comparison == !!comparison) 
  
  
  #################################################################### 
  df <- df %>% 
    dplyr::select('Zone ID' = Zone_ID,
                  Strip_Type,
                  
                  `yield with fert lower than GSP` = lower_than_GSP,
                  `yield with fert higher than GSP` = higher_than_GSP,
                  `yield for GSP` = the_GSP,
                  
                  GSP_vs_lower,         
                  GSP_vs_higher,
                  
                  'yield response' = yld_response,
                  `Significant` = Significant_practical) 
  df <- df %>% 
    arrange(`Zone ID`)
  
  
  ### round the values so the tabel looks better
  df <- df %>% 
    mutate( `yield with fert lower than GSP` = round(`yield with fert lower than GSP`, digits = 2),
            `yield with fert higher than GSP` = round(`yield with fert higher than GSP`, digits = 2)
            )
  
  
  
  ##### This table has some key bits that we need... but we also need info from Sean DB
  ###################################################################################################
  recom_rateDB <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_April7 2021.xlsx")
  ##########################################################################################################################################
  
  # select only a few clms with recommedation 
  recom_rateDB <- recom_rateDB %>% 
    dplyr::select(Zone_ID =    `Paddock code` ,
                  p_rec =           `P rec`,
                  n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                  n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                  n_rec_yld_high =  `N Rec (> 5 t/ha)`,
                  SM_comment_Soil_N = `SM comment Soil N`,
                  SM_comment_Soil_P = `SM comment Soil P`,
                  SM_comment_Plant_Tissue = `SM comment Plant Tissue`
    ) 
  
  recom_rateDB <-  dplyr::mutate(recom_rateDB,  maxN = apply(recom_rateDB[3:5], 1, max, na.rm = TRUE))
  
  
  # remove redunant clm and replace inf
  recom_rateDB <- recom_rateDB %>% 
    mutate(
      maxN = case_when(
        maxN >= 0 ~ maxN,
        TRUE ~ NA_real_
      )
    )
  
  # join Sean results with the comparision
  high_v_low_comp_results_Sean <- left_join(df,recom_rateDB,
                                            by = c(`Zone ID` = "Zone_ID" ))
  
  
  high_v_low_comp_results_Sean <-high_v_low_comp_results_Sean %>% 
    dplyr::select(-n_rec_yld_low,
                  -n_rec_yld_med,
                  -n_rec_yld_high)
  
  ## make new clm recommdation from soil test
  
  high_v_low_comp_results_Sean <- high_v_low_comp_results_Sean %>% 
    mutate(soil_test_indicates = case_when(
      Strip_Type == 	"P Strip" & p_rec > 5 ~ "respose likely",
      Strip_Type == 	"P Strip" & p_rec <= 5 ~ "respose unlikely",
      Strip_Type == 	"N Strip" & maxN > 0 ~ "respose likely",
      Strip_Type == 	"N Strip" & maxN <= 0 ~ "respose unlikely",
      TRUE ~ "NA"
    ))
  
  return(high_v_low_comp_results_Sean)}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
################################################################################################
#### The data frame is now ready for summary stats. - df_step1
################################################################################################

function_table <- function(comparison,Strip_type, df_step1 ){
  
  # count number of trials N and P for each yield resposne
  
  ##########################################################################################
  x_trials_lowVsHigh_fert <- df_step1 %>% filter(Strip_Type == Strip_type[1] ) %>% 
    group_by(`yield response`) %>% 
    summarise("count by fert type" = n())
  
  x_soil_test_likley <-df_step1 %>% 
    filter(Strip_Type == Strip_type[1] ) %>% 
    group_by(`yield response`, soil_test_indicates) %>% 
    summarise("count by soil test" = n()) %>% 
    arrange(soil_test_indicates)
  
  # x_trials_lowVsHigh_fert  
  # x_soil_test_likley 
  
  pivot1_count <- pivot_wider(x_trials_lowVsHigh_fert,
                              names_from = `yield response`, 
                              values_from = `count by fert type`) %>% 
    mutate(grouping = "all")
  
  
  pivot2_count <-pivot_wider(x_soil_test_likley,
                             names_from = `yield response`,
                             values_from = `count by soil test`) %>% 
    rename(grouping = soil_test_indicates)
  
  
  # pivot1_count
  # pivot2_count
  
  table_count <- rbind(pivot1_count, pivot2_count)
  #table_count
  
  table_count$Sum <- rowSums(table_count[,1:3], na.rm=TRUE)
  table_count <- table_count %>%  mutate(
    positive =      round(positive, digits = 2),
    no_response = round(no_response, digits = 2),
    negative =     round(negative, digits = 2)
  )
  
  
  #######################################################################################################
  ### as above but with mean yield difference
  
  x_trials_lowVsHigh_fert_mean <- df_step1 %>% filter(Strip_Type == Strip_type[1] ) %>%
    group_by(`yield response`) %>% 
    summarise('Average of yield difference' = mean(`Mean yield difference`,na.rm = TRUE )) 
  
  
  mean_soil_test_likley <-df_step1 %>% 
    filter(Strip_Type == Strip_type[1] ) %>% 
    group_by(`yield response`, soil_test_indicates) %>% 
    summarise('Average of yield difference' = mean(`Mean yield difference`,na.rm = TRUE )) %>% 
    arrange(soil_test_indicates)
  
  
  
  # mean_soil_test_likley 
  # x_trials_lowVsHigh_fert_mean
  
  pivot1_mean <- pivot_wider(x_trials_lowVsHigh_fert_mean,
                             names_from = `yield response`, 
                             values_from = `Average of yield difference`) %>% 
    mutate(grouping = "all")
  
  
  pivot2_mean <-pivot_wider(mean_soil_test_likley,
                            names_from = `yield response`,
                            values_from = `Average of yield difference`) %>% 
    rename(grouping = soil_test_indicates)
  
  
  # pivot1_mean
  # pivot2_mean
  table_mean <- rbind(pivot1_mean, pivot2_mean)
  #table_mean
  
  table_mean <- table_mean %>%  mutate(
    positive =      round(positive, digits =  3),
    no_response = round(no_response, digits =  3),
    negative =     round(negative, digits =  3)
  )

  
  #########################################################################################
  table_mean <- table_mean %>%  dplyr::mutate(summary = "mean yld difference", Sum = "NA")
  table_count <- table_count %>%  dplyr::mutate(summary = "count")
  
  # table_count
  # table_mean
  
  Table_1 <- data.frame(rbind(table_mean,table_count ))
  
  Table_1 <-Table_1 %>%  dplyr::mutate(Strip_trial = Strip_type,
                                       comparision = comparison)
  
  Table_1
  #order table
  Table_1 <- Table_1 %>%  dplyr::select(grouping, positive, no_response, negative,Sum,
                                 summary, Strip_trial, comparision)
  
  
  
  
  name_of_summary_table <- paste0("Yield Resposne to ", comparison, " fertiliser, for ", Strip_type)
  
  # Create a gt table based on preprocessed
  #  table data
  
  
  
  display_table <- Table_1 %>%
    filter(grouping != "NA") %>%
    #select() %>%
    arrange(summary, grouping) %>% 
    gt() %>%
    tab_header(
      title = name_of_summary_table,
      subtitle = "all zones processed - missing data has no recom rate (data filtered out")
  
  
  return(display_table)}
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
rm(list = c('GR_comparison','df_step1'))

#high_low_comp_t <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/hign_low_t_test_merged_3b.csv")
GR_comparison <-  read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/GSP_low_high_comparision_t_test_merged_3d.csv")
names(GR_comparison)
unique(GR_comparison$comparison)

##!!! User input required !!!! 1. Filter on comparison
# unique(high_low_comp_t$comparison)
#high_low_comp_t$comparison <- as.character(high_low_comp_t$comparison)
#str(high_low_comp_t$comparison)

#comparison <-  "GSP_v_high"
comparison <-  "GSP_v_low"



##!!! User input required !!!!
#Strip_type <-  "P Strip"
Strip_type <-  "N Strip"

df_step1 <- function_filter_data(comparison, Strip_type, GR_comparison) 

comparison
names(df_step1)

df_step1 <- df_step1 %>%
  dplyr::select( ## which one?
    #`Mean yield difference` = GSP_vs_higher,
    `Mean yield difference` = GSP_vs_lower,
    #plus all the others
    'Zone ID',
    Strip_Type,
    `yield response`,
    'yield with fert lower than GSP',
    'yield with fert higher than GSP',
    'yield response',
    Significant,
    p_rec,
    SM_comment_Soil_N,
    SM_comment_Soil_P,
    SM_comment_Plant_Tissue,
    maxN,
    soil_test_indicates) %>% 
  mutate(`Mean yield difference` = round(`Mean yield difference`, digits = 2))             

assign(paste0(comparison, "_", substr(Strip_type, start = 1, stop=1)),function_table(comparison,Strip_type, df_step1) )

paste0(comparison, "_", substr(Strip_type, start = 1, stop=1))

GSP_v_high_P #done
GSP_v_high_N #done

GSP_v_low_P #done
GSP_v_low_N #done



gtsave(GSP_v_high_P, "C:/Users/ouz001/working_from_home/soil_testing/Soil_testing_reports/GSP_v_high_P.png")
gtsave(GSP_v_high_N, "C:/Users/ouz001/working_from_home/soil_testing/Soil_testing_reports/GSP_v_high_N.png")

gtsave(GSP_v_low_P, "C:/Users/ouz001/working_from_home/soil_testing/Soil_testing_reports/GSP_v_low_P.png")
gtsave(GSP_v_low_N, "C:/Users/ouz001/working_from_home/soil_testing/Soil_testing_reports/GSP_v_low_N.png")

