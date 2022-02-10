# Work on US import data of wood and forest products, by value and quantity
# Simeone Consulting, LLC


library(tidyr)
#library(fuzzyjoin)
library(data.table)
library(dplyr)
library(readxl)
#library(stringr)
#library(lubridate)
library(janitor)
options(scipen = 100)

dataPath <- "C:\\Users\\simeo\\Desktop\\US_WoodForestProducts_Imports_HTS\\"

#confirm or pre-process raw data file downloaded or received from US Census or International 
#Trade Commission (ITC) so that the column headings have no spaces 
#(e.g. use underscore instead of space)


##### Read in raw data and combine together with table specifying HTS codes that require Lacey Act declarations #####

#read in yearly data files received directly from US Census and filter out rows where quantities 
#and values are both 0
HTSdata2015 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_May2020\\WoodForestRequest15.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)
  
HTSdata2016 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_May2020\\WoodForestRequest16.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

HTSdata2017 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_May2020\\WoodForestRequest17.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

HTSdata2018 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_May2020\\WoodForestRequest18.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

HTSdata2019 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_May2020\\WoodForestRequest19.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

#read in any additional data (making sure all column headings match) -e.g. Ch. 47 (Pulp), and Ch. 48 (paper) yearly HTS10
LateArrival_Ch47_Ch48_2015_2019 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_May2020\\LateArrival_Ch47_Ch48_2015_2019.xlsx")) 

#combine all years
htstradedata <- HTSdata2015 %>%
  rbind(HTSdata2016) %>%
  rbind(HTSdata2017) %>%
  rbind(HTSdata2018) %>%
  rbind(HTSdata2019) %>%
  rbind(LateArrival_Ch47_Ch48_2015_2019)

# remove yearly files now that they're combined
rm(HTSdata2015, HTSdata2016, HTSdata2017, HTSdata2018, HTSdata2019, LateArrival_Ch47_Ch48_2015_2019)

# write out combined 2015-2019 raw data
fwrite(htstradedata, paste0(dataPath, "OutputFiles\\US_imports_wood_2015_2016_2017_2018_2019.csv"))

######Preprocess and combine with US Census 2019+ data retrieved via API 

#read in data file with  where quantities #and values are both 0

HTS_data_2015_2016_2017_2018_2019 <-read.csv(paste0(dataPath, "OutputFiles\\US_imports_wood_2015_2016_2017_2018_2019.csv"), stringsAsFactors = FALSE) %>%
  clean_names() %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)
  
  HTSdata2019_2020_2021 <- read.csv(paste0(dataPath, "OutputFiles\\US_imports_wood_2019_2020_2021.csv"), stringsAsFactors = FALSE) %>%
  clean_names() %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

data.frame(HTSdata2019_2020_2021 = colnames(HTSdata2019_2020_2021), x = colnames(HTS_data_2015_2016_2017_2018_2019))
colnames(HTSdata2019_2020_2021)
colnames(HTS_data_2015_2016_2017_2018_2019)

#standardize older 2015-2019 dataset so it resembles newer data pulled through Census API
HTS_data_2015_2016_2017_2018_2019$comm_lvl <- "HS10"
colnames(HTS_data_2015_2016_2017_2018_2019)[3]<-"i_commodity" 
colnames(HTS_data_2015_2016_2017_2018_2019)[4]<-"i_commodity_ldesc" 
colnames(HTS_data_2015_2016_2017_2018_2019)[6]<-"cty_name" 
colnames(HTS_data_2015_2016_2017_2018_2019)[7]<-"district" 
colnames(HTS_data_2015_2016_2017_2018_2019)[8]<-"dist_name" 
colnames(HTS_data_2015_2016_2017_2018_2019)[13]<-"unit_qy1" 

#HTS_data_2015_2016_2017_2018_2019$i_commodity <- as.character(HTS_data_2015_2016_2017_2018_2019$i_commodity)
#HTSdata2019_2020_2021$i_commodity <- as.character(HTSdata2019_2020_2021$i_commodity)
  as.character(HTSdata2019_2020_2021$i_commodity)

# Compare initial 2019 data to updated 2019 prior to deleting older 2019 data
  

 Old2019_yrlysum_byhts <- HTS_data_2015_2016_2017_2018_2019 %>%
    filter(year == "2019") %>%
    group_by(i_commodity) %>%
    summarize(tot_gen_val_old2019 = sum(gen_val_mo))
  
  New2019_yrlysum_byhts <- HTSdata2019_2020_2021 %>%
    filter(year == "2019") %>%
    group_by(i_commodity) %>%
    summarize(tot_gen_val_new2019 = sum(gen_val_mo))
  
  
  New2019_yrlysum_byhts <- New2019_yrlysum_byhts %>%
     mutate(diff_new_minus_old = tot_gen_val_new2019 - Old2019_yrlysum_byhts$tot_gen_val_old2019)
  
  
########## Bring in new data, preprocess to make sure column headings are the same, and delete older yearly data which will have been updated by new batch #####
  
  
#Combine old and new data, deleting old 2019 data that will be replaced by new data
  HTSdata2015_2016_2017_2018 <- HTS_data_2015_2016_2017_2018_2019 %>%
    filter(year != "2019")
  
  Wood_HTS_2015_2016_2017_2018_2019_2020_2021 <- rbind(HTSdata2015_2016_2017_2018, HTSdata2019_2020_2021)

#Standarize country capitalization issues between datasets
  Wood_HTS_2015_2016_2017_2018_2019_2020_2021 <- Wood_HTS_2015_2016_2017_2018_2019_2020_2021 %>%
    mutate(i_commodity = as.character(i_commodity), 
         cty_name = str_to_title(cty_name))
  
#write out full combined raw dataset 
  fwrite(Wood_HTS_2015_2016_2017_2018_2019_2020_2021, 
         paste0(dataPath, "OutputFiles\\Wood_HTS_2015_2016_2017_2018_2019_2020_2021_9Feb2022.csv"))
  
  
rm(HTS_data_2015_2016_2017_2018_2019, HTSdata2015_2016_2017_2018, HTSdata2019_2020_2021, New2019_yrlysum_byhts, Old2019_yrlysum_byhts)  
