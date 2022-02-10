# Summarizing China's role in US forest product imports 
#US Lacey Act Plant Declaration Requirement Analysis for US import data of wood and forest products, by value and quantity
# Simeone Consulting, LLC


library(tidyr)
#library(fuzzyjoin)
library(data.table)
library(dplyr)
library(readxl)
#library(stringr)
#library(lubridate)
library(janitor)
library(reshape2)
library(writexl)
options(scipen = 100)

dataPath <- "C:\\Users\\simeo\\Desktop\\US_WoodForestProducts_Imports_HTS\\"


##### Work with combined trade data, joined with Lacey Act declaration requirements #####
##### if bringing in already produced combined dataset to work with, then...#####

htstradedata <- fread(paste0(dataPath, "OutputFiles\\htstradedata_laceydeclarations_9Feb2022.csv")) %>%
  mutate(HTS2 = as.character(HTS2),
         HTS4 = as.character(HTS4), 
         HTS6 = as.character(HTS6), 
         HTS8 = as.character(HTS8), 
         HTS10 = as.character(HTS10))


#summarize yearly totals by hts10, with commodity descrip, retaining dec_req split
yrlysum_htstradedata <- htstradedata %>%
  group_by(year, dec_req, i_commodity_ldesc, HTS10, HTS8, HTS6, HTS4, HTS2, ExclusivelyContainsWood, ImplementationPhase, DeclarationFormRequiredBeginningDate, Gen_class) %>%
  summarize(tot_gen_val_by_hts10 = sum(gen_val_mo), 
            tot_con_val_by_hts10 = sum(con_val_mo)) %>%
  ungroup() %>%
  #add in column to compare value of "General US imports" to "US Imports for Consumption" (https://www.census.gov/foreign-trade/guide/sec2.html#gen_imports) 
  mutate(diff_gen_val_minus_con_val = tot_gen_val_by_hts10 - tot_con_val_by_hts10) %>%
  #add in column with link to CBP CROSS Database for each HTS10
  mutate(Examples_from_CBP_CROSS = paste0("https://rulings.cbp.gov/search?term=", 
                                          substr(HTS10,1,4), ".", 
                                          substr(HTS10,5,6), ".",
                                          substr(HTS10,7,10)))


############Overview summary US imports from China compared to ROW for total wood products for HS 44, 94, 47, 48
htstradedata <- htstradedata %>%
  mutate(exporter = ifelse(cty_name == "China", "China", ifelse(cty_name == "Vietnam", "Vietnam","ROW")))
### Overview of total wood products: HS 44, 47, 48, 94
year_country_summary <- htstradedata %>%
  filter(HTS2 == "44" | HTS2 == "94") %>%
 # mutate(Exporter = ifelse(HTS_SIMP_Listed =="2", 1, HTS_SIMP_Listed)) %>%
  group_by(year, exporter) %>%
  summarize(Value = sum(as.numeric(gen_val_mo)))

year_China <- year_country_summary %>%
  filter(exporter == "China") %>%
  mutate(China = Value) %>%
  select(-exporter, -Value)

year_Vietnam <- year_country_summary %>%
  filter(exporter == "Vietnam") %>%
  mutate(China = Value) %>%
  select(-exporter, -Value)

year_ROW <- year_country_summary %>%
  filter(exporter == "ROW") %>%
  mutate(Rest_of_World_ROW = Value) %>%
  select(-exporter, -Value)

year_total_value <- year_country_summary %>%
  group_by(year) %>%
  summarize(total_value = sum(Value))

yearly_summary_China <- inner_join(year_China, year_ROW, by="year")
yearly_summary_China <- inner_join(yearly_summary_China, year_total_value, by="year")
yearly_summary_China <- yearly_summary_China %>%
  mutate(Percent_Tot_Imported_from_China = China / total_value)

### Overview of solid wood products: HS 44 94
year_China_summary_44_94 <- htstradedata %>%
  filter(HTS2 == "44" | HTS2 == "94") %>%
  # mutate(Exporter = ifelse(HTS_SIMP_Listed =="2", 1, HTS_SIMP_Listed)) %>%
  group_by(year, exporter) %>%
  summarize(Value = sum(as.numeric(gen_val_mo)))

year_China_44_94 <- year_China_summary_44_94 %>%
   filter(exporter == "China") %>%
  mutate(China = Value) %>%
  select(-exporter, -Value)

year_ROW_44_94 <- year_China_summary_44_94 %>%
  filter(exporter == "ROW") %>%
  mutate(Rest_of_World_ROW = Value) %>%
  select(-exporter, -Value)

year_total_value_44_94 <- year_China_summary_44_94 %>%
  group_by(year) %>%
  summarize(total_value = sum(Value))

yearly_summary_China_44_94 <- inner_join(year_China_44_94, year_ROW_44_94, by="year")
yearly_summary_China_44_94 <- inner_join(yearly_summary_China_44_94, year_total_value_44_94, by="year")
yearly_summary_China_44_94 <- yearly_summary_China_44_94 %>%
  mutate(Percent_Tot_Imported_from_China = China / total_value)

# year_SIMP_summary <- htstradedata_joined_simp %>%
#   filter(Duplicate_HTS == 0) %>%
#   mutate(HTS_SIMP = ifelse(HTS_SIMP_Listed =="2", 1, HTS_SIMP_Listed)) %>%
#   group_by(year, HTS_SIMP)%>%
#   summarize(Value = sum(as.numeric(gen_val_mo)))
# 
# year_SIMP_listed <- year_SIMP_summary %>%
#   filter(HTS_SIMP == 1) %>%
#   mutate(HTS_SIMP_listed = Value) %>%
#   select(-HTS_SIMP, -Value)
# 
# year_SIMP_not_listed <- year_SIMP_summary %>%
#   filter(HTS_SIMP == 0) %>%
#   mutate(HTS_SIMP_not_listed = Value) %>%
#   select(-HTS_SIMP, -Value)
# 

# 




