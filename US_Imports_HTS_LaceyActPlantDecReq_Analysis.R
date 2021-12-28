# US Lacey Act Plant Declaration Requirement Analysis for US import data of wood and forest products, by value and quantity
# Simeone Consulting, LLC


library(tidyr)
#library(fuzzyjoin)
library(data.table)
library(dplyr)
library(readxl)
#library(stringr)
#library(lubridate)
library(janitor)

dataPath <- "C:\\Users\\simeo\\Desktop\\US_WoodForestProducts_Imports_HTS\\"


########### Working with most updated combined data ##############  
#read in raw US Census data
htstradedata <- read.csv(paste0(dataPath, "OutputFiles\\Wood_HTS_2015_2016_2017_2018_2019_2020_2021_27Dec2021.csv"), stringsAsFactors = FALSE) %>%
  mutate(i_commodity = as.character(i_commodity))


#read in list of codes to analyze (codes requiring PPQ 505 forms)
laceydeclarations <- read.csv(paste0(dataPath, "HTS_Chapters_Requiring_LaceyDeclarationForm_Dec2021.csv"), stringsAsFactors = FALSE) %>%
  mutate(DeclarationFormRequiredBeginningDate = as.POSIXct(DeclarationFormRequiredBeginningDate, format = "%m/%d/%Y"), 
         HTS_Codes = as.character(HTS_Codes))


#join the table that specifies whether PPQ505 form is required to the trade data
# create columns in hts trade data representing 2, 4, 6, 8, and 10 digit codes
htstradedata <- htstradedata %>%
  mutate(HTS2 = substr(i_commodity, 1, 2),
         HTS4 = substr(i_commodity, 1, 4),
         HTS6 = substr(i_commodity, 1, 6),
         HTS8 = substr(i_commodity, 1, 8),
         HTS10 = substr(i_commodity, 1, 10)) %>%
  select(-i_commodity, -comm_lvl, -district, -cty_code)


# join all 2-digit hts codes to 2 digit codes in table with lacey declaration requirements (not applicable now, but maybe in the future as declaration requirements change)
join_hts2 <- left_join(htstradedata, laceydeclarations, by = c("HTS2" = "HTS_Codes"))
nas_hts2 <- join_hts2 %>%
  filter(is.na(ImplementationPhase)) %>%
  select(-ExampleDescription, -ImplementationPhase, -DeclarationFormRequiredBeginningDate, 
         -ExclusionsNoDecReq, -ExclusivelyContainsWood, -Gen_class)
join_hts2_matched <- join_hts2 %>%
  filter(!is.na(ImplementationPhase)) %>%
  mutate(dec_req = ifelse(ExclusionsNoDecReq == 1, "Declaration Form Not Required", "0"),
         dec_req = ifelse(dec_req == 0 & ImplementationPhase == 7, "Declaration Form Proposed", 
                          ifelse(dec_req == 0 & ImplementationPhase <= 6, "Declaration Form Required", dec_req)))

# join all 4-digit hts codes to 4 digit codes in table with lacey declaration requirements
join_hts4 <- left_join(nas_hts2, laceydeclarations, by = c("HTS4" = "HTS_Codes"))
nas_hts4 <- join_hts4 %>%
  filter(is.na(ImplementationPhase)) %>%
  select(-ExampleDescription, -ImplementationPhase, -DeclarationFormRequiredBeginningDate, 
         -ExclusionsNoDecReq, -ExclusivelyContainsWood, -Gen_class)
join_hts4_matched <- join_hts4 %>%
  filter(!is.na(ImplementationPhase))%>%
  mutate(dec_req = ifelse(ExclusionsNoDecReq == 1, "Declaration Form Not Required", "0"),
         dec_req = ifelse(dec_req == 0 & ImplementationPhase == 7, "Declaration Form Proposed", 
                          ifelse(dec_req == 0 & ImplementationPhase <= 6, "Declaration Form Required", dec_req)))

# join all 6-digit hts codes to 6 digit codes in table with lacey declaration requirements
join_hts6 <- left_join(nas_hts4, laceydeclarations, by = c("HTS6" = "HTS_Codes"))
nas_hts6 <- join_hts6 %>%
  filter(is.na(ImplementationPhase)) %>%
  select(-ExampleDescription, -ImplementationPhase, -DeclarationFormRequiredBeginningDate, 
         -ExclusionsNoDecReq, -ExclusivelyContainsWood, -Gen_class)
join_hts6_matched <- join_hts6 %>%
  filter(!is.na(ImplementationPhase)) %>%
  mutate(dec_req = ifelse(ExclusionsNoDecReq == 1, "Declaration Form Not Required", "0"),
         dec_req = ifelse(dec_req == 0 & ImplementationPhase == 7, "Declaration Form Proposed", 
                          ifelse(dec_req == 0 & ImplementationPhase <= 6, "Declaration Form Required", dec_req)))

# join all 8-digit hts codes to 8 digit codes in table with lacey declaration requirements
join_hts8 <- left_join(nas_hts6, laceydeclarations, by = c("HTS8" = "HTS_Codes"))
nas_hts8 <- join_hts8 %>%
  filter(is.na(ImplementationPhase)) %>%
  select(-ExampleDescription, -ImplementationPhase, -DeclarationFormRequiredBeginningDate, 
         -ExclusionsNoDecReq, -ExclusivelyContainsWood, -Gen_class)
join_hts8_matched <- join_hts8 %>%
  filter(!is.na(ImplementationPhase)) %>%
  mutate(dec_req = ifelse(ExclusionsNoDecReq == 1, "Declaration Form Not Required", "0"),
         dec_req = ifelse(dec_req == 0 & ImplementationPhase == 7, "Declaration Form Proposed", 
                          ifelse(dec_req == 0 & ImplementationPhase <= 6, "Declaration Form Required", dec_req)))


# join all 10-digit hts codes to 10 digit codes in table with lacey declaration requirements 
join_hts10 <- left_join(nas_hts8, laceydeclarations, by = c("HTS10" = "HTS_Codes")) %>%
  mutate(dec_req = ifelse(is.na(ExclusionsNoDecReq) | ExclusionsNoDecReq == 1, "Declaration Form Not Required", "0"),
         dec_req = ifelse(dec_req == 0 & ImplementationPhase == 7, "Declaration Form Proposed", 
                          ifelse(dec_req == 0 & ImplementationPhase <= 6, "Declaration Form Required", dec_req)))

htstradedata <- join_hts10 %>%
  rbind(join_hts2_matched) %>%
  rbind(join_hts4_matched) %>%
  rbind(join_hts6_matched) %>%
  rbind(join_hts8_matched)

# clean up extra variables
rm(join_hts10, join_hts2, join_hts2_matched, join_hts4, join_hts4_matched, join_hts6, join_hts6_matched, join_hts8, join_hts8_matched)
rm(nas_hts2, nas_hts4, nas_hts6, nas_hts8)

# overwrite exclusions
exclusions <- laceydeclarations %>%
  filter(ExclusionsNoDecReq == 1) %>%
  mutate(code_length = nchar(HTS_Codes))
# * will need to modify code to account for future exclusions of different hts code lengths

htstradedata <- htstradedata %>%
  mutate(dec_req = ifelse(substr(HTS10, 1, 8) %in% exclusions$HTS_Codes, "Declaration Form Not Required", dec_req)) %>%
  select(-ExclusionsNoDecReq)

# mark everything in ch 44, 47, 48, and 6 hts6 codes in ch 94 as exclusively wood/wood fiber
htstradedata <- htstradedata %>%
  mutate(ExclusivelyContainsWood = ifelse(HTS2 == "44" | HTS2 == "47" | HTS2 == "48", 1, ExclusivelyContainsWood),
         ExclusivelyContainsWood = ifelse(HTS6 == "940161" | HTS6 == "940169"  | HTS6 == "940330" |
                                            HTS6 == "940340" | HTS6 == "940350" | HTS6 == "940360", 
                                          1, ExclusivelyContainsWood))

#write out combined data file for all years
fwrite(htstradedata, 
       paste0(dataPath, "OutputFiles\\htstradedata_laceydeclarations_27Dec2021.csv"), dateTimeAs = "write.csv")



##### Work with combined trade data, joined with Lacey Act declaration requirements #####
##### if bringing in already produced combined dataset to work with, then...#####

htstradedata <- fread(paste0(dataPath, "OutputFiles\\htstradedata_laceydeclarations.csv"))


#summarize yearly totals by hts10, with commodity descrip, retaining dec_req split
yrlysum_htstradedata <- htstradedata %>%
  group_by(year, dec_req, i_commodity_ldesc, HTS10, HTS8, HTS6, HTS4, HTS2, ExclusivelyContainsWood, ImplementationPhase, DeclarationFormRequiredBeginningDate) %>%
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


#write out file for all 2020 HTS10
yrlysum_htstradedata_2020<- yrlysum_htstradedata %>%
  filter(year == "2020") %>%
  arrange(desc(tot_gen_val_by_hts10))

fwrite(yrlysum_htstradedata_2020, paste0(dataPath, "OutputFiles\\yrlysum_htstradedata_2020.csv"), dateTimeAs = "write.csv")

#write out file for all 2021 HTS10
yrlysum_htstradedata_2021<- yrlysum_htstradedata %>%
  filter(year == "2021") %>%
  arrange(desc(tot_gen_val_by_hts10))

fwrite(yrlysum_htstradedata_2021, paste0(dataPath, "OutputFiles\\yrlysum_htstradedata_2021.csv"), dateTimeAs = "write.csv")




#summarize average over full time series by hts10, with commodity descrip, retaining dec_req split
avg_multiyear_htstradedata <- yrlysum_htstradedata %>%
  group_by(dec_req, i_commodity_ldesc, HTS10, HTS8, HTS6, HTS4, HTS2, ExclusivelyContainsWood, Examples_from_CBP_CROSS) %>%
  summarize(avg_gen_val_by_hts10 = mean(tot_gen_val_by_hts10), 
            avg_con_val_by_hts10 = mean(tot_con_val_by_hts10))



################ Create seprate data files for specific HTS Chapters ##############

## Work with Ch 44 data ##

#calculate most recent HTS2 for Ch. 44 total gen values by whether declaration is required  
HTS2Ch44_yrlysum_htstradedata <- yrlysum_htstradedata %>%
  filter(HTS2 == "44", year == "2020") %>%
  group_by(dec_req) %>%
  summarize(tot_gen_val_2020_by_hts2 = sum(tot_gen_val_by_hts10))

Ch44_2020_totval <- sum(HTS2Ch44_yrlysum_htstradedata$tot_gen_val_2020_by_hts2)

HTS2Ch44_yrlysum_htstradedata <- HTS2Ch44_yrlysum_htstradedata %>%
  mutate(pct2020val = (tot_gen_val_2020_by_hts2 / Ch44_2020_totval))

#calculate time series average HTS2 for Ch. 44 total gen values by whether declaration is required  
HTS2Ch44_avg_multiyear_htstradedata <- avg_multiyear_htstradedata %>%
  filter(HTS2 == "44") %>%
  group_by(dec_req) %>%
  summarize(tot_gen_val_avg_multiyear_by_hts2 = sum(avg_gen_val_by_hts10))

Ch44_avg_multiyear_totval <- sum(HTS2Ch44_avg_multiyear_htstradedata$tot_gen_val_avg_multiyear_by_hts2)  

HTS2Ch44_avg_multiyear_htstradedata <- HTS2Ch44_avg_multiyear_htstradedata %>%
  mutate(pctyravgval = (tot_gen_val_avg_multiyear_by_hts2 / Ch44_avg_multiyear_totval))

#join 2020 HTS2 and multi year average HTS2 for Ch. 44 
Ch44_summary <- left_join(HTS2Ch44_yrlysum_htstradedata, HTS2Ch44_avg_multiyear_htstradedata, 
                          by = "dec_req")

# clean up variables
rm(HTS2Ch44_avg_multiyear_htstradedata, HTS2Ch44_yrlysum_htstradedata)

#write summary to file
fwrite(Ch44_summary, paste0(dataPath, "OutputFiles\\Ch44_summary.csv"), dateTimeAs = "write.csv")



#write out table of 10-digit Ch. 44 data for 2019, sorted by value, with added column of percent of total general value each HTS10 contributes
Ch44_2019_htstradedata <- yrlysum_htstradedata %>%
  filter(HTS2 == "44", year == "2019") %>%
  arrange(desc(tot_gen_val_by_hts10))

Ch44_2019_totval <- sum(Ch44_2019_htstradedata$tot_gen_val_by_hts10)

Ch44_2019_htstradedata <- Ch44_2019_htstradedata %>%
  mutate(pct_tot_gen_val = (tot_gen_val_by_hts10 / Ch44_2019_totval))  
fwrite(Ch44_2019_htstradedata, paste0(dataPath, "OutputFiles\\Ch44_2019_HTS10_DeclarationRequirements.csv"), dateTimeAs = "write.csv")



## Work with Ch 94 data ##

#calculate 2019 HTS2 for Ch. 94 total gen values by whether declaration is required  
HTS2Ch94_yrlysum_htstradedata <- yrlysum_htstradedata %>%
  filter(HTS2 == "94", year == "2019") %>%
  group_by(dec_req) %>%
  summarize(tot_gen_val_2019_by_hts2 = sum(tot_gen_val_by_hts10))

Ch94_2019_totval <- sum(HTS2Ch94_yrlysum_htstradedata$tot_gen_val_2019_by_hts2)

HTS2Ch94_yrlysum_htstradedata <- HTS2Ch94_yrlysum_htstradedata %>%
  mutate(pct2019val = (tot_gen_val_2019_by_hts2 / Ch94_2019_totval))

#calculate 5-year average HTS2 for Ch. 94 total gen values by whether declaration is required  
HTS2Ch94_5yravg_htstradedata <- avg5yr_htstradedata %>%
  filter(HTS2 == "94") %>%
  group_by(dec_req) %>%
  summarize(tot_gen_val_5yravg_by_hts2 = sum(avg5yr_gen_val_by_hts10))

Ch94_5yravg_totval <- sum(HTS2Ch94_5yravg_htstradedata$tot_gen_val_5yravg_by_hts2)  

HTS2Ch94_5yravg_htstradedata <- HTS2Ch94_5yravg_htstradedata %>%
  mutate(pct5yravgval = (tot_gen_val_5yravg_by_hts2 / Ch94_5yravg_totval))

#join 2019 HTS2 and 5-year average HTS2 for Ch. 94 
Ch94_summary <- left_join(HTS2Ch94_yrlysum_htstradedata, HTS2Ch94_5yravg_htstradedata, 
                          by = "dec_req")

# clean up variables
rm(HTS2Ch94_5yravg_htstradedata, HTS2Ch94_yrlysum_htstradedata)

#write summary to file
fwrite(Ch94_summary, paste0(dataPath, "OutputFiles\\Ch94_summary.csv"), dateTimeAs = "write.csv")

# clean up variables
rm(Ch44_5yravg_totval, Ch94_5yravg_totval)

#write out table of 10-digit Ch. 94 data, sorted by value
Ch94_2019_htstradedata <- yrlysum_htstradedata %>%
  filter(HTS2 == "94", year == "2019") %>%
  arrange(desc(tot_gen_val_by_hts10))

Ch94_2019_totval <- sum(Ch94_2019_htstradedata$tot_gen_val_by_hts10)

Ch94_2019_htstradedata <- Ch94_2019_htstradedata %>%
  mutate(pct_tot_gen_val = (tot_gen_val_by_hts10 / Ch94_2019_totval))  
fwrite(Ch94_2019_htstradedata, paste0(dataPath, "OutputFiles\\Ch94_2019_HTS10_DeclarationRequirements.csv"), dateTimeAs = "write.csv")

# clean up variables
rm(Ch44_2019_totval, Ch94_2019_totval)


## Work with everything other than Ch 44 and 94 data ##
Non44_non94_2019_htstradedata <- yrlysum_htstradedata %>%
  filter(year == "2019") %>%
  filter(HTS2 != "44") %>%
  filter(HTS2 != "94") %>%
  arrange(desc(tot_gen_val_by_hts10))
fwrite(Non44_non94_2019_htstradedata, paste0(dataPath, "OutputFiles\\Non44_Non94_2019_HTS10_DeclarationRequirements.csv"), dateTimeAs = "write.csv")

#######################################################
