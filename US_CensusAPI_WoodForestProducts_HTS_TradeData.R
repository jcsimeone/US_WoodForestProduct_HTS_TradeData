# Work on US import data of wood and forest products, by value and quantity
# Simeone Consulting, LLC


# Add key to .Renviron
Sys.setenv(CENSUS_KEY="6a6b224a3057a174ebd5cd67109f2f4800d270a9")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

dataPath <- "C:\\Users\\simeo\\Desktop\\US_WoodForestProducts_Imports_HTS\\"

library(tidyr)
#library(fuzzyjoin)
library(data.table)
library(dplyr)
library(readxl)
library(censusapi)
library(rjson)
library(jsonlite)
library(httr)
library(tibble)
library(janitor)
#library(stringr)
#library(lubridate)

imports <- getCensus(
  name = "timeseries/intltrade/imports/enduse",
  vars = c("CTY_CODE", "CTY_NAME", "I_ENDUSE", "I_ENDUSE_LDESC", "GEN_VAL_MO", "CON_VAL_MO"),
  time = "2018-01")
head(imports)


intltrade_vars <- listCensusMetadata(
  name = "timeseries/intltrade/imports/hs", 
  type = "variables")
head(intltrade_vars)

ch44_imports <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH" , "I_COMMODITY_LDESC", "CTY_NAME","DISTRICT","DIST_NAME","GEN_VAL_MO", "CON_VAL_MO", "GEN_QY1_MO	", "CON_QY1_MO", "UNIT_QY1"),
  #time = "from+2020-01+to+2021-06", 
  YEAR = "2021", 
  COMM_LVL = "HS10", 
  I_COMMODITY = "44*")
head(ch44_imports)


ch44_imports_all_rows <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "I_COMMODITY_LDESC","DISTRICT","CTY_CODE","CTY_NAME","DIST_NAME","GEN_VAL_MO", "CON_VAL_MO","GEN_QY1_MO","CON_QY1_MO"),
  #time = "from+2020-01+to+2021-06", 
  YEAR = "2021", 
  COMM_LVL = "HS10", 
  I_COMMODITY = "4403*", 
  UNIT_QY1 = "M")
head(ch44_imports_all_rows)

listCensusMetadata(
  name = "timeseries/intltrade/imports/hs", 
  type = "geography")

data <- fromJSON(file = paste0(dataPath, "hs44_2021.json"))
data_df <- as.data.frame(data)
t_data_df <- t(data_df)
t_data_df <- as.data.frame(t_data_df)



url <-"https://api.census.gov/data/timeseries/intltrade/imports/hs?key=6a6b224a3057a174ebd5cd67109f2f4800d270a9&get=MONTH%2CI_COMMODITY_LDESC%2CDISTRICT%2CCTY_CODE%2CCTY_NAME%2CDIST_NAME%2CGEN_VAL_MO%2CCON_VAL_MO%2CGEN_QY1_MO%2CCON_QY1_MO%2CUNIT_QY1&YEAR=2021&COMM_LVL=HS10&I_COMMODITY=44%2A"
test <- httr::GET(url)
cont_raw <- httr::content(test)
str(cont_raw, max.level = 3, list.len = 4)

data_raw_ugly <- jsonlite::fromJSON(rawToChar(test$content))
glimpse(data_raw_ugly, max.level = 3, list.len = 4)


df_dru <- as.data.frame(data_raw_ugly)
df_dru_col <- row_to_names(df_dru, row_number =1)


?as.data.frame
########Loop through to call batches of all data in Ch. 44

