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
  vars = c("MONTH", "I_COMMODITY_LDESC","DISTRICT","CTY_CODE","CTY_NAME","DIST_NAME","GEN_VAL_MO", "CON_VAL_MO","GEN_QY1_MO","CON_QY1_MO","UNIT_QY1", "QTY_1_MO", "QTY_1_MO_FLAG"),
  #time = "from+2020-01+to+2021-06", 
  YEAR = "2021", 
  COMM_LVL = "HS10", 
  I_COMMODITY = "4403*")
head(ch44_imports_all_rows)

listCensusMetadata(
  name = "timeseries/intltrade/imports/hs", 
  type = "geography")

########Loop through to call batches of all data in Ch. 44

