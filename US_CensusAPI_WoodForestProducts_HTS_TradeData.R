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
  vars = c("MONTH" , "I_COMMODITY_LDESC", "CTY_NAME","GEN_VAL_MO", "CON_VAL_MO"),
  #time = "from+2020-01+to+2021-06", 
  YEAR = "2021", 
  COMM_LVL = "HS10", 
  I_COMMODITY = "44*")
head(ch44_imports)

listCensusMetadata(
  name = "timeseries/intltrade/imports/hs", 
  type = "geography")


