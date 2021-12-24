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

########### Practice using cencusapi package to retrieve data from US Census Int'l Trade import data, 
#Abandonded because if interested in quantity and therefore unit variable, units comes in as NAs rather than actual units
# intltrade_vars <- listCensusMetadata(
#   name = "timeseries/intltrade/imports/hs", 
#   type = "variables")
# head(intltrade_vars)
# 
# ch44_imports <- getCensus(
#   name = "timeseries/intltrade/imports/hs",
#   vars = c("MONTH" , "I_COMMODITY_LDESC", "CTY_NAME","DISTRICT","DIST_NAME","GEN_VAL_MO", "CON_VAL_MO", "GEN_QY1_MO	", "CON_QY1_MO", "UNIT_QY1"),
#   #time = "from+2020-01+to+2021-06", 
#   YEAR = "2021", 
#   COMM_LVL = "HS10", 
#   I_COMMODITY = "44*")
# head(ch44_imports)
# 
# 
# ch44_imports_all_rows <- getCensus(
#   name = "timeseries/intltrade/imports/hs",
#   vars = c("MONTH", "I_COMMODITY_LDESC","DISTRICT","CTY_CODE","CTY_NAME","DIST_NAME","GEN_VAL_MO", "CON_VAL_MO","GEN_QY1_MO","CON_QY1_MO"),
#   #time = "from+2020-01+to+2021-06", 
#   YEAR = "2021", 
#   COMM_LVL = "HS10", 
#   I_COMMODITY = "4403*", 
#   UNIT_QY1 = "M")
# head(ch44_imports_all_rows)


#######Testing calling US Census data via webrowser, saving as JSON, and bringing into R
# data <- fromJSON(file = paste0(dataPath, "hs44_2021.json"))
# data_df <- as.data.frame(data)
# t_data_df <- t(data_df)
# t_data_df <- as.data.frame(t_data_df)


######### Test calling Census api through R (using httr) and convert matrix to data frame w/ first row as column headings
url <-"https://api.census.gov/data/timeseries/intltrade/imports/hs?key=6a6b224a3057a174ebd5cd67109f2f4800d270a9&get=MONTH%2CI_COMMODITY_LDESC%2CDISTRICT%2CCTY_CODE%2CCTY_NAME%2CDIST_NAME%2CGEN_VAL_MO%2CCON_VAL_MO%2CGEN_QY1_MO%2CCON_QY1_MO%2CUNIT_QY1&YEAR=2021&COMM_LVL=HS10&I_COMMODITY=44%2A"
batch <- httr::GET(url)
#cont_raw <- httr::content(batch)
#str(cont_raw, max.level = 3, list.len = 4)

batch_raw <- jsonlite::fromJSON(rawToChar(batch$content))
glimpse(batch_raw, max.level = 3, list.len = 4)


df_batch_raw <- as.data.frame(batch_raw)
df_batch_raw_col <- row_to_names(df_batch_raw, row_number =1)


########Test to Loop through to bring in batches of all data in Ch. 44 for 2019, 2020, 2021

#files <- list.files(inputPath)

compiled <- data.frame()

for(i in 2019:2021){
  
  print(i)
  infile <- fread(paste0(inputPath, "\\", files[i]))
  infile <- clean_names(infile)
  colnames(infile) <- gsub("_d_u_n_sv_r","_duns",colnames(infile))
  colnames(infile) <- gsub("_d_u_n_s_r","_duns",colnames(infile))
  infile <- as.data.frame(infile)
  
  if(i==1){
    compiled <- infile
  }else{
    compiled <- rbind(compiled, infile)
  }
  
  i=i+1
  
}

