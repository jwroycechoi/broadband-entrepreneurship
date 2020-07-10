rm(list=ls())

#### A practice to retrive public dataste from BigQuery database ####
## Tutorial information from 
## https://cloud.google.com/blog/products/gcp/google-cloud-platform-for-data-scientists-using-r-with-google-bigquery ##
## https://rpubs.com/shivanandiyer/BigRQuery ##
## Need to set up a Google Cloud platform project beforehand ##

#install.packages("bigrquery")
devtools::install_github("rstats-db/bigrquery")
install.packages("DBI")

library(tidyverse)
library(DBI)
library(bigrquery)

#### Set up the authentication for the Google Cloud Platform & BigQuery API ####

bq_auth()       # A browser window will pop up

#### Set up the connection ####


bq_con <- dbConnect(
  bigquery(),
  project = "measurement-lab",
  dataset = "mlab_statistics"
)

bq_con

ndt_unified_con <- dbConnect(
  bigquery(),
  project = "measurement-lab",
  dataset = "ndt",
  use_legacy_sql = FALSE
)

## See the list of tables in BigQuery dataset ##

dbListTables(bq_con)
dbListTables(ndt_unified_con)

#### Call the data tables from BigQuery dataset into a tibble ####

us_county_ndt_month <- tbl(bq_con, "us_county_ndt_month")
us_county_ndt_week <- tbl(bq_con, "us_county_ndt_week")

class(us_county_ndt_month)

ndt_unified_downloads <- tbl(ndt_unified_con, "unified_downloads")
ndt_unified_uploads <- tbl(ndt_unified_con, "unified_uploads")

## We can now use dplyr functions to play with these data tables ##

str(us_county_ndt_month)

summary(us_county_ndt_month)

texas_ndt_month <- us_county_ndt_month %>% filter(state == "Texas") %>% group_by(county_name) %>% 
  summarise(county = county_name,
            MEAN_dl = mean(MEAN_download_Mbps, na.rm = T),
            MEAN_up = mean(MEAN_upload_Mbps, na.rm = T),
            county_sample_dl = mean(county_dl_sample_size),
            county_sample_ul = mean(county_ul_sample_size)) %>% collect()


us_county_ndt_month %>% group_by(time_period) %>% summarise(meandl = mean(MEAN_download_Mbps, na.rm = T))
us_county_ndt_week %>% group_by(time_period) %>% summarise(meandl = mean(MEAN_download_Mbps, na.rm = T))

#### Explore the Unified view dataset ####

sql_query <- '
SELECT *
FROM `measurement-lab.ndt.unified_downloads`
WHERE client.Geo.city = "US" AND client.Geo.region = "TX"
'

bq_test_mlab <- bq_project_query("measurement-lab", sql_query)

bq_table_download(bq_test_mlab)

