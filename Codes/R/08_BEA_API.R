#### BEA API Interaction and Dataset Explore ####
## Use bea.R package. An introduction of how the package works is available in the GitHub page (https://github.com/us-bea/bea.R) ##
## Mainly we want to explore what kind of data is available and will be useful in terms of entrepreneurship.
## Specifically non-farm proprietorship information that is comparable across different states.

install.packages("bea.R")

library(bea.R)
library(tidyverse)

# Set up the API Key
beaKey <- "1C5D7A5A-D3C0-4919-8D47-820A118D7A56"

#### Checking datasets and available tables etc. ####
## BEA API exploration requires few prior knowledge on available datasets and tables
## Datasets available are listed here (https://apps.bea.gov/API/signup/index.cfm)
## Tables and parameters used for querying specific datasets are available in the appendices in the API user guide (https://apps.bea.gov/API/bea_web_service_api_user_guide.htm#tabs-9a)
## Another way to identify what you are looking for is playing around with interactive application provided by BEA here (https://apps.bea.gov/itable/itable.cfm).
## In our case we want to look at county level regional information. Specifically, nonfarm proprietors employment

### First, you can use beaSearch to see what data tables are available by keyword

beaSearch("Regional", beaKey, asHtml = T)     # Currently, regional dataset is depracated and seems like the bea.R package hasn't yet been fully updated

### Second, inspect the parameters and parameter values
## We can look at what parameters are available for querying the dataset 'Regional' by using code below
beaParams(beaKey = beaKey, 'Regional')
## Further, you can see what values are available for specific parameter using code below
beaParamVals(beaKey = beaKey, 'Regional', 'TableName')
beaParamVals(beaKey = beaKey, 'Regional', 'LineCode')

### Finally, we will use the parameter and parameter values to retrieve data table
## In terms of LineCode, we are interested in:
## 10: Total employment
## 20: Wage and salary employment
## 40: Proprietors employment
## 60: Nonfarm proprietors employment

## In terms of GeoFips, we will query all counties and then filter later on for the three states: Kansas, Maine, Texas
## In terms of Year, the research team has decided to look at the most recent year (2018) and 2012 as it is the year the nation started
## to recover from the Recession

## The LineCode parameter does not allow multiple values input.
## Therefore, we have to generate separate tables for each LineCode and merge afterwards

#### Retrive Data tables from BEA API ####
# LineCode 10
beaSpecs_10 <- list(
  'UserID' = beaKey,
  'Method' = 'GetData',
  'datasetname' = 'Regional',
  'TableName' = 'CAEMP25N',
  'LineCode' = 10,
  'GeoFips' = 'COUNTY',
  'Year' = '2012,2018'
)

bea_10 <- beaGet(beaSpecs_10, asWide = TRUE)

# LineCode 20
beaSpecs_20 <- list(
  'UserID' = beaKey,
  'Method' = 'GetData',
  'datasetname' = 'Regional',
  'TableName' = 'CAEMP25N',
  'LineCode' = 20,
  'GeoFips' = 'COUNTY',
  'Year' = '2012,2018'
)

bea_20 <- beaGet(beaSpecs_20, asWide = TRUE)

# LineCode 40
beaSpecs_40 <- list(
  'UserID' = beaKey,
  'Method' = 'GetData',
  'datasetname' = 'Regional',
  'TableName' = 'CAEMP25N',
  'LineCode' = 40,
  'GeoFips' = 'COUNTY',
  'Year' = '2012,2018'
)

bea_40 <- beaGet(beaSpecs_40, asWide = TRUE)

# LineCode 60
beaSpecs_60 <- list(
  'UserID' = beaKey,
  'Method' = 'GetData',
  'datasetname' = 'Regional',
  'TableName' = 'CAEMP25N',
  'LineCode' = 60,
  'GeoFips' = 'COUNTY',
  'Year' = '2012,2018'
)

bea_60 <- beaGet(beaSpecs_60, asWide = TRUE)

#### Clean, Filter the Data Tables and Merge ####
str(bea_10)
str(bea_20)

## BEA Total Employment
bea_10 <- bea_10 %>% filter(str_detect(GeoFips, "^48")|str_detect(GeoFips, "^20")|str_detect(GeoFips, "^23")) %>% 
  select(GeoFips, DataValue_2012, DataValue_2018) %>% 
  rename(totalemp_bea_2012 = DataValue_2012,
         totalemp_bea_2018 = DataValue_2018)
str(bea_10)
## BEA Wage and Salary Employment
bea_20 <- bea_20 %>% filter(str_detect(GeoFips, "^48")|str_detect(GeoFips, "^20")|str_detect(GeoFips, "^23")) %>% 
  select(GeoFips, DataValue_2012, DataValue_2018) %>% 
  rename(salaryemp_bea_2012 = DataValue_2012,
         salaryemp_bea_2018 = DataValue_2018)
## BEA Proprietors Employment
bea_40 <- bea_40 %>% filter(str_detect(GeoFips, "^48")|str_detect(GeoFips, "^20")|str_detect(GeoFips, "^23")) %>% 
  select(GeoFips, DataValue_2012, DataValue_2018) %>% 
  rename(proprietors_bea_2012 = DataValue_2012,
         proprietors_bea_2018 = DataValue_2018)
## BEA Nonfarm Proprietors Employment
bea_60 <- bea_60 %>% filter(str_detect(GeoFips, "^48")|str_detect(GeoFips, "^20")|str_detect(GeoFips, "^23")) %>% 
  select(GeoFips, DataValue_2012, DataValue_2018) %>% 
  rename(nonfarmproprietors_bea_2012 = DataValue_2012,
         nonfarmproprietors_bea_2018 = DataValue_2018)

## Merge Datasets ##
bea_nonfarmproprietor_txksme <- left_join(bea_10, bea_20, by = "GeoFips") %>% 
  left_join(., bea_40, by = "GeoFips") %>% left_join(., bea_60, by = "GeoFips")
str(bea_nonfarmproprietor_txksme)

## Calculate Several Variables ##
# Percentage variables and change
# Percentage of nonfarm proprietors in total employment
# Percentage change from 2012 to 2018

bea_nonfarmproprietor_txksme <- bea_nonfarmproprietor_txksme %>% 
  mutate(pct_nonfarm_bea_2012 = nonfarmproprietors_bea_2012 / totalemp_bea_2012,
         pct_nonfarm_bea_2018 = nonfarmproprietors_bea_2018 / totalemp_bea_2018,
         pct_chg_bea_2012_2018 = pct_nonfarm_bea_2018 - pct_nonfarm_bea_2012)
str(bea_nonfarmproprietor_txksme)

# Finally filter for Texas state
bea_nonfarmproprietor_tx <- bea_nonfarmproprietor_txksme %>% 
  filter(str_detect(GeoFips, "^48"))
