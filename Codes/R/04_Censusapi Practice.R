#### Experimenting with Census API ####
## Get familiar with interacting with US Census API through tidycensus package ##
#install.packages("tidycensus")    # tidycensus is great for working with population characteristics, but not all available APIs
#install.packages("censusapi")     # censusapi package allows us to access all available APIs provided by the Census Bureau
library(tidycensus)
library(tidyverse)
library(censusapi)

## To interact with the API you need to get your API key from http://api.census.gov/data/key_signup.html ##
## A good tutorial to start off by the developer Kyle Walker is at https://walker-data.com/tidycensus/articles/basic-usage.html ##
## Getting started with censusapi package: 
# https://github.com/hrecht/censusapi
# https://hrecht.github.io/censusapi/articles/getting-started.html
## After getting your key, activate it and set it up in tidycensus to beging querying ##

# For instance when using tidycensus package:
census_api_key("9001b546c2d77876a089119664dc25a4235eea37", install = T)

# Otherwise for the censusapi package
# Add key to .Renviron
Sys.setenv(CENSUS_KEY="9001b546c2d77876a089119664dc25a4235eea37")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

#### Practice Task: Access County Business Patterns dataset (2017) and parse by few states of our interest ####
## Dataset of interest: County Business Patterns (CBP)
## States of interest: Texas (48), Kansas (20), Maine (23)

## Look at all available API endpoints ##
csapis <- listCensusApis()
View(csapis)

## Once we know what dataset we are interested in, we can look up the metadata of the dataset, such as its variables ##
cbp_vars <- listCensusMetadata(name = "cbp", vintage = 2017, type = "variables")
View(cbp_vars)    # Here we can choose variables we are interested in, but now we want all variables to be retreived

View(listCensusMetadata(name = "timeseries/bds/firms", type = "variables"))
View(listCensusMetadata(name = "timeseries/bds/firms", type = "geography"))

## Also we can see which geographic levels we can get data for
listCensusMetadata(name = "cbp", vintage = 2017, type = "geography")
listCensusMetadata(name = "cbp", vintage = 2018, type = "geography")
View(listCensusMetadata(name = "cbp", vintage = 2018, type = "variables"))

## Now lets get the data of three states at the county level ##
cbp2017_txksme <- getCensus(name = "cbp",
                            vintage = 2017,
                            vars = c("LFO","LFO_LABEL","NAICS2017","NAICS2017_LABEL","NAICS2017_F","INDGROUP","INDLEVEL","SECTOR","SUBSECTOR","ESTAB","ESTAB_F","EMPSZES","EMPSZES_LABEL","EMP","EMP_F","PAYANN","PAYANN_F","PAYQTR1","PAYQTR1_F"),
                            region = "county:*",
                            regionin = "state:20,23,48")
## Drop unnecessary columns and add few additional ones that would be useful ##
cbp2017_txksme <- cbp2017_txksme %>% 
  select(-c("STATE","COUNTY")) %>% 
  mutate(state_name = case_when(state == "48" ~ "Texas",
                                state == "20" ~ "Kansas",
                                state == "23" ~ "Maine",
                                TRUE ~ state),
         county_FIPS = paste(state, county, sep = ""))

## Separate datasets for each state ##
cbp2017_tx <- cbp2017_txksme %>% 
  filter(state == "48")
cbp2017_ks <- cbp2017_txksme %>% 
  filter(state == "20")
cbp2017_me <- cbp2017_txksme %>% 
  filter(state == "23")

## Export datasets
write_csv(cbp2017_tx, "CBP_2017_TX.csv", na = "")
write_csv(cbp2017_ks, "CBP_2017_KS.csv", na = "")
write_csv(cbp2017_me, "CBP_2017_ME.csv", na = "")

#### ACS Broadband Subscription Data ####
View(listCensusMetadata(name = "acs/acs5", vintage = 2018, type = "variables"))
View(listCensusMetadata(name = "acs/acs5/profile", vintage = 2018, type = "variables"))
View(listCensusMetadata(name = "acs/acs5/subject", vintage = 2018, type = "variables"))
View(listCensusMetadata(name = "acs/acs5/subject", vintage = 2018, type = "geography"))

## ACS Detailed Table ##
acs_broadband <- getCensus(name = "acs/acs5",
                           vintage = 2018,
                           vars = c("B28002_002E","B28002_003E","B28002_004E","B28002_005E","B28002_006E","B28002_007E","B28002_008E","B28002_009E","B28002_010E","B28002_011E",
                                    "B28011_002E","B28011_003E","B28011_004E","B28011_005E","B28011_006E"),
                           region = "county:*",
                           regionin = "state:20,23,48")
## Broadband Subscription Percentage from Subject Table ##
acs_broadband_pct <- getCensus(name = "acs/acs5/subject", vintage = 2018,
                               vars = c("S2801_C02_017E","S2801_C02_019E","S2801_C02_016E","S2801_C02_018E"),
                               region = "county:*",
                               regionin = "state:20,23,48")

str(acs_broadband_pct)
table(acs_broadband_pct$state)

# Rename variables #
acs_broadband_pct <- acs_broadband_pct %>% 
  rename(pct_cellular_acs_2018 = S2801_C02_016E,
         pct_fixed_acs_2018 = S2801_C02_017E,
         pct_sat_acs_2018 = S2801_C02_018E,
         pct_nobroadband_acs_2018 = S2801_C02_019E) %>% 
  mutate(pct_anybroadband_acs_2018 = 100 - pct_nobroadband_acs_2018,
         county_FIPS = paste(state, county, sep = ""))

str(acs_broadband_pct)

acs_broadband_pct_tx <- acs_broadband_pct %>% 
  filter(state == "48")

tx_bb_entrepreneur_merged_v2 <- tx_bb_entrepreneur_merged_v2 %>% 
  mutate(county_FIPS = as.character(FIPS)) %>% 
  left_join(., acs_broadband_pct_tx, by = "county_FIPS")

write_csv(tx_bb_entrepreneur_merged_v2, "Broadband-Entrepreneurship-TX-merged_v2.csv")

