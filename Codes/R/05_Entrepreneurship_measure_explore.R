#### Exploring Economic Variables related to Entrepreneurship ####
install.packages("tidycensus")    # tidycensus is great for working with population characteristics, but not all available APIs
install.packages("censusapi")     # censusapi package allows us to access all available APIs provided by the Census Bureau
install.packages("bea.R")         # Bureau of Economic Analysis data access
library(tidycensus)
library(tidyverse)
library(censusapi)
library(bea.R)
library(ggplot2)
library(gridExtra)

## To interact with the API you need to get your API key from http://api.census.gov/data/key_signup.html ##
## A good tutorial to start off by the developer Kyle Walker is at https://walker-data.com/tidycensus/articles/basic-usage.html ##
## Getting started with censusapi package: 
# https://github.com/hrecht/censusapi
# https://hrecht.github.io/censusapi/articles/getting-started.html
## After getting your key, activate it and set it up in tidycensus to begin querying ##
census_api_key("9001b546c2d77876a089119664dc25a4235eea37", install = T, overwrite = T)
# Add key to .Renviron
Sys.setenv(CENSUS_KEY="9001b546c2d77876a089119664dc25a4235eea37")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

#### Explore datasets ####
## County Business Pattern dataset
## Business Dynamic Statistics (Timeseries)

## Look at all available API endpoints ##
csapis <- listCensusApis()
View(csapis)


#### CBP Dataset ####
## County Business Pattern dataset variables and geography
View(listCensusMetadata(name = "cbp", vintage = 2017, type = "variables"))
View(listCensusMetadata(name = "cbp", vintage = 2016, type = "variables"))
listCensusMetadata(name = "cbp", vintage = 2017, type = "geography")
listCensusMetadata(name = "cbp", vintage = 2016, type = "geography")

cbp2016_var <- listCensusMetadata(name = "cbp", vintage = 2016, type = "variables")$name

## Now lets get the data of three states at the county level ##
cbp2017_txksme <- getCensus(name = "cbp",
                            vintage = 2017,
                            vars = c("STATE","COUNTY","GEO_ID","CD","LFO","NAICS2017","INDGROUP","INDLEVEL","SECTOR","SUBSECTOR","ESTAB","EMPSZES","EMP","EMP_N","PAYANN","PAYANN_N","PAYQTR1","PAYQTR1_N"),
                            region = "county:*",
                            regionin = "state:20,23,48")

cbp2018_txksme <- getCensus(name = "cbp",
                            vintage = 2018,
                            vars = c("EMP","EMP_N","EMPSZES","ESTAB","GEO_ID","INDGROUP","INDLEVEL","LFO","NAICS2017","PAYANN","PAYQTR1","SECTOR"),
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

cbp2017_txksme <- cbp2017_txksme %>% 
  select(-c("CD","PAYANN_N","PAYQTR1_N")) %>% 
  mutate(EMP = as.numeric(EMP),
         EMPSZES = as.numeric(EMPSZES),
         ESTAB = as.numeric(ESTAB),
         PAYANN = as.numeric(PAYANN),
         PAYQTR1 = as.numeric(PAYQTR1))

cbp2018_txksme <- cbp2018_txksme %>% 
  mutate(state.name = case_when(state == "48" ~ "Texas",
                                state == "20" ~ "Kansas",
                                state == "23" ~ "Maine",
                                TRUE ~ state),
         county_FIPS = paste(state, county, sep = ""),
         EMP = as.numeric(EMP),
         EMPSZES = as.numeric(EMPSZES),
         ESTAB = as.numeric(ESTAB),
         PAYANN = as.numeric(PAYANN),
         PAYQTR1 = as.numeric(PAYQTR1))

cbp2017_tx <- cbp2017_txksme %>% filter(state_name == "Texas")
cbp2018_tx <- cbp2018_txksme %>% filter(state.name == "Texas")
str(cbp2017_tx)
str(cbp2018_tx)

#### BDS Dataset ####
## Business Dynamic Statistics dataset variables and geography
bds_vars <- listCensusMetadata(name = "timeseries/bds/firms", type = "variables")
View(listCensusMetadata(name = "timeseries/bds/firms", type = "variables"))
View(listCensusMetadata(name = "timeseries/bds/firms", type = "geography"))

bds2016 <- getCensus(name = "timeseries/bds/firms",
                     vars = c("estabs_entry","estabs_entry_rate","firms","fsize","metro","sic1","state","year","year2"),
                     region = "state:20,23,48",
                     time = 2014)

#### Nonemployer Statistics ####
nonemp2018 <- getCensus(name = "nonemp",
                        vintage = 2018,
                        vars = c("GEO_ID","INDGROUP","INDLEVEL","LFO","NAICS2017","NESTAB","NRCPTOT","RCPSZES","SECTOR"),
                        region = "county:*",
                        regionin = "state:20,23,48"
                        )

nonemp2017 <- getCensus(name = "nonemp",
                        vintage = 2017,
                        vars = c("GEO_ID","INDGROUP","INDLEVEL","LFO","NAICS2017","NESTAB","NRCPTOT","RCPSZES","SECTOR"),
                        region = "county:*",
                        regionin = "state:20,23,48")

str(nonemp2017)

nonemp2017 <- nonemp2017 %>% 
  mutate(NESTAB = as.numeric(NESTAB),
         NRCPTOT = as.numeric(NRCPTOT),
         RCPSZES = as.numeric(RCPSZES))

nonemp2018 <- nonemp2018 %>% 
  mutate(NESTAB = as.numeric(NESTAB),
         NRCPTOT = as.numeric(NRCPTOT),
         RCPSZES = as.numeric(RCPSZES))

#### Aggregating CBP Dataset ####

## Explore descriptive statistics ##
summary(cbp2017_tx[c("ESTAB","EMP","EMPSZES","PAYANN","PAYQTR1")])
summary(cbp2018_tx[c("ESTAB","EMP","EMPSZES","PAYANN","PAYQTR1")])

cbp2017_tx_agg <- cbp2017_tx %>% group_by(county_FIPS) %>% 
  summarise(avg_estab_2017 = mean(ESTAB),
            avg_emp_2017 = mean(EMP),
            avg_empszes_2017 = mean(EMPSZES),
            avg_payann_2017 = mean(PAYANN),
            avg_payqtr1_2017 = mean(PAYQTR1),
            total_estab_2017 = sum(ESTAB),
            total_emp_2017 = sum(EMP),
            pctnonemp_2017 = sum(EMP == 0) / n(),
            pctsmallent_2017 = sum(EMP > 0 & EMP <= 10) / n(),
            pctsmall_50_2017 = sum(EMP > 0 & EMP <= 50) / n(),
            total_2017 = n())

cbp2018_tx_agg <- cbp2018_tx %>% group_by(county_FIPS) %>% 
  summarise(avg_estab_2018 = mean(ESTAB),
            avg_emp_2018 = mean(EMP),
            avg_empszes_2018 = mean(EMPSZES),
            avg_payann_2018 = mean(PAYANN),
            avg_payqtr1_2018 = mean(PAYQTR1),
            total_estab_2018 = sum(ESTAB),
            total_emp_2018 = sum(EMP),
            pctnonemp_2018 = sum(EMP == 0) / n(),
            pctsmallent_2018 = sum(EMP > 0 & EMP <= 10) / n(),
            pctsmall_50_2018 = sum(EMP > 0 & EMP <= 50) / n(),
            total_2018 = n())

str(cbp2017_tx_agg)
str(cbp2018_tx_agg)

summary(cbp2017_tx_agg[c("pctnonemp_2017","pctsmallent_2017","pctsmall_50_2017")])
summary(cbp2018_tx_agg[c("pctnonemp_2018","pctsmallent_2018","pctsmall_50_2018")])

#### Aggregating Nonemployer Statistics ####
summary(nonemp2017[c("NESTAB","NRCPTOT","RCPSZES")])
# Filter Texas and non-farm industries (Based on NAICS sector definitions, I'm going to filter out sector "11": Agriculture, Forestry, Fishing, and Hunting)
nonemp2017_tx <- nonemp2017 %>%
  mutate(state.name = case_when(state == "48" ~ "Texas",
                                state == "20" ~ "Kansas",
                                state == "23" ~ "Maine",
                                TRUE ~ state),
         county_FIPS = paste(state, county, sep = "")) %>% 
  filter(state.name == "Texas" & SECTOR != "11")

nonemp2018_tx <- nonemp2018 %>%
  mutate(state.name = case_when(state == "48" ~ "Texas",
                                state == "20" ~ "Kansas",
                                state == "23" ~ "Maine",
                                TRUE ~ state),
         county_FIPS = paste(state, county, sep = "")) %>% 
  filter(state.name == "Texas"& SECTOR != "11")

# Group by county and calculate variables
# Get the total employment and establishment numbers of 2017 from Economic Census data
totalemp_est_2017 <- getCensus(name = "ecnbasic",
                               vintage = 2017,
                               vars = c("EMP","ESTAB","FIRM","GEO_ID","NAICS2017","SECTOR"),
                               region = "county:*",
                               regionin = "state:20,23,48")
# Filter and aggregate sum
totalemp_est_2017_tx <- totalemp_est_2017 %>% 
  mutate(state.name = case_when(state == "48" ~ "Texas",
                                state == "20" ~ "Kansas",
                                state == "23" ~ "Maine",
                                TRUE ~ state),
         county_FIPS = paste(state, county, sep = ""),
         EMP = as.numeric(EMP), ESTAB = as.numeric(ESTAB), FIRM = as.numeric(FIRM)) %>% 
  filter(state.name == "Texas") %>% 
  group_by(county_FIPS) %>% 
  summarise(EMP = sum(EMP, na.rm = T),
            ESTAB = sum(ESTAB, na.rm = T),
            FIRM = sum(FIRM, na.rm = T))

nonemp2017_tx_agg <- nonemp2017_tx %>% 
  group_by(county_FIPS) %>%
  summarise(avg_nestab_2017 = mean(NESTAB, na.rm = T),
            avg_nrcptot_2017 = mean(NRCPTOT, na.rm = T),
            median_nestab_2017 = median(NESTAB),
            totalnest_2017 = sum(NESTAB),
            total_ne_2017 = n())

nonemp2018_tx_agg <- nonemp2018 %>% 
  group_by(county_FIPS) %>%
  summarise(avg_nestab_2018 = mean(NESTAB, na.rm = T),
            avg_nrcptot_2018 = mean(NRCPTOT, na.rm = T),
            median_nestab_2018 = median(NESTAB),
            totalnest_2018 = sum(NESTAB),
            total_ne_2018 = n())

## Merge the two datasets from 2017 and 2018 ##
nonemp_tx <- left_join(nonemp2017_tx_agg, nonemp2018_tx_agg, by = "county_FIPS")

## Merge the two datasets from 2017 and 2018 ##
cbp_tx <- left_join(cbp2017_tx_agg, cbp2018_tx_agg, by = "county_FIPS")
str(cbp_tx)

# Calculate change variables
cbp_tx <- cbp_tx %>% 
  mutate(estab_change_2017_2018 = avg_estab_2018 - avg_estab_2017,
         emp_change_2017_2018 = avg_emp_2018 - avg_emp_2017,
         empsze_change_2017_2018 = avg_empszes_2018 - avg_empszes_2017,
         nonemp_change_2017_2018 = pctnonemp_2018 - pctnonemp_2017,
         smallbz_change_2017_2018 = pctsmallent_2018 - pctsmallent_2017,
         smallbz50_change_2017_2018 = pctsmall_50_2018 - pctsmall_50_2017)

str(cbp_tx)

## Merge to the combined dataset ##
str(tx_bb_entrepreneur_merged)
str(cbp_tx)
tx_bb_entrepreneur_merged_v2 <- tx_bb_entrepreneur_merged %>% 
  mutate(FIPS = as.character(FIPS)) %>% 
  left_join(., cbp_tx, by = c("FIPS" = "county_FIPS"))

#### Descriptive Exploration of the Entrepreneurship Variables ####

tx_bb_entrepreneur_merged_v2 %>% 
  select(IRR2010, pct_proprietors_employment_2017, venturedensitynov18, highlyactive_vdnov18, pctnonemp_2018,
         pctsmallent_2018, pctsmall_50_2018) %>% PerformanceAnalytics::chart.Correlation(histogram = T)


