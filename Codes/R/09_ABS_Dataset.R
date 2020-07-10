#### Annual Business Survey Dataset Import ####
### The Annual Business Survey is a newly started business survey by the Census Bureau starting from
### survey year 2017. This dataset replaces five-year Survey of Business Owners (SBO) for employer businesses, 
### the Annual Survey of Entrepreneurs (ASE), the Business R&D and Innovation for Microbusinesses survey (BRDI-M), 
### and the innovation section of the Business R&D and Innovation Survey (BRDI-S)

### The dataset may provide better depiction of firm-level information ###

### Annual Business Survey datasets are accessible through Census API

library(tidyverse)
library(censusapi)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="9001b546c2d77876a089119664dc25a4235eea37")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

#### Look at ABS Metadata ####
## Detailed information about available tables and variables can be found in PDF documents here (https://www.census.gov/programs-surveys/abs/technical-documentation/api.html)

View(listCensusMetadata(name = "abscs", vintage = 2017, type = "variables"))
View(listCensusMetadata(name = "ase/csa", vintage = 2015, type = "variables"))
View(listCensusMetadata(name = "sbo", vintage = 2012, type = "variables"))

#### Get ABS Dataset ####
abscs_2017 <- getCensus(name = "abscs",
                        vintage = 2017,
                        vars = c("EMP","EMP_F","EMPSZFI","EMPSZFI_LABEL","FIRMPDEMP","FIRMPDEMP_F","YIBSZFI","YIBSZFI_LABEL","NAICS2017","NAICS2017_LABEL"),
                        region = "county:*",
                        regionin = "state:20,23,48")

## Replicating the Process by getting the raw data ##
abs_api_test <- httr::GET(url = "https://api.census.gov/data/2017/abscs?get=EMP,EMP_F,EMPSZFI,FIRMPDEMP,FIRMPDEMP_F,YIBSZFI,YIBSZFI_LABEL,NAICS2017,NAICS2017_LABEL&for=county:*&in=state:48,20,23&key=9001b546c2d77876a089119664dc25a4235eea37")
abs_api_test <- jsonlite::fromJSON(httr::content(abs_api_test, as = "text"))
colnames(abs_api_test) <- abs_api_test[1,]
abs_api_test <- data.frame(abs_api_test)
abs_api_test <- abs_api_test[-1,]

