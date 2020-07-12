#### FCC Broadband 477 Data ####
## Interacting with FCC database directly ##
## Import 2019 data and clean it ##

## Use FCC API with RSocrata package ##
#install.packages("RSocrata")
library(RSocrata)
library(tidyverse)

fcc477_2019_raw <- read.socrata(
  "https://opendata.fcc.gov/resource/sgz3-kiqt.csv?stateabbr=TX",
  app_token = "VVKDIbbHVFF1HBztnQifUisCN",
  email = "jwroycechoi@gmail.com",
  password = "Wodnjs58640!"
)

str(fcc477_2019_raw)

## Clean and Convert Data Types ##
fcc477_2019 <- fcc477_2019_raw %>% 
  select(providername, dbaname, blockcode, techcode, maxaddown, maxadup, maxcirdown, maxcirup) %>% 
  mutate(blockcode = as.character(blockcode),
         county_FIPS = substr(blockcode, 1, 5),
         techcode = as.character(techcode),
         broadband = case_when(maxaddown >= 25 & maxadup >= 3 ~ 1, TRUE ~ 0))

fcc477_2019 <- fcc477_2019 %>% 
  group_by(county_FIPS) %>% 
  mutate(broadband_pct = sum(broadband, na.rm = T) / n(),
         mediandown = median(maxaddown),
         medianup = median(maxadup),
         prom_provider = names(which(table(providername) == max(table(providername)))[1]),
         prom_tech = names(which(table(techcode) == max(table(techcode)))[1])) %>% 
  arrange(desc(maxaddown, county_FIPS)) %>% ungroup() %>% 
  distinct(county_FIPS, .keep_all = T)

write_csv2(fcc477_2019, "FCC477_TX_June_2019.csv")


