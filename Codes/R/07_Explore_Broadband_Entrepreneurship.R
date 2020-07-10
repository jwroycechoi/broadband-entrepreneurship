#### Looking Deeper into Broadband and Entrepreneurship Measures ####
## Package Import ##
library(maptools)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(spdep)
library(tigris)
library(ggplot2)
library(sf)
library(sp)
library(stargazer)
library(gridExtra)

#### Import the merged dataset V2 ####
tx_bb_entrepreneur_merged_v2 <- read_csv("https://raw.githubusercontent.com/jwroycechoi/broadband-entrepreneurship/master/Datasets/Broadband-Entrepreneurship-TX-merged_v2.csv")

## Set up spatial dataframe
tx_county <- counties("Texas")
tx_bb_entrepreneur_merged_v3 <- sp::merge(tx_county, tx_bb_entrepreneur_merged_v2,
                                          by.x = "GEOID", by.y = "FIPS")

#### Broadband Measures ####
## Our braodband measures are 
# FCC Broadband: FCC broadband availability from dataset provided by Microsoft (2017)
# MS Broadband: Broadband availability by MS (Nov 2019)
# M-Lab Broadband
# ACS Broadband Subscription

#### Descriptive Statistics ####
tx_bb_entrepreneur_merged_v2 %>% 
  select(pct_broadband_FCC, pct_broadband_MS, pctbbfrac_ASU, pct_broadband_mlab) %>% 
  stargazer(as.data.frame(.), type = "text")

#### Distribution of Broadband Measures ####
grid.arrange(
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_FCC)) + geom_histogram() + theme_minimal() + xlab("FCC Broadband"),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_MS)) + geom_histogram() + theme_minimal() + xlab("MS Broadband"),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pctbbfrac_ASU)) + geom_histogram() + theme_minimal() + xlab("ACS Broadband Subscription"),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_mlab)) + geom_histogram() + theme_minimal() + xlab("M-Lab Broadband"),
  nrow = 2, ncol = 2, top = "Broadband Measure Distribution"
)

#### Correlations b/w Broadband Measures ####
grid.arrange(
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_FCC, y = pct_broadband_MS)) + geom_point() + geom_smooth(method = "lm") + theme_minimal() + ylab("Microsoft Broadband") + xlab("FCC Broadband"),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_FCC, y = pctbbfrac_ASU)) + geom_point() + geom_smooth(method = "lm") + ylab("ACS BB Subscription") + xlab("FCC Broadband") + theme_minimal(),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_FCC, y = pct_broadband_mlab)) + geom_point() + geom_smooth(method = "lm") + theme_minimal() + ylab("M-Lab Broadband") + xlab("FCC Broadband"),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_MS, y = pct_broadband_mlab)) + geom_point() + geom_smooth(method = "lm") + theme_minimal() + ylab("M-Lab Broadband") + xlab("Microsoft Broadband"),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pct_broadband_MS, y = pctbbfrac_ASU)) + geom_point() + geom_smooth(method = "lm") + ylab("ACS BB Subscription") + xlab("Microsoft Broadband") + theme_minimal(),
  ggplot(tx_bb_entrepreneur_merged_v2, aes(x = pctbbfrac_ASU, y = pct_broadband_mlab)) + geom_point() + geom_smooth(method = "lm") + theme_minimal() + ylab("M-Lab Broadband") + xlab("ACS BB Subscription"),
  nrow = 2, ncol = 3, top = "Correlations b/w Broadband Measures"
)

#### Mapping Broadband Measures ####
## Create continuous color palette function based on FCC broadband range (0~1) ##
pal <- colorNumeric(palette = "YlOrRd", domain = tx_bb_entrepreneur_merged_v2$pct_broadband_FCC)
## Set a popup template ##
popup_tx <- paste0("<strong>", tx_bb_entrepreneur_merged_v3$county,
                   "</strong><br />FCC: ", tx_bb_entrepreneur_merged_v3$pct_broadband_FCC*100, "%",
                   "<br />MS: ", tx_bb_entrepreneur_merged_v3$pct_broadband_MS*100, "%",
                   "<br />ACS: ", tx_bb_entrepreneur_merged_v3$pctbbfrac_ASU*100, "%",
                   "<br />M-Lab: ", round(tx_bb_entrepreneur_merged_v3$pct_broadband_mlab*100, digits = 1), "%")
head(popup_tx)
## Create a map object ##
map <- leaflet(tx_bb_entrepreneur_merged_v3)

## FCC Broadband Map ##
map %>% addTiles() %>% setView(lng = -100.000, lat = 31.000, zoom = 6) %>% 
  addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                                   color = ~pal(pct_broadband_FCC),
              popup = ~popup_tx) %>% 
  addLegend("bottomright", pal = pal, values = ~pct_broadband_FCC,
            title = "FCC Broadband (%)",
            labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)

## MS Broadband Map ##
map %>% addTiles() %>% setView(lng = -100.000, lat = 31.000, zoom = 6) %>% 
  addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
              color = ~pal(pct_broadband_MS),
              popup = ~popup_tx) %>% 
  addLegend("bottomright", pal = pal, values = ~pct_broadband_MS,
            title = "MS Broadband (%)",
            labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)

## ACS Broadband Subscription Map ##
map %>% addTiles() %>% setView(lng = -100.000, lat = 31.000, zoom = 6) %>% 
  addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
              color = ~pal(pctbbfrac_ASU),
              popup = ~popup_tx) %>% 
  addLegend("bottomright", pal = pal, values = ~pctbbfrac_ASU,
            title = "ACS Broadband Subscription (%)",
            labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)

## M-Lab Broadband Map ##
map %>% addTiles() %>% setView(lng = -100.000, lat = 31.000, zoom = 6) %>% 
  addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
              color = ~pal(pct_broadband_mlab),
              popup = ~popup_tx) %>% 
  addLegend("bottomright", pal = pal, values = ~pct_broadband_mlab,
            title = "M-Lab Broadband (%)",
            labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)


#### Importing New FCC 477 Data as of June 2019 ####
## Use FCC API with RSocrata package ##
install.packages("RSocrata")
library(RSocrata)

fcc477_2019 <- read.socrata(
  "https://opendata.fcc.gov/resource/sgz3-kiqt.csv?stateabbr=TX",
  app_token = "VVKDIbbHVFF1HBztnQifUisCN",
  email = "jwroycechoi@gmail.com",
  password = "Wodnjs58640!"
)


#### Entrepreneurship Measures ####
str(tx_bb_entrepreneur_merged_v2)
## Entrepreneurship related variables in the datasets are:
# pct_proprietors_employment_ : % of sole proprietors in total employment (2000, 2010, 2017) (for TX only)
# venturedensity : Venture density by GoDaddy database (May 2018, Nov 2018, Feb 2019, Sep 2019, Oct 2019, Nov 2019, Dec 2019)
# venturedensity_mean : Average venture density
# highlyactive_vd : Highly active venter density by GoDaddy database (May 2018, Nov 2018, Feb 2019, Sep 2019, Oct 2019, Nov 2019, Dec 2019)
# highlyactive_vd_mean : Average highly active venture density
# pctnonemp_ : % of business establishment with 0 employee (2017, 2018)
# pctsmallent_ : % of business establishment with 1-10 employee (2017, 2018)
# pctsmall_50_ : % of business establishment with 11-50 employee (2017, 2018)
# total_ne_ : # of nonemployer observation per county (2017, 2018)
# avg_nestab_ : Average number of nonemployer establishment per county (2017, 2018)
# median_nestab_ : Median of nonemployer establishment per county (2017, 2018)



