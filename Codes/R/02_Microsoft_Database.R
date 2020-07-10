#### Download and explore broadband dataset shared by Microsoft ####
#### Data available at https://github.com/microsoft/USBroadbandUsagePercentages ####

## Here we import the dataset directly from the GitHub repo ##

ms_broadband <- read.csv("https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data.csv", header = T)

str(ms_broadband)

ms_broadband %>% filter(ST == "TX")
