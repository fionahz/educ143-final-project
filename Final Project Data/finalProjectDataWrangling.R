library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gridExtra)
library(cluster)   
library(dendextend)
library(fpc)
library(RCA)
library(tidyverse)
library(dotwhisker)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
install.packages("glmnet")
library(glmnet)
library(broom)
library(caret)


# Set your working directory
setwd("/Users/michelleleung/Documents/GitHub/educ143-final-project/Final Project Data/")

pub_school_data <- read_tsv("pubschls.txt")
pub_school_data <- pub_school_data %>%
  filter(Latitude != "No Data", Longitude != "No Data")
caaspp_score_data_19 <- read_delim("sb_ca2019_1_csv_v3.txt", delim=",")
caaspp_score_data_18 <- read_delim("sb_ca2018_1_csv_v3.txt", delim=",")
caaspp_score_data_17 <- read_delim("sb_ca2017_1_csv_v2.txt", delim=",")
caaspp_score_data_16 <- read_delim("sb_ca2016_1_csv_v3.txt", delim=",")
caaspp_score_data_15 <- read_delim("sb_ca2015_1_csv_v3.txt", delim=",")

fire_incident_data <- read_csv("mapdataall.csv") 
fire_incident_data <- fire_incident_data %>%
  filter(as.numeric(substring(as.character(incident_date_extinguished), 1, 4)) > 2013)

fire_incidents_2014 <- fire_incident_data %>%
  filter(as.numeric(substring(as.character(incident_date_extinguished), 1, 4)) == 2014)

fire_incidents_2015 <- fire_incident_data %>%
  filter(as.numeric(substring(as.character(incident_date_extinguished), 1, 4)) == 2015)

fire_incidents_2016 <- fire_incident_data %>%
  filter(as.numeric(substring(as.character(incident_date_extinguished), 1, 4)) == 2016)

fire_incidents_2017 <- fire_incident_data %>%
  filter(as.numeric(substring(as.character(incident_date_extinguished), 1, 4)) == 2017)

fire_incidents_2018 <- fire_incident_data %>%
  filter(as.numeric(substring(as.character(incident_date_extinguished), 1, 4)) == 2018)

# Web Scraping to get site details for sites measuring PM25 Daily Data 
aq_pm25_sites_2014_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2014&param=PM25&units=001&o3area=&o3pa8=&county_name=--COUNTY--&latitude=A-Whole+State&basin=--AIR+BASIN--&IDType=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=SITELIST&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_sites_2014  <- aq_pm25_sites_2014_url %>%  
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_sites_2014 <- data.frame(aq_pm25_sites_2014[[1]])
aq_pm25_sites_2014 <- aq_pm25_sites_2014[3:nrow(aq_pm25_sites_2014),]

aq_pm25_sites_2014 <- aq_pm25_sites_2014 %>%
  rename(number = X1, basin = X2, county = X3, site_name = X4, real_time = X5, site = X6, met_id = X7, latitude = X8, longitude = X9, elevation = X10, obs_for_year = X11) %>%
  select(basin, county, site_name, site, latitude, longitude, elevation, obs_for_year)

aq_pm25_sites_2015_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2015&param=PM25&units=001&o3area=&o3pa8=&county_name=--COUNTY--&latitude=A-Whole+State&basin=--AIR+BASIN--&IDType=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=SITELIST&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_sites_2015  <- aq_pm25_sites_2015_url %>%  
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_sites_2015 <- data.frame(aq_pm25_sites_2015[[1]])
aq_pm25_sites_2015 <- aq_pm25_sites_2015[3:nrow(aq_pm25_sites_2015),]

aq_pm25_sites_2015 <- aq_pm25_sites_2015 %>%
  rename(number = X1, basin = X2, county = X3, site_name = X4, real_time = X5, site = X6, met_id = X7, latitude = X8, longitude = X9, elevation = X10, obs_for_year = X11) %>%
  select(basin, county, site_name, site, latitude, longitude, elevation, obs_for_year)

aq_pm25_sites_2016_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2016&param=PM25&units=001&o3area=&o3pa8=&county_name=--COUNTY--&latitude=A-Whole+State&basin=--AIR+BASIN--&IDType=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=SITELIST&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_sites_2016  <- aq_pm25_sites_2016_url %>%  
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_sites_2016 <- data.frame(aq_pm25_sites_2016[[1]])
aq_pm25_sites_2016 <- aq_pm25_sites_2016[3:nrow(aq_pm25_sites_2016),]

aq_pm25_sites_2016 <- aq_pm25_sites_2016 %>%
  rename(number = X1, basin = X2, county = X3, site_name = X4, real_time = X5, site = X6, met_id = X7, latitude = X8, longitude = X9, elevation = X10, obs_for_year = X11) %>%
  select(basin, county, site_name, site, latitude, longitude, elevation, obs_for_year)

aq_pm25_sites_2017_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2017&param=PM25&units=001&o3area=&o3pa8=&county_name=--COUNTY--&latitude=A-Whole+State&basin=--AIR+BASIN--&IDType=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=SITELIST&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_sites_2017  <- aq_pm25_sites_2017_url %>%  
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_sites_2017 <- data.frame(aq_pm25_sites_2017[[1]])
aq_pm25_sites_2017 <- aq_pm25_sites_2017[3:nrow(aq_pm25_sites_2017),]

aq_pm25_sites_2017 <- aq_pm25_sites_2017 %>%
  rename(number = X1, basin = X2, county = X3, site_name = X4, real_time = X5, site = X6, met_id = X7, latitude = X8, longitude = X9, elevation = X10, obs_for_year = X11) %>%
  select(basin, county, site_name, site, latitude, longitude, elevation, obs_for_year)

aq_pm25_sites_2018_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2018&param=PM25&units=001&o3area=&o3pa8=&county_name=--COUNTY--&latitude=A-Whole+State&basin=--AIR+BASIN--&IDType=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=SITELIST&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_sites_2018  <- aq_pm25_sites_2018_url %>%  
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_sites_2018 <- data.frame(aq_pm25_sites_2018[[1]])
aq_pm25_sites_2018 <- aq_pm25_sites_2018[3:nrow(aq_pm25_sites_2018),]

aq_pm25_sites_2018 <- aq_pm25_sites_2018 %>%
  rename(number = X1, basin = X2, county = X3, site_name = X4, real_time = X5, site = X6, met_id = X7, latitude = X8, longitude = X9, elevation = X10, obs_for_year = X11) %>%
  select(basin, county, site_name, site, latitude, longitude, elevation, obs_for_year)

aq_pm25_sites_2019_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2019&param=PM25&units=001&o3area=&o3pa8=&county_name=--COUNTY--&latitude=A-Whole+State&basin=--AIR+BASIN--&IDType=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=SITELIST&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_sites_2019  <- aq_pm25_sites_2019_url %>%  
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_sites_2019 <- data.frame(aq_pm25_sites_2019[[1]])
aq_pm25_sites_2019 <- aq_pm25_sites_2019[3:nrow(aq_pm25_sites_2019),]

aq_pm25_sites_2019 <- aq_pm25_sites_2019 %>%
  rename(number = X1, basin = X2, county = X3, site_name = X4, real_time = X5, site = X6, met_id = X7, latitude = X8, longitude = X9, elevation = X10, obs_for_year = X11) %>%
  select(basin, county, site_name, site, latitude, longitude, elevation, obs_for_year)

# Web Scraping to get yearly site summary info for air quality (specifically particulate matter)

aq_pm25_2014_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?param=PM25&units=001&year=2014&county_name=--COUNTY--&basin=--AIR+BASIN--&latitude=A-Whole+State&report=ASRPT&order=basin%2Ccounty_name%2Cs.name&submit=Retrieve+Data&ptype=aqd&std15=")
aq_pm25_2014_year  <- aq_pm25_2014_url %>%
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_2014_year <- data.frame(aq_pm25_2014_year[[1]])
aq_pm25_2014_year <- aq_pm25_2014_year[4:nrow(aq_pm25_2014_year),]

aq_pm25_2014_year <- aq_pm25_2014_year %>%
  rename(basin = X1, county = X2, site_name_and_monitor_id_2014 = X3, days_above_nat_std_2014 = X4, hr_max_2014 = X5, day_max_2014 = X6)

aq_pm25_2015_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2015&param=PM25&units=001&county_name=--COUNTY--&basin=--AIR+BASIN--&latitude=A-Whole+State&std15=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=ASRPT&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_2015_year  <- aq_pm25_2015_url %>%
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_2015_year <- data.frame(aq_pm25_2015_year[[1]])
aq_pm25_2015_year <- aq_pm25_2015_year[4:nrow(aq_pm25_2015_year),]

aq_pm25_2015_year <- aq_pm25_2015_year %>%
  rename(basin = X1, county = X2, site_name_and_monitor_id_2015 = X3, days_above_nat_std_2015 = X4, hr_max_2015 = X5, day_max_2015 = X6)

aq_pm25_2016_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2016&param=PM25&units=001&county_name=--COUNTY--&basin=--AIR+BASIN--&latitude=A-Whole+State&std15=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=ASRPT&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_2016_year  <- aq_pm25_2016_url %>%
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_2016_year <- data.frame(aq_pm25_2016_year[[1]])
aq_pm25_2016_year <- aq_pm25_2016_year[4:nrow(aq_pm25_2016_year),]

aq_pm25_2016_year <- aq_pm25_2016_year %>%
  rename(basin = X1, county = X2, site_name_and_monitor_id_2016 = X3, days_above_nat_std_2016 = X4, hr_max_2016 = X5, day_max_2016 = X6)

aq_pm25_2017_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2017&param=PM25&units=001&county_name=--COUNTY--&basin=--AIR+BASIN--&latitude=A-Whole+State&std15=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=ASRPT&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_2017_year  <- aq_pm25_2017_url %>%
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_2017_year <- data.frame(aq_pm25_2017_year[[1]])
aq_pm25_2017_year <- aq_pm25_2017_year[4:nrow(aq_pm25_2017_year),]

aq_pm25_2017_year <- aq_pm25_2017_year %>%
  rename(basin = X1, county = X2, site_name_and_monitor_id_2017 = X3, days_above_nat_std_2017 = X4, hr_max_2017 = X5, day_max_2017 = X6)

aq_pm25_2018_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2018&param=PM25&units=001&county_name=--COUNTY--&basin=--AIR+BASIN--&latitude=A-Whole+State&std15=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=ASRPT&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_2018_year  <- aq_pm25_2018_url %>%
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_2018_year <- data.frame(aq_pm25_2018_year[[1]])
aq_pm25_2018_year <- aq_pm25_2018_year[4:nrow(aq_pm25_2018_year),]

aq_pm25_2018_year <- aq_pm25_2018_year %>%
  rename(basin = X1, county = X2, site_name_and_monitor_id_2018 = X3, days_above_nat_std_2018 = X4, hr_max_2018 = X5, day_max_2018 = X6)

aq_pm25_2019_url <- read_html("https://www.arb.ca.gov/aqmis2/display.php?year=2019&param=PM25&units=001&county_name=--COUNTY--&basin=--AIR+BASIN--&latitude=A-Whole+State&std15=&o3switch=new&hours=all&ptype=aqd&mon=&day=&report=ASRPT&statistic=DAVG&order=&btnsubmit=Update+Display")
aq_pm25_2019_year  <- aq_pm25_2019_url %>%
  html_nodes("table") %>% 
  .[3] %>%
  html_table(fill = TRUE)

aq_pm25_2019_year <- data.frame(aq_pm25_2019_year[[1]])
aq_pm25_2019_year <- aq_pm25_2019_year[4:nrow(aq_pm25_2019_year),]

aq_pm25_2019_year <- aq_pm25_2019_year %>%
  rename(basin = X1, county = X2, site_name_and_monitor_id_2019 = X3, days_above_nat_std_2019 = X4, hr_max_2019 = X5, day_max_2019 = X6)

aq_pm25_2014_year <- aq_pm25_2014_year %>%
  mutate(site_name = gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(site_name_and_monitor_id_2014))) %>%
  group_by(site_name) %>%
  slice(which.max(days_above_nat_std_2014))

aq_pm25_2015_year <- aq_pm25_2015_year %>%
  mutate(site_name = gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(site_name_and_monitor_id_2015))) %>%
  group_by(site_name) %>%
  slice(which.max(days_above_nat_std_2015))

aq_pm25_2016_year <- aq_pm25_2016_year %>%
  mutate(site_name = gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(site_name_and_monitor_id_2016))) %>%
  group_by(site_name) %>%
  slice(which.max(days_above_nat_std_2016))

aq_pm25_2017_year <- aq_pm25_2017_year %>%
  mutate(site_name = gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(site_name_and_monitor_id_2017))) %>%
  group_by(site_name) %>%
  slice(which.max(days_above_nat_std_2017))

aq_pm25_2018_year <- aq_pm25_2018_year %>%
  mutate(site_name = gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(site_name_and_monitor_id_2018))) %>%
  group_by(site_name) %>%
  slice(which.max(days_above_nat_std_2018))

aq_pm25_2019_year <- aq_pm25_2019_year %>%
  mutate(site_name = gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(site_name_and_monitor_id_2019))) %>%
  group_by(site_name) %>%
  slice(which.max(days_above_nat_std_2019))

seda_data <- read_csv("SEDA_cov_school_pool_v30.csv")
seda_data_CA <- seda_data %>% filter(stateabb=="CA")
combine <- left_join(pub_school_data, seda_data_CA, by=c("School" = "schnam"))


seda_data_CA$ncessch <- substr(seda_data_CA$ncessch, 8, 12)
combine <- left_join(pub_school_data, seda_data_CA, by=c("NCESSchool" = "ncessch"))
combine_filter <- combine %>% filter(NCESSchool != "No Data" )

# Combine Air Quality Data with School Codes
aq_site_data_2014 <- inner_join(aq_pm25_2014_year, aq_pm25_sites_2014, by=c("site_name"))
aq_site_data_2015 <- inner_join(aq_pm25_2015_year, aq_pm25_sites_2015, by=c("site_name"))
aq_site_data_2016 <- inner_join(aq_pm25_2016_year, aq_pm25_sites_2016, by=c("site_name"))
aq_site_data_2017 <- inner_join(aq_pm25_2017_year, aq_pm25_sites_2017, by=c("site_name"))
aq_site_data_2018 <- inner_join(aq_pm25_2018_year, aq_pm25_sites_2018, by=c("site_name"))

aq_2014_collection_latlngs <- aq_site_data_2014 %>%
  select(site_name, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2014 <- school_latlngs

for (i in sequence(nrow(aq_2014_collection_latlngs))) {
  site_column_name <- as.character(aq_2014_collection_latlngs[i, "site_name"])
  site_lat <- as.numeric(aq_2014_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2014_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2014 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site_name_2014", values_to = 'site_distance_2014')

schl_ext_2014 <- temp_2014 %>%
  group_by(CDSCode) %>%
  summarize(site_distance_2014 = min(site_distance_2014)) %>%
  inner_join(temp_2014, by=c('CDSCode', 'site_distance_2014'))

site_distance_summary_2014 <- site_distance_summary_2014 %>%
  left_join(schl_ext_2014, by='CDSCode')


aq_2015_collection_latlngs <- aq_site_data_2015 %>%
  select(site_name, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2015 <- school_latlngs

for (i in sequence(nrow(aq_2015_collection_latlngs))) {
  site_column_name <- as.character(aq_2015_collection_latlngs[i, "site_name"])
  site_lat <- as.numeric(aq_2015_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2015_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2015 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site_name_2015", values_to = 'site_distance_2015')

schl_ext_2015 <- temp_2015 %>%
  group_by(CDSCode) %>%
  summarize(site_distance_2015 = min(site_distance_2015)) %>%
  inner_join(temp_2015, by=c('CDSCode', 'site_distance_2015'))

site_distance_summary_2015 <- site_distance_summary_2015 %>%
  left_join(schl_ext_2015, by='CDSCode')


aq_2016_collection_latlngs <- aq_site_data_2016 %>%
  select(site_name, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2016 <- school_latlngs

for (i in sequence(nrow(aq_2016_collection_latlngs))) {
  site_column_name <- as.character(aq_2016_collection_latlngs[i, "site_name"])
  site_lat <- as.numeric(aq_2016_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2016_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2016 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site_name_2016", values_to = 'site_distance_2016')

schl_ext_2016 <- temp_2016 %>%
  group_by(CDSCode) %>%
  summarize(site_distance_2016 = min(site_distance_2016)) %>%
  inner_join(temp_2016, by=c('CDSCode', 'site_distance_2016'))

site_distance_summary_2016 <- site_distance_summary_2016 %>%
  left_join(schl_ext_2016, by='CDSCode')

aq_2017_collection_latlngs <- aq_site_data_2017 %>%
  select(site_name, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2017 <- school_latlngs

for (i in sequence(nrow(aq_2017_collection_latlngs))) {
  site_column_name <- as.character(aq_2017_collection_latlngs[i, "site_name"])
  site_lat <- as.numeric(aq_2017_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2017_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2017 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site_name_2017", values_to = 'site_distance_2017')

schl_ext_2017 <- temp_2017 %>%
  group_by(CDSCode) %>%
  summarize(site_distance_2017 = min(site_distance_2017)) %>%
  inner_join(temp_2017, by=c('CDSCode', 'site_distance_2017'))

site_distance_summary_2017 <- site_distance_summary_2017 %>%
  left_join(schl_ext_2017, by='CDSCode')


aq_2018_collection_latlngs <- aq_site_data_2018 %>%
  select(site_name, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2018 <- school_latlngs

for (i in sequence(nrow(aq_2018_collection_latlngs))) {
  site_column_name <- as.character(aq_2018_collection_latlngs[i, "site_name"])
  site_lat <- as.numeric(aq_2018_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2018_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2018 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site_name_2018", values_to = 'site_distance_2018')

schl_ext_2018 <- temp_2018 %>%
  group_by(CDSCode) %>%
  summarize(site_distance_2018 = min(site_distance_2018)) %>%
  inner_join(temp_2018, by=c('CDSCode', 'site_distance_2018'))

site_distance_summary_2018 <- site_distance_summary_2018 %>%
  left_join(schl_ext_2018, by='CDSCode')

# Combine Fire Incident Data with Public School Codes
fire_size_and_latlngs_2014 <- fire_incidents_2014 %>%
  select(incident_name, incident_latitude, incident_longitude, incident_acres_burned)

fire_significance_summary_2014 <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

for (i in sequence(nrow(fire_size_and_latlngs_2014))) {
  fire_column_name <- as.character(fire_size_and_latlngs_2014[i, "incident_name"])
  fire_lat <- as.numeric(fire_size_and_latlngs_2014[i, "incident_latitude"])
  fire_long <- as.numeric(fire_size_and_latlngs_2014[i, "incident_longitude"])
  fire_size <- as.numeric(fire_size_and_latlngs_2014[i, "incident_acres_burned"])
  # Fire significance to a school is calculated by dividing the fire's size by fire's distance from the school
  fire_significance_summary_2014[[fire_column_name]] <- (fire_size/sqrt((fire_significance_summary_2014$Latitude - fire_lat)^2 + (fire_significance_summary_2014$Longitude - fire_long)^2))
}

fire_significance_summary_2014 <- fire_significance_summary_2014 %>%
  mutate(weighted_fires_sum_2014 = rowSums(select(.,4:ncol(fire_significance_summary_2014))))


fire_size_and_latlngs_2015 <- fire_incidents_2015 %>%
  select(incident_name, incident_latitude, incident_longitude, incident_acres_burned)

fire_significance_summary_2015 <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

for (i in sequence(nrow(fire_size_and_latlngs_2015))) {
  fire_column_name <- as.character(fire_size_and_latlngs_2015[i, "incident_name"])
  fire_lat <- as.numeric(fire_size_and_latlngs_2015[i, "incident_latitude"])
  fire_long <- as.numeric(fire_size_and_latlngs_2015[i, "incident_longitude"])
  fire_size <- as.numeric(fire_size_and_latlngs_2015[i, "incident_acres_burned"])
  # Fire significance to a school is calculated by dividing the fire's size by fire's distance from the school
  fire_significance_summary_2015[[fire_column_name]] <- (fire_size/sqrt((fire_significance_summary_2015$Latitude - fire_lat)^2 + (fire_significance_summary_2015$Longitude - fire_long)^2))
}

fire_significance_summary_2015 <- fire_significance_summary_2015 %>%
  mutate(weighted_fires_sum_2015 = rowSums(select(.,4:ncol(fire_significance_summary_2015))))

fire_size_and_latlngs_2016 <- fire_incidents_2016 %>%
  select(incident_name, incident_latitude, incident_longitude, incident_acres_burned)

fire_significance_summary_2016 <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

for (i in sequence(nrow(fire_size_and_latlngs_2016))) {
  fire_column_name <- as.character(fire_size_and_latlngs_2016[i, "incident_name"])
  fire_lat <- as.numeric(fire_size_and_latlngs_2016[i, "incident_latitude"])
  fire_long <- as.numeric(fire_size_and_latlngs_2016[i, "incident_longitude"])
  fire_size <- as.numeric(fire_size_and_latlngs_2016[i, "incident_acres_burned"])
  # Fire significance to a school is calculated by dividing the fire's size by fire's distance from the school
  fire_significance_summary_2016[[fire_column_name]] <- (fire_size/sqrt((fire_significance_summary_2016$Latitude - fire_lat)^2 + (fire_significance_summary_2016$Longitude - fire_long)^2))
}

fire_significance_summary_2016 <- fire_significance_summary_2016 %>%
  mutate(weighted_fires_sum_2016 = rowSums(select(.,4:ncol(fire_significance_summary_2016))))

fire_size_and_latlngs_2017 <- fire_incidents_2017 %>%
  select(incident_name, incident_latitude, incident_longitude, incident_acres_burned)

fire_significance_summary_2017 <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

for (i in sequence(nrow(fire_size_and_latlngs_2017))) {
  fire_column_name <- as.character(fire_size_and_latlngs_2017[i, "incident_name"])
  fire_lat <- as.numeric(fire_size_and_latlngs_2017[i, "incident_latitude"])
  fire_long <- as.numeric(fire_size_and_latlngs_2017[i, "incident_longitude"])
  fire_size <- as.numeric(fire_size_and_latlngs_2017[i, "incident_acres_burned"])
  # Fire significance to a school is calculated by dividing the fire's size by fire's distance from the school
  fire_significance_summary_2017[[fire_column_name]] <- (fire_size/sqrt((fire_significance_summary_2017$Latitude - fire_lat)^2 + (fire_significance_summary_2017$Longitude - fire_long)^2))
}

fire_significance_summary_2017 <- fire_significance_summary_2017 %>%
  mutate(weighted_fires_sum_2017 = rowSums(select(.,4:ncol(fire_significance_summary_2017))))

fire_size_and_latlngs_2018 <- fire_incidents_2018 %>%
  select(incident_name, incident_latitude, incident_longitude, incident_acres_burned)

fire_significance_summary_2018 <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

for (i in sequence(nrow(fire_size_and_latlngs_2018))) {
  fire_column_name <- as.character(fire_size_and_latlngs_2018[i, "incident_name"])
  fire_lat <- as.numeric(fire_size_and_latlngs_2018[i, "incident_latitude"])
  fire_long <- as.numeric(fire_size_and_latlngs_2018[i, "incident_longitude"])
  fire_size <- as.numeric(fire_size_and_latlngs_2018[i, "incident_acres_burned"])
  # Fire significance to a school is calculated by dividing the fire's size by fire's distance from the school
  fire_significance_summary_2018[[fire_column_name]] <- (fire_size/sqrt((fire_significance_summary_2018$Latitude - fire_lat)^2 + (fire_significance_summary_2018$Longitude - fire_long)^2))
}

fire_significance_summary_2018 <- fire_significance_summary_2018 %>%
  mutate(weighted_fires_sum_2018 = rowSums(select(.,4:ncol(fire_significance_summary_2018))))

# Klint's ideas: plot fire index on the x axis, on the y axis do year-to-year chance in scores 

# Unsupervised learning as a way to explore vs reduce
# Focus on exploration -- more interesting
# Things like PCA, Things like K-means

# Cluster data
# For schools in a particular cluster, (cluster on some variables, leave a couple out)
# Cluster schools based on demographic variables as example
# 

# Are there achievement trends and fire exposure trends that correlate?
# Is more fire exposure associated with lower academic achievement 

# SEDA dataset has "covariant" file at school-level.  Match with pub_school_data
# has racial composition, % free and reduced lunch, etc

#finding difference in scores between years
caaspp_15_16 <- left_join(caaspp_score_data_15, caaspp_score_data_16, by=c("School Code"="School Code", "Grade"="Grade", "Test Id"= "Test Id"))


caaspp_15_pw <- caaspp_score_data_15 %>% select(`County Code`, `District Code`, `School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
group_by(`County Code`, `District Code`, `School Code`, `Test Id`, `Grade`) %>%
summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_16_pw <- caaspp_score_data_16 %>% select(`County Code`, `District Code`, `School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`County Code`, `District Code`, `School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_17_pw <- caaspp_score_data_17 %>% select(`County Code`, `District Code`, `School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`County Code`, `District Code`, `School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_18_pw <- caaspp_score_data_18 %>% select(`County Code`, `District Code`, `School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`County Code`, `District Code`, `School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_19_pw <- caaspp_score_data_19 %>% select(`County Code`, `District Code`, `School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`County Code`, `District Code`, `School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_combine <- caaspp_15_pw %>%
  rename(caaspp_15=caaspp) %>%
  inner_join(caaspp_16_pw, by=c("County Code", "District Code", "School Code", "Grade", "Test Id")) %>%
  rename(caaspp_16=caaspp) %>%
  inner_join(caaspp_17_pw, by=c("County Code", "District Code", "School Code", "Grade", "Test Id")) %>%
  rename(caaspp_17=caaspp) %>%
  inner_join(caaspp_18_pw, by=c("County Code", "District Code", "School Code", "Grade", "Test Id")) %>%
  rename(caaspp_18=caaspp) %>%
  inner_join(caaspp_19_pw, by=c("County Code", "District Code", "School Code", "Grade", "Test Id")) %>%
  rename(caaspp_19=caaspp) 

caaspp_combine_pw <- caaspp_combine %>% pivot_wider(names_from = `Test Id`, 
                               values_from = c(caaspp_15, caaspp_16, caaspp_17, caaspp_18, caaspp_19)) %>%
  ungroup() %>%
  transmute(`County Code` = `County Code`, `District Code` = `District Code`, `School Code` = `School Code`, `Grade` = `Grade`, caaspp1_15_16 = caaspp_16_1-caaspp_15_1,
            caaspp2_15_16 = caaspp_16_2-caaspp_15_2, caaspp1_16_17 = caaspp_17_1-caaspp_16_1,
            caaspp2_16_17 = caaspp_17_2-caaspp_16_2, caaspp1_17_18 = caaspp_18_1-caaspp_17_1,
            caaspp2_17_18 = caaspp_18_2-caaspp_17_2, caaspp1_18_19 = caaspp_19_1-caaspp_18_1,
            caaspp2_18_19 = caaspp_19_2-caaspp_18_2)

caaspp_combine_pw <- caaspp_combine_pw %>% unite("CDSCode", 1:3, sep="", remove=FALSE)

final_school <- left_join(combine_filter, caaspp_combine_pw, by = c("CDSCode"))

# TODO: Redoing Fire Stuff -- Get rid of stuff that mentions 1969 or similar
# Group by year.  
# Sum significance by year.  

# Office hour notes:  What learning are we doing 
# Does fire exposure affect academic performance?
#   For each school, for each year, make new variable: current_score - year_before_score 
#     ^^^ Easiest way to do this: 

fire_data_2014 <- fire_significance_summary_2014 %>%
  select(CDSCode, weighted_fires_sum_2014)

fire_data_2015 <- fire_significance_summary_2015 %>%
  select(CDSCode, weighted_fires_sum_2015)

fire_data_2016 <- fire_significance_summary_2016 %>%
  select(CDSCode, weighted_fires_sum_2016)

fire_data_2017 <- fire_significance_summary_2017 %>%
  select(CDSCode, weighted_fires_sum_2017)

fire_data_2018 <- fire_significance_summary_2018 %>%
  select(CDSCode, weighted_fires_sum_2018)

final_school_and_fire_data <- inner_join(final_school, fire_data_2014, by=c("CDSCode"))
final_school_and_fire_data <- inner_join(final_school_and_fire_data, fire_data_2015, by=c("CDSCode"))
final_school_and_fire_data <- inner_join(final_school_and_fire_data, fire_data_2016, by=c("CDSCode"))
final_school_and_fire_data <- inner_join(final_school_and_fire_data, fire_data_2017, by=c("CDSCode"))
final_school_and_fire_data <- inner_join(final_school_and_fire_data, fire_data_2018, by=c("CDSCode"))


aq_site_2014 <- site_distance_summary_2014 %>%
  select(CDSCode, site_name_2014, site_distance_2014)

aq_site_2015 <- site_distance_summary_2015 %>%
  select(CDSCode, site_name_2015, site_distance_2015)

aq_site_2016 <- site_distance_summary_2016 %>%
  select(CDSCode, site_name_2016, site_distance_2016)

aq_site_2017 <- site_distance_summary_2017 %>%
  select(CDSCode, site_name_2017, site_distance_2017)

aq_site_2018 <- site_distance_summary_2018 %>%
  select(CDSCode, site_name_2018, site_distance_2018)

final_school_fire_and_site_data <- left_join(final_school_and_fire_data, aq_site_2014, by=c("CDSCode"))
final_school_fire_and_site_data <- left_join(final_school_fire_and_site_data, aq_site_2015, by=c("CDSCode"))
final_school_fire_and_site_data <- left_join(final_school_fire_and_site_data, aq_site_2016, by=c("CDSCode"))
final_school_fire_and_site_data <- left_join(final_school_fire_and_site_data, aq_site_2017, by=c("CDSCode"))
final_school_fire_and_site_data <- left_join(final_school_fire_and_site_data, aq_site_2018, by=c("CDSCode"))

aq_data_2014 <- aq_site_data_2014 %>%
  select(site_name, days_above_nat_std_2014, day_max_2014)

aq_data_2015 <- aq_site_data_2015 %>%
  select(site_name, days_above_nat_std_2015, day_max_2015)

aq_data_2016 <- aq_site_data_2016 %>%
  select(site_name, days_above_nat_std_2016, day_max_2016)

aq_data_2017 <- aq_site_data_2017 %>%
  select(site_name, days_above_nat_std_2017, day_max_2017)

aq_data_2018 <- aq_site_data_2018 %>%
  select(site_name, days_above_nat_std_2018, day_max_2018)

final_school_and_environment_data <- left_join(final_school_fire_and_site_data, aq_data_2014, by=c("site_name_2014" = "site_name"))
final_school_and_environment_data <- left_join(final_school_and_environment_data, aq_data_2015, by=c("site_name_2015" = "site_name"))
final_school_and_environment_data <- left_join(final_school_and_environment_data, aq_data_2016, by=c("site_name_2016" = "site_name"))
final_school_and_environment_data <- left_join(final_school_and_environment_data, aq_data_2017, by=c("site_name_2017" = "site_name"))
final_school_and_environment_data <- left_join(final_school_and_environment_data, aq_data_2018, by=c("site_name_2018" = "site_name"))

final_school_and_environment_data <- final_school_and_environment_data %>%
  mutate(days_above_nat_std_2014 = as.numeric(days_above_nat_std_2014)) %>%
  mutate(days_above_nat_std_2015 = as.numeric(days_above_nat_std_2015)) %>%
  mutate(days_above_nat_std_2016 = as.numeric(days_above_nat_std_2016)) %>%
  mutate(days_above_nat_std_2017 = as.numeric(days_above_nat_std_2017)) %>%
  mutate(days_above_nat_std_2018 = as.numeric(days_above_nat_std_2018)) %>%
  mutate(day_max_2014 = as.numeric(day_max_2014)) %>%
  mutate(day_max_2015 = as.numeric(day_max_2015)) %>%
  mutate(day_max_2016 = as.numeric(day_max_2016)) %>%
  mutate(day_max_2017 = as.numeric(day_max_2017)) %>%
  mutate(day_max_2018 = as.numeric(day_max_2018)) 


final_school_and_environment_data <- final_school_and_environment_data %>%
  filter(!is.na(caaspp1_15_16)) %>%
  filter(!is.na(caaspp1_16_17)) %>%
  filter(!is.na(caaspp1_17_18)) %>%
  filter(!is.na(caaspp1_18_19)) %>%
  filter(!is.na(caaspp2_15_16)) %>%
  filter(!is.na(caaspp2_16_17)) %>%
  filter(!is.na(caaspp2_17_18)) %>%
  filter(!is.na(caaspp2_18_19))
  
# schl_envrnment_z <- mutate_all(select(final_school_and_environment_data, 
#                                       -site_name_2018,
#                                       -site_name_2017,
#                                       -site_name_2016,
#                                       -site_name_2015,
#                                       -site_name_2014,
#                                       -`County Code`,
#                                       -`District Code`,
#                                       -`School Code`,
#                                       -sch_sped, 
#                                       -urbanicity, 
#                                       -level, 
#                                       -type, 
#                                       -stateabb,
#                                       -schcity,
#                                       -schnam,
#                                       -LastUpDate,
#                                       -AdmEmail3,
#                                       -AdmLName3,
#                                       -AdmFName3,
#                                       -AdmEmail2,
#                                       -AdmLName2,
#                                       -AdmFName2,
#                                       -AdmEmail1,
#                                       -AdmLName1,
#                                       -AdmFName1,
#                                       -Latitude,
#                                       -Longitude,
#                                       -FederalDFCDistrictID,
#                                       -YearRoundYN,
#                                       -Magnet,
#                                       -Virtual,
#                                       -GSserved,
#                                       -GSoffered,
#                                       -EILName,
#                                       -EILCode,
#                                       -EdOpsName,
#                                       -EdOpsCode,
#                                       -SOCType,
#                                       -SOC,
#                                       -DOCType,
#                                       -DOC,
#                                       -FundingType,
#                                       -CharterNum,
#                                       -Charter,
#                                       -ClosedDate,
#                                       -OpenDate,
#                                       -WebSite,
#                                       -Ext,
#                                       -Phone,
#                                       -MailState,
#                                       -MailZip,
#                                       -MailCity,
#                                       -MailStrAbr,
#                                       -MailStreet,
#                                       -State,
#                                       -Zip,
#                                       -City,
#                                       -StreetAbr,
#                                       -Street,
#                                       -School,
#                                       -District,
#                                       -County,
#                                       -StatusType,
#                                       -NCESSchool,
#                                       -NCESDist,
#                                       -CDSCode), scale) 

final_school_and_environment_data <-  final_school_and_environment_data %>% 
  mutate(day_site_15 = (day_max_2015 / site_distance_2015)) %>% 
  mutate(day_site_16 = (day_max_2016 / site_distance_2016)) %>% 
  mutate(day_site_17 = (day_max_2017 / site_distance_2017)) %>% 
  mutate(day_site_18 = (day_max_2018 / site_distance_2018)) 


schl_envrnment <- final_school_and_environment_data %>%
  select(
         # perwht,
          perind,
          perasn,
          perhsp,
          perblk,
          perfl,
          perrl,
         perfrl,
         perecd,
         gifted_tot,
         disab_tot,
         lep,
          caaspp1_15_16,
          caaspp1_16_17,
          caaspp1_17_18,
          caaspp1_18_19,
          caaspp2_15_16,
          caaspp2_16_17,
          caaspp2_17_18,
          caaspp2_18_19,
         weighted_fires_sum_2014,
         weighted_fires_sum_2015,
         weighted_fires_sum_2016,
         weighted_fires_sum_2017,
         weighted_fires_sum_2018,
         days_above_nat_std_2014,
         days_above_nat_std_2015,
         days_above_nat_std_2016,
         days_above_nat_std_2017,
         days_above_nat_std_2018, 
         CDSCode,
         day_site_15, 
         day_site_16, 
         day_site_17, 
         day_site_18, 
         days_above_nat_std_2014, 
         days_above_nat_std_2015, 
         days_above_nat_std_2016, 
         days_above_nat_std_2017, 
         days_above_nat_std_2018)



schl_envrnment_no_na <- schl_envrnment %>%
  drop_na()

# schl_envrnment_z <- mutate_all(schl_envrnment_no_na, scale)



set.seed(1234)
# schl_envrnment_clusters <- kmeans(schl_envrnment_z, 5, nstart = 25)
# 
# plotcluster(schl_envrnment_z, schl_envrnment_clusters$cluster)

schl_envrnment_noCDSCode <- select(schl_envrnment_no_na, -CDSCode)
names(schl_envrnment_noCDSCode)
schl_envrnment_pca <- PCA(schl_envrnment_noCDSCode)

get_eigenvalue(schl_envrnment_pca)

# schl_envrnment_pca_0 <- PCA(schl_envrnment_no_na)
# get_eigenvalue(schl_envrnment_pca_0)
# fviz_eig(schl_envrnment_pca_0, addlabels = TRUE, ylim = c(0, 40))

roc_schl_15_16 <- ggplot(prob, aes(d = Observed, m = Probability, color = Model)) + 
  geom_roc(labels = FALSE) + #takes Observed, Probability, figures out y/x axes
  labs(title = "ROC curves",
       y = "Prop. true positives",
       x = "Prop. false positives") +
  theme_bw() +
  facet_grid(Sample ~ .)
roc_schl_15_16


var <- get_pca_var(schl_envrnment_pca)
var
var$coord

fviz_pca_var(schl_envrnment_pca, col.var = "cos2",
             gradient.cols = c("forestgreen", "orange", "red"))

# There should be 6 of these, but I cannot, for the life of me, figure out how to get 6
d1 <- fviz_contrib(schl_envrnment_pca, choice = "var", axes = 1, title = "D1")
d2 <- fviz_contrib(schl_envrnment_pca, choice = "var", axes = 2, title = "D2")
d3 <- fviz_contrib(schl_envrnment_pca, choice = "var", axes = 3, title = "D3")
d4 <- fviz_contrib(schl_envrnment_pca, choice = "var", axes = 4, title = "D4")

grid.arrange(d1, d2, d3, d4, nrow = 2,  
             top = ("Contributions by dimension"))


######################## regression 15-16 ########################

set.seed(1234)  
train <- sample_frac(schl_envrnment_no_na, 0.8)
test <- filter(schl_envrnment_no_na, !schl_envrnment_no_na$CDSCode %in% 
                 train$CDSCode)

mod_glm_15_16 <- glm(caaspp1_15_16 ~ 
               perind +
               perasn +
               perhsp +
               perblk +
               perfrl +
               perecd +
               gifted_tot +
               disab_tot +
               lep + 
               weighted_fires_sum_2015 +
               days_above_nat_std_2015 +
               day_site_15 +
               days_above_nat_std_2015, 
               data = train)


results_15_16 <- summary(mod_glm_15_16)
coefficients_15_16 <- as_tibble(results_15_16$coefficients, rownames = "Variable")
arrange(coefficients_15_16, desc(Estimate)) 

dwplot(mod_glm_15_16)

mod_glm_coefs_15_16 <- coef(mod_glm_15_16) %>%
  tidy() %>%
  filter(names != "(Intercept)") %>%
  top_n(25, wt = x)

?tidy

ggplot(mod_glm_coefs_15_16, aes(x, reorder(names, x))) +
  geom_point() +
  labs(title = "Top 25 largest coefficients logistic regression",
       x = "Coefficient",
       y = "") +
  theme_bw()

######################## regularization_15_16 #########################
train_x_15_16 <- model.matrix(caaspp1_15_16 ~
                            perind +
                              perasn +
                              perhsp +
                              perblk +
                              perfrl +
                              perecd +
                              gifted_tot +
                              disab_tot +
                              lep + 
                              weighted_fires_sum_2015 +
                              days_above_nat_std_2015 +
                              day_site_15 +
                              days_above_nat_std_2015, 
                            train)
train_y_15_16 <- train %>%
  select(caaspp1_15_16) %>%
  mutate(caaspp1_15_16 = as.numeric(caaspp1_15_16)) %>%
  as.matrix()

#~~~~~RIDGE regression:
mod_ridge_15_16 <- glmnet(x = train_x_15_16,
                    y = train_y_15_16,
                    alpha = 0)
plot(mod_ridge_15_16)
#L1 Norm is the weight given to the shrinkage

# To decide what lamba to choose, we use cross-validation: put cv. in front of
# glmnet. The default is 10 folds. 
mod_ridge_15_16 <- cv.glmnet(x = train_x_15_16,
                       y = train_y_15_16,
                       alpha = 0)
plot(mod_ridge_15_16)

mod_ridge_coefs_15_16 <- coef(mod_ridge_15_16, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) 
mod_ridge_coefs_15_16

ggplot(mod_ridge_coefs_15_16, aes(value, reorder(row, value))) +
  geom_point() +
  labs(title = "Top 25 largest coefficients ridge regression",
       x = "Coefficient",
       y = "") +
  theme_bw()

#compare with the regular regression:
mod_glm_15_16_coefs <- coef(mod_glm_15_16) %>%
  tidy() %>%
  filter(names != "(Intercept)") %>%
  top_n(25, wt = x)

ggplot(mod_glm_15_16_coefs, aes(x, reorder(names, x))) +
  geom_point() +
  labs(title = "Top 25 largest coefficients logistic regression",
       x = "Coefficient",
       y = "") +
  theme_bw()


#~~~~~LASSO regression:
mod_lasso_15_16 <- glmnet(x = train_x_15_16,
                    y = train_y_15_16,
                    alpha = 1) #lasso is set alpha to 1, not 0
#lasso will shrink coefficients to 0
#if low lambda, many coefficients will be kicked out

plot(mod_lasso_15_16)

# Again a plot for the coefficients at the preferred lamba
mod_lasso_15_16 <- cv.glmnet(x = train_x_15_16,
                       y = train_y_15_16,
                       alpha = 1)

coef_mod_lasso_15_16 <- coef(mod_lasso_15_16, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value))

ggplot(coef_mod_lasso_15_16, aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 largest coefficients lasso regression") +
  xlab("Coefficient") +
  ylab("") +
  theme_bw()


#~~~~~ELASTIC NET. We can start with half ridge half lasso by
# stating alpha is 0.5 (0.1 would be more ridge than lasso, 0.9 would be more 
# lasso than ridge..etc)
mod_enet_15_16 <- cv.glmnet(x = train_x_15_16,
                            y = train_y_15_16,
                      alpha = 0.5)


# Cross validation can be used to determine how much ridge and how much lasso
# is best (alpha) and how much shrinkage would be best (lamba).For this we 
# will use the train() function from the caret package.
mod_enet_15_16 <- train(caaspp1_15_16 ~ 
                        perind +
                          perasn +
                          perhsp +
                          perblk +
                          perfrl +
                          perecd +
                          gifted_tot +
                          disab_tot +
                          lep + 
                          weighted_fires_sum_2015 +
                          days_above_nat_std_2015 +
                          day_site_15 +
                          days_above_nat_std_2015, 
                  data = schl_envrnment_no_na,
                  method = "glmnet",
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength = 10)

# The print() function will show you what value of alpha and lamba was settled
# on. It also gives you an accuracy value. The Kappa value is also an index of 
# accuracy, except that it is normalized at the baseline of random chance on 
# your dataset. It is best to use Kappa with imbalance in the classes
# (e.g. 70-30 split for classes 0 and 1 and you can achieve 70% accuracy by 
# predicting all instances are for class 0).
print(mod_enet_15_16)

# Running base R's plot() function on the object will return this visualization.
# It shows you the accuracy rates with variable mixings of ridge and lasso.
plot(mod_enet_15_16)


# Let's look at the final results! Getting the coefficients is a bit tricky
# as we need to dig in the final model and get the optimal lambda
coef_mod_enet_15_16 <- coef(mod_enet_15_16$finalModel, mod_enet_15_16$bestTune$lambda) %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value))

ggplot(coef_mod_enet_15_16, aes(value, reorder(row, value))) +
  geom_point() +
  labs(title = "Top 25 largest coefficients elastic net regression",
       x = "Coefficient",
       y = "") +
  theme_bw()


######################## regression 16-17 ########################

mod_glm_16_17 <- glm(caaspp1_16_17 ~ 
                       perind +
                       perasn +
                       perhsp +
                       perblk +
                       perfrl +
                       perecd +
                       gifted_tot +
                       disab_tot +
                       lep + 
                       weighted_fires_sum_2016 +
                       days_above_nat_std_2016 +
                       day_site_16 +
                       days_above_nat_std_2016, 
                     data = train)

results_16_17 <- summary(mod_glm_16_17)
coefficients_16_17 <- as_tibble(results_16_17$coefficients, rownames = "Variable")
arrange(coefficients_16_17, desc(Estimate)) 

dwplot(mod_glm_16_17)

######################## regression 17-18 ########################

mod_glm_17_18 <- glm(caaspp1_17_18 ~ 
                       perind +
                       perasn +
                       perhsp +
                       perblk +
                       perfrl +
                       perecd +
                       gifted_tot +
                       disab_tot +
                       lep + 
                       weighted_fires_sum_2017 +
                       days_above_nat_std_2017 +
                       day_site_17 +
                       days_above_nat_std_2017, 
                     data = train)

results_17_18 <- summary(mod_glm_17_18)
coefficients_17_18 <- as_tibble(results_17_18$coefficients, rownames = "Variable")
arrange(coefficients_17_18, desc(Estimate))

dwplot(mod_glm_17_18)

# #check for colinearity
# # continuous_17_18 <- train %>%
# #  select(perind,
# #           perasn,
#  #          perhsp,
#            perblk,
#            perfrl,
#            perecd,
#            gifted_tot,
#            disab_tot,
#            lep, 
#            weighted_fires_sum_2017,
#            days_above_nat_std_2017,
#            day_site_17,
#            days_above_nat_std_2017) 
# predictors_17_18 <- colnames(continuous_17_18)
# 
# probabilities_17_18 <- predict(mod_glm_17_18, type = "response")
# 
# continuous_17_18 <- continuous_17_18 %>%
#   mutate(logit = log(probabilities_17_18/(1-probabilities_17_18))) %>%
#   pivot_longer(cols = -logit,
#                names_to = "predictors", 
#                values_to = "predictor_value")
# 
# ggplot(continuous_17_18, aes(logit, predictor_value))+
#   geom_point(size = 0.5, alpha = 0.5) +
#   geom_smooth(method = "loess") + 
#   theme_bw() + 
#   facet_wrap(~predictors, scales = "free_y")
# 

######################## regression 18-19 ########################
mod_glm_18_19 <- glm(caaspp1_18_19 ~ 
                       perind +
                       perasn +
                       perhsp +
                       perblk +
                       perfrl +
                       perecd +
                       gifted_tot +
                       disab_tot +
                       lep + 
                       weighted_fires_sum_2018 +
                       days_above_nat_std_2018 +
                       day_site_18 +
                       days_above_nat_std_2018, 
                     data = train)

results_18_19 <- summary(mod_glm_18_19)
coefficients_18_19 <- as_tibble(results_18_19$coefficients, rownames = "Variable")
arrange(coefficients_18_19, desc(Estimate))

dwplot(mod_glm_18_19)


#CHECKING for colinearity




