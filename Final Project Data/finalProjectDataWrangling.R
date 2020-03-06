library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gridExtra)
library(cluster)   
library(dendextend)
library(fpc)
library(RCA)
library(rvest)


# Set your working directory
  setwd("/Users/michaelaelias/Documents/GitHub/educ143-final-project/Final Project Data/educ143-final-project/Final Project Data/")

pub_school_data <- read_tsv("pubschls.txt")
pub_school_data <- pub_school_data %>%
  filter(Latitude != "No Data", Longitude != "No Data")
caaspp_score_data_19 <- read_delim("sb_ca2019_1_csv_v3.txt", delim=",")
caaspp_score_data_18 <- read_delim("sb_ca2018_1_csv_v3.txt", delim=",")
caaspp_score_data_17 <- read_delim("sb_ca2017_1_csv_v2.txt", delim=",")
caaspp_score_data_16 <- read_delim("sb_ca2016_1_csv_v3.txt", delim=",")
caaspp_score_data_15 <- read_delim("sb_ca2015_1_csv_v3.txt", delim=",")

fire_incident_data <- read_csv("mapdataall.csv")

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

# toy %>% mutate(x6 = case_when(is.na)) // Make empty strings into NAs

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

# aq_pm25_data_2014 <- read_csv("PM25_PICKDATA_2014-12-31.csv")
# aq_pm25_data_2015 <- read_csv("PM25_PICKDATA_2015-12-31.csv")
# aq_pm25_data_2016 <- read_csv("PM25_PICKDATA_2016-12-31.csv")
# aq_pm25_data_2017 <- read_csv("PM25_PICKDATA_2017-12-31.csv")
# aq_pm25_data_2018 <- read_csv("PM25_PICKDATA_2018-12-31.csv")

seda_data <- read_csv("SEDA_cov_school_pool_v30.csv")
seda_data_CA <- seda_data %>% filter(stateabb=="CA")
combine <- left_join(pub_school_data, seda_data_CA, by=c("School" = "schnam"))


seda_data_CA$ncessch <- substr(seda_data_CA$ncessch, 8, 12)
combine <- left_join(pub_school_data, seda_data_CA, by=c("NCESSchool" = "ncessch"))
combine_filter <- combine %>% filter(NCESSchool != "No Data" )

# TODO: Before doing this, relable column names by year!
# caaspp_score_data_15_16 <- left_join(caaspp_score_data_15, caaspp_score_data_16, by = c("School Code", "Grade"))
# caaspp_score_data_15_16_17 <- left_join(caaspp_score_data_15_16, caaspp_score_data_17, by = c("School Code", "Grade"))
# caaspp_score_data_15_16_17_18 <- left_join(caaspp_score_data_15_16_17, caaspp_score_data_18, by = c("School Code", "Grade"))
# caaspp_score_data_all <- left_join(caaspp_score_data_15_16_17_18, caaspp_score_data_19, by = c("School Code", "Grade"))


# PLAN FOR COMBINING DATA:

# Loop through pub_school_data and perform the following actions for each row:
#      current_distance = MAX_INT
#      smallest_distance = MAX_INT
#      closest_collection_site_XXXX = ""
#      Loop through aq_XXXX_collection_sites and perform the following calculation for each row:
#           current_distance = sqrt((school_lat - collection_site_lat)^2 + (school_lon - collection_site_lon)^2)
#           if (current_distance < smallest_distance) 
#               smallest_distance = current_distance 
#               closest_collection_site = collection_site_name
#      add closest_collection_site_XXXX variable to current row in pub_school_data 
#      repeat for all years from 2014 to 2019
# 
#   For (i in sequence(nrow(collection_sites_XXX)))
#      pub_school_data$[collection_site_id_XXXX_string] <-
#            sqrt((pub_school_data[, lat] - collection_sites_XXXX[i, ])^2 + (pub_school_data[, lon] - collection_sites_XXXX[i,lon])^2)
#
#   

aq_2014_collection_latlngs <- aq_pm25_sites_2014 %>%
  select(site, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2014 <- school_latlngs

for (i in sequence(nrow(aq_2014_collection_latlngs))) {
  site_column_name <- as.character(aq_2014_collection_latlngs[i, "site"])
  site_lat <- as.numeric(aq_2014_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2014_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2014 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site", values_to = 'distance') %>%
  mutate(site = as.numeric(site))

schl_ext_2014 <- temp_2014 %>%
  group_by(CDSCode) %>%
  summarize(distance = min(distance)) %>%
  inner_join(temp_2014, by=c('CDSCode', 'distance'))

site_distance_summary_2014 <- site_distance_summary_2014 %>%
  left_join(schl_ext_2014, by='CDSCode')


aq_2015_collection_latlngs <- aq_pm25_sites_2015 %>%
  select(site, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2015 <- school_latlngs

for (i in sequence(nrow(aq_2015_collection_latlngs))) {
  site_column_name <- as.character(aq_2015_collection_latlngs[i, "site"])
  site_lat <- as.numeric(aq_2015_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2015_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2015 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site", values_to = 'distance') %>%
  mutate(site = as.numeric(site))

schl_ext_2015 <- temp_2015 %>%
  group_by(CDSCode) %>%
  summarize(distance = min(distance)) %>%
  inner_join(temp_2015, by=c('CDSCode', 'distance'))

site_distance_summary_2015 <- site_distance_summary_2015 %>%
  left_join(schl_ext_2015, by='CDSCode')


aq_2016_collection_latlngs <- aq_pm25_sites_2016 %>%
  select(site, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2016 <- school_latlngs

for (i in sequence(nrow(aq_2016_collection_latlngs))) {
  site_column_name <- as.character(aq_2016_collection_latlngs[i, "site"])
  site_lat <- as.numeric(aq_2016_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2016_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2016 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site", values_to = 'distance') %>%
  mutate(site = as.numeric(site))

schl_ext_2016 <- temp_2016 %>%
  group_by(CDSCode) %>%
  summarize(distance = min(distance)) %>%
  inner_join(temp_2016, by=c('CDSCode', 'distance'))

site_distance_summary_2016 <- site_distance_summary_2016 %>%
  left_join(schl_ext_2016, by='CDSCode')

aq_2017_collection_latlngs <- aq_pm25_sites_2017 %>%
  select(site, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2017 <- school_latlngs

for (i in sequence(nrow(aq_2017_collection_latlngs))) {
  site_column_name <- as.character(aq_2017_collection_latlngs[i, "site"])
  site_lat <- as.numeric(aq_2017_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2017_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2017 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site", values_to = 'distance') %>%
  mutate(site = as.numeric(site))

schl_ext_2017 <- temp_2017 %>%
  group_by(CDSCode) %>%
  summarize(distance = min(distance)) %>%
  inner_join(temp_2017, by=c('CDSCode', 'distance'))

site_distance_summary_2017 <- site_distance_summary_2017 %>%
  left_join(schl_ext_2017, by='CDSCode')


aq_2018_collection_latlngs <- aq_pm25_sites_2018 %>%
  select(site, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2018 <- school_latlngs

for (i in sequence(nrow(aq_2018_collection_latlngs))) {
  site_column_name <- as.character(aq_2018_collection_latlngs[i, "site"])
  site_lat <- as.numeric(aq_2018_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2018_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2018 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site", values_to = 'distance') %>%
  mutate(site = as.numeric(site))

schl_ext_2018 <- temp_2018 %>%
  group_by(CDSCode) %>%
  summarize(distance = min(distance)) %>%
  inner_join(temp_2018, by=c('CDSCode', 'distance'))

site_distance_summary_2018 <- site_distance_summary_2018 %>%
  left_join(schl_ext_2018, by='CDSCode')


aq_2019_collection_latlngs <- aq_pm25_sites_2019 %>%
  select(site, latitude, longitude)

school_latlngs <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

site_distance_summary_2019 <- school_latlngs

for (i in sequence(nrow(aq_2019_collection_latlngs))) {
  site_column_name <- as.character(aq_2019_collection_latlngs[i, "site"])
  site_lat <- as.numeric(aq_2019_collection_latlngs[i, "latitude"])
  site_long <- as.numeric(aq_2019_collection_latlngs[i, "longitude"])
  school_latlngs[[site_column_name]] <- sqrt((school_latlngs$Latitude - site_lat)^2 + (school_latlngs$Longitude - site_long)^2)
}

temp_2019 <- school_latlngs %>%
  select(-Latitude, -Longitude) %>%
  pivot_longer(-CDSCode, names_to="site", values_to = 'distance') %>%
  mutate(site = as.numeric(site))

schl_ext_2019 <- temp_2019 %>%
  group_by(CDSCode) %>%
  summarize(distance = min(distance)) %>%
  inner_join(temp_2019, by=c('CDSCode', 'distance'))

site_distance_summary_2019 <- site_distance_summary_2019 %>%
  left_join(schl_ext_2019, by='CDSCode')

#finding difference in scores between years
caaspp_15_16 <- left_join(caaspp_score_data_15, caaspp_score_data_16, by=c("School Code"="School Code", "Grade"="Grade", "Test Id"= "Test Id"))


caaspp_15_pw <- caaspp_score_data_15 %>% select(`School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
group_by(`School Code`, `Test Id`, `Grade`) %>%
summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_16_pw <- caaspp_score_data_16 %>% select(`School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_17_pw <- caaspp_score_data_17 %>% select(`School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_18_pw <- caaspp_score_data_18 %>% select(`School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_19_pw <- caaspp_score_data_19 %>% select(`School Code`,`Mean Scale Score`, `Test Id`, `Grade`) %>%
  filter(`Mean Scale Score` != "*") %>%
  group_by(`School Code`, `Test Id`, `Grade`) %>%
  summarize(caaspp=mean(as.numeric(`Mean Scale Score`), na.rm = TRUE))

caaspp_combine <- caaspp_15_pw %>%
  rename(caaspp_15=caaspp) %>%
  inner_join(caaspp_16_pw, by=c("School Code", "Grade", "Test Id")) %>%
  rename(caaspp_16=caaspp) %>%
  inner_join(caaspp_17_pw, by=c("School Code", "Grade", "Test Id")) %>%
  rename(caaspp_17=caaspp) %>%
  inner_join(caaspp_18_pw, by=c("School Code", "Grade", "Test Id")) %>%
  rename(caaspp_18=caaspp) %>%
  inner_join(caaspp_19_pw, by=c("School Code", "Grade", "Test Id")) %>%
  rename(caaspp_19=caaspp) 

caaspp_combine_pw <- caaspp_combine %>% pivot_wider(names_from = `Test Id`, 
                               values_from = c(caaspp_15, caaspp_16, caaspp_17, caaspp_18, caaspp_19)) %>%
  ungroup() %>%
  transmute(`School Code` = `School Code`, `Grade` = `Grade`, caaspp1_15_16 = caaspp_16_1-caaspp_15_1,
            caaspp2_15_16 = caaspp_16_2-caaspp_15_2, caaspp1_16_17 = caaspp_17_1-caaspp_16_1,
            caaspp2_16_17 = caaspp_17_2-caaspp_16_2, caaspp1_17_18 = caaspp_18_1-caaspp_17_1,
            caaspp2_17_18 = caaspp_18_2-caaspp_17_2, caaspp1_18_19 = caaspp_19_1-caaspp_18_1,
            caaspp2_18_19 = caaspp_19_2-caaspp_18_2)

combine_filter$CDSCode <- substr(combine_filter$CDSCode, 8, 14)

final_school <- left_join(combine_filter, caaspp_combine_pw, by = c("CDSCode" = "School Code"))






# Loop through pub_school_data and perform the following actions for each row:
#     Loop through fire_incident_data
#         distance = sqrt((school_lat - fire_incident_lat)^2 + (school_lon - fire_incident_lon)^2)
#         significant = sig_calc(fire_size, distance) //TODO: Define sig_calc, should return binary
#         if (significant) 
#              ??? IDEAS: ???
#                  add a significant_fires_XXXX variables to pub_school_data, and raise its count for the current school
#                  add variables for signficatnt_fires_month_XXXX, and raise its count according to the incident's date
#                  !!! add a variable for each fire to each observation, and weight the variable by distance from the current observation - Klint's suggestion


fire_size_and_latlngs <- fire_incident_data %>%
  select(incident_name, incident_latitude, incident_longitude, incident_acres_burned)

fire_significance_summary <- pub_school_data %>%
  select(CDSCode, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) 

for (i in sequence(nrow(fire_incident_data))) {
  fire_column_name <- as.character(fire_size_and_latlngs[i, "incident_name"])
  fire_lat <- as.numeric(fire_size_and_latlngs[i, "incident_latitude"])
  fire_long <- as.numeric(fire_size_and_latlngs[i, "incident_longitude"])
  fire_size <- as.numeric(fire_size_and_latlngs[i, "incident_acres_burned"])
  # Fire significance to a school is calculated by dividing the fire's size by fire's distance from the school
  fire_significance_summary[[fire_column_name]] <- (fire_size/sqrt((fire_significance_summary$Latitude - fire_lat)^2 + (fire_significance_summary$Longitude - fire_long)^2))
}

fire_significance_summary <- fire_significance_summary %>%
  mutate(weighted_fires_sum = rowSums(select(.,4:ncol(fire_significance_summary))))

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









