library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gridExtra)
library(cluster)   
library(dendextend)
library(fpc)
library(RCA)


# Set your working directory
setwd("/Users/fionahall-zazueta/Documents/GitHub/educ143-final-project/Final Project Data")

pub_school_data <- read_tsv("pubschls.txt")
caaspp_score_data_19 <- read_delim("sb_ca2019_1_csv_v3.txt", delim=",")
caaspp_score_data_18 <- read_delim("sb_ca2018_1_csv_v3.txt", delim=",")
caaspp_score_data_17 <- read_delim("sb_ca2017_1_csv_v2.txt", delim=",")
caaspp_score_data_16 <- read_delim("sb_ca2016_1_csv_v3.txt", delim=",")
caaspp_score_data_15 <- read_delim("sb_ca2015_1_csv_v3.txt", delim=",")

fire_incident_data <- read_csv("mapdataall.csv")

aq_2014_collection_sites <- read_delim("aq_2014.txt", delim=",")
aq_2014_collection_sites <- aq_2014_collection_sites %>%
  filter(!is.na(name))

aq_2015_collection_sites <- read_delim("aq_2015.txt", delim=",")
aq_2015_collection_sites <- aq_2015_collection_sites %>%
  filter(!is.na(name))

aq_2016_collection_sites <- read_delim("aq_2016.txt", delim=",")
aq_2016_collection_sites <- aq_2016_collection_sites %>%
  filter(!is.na(name))

aq_2017_collection_sites <- read_delim("aq_2017.txt", delim=",")
aq_2017_collection_sites <- aq_2017_collection_sites %>%
  filter(!is.na(name))

aq_2018_collection_sites <- read_delim("aq_2018.txt", delim=",")
aq_2018_collection_sites <- aq_2018_collection_sites %>%
  filter(!is.na(name))

aq_2019_collection_sites <- read_delim("aq_2019.txt", delim=",")
aq_2019_collection_sites <- aq_2019_collection_sites %>%
  filter(!is.na(name))

aq_pm25_data_2014 <- read_csv("PM25_PICKDATA_2014-12-31.csv")
aq_pm25_data_2015 <- read_csv("PM25_PICKDATA_2015-12-31.csv")
aq_pm25_data_2016 <- read_csv("PM25_PICKDATA_2016-12-31.csv")
aq_pm25_data_2017 <- read_csv("PM25_PICKDATA_2017-12-31.csv")
aq_pm25_data_2018 <- read_csv("PM25_PICKDATA_2018-12-31.csv")

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

# Loop through pub_school_data and perform the following actions for each row:
#     Loop through fire_incident_data
#         distance = sqrt((school_lat - fire_incident_lat)^2 + (school_lon - fire_incident_lon)^2)
#         significant = sig_calc(fire_size, distance) //TODO: Define sig_calc, should return binary
#         if (significant) 
#              ??? IDEAS: ???
#                  add a significant_fires_XXXX variables to pub_school_data, and raise its count for the current school
#                  add variables for signficatnt_fires_month_XXXX, and raise its count according to the incident's date
#                  !!! add a variable for each fire to each observation, and weight the variable by distance from the current observation - Klint's suggestion

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









