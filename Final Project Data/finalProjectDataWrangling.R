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
caaspp_score_data_15_16 <- left_join(caaspp_score_data_15, caaspp_score_data_16, by = c("School Code", "Grade"))
caaspp_score_data_15_16_17 <- left_join(caaspp_score_data_15_16, caaspp_score_data_17, by = c("School Code", "Grade"))
caaspp_score_data_15_16_17_18 <- left_join(caaspp_score_data_15_16_17, caaspp_score_data_18, by = c("School Code", "Grade"))
caaspp_score_data_all <- left_join(caaspp_score_data_15_16_17_18, caaspp_score_data_19, by = c("School Code", "Grade"))

