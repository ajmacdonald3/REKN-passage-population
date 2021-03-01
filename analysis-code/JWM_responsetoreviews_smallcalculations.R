################################################################################
# JWM response to reviews little edits
#
################################################################################

library(tidyverse)
library(lubridate)
library(bb2enchist)
#library(rjags)
library(car)
library(reshape)
#library(jagsUI)
library(R2ucare)
library(RMark)

# check number of excluded resights
# 2018
# removing no resights
bandedbirds <- readRDS("data-files/ALLCAMPS2018_bandedbirds_resights.rds")
resight_periods <- readRDS("data-files/ALLCAMPS2018_resightperiods.rds")

resight_data <- format_bandedbirds(bandedbirds, sp = "REKN", cert = FALSE)

resight_data_custom <- custom_period(resight_data, resight_periods)

create_enchist(resight_data_custom)

# removing uncertain resights
resight_data <- bandedbirds %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100)

resight_data <- format_bandedbirds(resight_data, sp = "REKN", cert = FALSE)

resight_data_custom <- custom_period(resight_data, resight_periods)

create_enchist(resight_data_custom)

# 2017
# removing no resights
bandedbirds <- readRDS("./data-files/ALLCAMPS2017_bandedbirds_resights.rds")
resight_periods <- readRDS("./data-files/ALLCAMPS2017_resightperiods.rds")

bandedbirds <- bandedbirds %>%
  fill(LocationID) %>% 
  filter(!str_detect(LocationID, "NORTHBLUFFPT"))

resight_data <- format_bandedbirds(bandedbirds, sp = "REKN", cert = FALSE)

resight_data_custom <- custom_period(resight_data, resight_periods)

create_enchist(resight_data_custom)

# removing uncertain resights
resight_data <- bandedbirds %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100)

resight_data <- format_bandedbirds(resight_data, sp = "REKN", cert = FALSE)

resight_data_custom <- custom_period(resight_data, resight_periods)

create_enchist(resight_data_custom)

# check none seen first in last two periods
# 2018
bandedbirds <- readRDS("data-files/ALLCAMPS2018_bandedbirds_resights.rds")
resight_periods <- readRDS("data-files/ALLCAMPS2018_resightperiods.rds")

resight_data <- bandedbirds %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100)

resight_data <- format_bandedbirds(resight_data, sp = "REKN", cert = FALSE)

resight_data_custom <- custom_period(resight_data, resight_periods)

first_resight <- resight_data_custom %>% 
  group_by(FlagID) %>% 
  summarize(FirstSight = min(ResightPeriod)) %>% 
  arrange(FirstSight)

# 2017
bandedbirds <- readRDS("./data-files/ALLCAMPS2017_bandedbirds_resights.rds")
resight_periods <- readRDS("./data-files/ALLCAMPS2017_resightperiods.rds")

resight_data <- bandedbirds %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100)

resight_data <- format_bandedbirds(resight_data, sp = "REKN", cert = FALSE)

resight_data_custom <- custom_period(resight_data, resight_periods)

first_resight <- resight_data_custom %>% 
  group_by(FlagID) %>% 
  summarize(FirstSight = min(ResightPeriod)) %>% 
  arrange(FirstSight)

# get weather stats
ymo_weather <- read_csv("data-files/moosonee_weather_data.csv")

# filter and summarize means/totals

ymo_weather <- ymo_weather %>% 
  select(Year, Month, `Mean Temp (C)`, `Total Precip (mm)`) %>% 
  filter(Year %in% c(2017, 2018)) %>% 
  filter(Month %in% c(7, 8, 9))

ymo_temp <- ymo_weather %>% 
  group_by(Year) %>% 
  summarize(spring_temp = mean(`Mean Temp (C)`, na.rm = TRUE))

ymo_precip <- ymo_weather %>% 
  group_by(Year) %>% 
  summarize(summer_precip = sum(`Total Precip (mm)`, na.rm = TRUE))

# GOF checks
# read in and format data
rekn <- read_inp("./data-files/PISKLR2017_enchist_inp.inp")

rekn_hist <- rekn$encounter_histories
rekn_freq <- rekn$sample_size

# run GOF tests

test3sr(rekn_hist, rekn_freq) # transience - significant
test3sm(rekn_hist, rekn_freq) # resighting rates - not significant
test2ct(rekn_hist, rekn_freq) # equal resightability - significant
test2cl(rekn_hist, rekn_freq) # equal resightability (before and after) - not significant
overall_CJS(rekn_hist, rekn_freq) # significant

# chat = chi-square/df = 10.331/21 = 0.492

# read in and format data
rekn <- read_inp("./data-files/ALLCAMPS2018_enchist_inp.inp")

rekn_hist <- rekn$encounter_histories
rekn_freq <- rekn$sample_size

# run GOF tests

test3sr(rekn_hist, rekn_freq) # transience - significant
test3sm(rekn_hist, rekn_freq) # resighting rates - not significant
test2ct(rekn_hist, rekn_freq) # equal resightability - significant
test2cl(rekn_hist, rekn_freq) # equal resightability (before and after) - not significant
overall_CJS(rekn_hist, rekn_freq) # significant

# chat = chi-square/df = 103.539/44 = 2.353
