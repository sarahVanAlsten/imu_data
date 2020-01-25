########################################################
#Author: Sarah Van Alsten
#Date created: January 24, 2020
#Read in countyhealth files to calculate county level poverty
#Then recode to weighted value for IMU calculation
#Packages used: tidyverse
##################################################
library(tidyverse)
library(httr)
library(janitor)

#county health rankings are 2 yrs behind actual.
#change incoming dataset name to reflect that
#census_api_key("64a0850641b8da160f1ae3d7dcd6408ee4b4ddc2")

#use readr to download countyhealth files ins
#read in data dictionary for 2019 to change column names
httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2019.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth17 <- read_csv(tf, skip = 1)
#clean names
countyhealth07 <- janitor::clean_names(countyhealth07)

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2018_0.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth16 <- read_csv(tf, skip = 1) %>% clean_names()

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2017.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth15 <- read_csv(tf, skip = 1) %>% clean_names()

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2016.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth14 <- read_csv(tf, skip = 1) %>% clean_names()

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2015.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth13 <- read_csv(tf, skip = 1) %>% clean_names()

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2014.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth12 <- read_csv(tf, skip = 1) %>% clean_names()

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2013.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth11 <- read_csv(tf, skip = 1) %>% clean_names()

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2012.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth10 <- read_csv(tf, skip = 1) %>% clean_names()

httr::GET("https://www.countyhealthrankings.org/sites/default/files/analytic_data2011.csv",
          write_disk(tf <- tempfile(fileext = ".csv")))
countyhealth09 <- read_csv(tf, skip = 1) %>% clean_names()


#before merging, add year to names to identify dataset of origin
names(countyhealth09) <- paste0(names(countyhealth09), "_09")
names(countyhealth10) <- paste0(names(countyhealth10), "_10")
names(countyhealth11) <- paste0(names(countyhealth11), "_11")
names(countyhealth12) <- paste0(names(countyhealth12), "_12")
names(countyhealth13) <- paste0(names(countyhealth13), "_13")
names(countyhealth14) <- paste0(names(countyhealth14), "_14")
names(countyhealth15) <- paste0(names(countyhealth15), "_15")
names(countyhealth16) <- paste0(names(countyhealth16), "_16")
names(countyhealth17) <- paste0(names(countyhealth17), "_17")

#merge
countyhealth <- countyhealth09 %>%
  full_join(countyhealth10, by = c("fipscode_09" = "fipscode_10")) %>%
  full_join(countyhealth11, by = c("fipscode_09" = "fipscode_11")) %>%
  full_join(countyhealth12, by = c("fipscode_09" = "fipscode_12")) %>%
  full_join(countyhealth13, by = c("fipscode_09" = "fipscode_13")) %>%
  full_join(countyhealth14, by = c("fipscode_09" = "fipscode_14")) %>%
  full_join(countyhealth15, by = c("fipscode_09" = "fipscode_15")) %>%
  full_join(countyhealth16, by = c("fipscode_09" = "fipscode_16")) %>%
  full_join(countyhealth17, by = c("fipscode_09" = "fipscode_17"))

#write out the file
write_csv(countyhealth, "data//countyhealth.csv")

#select the population over 65, and multiple by 100 to get %
elder <- countyhealth %>%
  select_at(vars(contains("v053_rawvalue"))) %>%
  mutate_all(.funs = ~(as.numeric(as.character(.))*100))


#recode the over 65 vars
oldRecode <- function(x){
  ifelse(is.na(x), NA,
         ifelse(x<=7, 20.2,
                ifelse(x<=8, 20.1,
                       ifelse(x<=9, 19.9,
                              ifelse(x<=10, 19.8,
                                     ifelse(x<=11, 19.6,
                                            ifelse(x<=12, 19.4,
                                                   ifelse(x<=13, 19.1,
                                                          ifelse(x<=14, 18.9,
                                                                 
                                                                 ifelse(x<=15, 18.7,
                                                                        ifelse(x<=16, 17.8,
                                                                               ifelse(x<=17, 16.1,
                                                                                      ifelse(x<=18, 14.4,
                                                                                             ifelse(x<=19, 12.8,
                                                                                                    ifelse(x<=20, 11.2,
                                                                                                           ifelse(x<=21, 9.8,
                                                                                                                  ifelse(x<=22, 8.9,
                                                                                                                         ifelse(x<=23, 8,
                                                                                                                                ifelse(x<=24,7,
                                                                                                                                       ifelse(x<=25, 6.1,
                                                                                                                                              ifelse(x<=26, 5.1,
                                                                                                                                                     ifelse(x<=27, 4,
                                                                                                                                                            ifelse(x<=28, 2.8,
                                                                                                                                                                   ifelse(x<=29, 1.7,
                                                                                                                                                                          ifelse(x<=30, 0.6,0)))))))))))))))))))))))))
  
  
  
}

elder <- elder %>%
  mutate_all(.funs = ~(oldRecode(.)))

#rename to remind myself this is the IMU
elder_imu <- elder

#add back in county fips code and name
elder_imu$fips <- countyhealth$fipscode_09
elder_imu$county <- countyhealth$county_09

#write it out
write_csv(elder_imu,"data//elder_imu.csv")
