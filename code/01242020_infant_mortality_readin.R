########################################################
#Author: Sarah Van Alsten
#Date created: January 24, 2020
#Read in countyhealth files to calculate infant mortality rates
#Then recode to weighted value for IMU calculation
#Packages used: tidyverse
##################################################
library(tidyverse)
library(httr)
library(janitor)

countyhealth <- read_csv("data//countyhealth.csv")

infant <- countyhealth %>%
  select_at(vars(contains("v129_rawvalue")))

#year 11 needs to be divided by 100
infant$v129_rawvalue_11 <- infant$v129_rawvalue_11 / 100


#classify the infant mortality
recodeInf <- function(x){
  ifelse(is.na(x), NA,
         ifelse(x<=8, 26,
                ifelse(x<=9, 25.6,
                       ifelse(x<=10, 24.8,
                              ifelse(x<=11, 24,
                                     ifelse(x<=12, 23.2,
                                            ifelse(x<=13, 22.4,
                                                   ifelse(x<=14, 20.5,
                                                          ifelse(x<=15, 20.5,
                                                                 ifelse(x<=16, 19.5,
                                                                        ifelse(x<=17, 18.5,
                                                                               ifelse(x<=18, 17.5,
                                                                                      ifelse(x<=19, 16.4,
                                                                                             ifelse(x<=20, 15.3,
                                                                                                    ifelse(x<=21, 14.2,
                                                                                                           ifelse(x<=22, 13.1,
                                                                                                                  ifelse(x<=23, 11.9,
                                                                                                                         ifelse(x<=24, 10.8,
                                                                                                                                ifelse(x<=25,9.6,
                                                                                                                                       ifelse(x<=26, 8.5,
                                                                                                                                              ifelse(x<=27, 7.3,
                                                                                                                                                     ifelse(x<=28, 6.1,
                                                                                                                                                            ifelse(x<=29, 5.4,
                                                                                                                                                                   ifelse(x<=30, 5,
                                                                                                                                                                          ifelse(x<=31, 4.7,
                                                                                                                                                                                 ifelse(x<=32, 4.3,
                                                                                                                                                                                        ifelse(x<=33, 4,
                                                                                                                                                                                               ifelse(x<=34, 3.6,
                                                                                                                                                                                                      ifelse(x<=35, 3.3,
                                                                                                                                                                                                             ifelse(x<=36, 3,
                                                                                                                                                                                                                    ifelse(x<=37, 2.6,
                                                                                                                                                                                                                           ifelse(x<=39, 2.0,
                                                                                                                                                                                                                                  ifelse(x<=41, 1.4,
                                                                                                                                                                                                                                         ifelse(x<=43, 0.8,
                                                                                                                                                                                                                                                ifelse(x<= 45, 0.2,0)))))))))))))))))))))))))))))))))))
}

#recode to reflect IMUs
infant_imu <- infant %>%
  mutate_all(.funs = ~(recodeInf(.)))
  

#add the county and fips code back in
infant_imu$fips <- countyhealth$fipscode_09
infant_imu$county <- countyhealth$county_09

#write it out
write_csv(infant_imu,"data//infant_imu.csv")
