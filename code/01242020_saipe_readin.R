########################################################
#Author: Sarah Van Alsten
#Date created: January 24, 2020
#Read in SAIPE files to calculate county level poverty
#Then recode to weighted value for IMU calculation
#Packages used: tidyverse
##################################################
library(tidyverse)
library(httr)
library(janitor)

#use readxl to download SAIPE files ins
#read in data dictionary for 2019 to change column names
httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2009/2009-state-and-county/est09all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe09 <- readxl::read_xls(tf, skip = 2)

#clean names
saipe09 <- janitor::clean_names(saipe09)

httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2010/2010-state-and-county/est10all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe10 <- readxl::read_xls(tf, skip = 2) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2011/2011-state-and-county/est11all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe11 <- readxl::read_xls(tf, skip = 2) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2012/2012-state-and-county/est12all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe12 <- readxl::read_xls(tf, skip = 2) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2013/2013-state-and-county/est13all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe13 <- readxl::read_xls(tf, skip = 3) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2014/2014-state-and-county/est14all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe14 <- readxl::read_xls(tf, skip = 3) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2015/2015-state-and-county/est15all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe15 <- readxl::read_xls(tf, skip = 3) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2016/2016-state-and-county/est16all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe16 <- readxl::read_xls(tf, skip = 3) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2017/2017-state-and-county/est17all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe17 <- readxl::read_xls(tf, skip = 3) %>% clean_names()


httr::GET("https://www2.census.gov/programs-surveys/saipe/datasets/2018/2018-state-and-county/est18all.xls",
          write_disk(tf <- tempfile(fileext = ".xls")))
saipe18 <- readxl::read_xls(tf, skip = 3) %>% clean_names()


#Merge them all by county code: first generate a 5 digit code by state and county fips
#if the county code is 1 char, add 00 to front. if 2 char, add 0.
recodeCountyFips <- function(dat){
  dat$state.new <- ifelse(nchar(dat$state_fips) > 2, NA,
                          as.character(dat$state_fips))
  
  dat$county_fips <- as.character(dat$county_fips)
  dat$county.new <- ifelse(nchar(dat$county_fips) == 1,
                           paste0("00", dat$county_fips),
                           ifelse(nchar(dat$county_fips) == 2,
                                  paste0("0", dat$county_fips),
                           dat$county_fips))
  
  dat$fips_full <- paste0(dat$state.new, dat$county.new)
  return(dat)
}
recodeCountyFips2 <- function(dat){
  dat$state.new <- ifelse(nchar(dat$state_fips_code) > 2, "NA",
                          as.character(dat$state_fips_code))

  dat$county_fips_code <- as.character(dat$county_fips_code)
  dat$county.new <- ifelse(nchar(dat$county_fips_code) == 1,
                           paste0("00", dat$county_fips_code),
                           ifelse(nchar(dat$county_fips_code) == 2,
                                  paste0("0", dat$county_fips_code),
                                  dat$county_fips_code))
  
  dat$fips_full <- paste0(dat$state.new, dat$county.new)
  return(dat)
}

saipe09 <- recodeCountyFips(saipe09)
saipe10 <- recodeCountyFips(saipe10)
saipe11 <- recodeCountyFips(saipe11)
saipe12 <- recodeCountyFips2(saipe12)
saipe13 <- recodeCountyFips2(saipe13)
saipe14 <- recodeCountyFips2(saipe14)
saipe15 <- recodeCountyFips2(saipe15)
saipe16 <- recodeCountyFips2(saipe16)
saipe17 <- recodeCountyFips2(saipe17)
saipe18 <- recodeCountyFips2(saipe18)

#before merging, add year to names to identify dataset of origin
names(saipe09) <- paste0(names(saipe09), "_09")
names(saipe10) <- paste0(names(saipe10), "_10")
names(saipe11) <- paste0(names(saipe11), "_11")
names(saipe12) <- paste0(names(saipe12), "_12")
names(saipe13) <- paste0(names(saipe13), "_13")
names(saipe14) <- paste0(names(saipe14), "_14")
names(saipe15) <- paste0(names(saipe15), "_15")
names(saipe16) <- paste0(names(saipe16), "_16")
names(saipe17) <- paste0(names(saipe17), "_17")
names(saipe18) <- paste0(names(saipe18), "_18")

#finally merge
saipe <- saipe09 %>%
  full_join(saipe10, by = c("fips_full_09" = "fips_full_10")) %>%
  full_join(saipe11, by = c("fips_full_09" = "fips_full_11")) %>%
  full_join(saipe12, by = c("fips_full_09" = "fips_full_12")) %>%
  full_join(saipe13, by = c("fips_full_09" = "fips_full_13")) %>%
  full_join(saipe14, by = c("fips_full_09" = "fips_full_14")) %>%
  full_join(saipe15, by = c("fips_full_09" = "fips_full_15")) %>%
  full_join(saipe16, by = c("fips_full_09" = "fips_full_16")) %>%
  full_join(saipe17, by = c("fips_full_09" = "fips_full_17")) %>%
  full_join(saipe18, by = c("fips_full_09" = "fips_full_18")) 

#write out the file
write_csv(saipe, "data//saipe.csv")


####################################################################
#now just keep the overall poverty rate
poverty <- saipe %>%
  select_at(vars(contains("poverty_percent_all_ages")))


povRecode <- function(x){
  return(
    ifelse(is.na(x), NA,
           ifelse(x <= .1, 25.1,
                  ifelse(x <= 1, 24.6,
                         ifelse(x <= 4, 23.7,
                         ifelse(x <= 6, 22.8,
                                ifelse(x<=8, 21.9,
                                       ifelse(x<=10, 21,
                                              ifelse(x<=12, 20,
                                                     ifelse(x<=14, 18.7,
                          ifelse(x<=16, 17.4,
                                 ifelse(x<=18, 16.2,
                                        ifelse(x<=20, 14.9,
                                               ifelse(x<=22, 13.6,
                                                      ifelse(x<=24, 12.2,
                                                             ifelse(x<=26, 10.9,
                                                                    ifelse(x<=28, 9.3,
                ifelse(x<=30, 7.8,
                       ifelse(x<=32, 6.6,
                              ifelse(x<=34, 5.6,
                                     ifelse(x<=36, 4.7,
                                            ifelse(x<=38, 3.4,
                                                   ifelse(x<=40, 2.1,
                                                          ifelse(x<=42, 1.3,
                      ifelse(x<=44, 1,
                             ifelse(x<=46, 0.7,
                                    ifelse(x<=48, 0.4,
                                           ifelse(x<=50, 0.1, 0)))))))))))))))))))))))))))
  )
}


#recode to poverty_imu
pov_imu <- poverty %>%
  mutate_all(.funs = ~(as.numeric(.))) %>%
  mutate_all(.funs = ~(povRecode(.)))

#add county identifiers back on
pov_imu$fips <- saipe$fips_full_09
pov_imu$county <- saipe$name_09

#write this out
write_csv(pov_imu, "data\\pov_imu.csv")

