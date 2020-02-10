########################################################
#Author: Sarah Van Alsten
#Date created: January 24, 2020
#Read in HRSA MUA file
#Then recode to weighted value for IMU calculation
#Packages used: tidyverse
##################################################
library(tidyverse)
library(httr)
library(janitor)

hrsa <- readxl::read_xlsx("data\\county_IMU_scores_from_hrsa.xlsx")

#look at data
hrsa <- janitor::clean_names(hrsa)

#how many are withdrawing
table(hrsa$status)
