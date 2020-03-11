##############################################
#Sarah Van Alsten
#February 23, 2020
#Purpose: Do RD with the AHRF/IMU data
#Packages: tidyverse, rdd, rddtools, rddapp
##############################################
library(tidyverse)
library(rdd)
library(rddtools)
library(rddapp)
#remotes::install_github(repo = "MatthieuStigler/RDDtools", subdir = "RDDtools")

# Unadjusted Models -------------------------------------------------------
#read in data
merge.mort <- read_csv("data\\merged_mort_Feb28.csv")
merge.mortb <- read_csv("data\\merged_mort.csv")

merge.mort$designation_type <- merge.mortb$designation_type

merge.mortb <- merge.mort[is.na(merge.mort$designation_type) | 
                            (merge.mort$designation_type !=
                               "Medically Underserved Area – Governor’s Exception" &
                               merge.mort$designation_type !=
                               "Medically Underserved Population – Governor’s Exception") &
                            merge.mort$imu14 > 40,]

other.pivot<- read_csv("data\\otherpivot.csv")

table(other.pivot$county %in% merge.mortb$countyname)
other.pivot$County <- paste(other.pivot$county, other.pivot$state)

merge.mortb <- merge.mortb %>%
  left_join(other.pivot, by = "County")

#countyhealth for smoking
smoke <- read_csv('C:\\Users\\Owner\\OneDrive\\Documents\\Spring2020\\Causal_Inf\\Health_Shortage_Data\\healthRankings\\county14.csv')
smoke <- smoke[,c("Adult.smoking.Value", "FIPS","County", "statecode", "State")]
smoke$county <- paste(smoke$County, smoke$State)

merge.mortb <- merge.mortb %>%
  left_join(smoke, by = "county")

################################################

#2014 was year in which new IMU scoring came out
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2014a)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2014c)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2014h)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2015a)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2015c)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2015h)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2016a)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2016c)
plot(merge.mortb$imu14, merge.mortb$Age.Adjusted.Rate_2016h)

#all cause mortality in 2014
###########################################################
ac.14 <- RDestimate(Age.Adjusted.Rate_2014a ~ imu14, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.14)
plot(ac.14)

ac.14c <- RDestimate(Crude.Rate_2014a ~ imu14, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.14c)
plot(ac.14c)
#################################################################
#heartdz mortality in 2014
###########################################################
hd.14 <- RDestimate(Age.Adjusted.Rate_2014h ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.14)
plot(hd.14)

hd.14c <- RDestimate(Crude.Rate_2014h ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.14c)
plot(hd.14c)
#################################################################
#cancer mortality in 2014
###########################################################
ca.14 <- RDestimate(Age.Adjusted.Rate_2014c ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.14)
plot(ca.14)

ca.14c <- RDestimate(Crude.Rate_2014c ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.14c)
plot(ca.14c)

##################################################################
#2015: All cause
###########################################################
ac.15 <- RDestimate(Age.Adjusted.Rate_2015a ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.15)
plot(ac.15)

ac.15c <- RDestimate(Crude.Rate_2015a ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.15c)
plot(ac.15c)
#################################################################
#heartdz mortality in 2015
###########################################################
hd.15 <- RDestimate(Age.Adjusted.Rate_2015h ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.15)
plot(hd.15)

hd.15c <- RDestimate(Crude.Rate_2015h ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.15c)
plot(hd.15c)
#################################################################
#cancer mortality in 2015
###########################################################
ca.15 <- RDestimate(Age.Adjusted.Rate_2015c ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.15)
plot(ca.15)

ca.15c <- RDestimate(Crude.Rate_2015c ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.15c)
plot(ca.15c)

##################################################################
##################################################################
#2016: All cause
###########################################################
ac.16 <- RDestimate(Age.Adjusted.Rate_2016a ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.16)
plot(ac.16)

ac.16c <- RDestimate(Crude.Rate_2016a ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.16c)
plot(ac.16c)
#################################################################
#heartdz mortality in 2016
###########################################################
hd.16 <- RDestimate(Age.Adjusted.Rate_2016h ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.16)
plot(hd.16)

hd.16c <- RDestimate(Crude.Rate_2016h ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.16c)
plot(hd.16c)
#################################################################
#cancer mortality in 2015
###########################################################
ca.16 <- RDestimate(Age.Adjusted.Rate_2016c ~ imu14, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.16)
plot(ca.16)

ca.16c <- RDestimate(Crude.Rate_2016c ~ imu14, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.16c)
plot(ca.16c)

##################################################################################
#make adjustments for county obesity rate, uninsurance rate, and unemployment
merge.mortb <- janitor::clean_names(merge.mortb)

ac.14 <- RDestimate(age_adjusted_rate_2014a ~ imu14 | unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.14)
plot(ac.14)

ac.14c <- RDestimate(crude_rate_2014a ~ imu14 | unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.14c)
plot(ac.14c)
#################################################################
#heartdz mortality in 2014
###########################################################
hd.14 <- RDestimate(age_adjusted_rate_2014h ~ imu14 | unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.14)
plot(hd.14)

hd.14c <- RDestimate(crude_rate_2014h ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.14c)
plot(hd.14c)
#################################################################
#cancer mortality in 2014
###########################################################
ca.14 <- RDestimate(age_adjusted_rate_2014c ~ imu14| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.14)
plot(ca.14)

ca.14c <- RDestimate(crude_rate_2014c ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.14c)
plot(ca.14c)

##################################################################
#2015: All cause
###########################################################
ac.15 <- RDestimate(age_adjusted_rate_2015a ~ imu14| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value, 
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.15)
plot(ac.15)

ac.15c <- RDestimate(crude_rate_2015a ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.15c)
plot(ac.15c)
#################################################################
#heartdz mortality in 2015
###########################################################
hd.15 <- RDestimate(age_adjusted_rate_2015h ~ imu14| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.15)
plot(hd.15)

hd.15c <- RDestimate(crude_rate_2015h ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.15c)
plot(hd.15c)
#################################################################
#cancer mortality in 2015
###########################################################
ca.15 <- RDestimate(age_adjusted_rate_2015c ~ imu14| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.15)
plot(ca.15)

ca.15c <- RDestimate(crude_rate_2015c ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.15c)
plot(ca.15c)

##################################################################
#2016: All cause
###########################################################
ac.16 <- RDestimate(age_adjusted_rate_2016a ~ imu14| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                     cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.16)
plot(ac.16)

ac.16c <- RDestimate(crude_rate_2016a ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.16c)
plot(ac.16c)
#################################################################
#heartdz mortality in 2016
###########################################################
hd.16 <- RDestimate(age_adjusted_rate_2016h ~ imu14| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.16)
plot(hd.16)

hd.16c <- RDestimate(crude_rate_2016h ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.16c)
plot(hd.16c)
#################################################################
#cancer mortality in 2015
###########################################################
ca.16 <- RDestimate(age_adjusted_rate_2016c ~ imu14| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.16)
plot(ca.16)

ca.16c <- RDestimate(crude_rate_2016c ~ imu14| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.16c)
plot(ca.16c)


###################################################
#McCrary Density tests
DCdensity(merge.mortb$imu14, 62, bw = 11.9)
DCdensity(merge.mortb$imu14, 62, bw = 9.5)
DCdensity(merge.mortb$imu14, 62, bw = 8)
