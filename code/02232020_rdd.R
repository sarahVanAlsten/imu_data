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

# Unadjusted Models -------------------------------------------------------
#read in data
merge.mortb <- read_csv("data\\merged_mort_Feb28.csv")


# merge.mortb <- merge.mort2[is.na(merge.mort2$designation_type) | 
#                             (merge.mort2$designation_type !=
#                             "Medically Underserved Area – Governor’s Exception" &
#                             merge.mort2$designation_type !=
#                             "Medically Underserved Population – Governor’s Exception") &
#                             merge.mort2$imu10 > 20,]
# 
# table(is.na(merge.mortb$imu10))
##################################################################################
rd <- RDestimate(Age.Adjusted.Rate_2011a ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = TRUE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2011a ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)
#####################################################################################
#Cancer models
rd <- RDestimate(Age.Adjusted.Rate_2010c ~ imu10, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2010c ~ imu10, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)
########################################################################################
#heart dz models
rd <- RDestimate(Age.Adjusted.Rate_2010h ~ imu10, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2010h ~ imu10, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)
##################################################################################
#All cause models
rd <- RDestimate(Age.Adjusted.Rate_2011a ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2011a ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)
#####################################################################################
#Cancer models
rd <- RDestimate(Age.Adjusted.Rate_2011c ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2011c ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)
########################################################################################
#heart dz models
rd <- RDestimate(Age.Adjusted.Rate_2011h ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2011h ~ imu11, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

##################################################################################
#All cause models
rd <- RDestimate(Age.Adjusted.Rate_2012a ~ imu12, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2012a ~ imu12, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)
#####################################################################################
#Cancer models
rd <- RDestimate(Age.Adjusted.Rate_2012c ~ imu12, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2012c ~ imu12, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)
########################################################################################
#heart dz models
rd <- RDestimate(Age.Adjusted.Rate_2012h ~ imu12, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)

rd <- RDestimate(Crude.Rate_2012h ~ imu12, data = merge.mortb,
                 subset = NULL, cutpoint = 62, bw = NULL,
                 kernel = "triangular", se.type = "HC1", cluster = NULL,
                 verbose = FALSE, model = TRUE, frame = FALSE)
summary(rd)
plot(rd)



data <- rdd_data(merge.mortb$Crude.Rate_2010a, merge.mortb$imu10, cutpoint = 62)
data <- rdd_data(merge.mortb$Age.Adjusted.Rate_2010a, merge.mortb$imu10, cutpoint = 62)

rdd <- rddtools::rdd_reg_lm(data, slope = "same")
summary(rdd)
plot(rdd, xlab = "IMU", ylab = "Y")

merge.mortb[merge.mortb$Age.Adjusted.Rate_2010a < 400 & 
              !is.na(merge.mortb$Age.Adjusted.Rate_2010a ), "name3"]
