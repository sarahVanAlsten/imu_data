##########################################################
#Sarah Van Alsten
#Use countyhealthrankings to get score
#############################################################
library(tidyverse)

newdat <- read_csv("C://Users//Owner//OneDrive//Documents//Spring2020//imu_data//CHR_TRENDS_CSV_2019.csv")

#might also want adult obesity rate, unemployment, and physical inactivity
other <- newdat[newdat$measurename %in% c("Adult obesity",
                                          "Physical inactivity",
                                          "Unemployment rate",
                                          "Uninsured", "Premature Death"),]

other.pivot <- other %>%
  select(rawvalue, county, state, measurename, yearspan) %>%
  drop_na(rawvalue, county, state) %>%
  distinct()%>%
  pivot_wider(id_cols =  c("county", "state"),
              values_from = "rawvalue",
              names_from = c("measurename", "yearspan"))
write_csv(other.pivot, "data\\otherpivot.csv")
##########################################################################

# calculate IMU ----------------------------------------------------------


#now, assign the ratio to it
weight_pcp <- function(x){
  if(is.na(x)){
    return(NA)
  }else if (x <= 0.05){
    return(0)
  } else if (x <= .1){
    return(0.5)
  } else if (x <= .15){
    return(1.5)
  } else if (x <= .2){
    return(2.8)
  } else if (x <= .25){
    return(4.1)
  } else if (x <= .3){
    return(5.7)
  } else if (x <= .35){
    return(7.3)
  } else if (x <= .4){
    return(9.0)
  } else if (x <= .45){
    return(10.7)
  } else if (x <= .5){
    return(12.6)
  } else if (x <= .55){
    return(14.8)
  } else if (x <= .6){
    return(16.9)
  } else if (x <= .65){
    return(19.1)
  } else if (x <= .7){
    return(20.7)
  } else if (x <= .75){
    return(21.9)
  } else if (x <= .8){
    return(23.1)
  }else if (x <= .85){
    return(24.3)
  }else if (x <= .90){
    return(25.3)
  }else if (x <= .95){
    return(25.9)
  }else if (x <= 1){
    return(26.6)
  }else if (x <= 1.05){
    return(27.2)
  }else if (x <= 1.1){
    return(27.7)
  }else if (x <= 1.15){
    return(28)
  }else if (x <= 1.2){
    return(28.3)
  }else if (x <= 1.25){
    return(28.6)
  }else {
    return(28.7)
  }
  
}


##########################################################################
#read in the poverty IMU scores
pov <- read_csv("data\\pov_imu.csv")
names(pov) <- str_replace_all(names(pov), "poverty_percent_all_ages", "pv")

#read in elder imu scores
old <- read_csv("data\\elder_imu.csv")
names(old) <- str_replace_all(names(old), pattern = "v053_rawvalue", "old")

#read in county health data for infant lbw/infant mortality
#note there is a lag in years based on how long it takes to process
county17 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2019.csv")
county16 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2018_0.csv")
county15 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2017.csv")
county14 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2016.csv")
county13 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2015.csv")
county12 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2014.csv")
county11 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2013.csv")
county10 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2012.csv")

county17 <- janitor::clean_names(county17)
county16 <- janitor::clean_names(county16)
county15 <- janitor::clean_names(county15)
county14 <- janitor::clean_names(county14)
county13 <- janitor::clean_names(county13)
county12 <- janitor::clean_names(county12)
county11 <- janitor::clean_names(county11)
county10 <- janitor::clean_names(county10)

#attach a year digit to each col name to id dataset it came from
names(county17) <- paste0(names(county17), "_17")
names(county16) <- paste0(names(county16), "_16")
names(county15) <- paste0(names(county15), "_15")
names(county14) <- paste0(names(county14), "_14")
names(county13) <- paste0(names(county13), "_13")
names(county12) <- paste0(names(county12), "_12")
names(county11) <- paste0(names(county11), "_11")
names(county10) <- paste0(names(county10), "_10")

#now merge together
county <- county17 %>%
  right_join(county16, by = c("x5_digit_fips_code_17" = "x5_digit_fips_code_16"))
county <- county %>%
  right_join(county15, by = c("x5_digit_fips_code_17" = "x5_digit_fips_code_15")) %>%
  right_join(county14, by = c("x5_digit_fips_code_17" = "x5_digit_fips_code_14")) %>%
  right_join(county13, by = c("x5_digit_fips_code_17" = "x5_digit_fips_code_13")) %>%
  right_join(county12, by = c("x5_digit_fips_code_17" = "x5_digit_fips_code_12")) %>%
  right_join(county11, by = c("x5_digit_fips_code_17" = "x5_digit_fips_code_11")) %>%
  right_join(county10, by = c("x5_digit_fips_code_17" = "x5_digit_fips_code_10"))
county <- county[county$state_fips_code_17 != "statecode",]


#now get cols I want
primaryCare <- county %>%
  select_at(vars(contains("primary_care_physic")))

#get amt per 1000 pop
primaryCare <- primaryCare %>%
  mutate_at(vars(contains("raw_value")), .funs = ~(as.numeric(.) * 1000))

#keep raw vals only
primaryCare <- primaryCare %>%
  select_at(vars(contains("raw_value")))

#apply the weight
primaryCare <- primaryCare %>%
  rowwise()%>%
  mutate_all(.funs = ~weight_pcp(.))

#add back on county fips and state and county
primaryCare$x5_digit_fips_code <- county$x5_digit_fips_code_17
primaryCare$state <- county$state_abbreviation_17
primaryCare$countyname <- county$name_17

##########################################################################################
#do it again for infant health index

#select needed cols
inf <- county %>%
  select_at(vars(contains("infant_mortality")))
inf2 <- county %>%
  select_at(vars(contains("low_birth")))

#bind in one df
inf <- cbind(inf, inf2)

#only keep actual raw value
inf <- inf %>%
  select_at(vars(contains("raw")))

#put it in correct scale
inf <- inf %>%
  mutate_at(vars(contains("low_birth")), .funs = ~(as.numeric(.)*100))
inf <- inf %>%
  mutate_at(vars(contains("infant_mortality_raw_value")), .funs = ~(as.numeric(.)))

#make a composite measure of infant health
inf$inf_new_11 <- ifelse(is.na(inf$infant_mortality_raw_value_11),
                         inf$low_birthweight_raw_value_11,
                         inf$infant_mortality_raw_value_11)

inf$inf_new_12 <- ifelse(is.na(inf$infant_mortality_raw_value_12),
                         inf$low_birthweight_raw_value_12,
                         inf$infant_mortality_raw_value_12)

inf$inf_new_13 <- ifelse(is.na(inf$infant_mortality_raw_value_13),
                         inf$low_birthweight_raw_value_13,
                         inf$infant_mortality_raw_value_13)

inf$inf_new_14 <- ifelse(is.na(inf$infant_mortality_raw_value_14),
                         inf$low_birthweight_raw_value_14,
                         inf$infant_mortality_raw_value_14)

inf$inf_new_15 <- ifelse(is.na(inf$infant_mortality_raw_value_15),
                         inf$low_birthweight_raw_value_15,
                         inf$infant_mortality_raw_value_15)

inf$inf_new_16 <- ifelse(is.na(inf$infant_mortality_raw_value_16),
                         inf$low_birthweight_raw_value_16,
                         inf$infant_mortality_raw_value_16)

inf$inf_new_17 <- ifelse(is.na(inf$infant_mortality_raw_value_17),
                         inf$low_birthweight_raw_value_17,
                         inf$infant_mortality_raw_value_17)



#classify the infant mortality
recodeInf <- function(x) {
  ifelse(is.na(x), NA,
         ifelse(x <= 8, 26,
                ifelse(x <= 9, 25.6,
                       ifelse(
                         x <= 10, 24.8,
                         ifelse(x <= 11, 24,
                                ifelse(
                                  x <= 12, 23.2,
                                  ifelse(x <= 13, 22.4,
                                         ifelse(
                                           x <= 14, 20.5,
                                           ifelse(x <= 15, 20.5,
                                                  ifelse(
                                                    x <= 16, 19.5,
                                                    ifelse(x <=
                                                             17, 18.5,
                                                           ifelse(
                                                             x <= 18, 17.5,
                                                             ifelse(x <=
                                                                      19, 16.4,
                                                                    ifelse(
                                                                      x <= 20, 15.3,
                                                                      ifelse(x <=
                                                                               21, 14.2,
                                                                             ifelse(
                                                                               x <= 22, 13.1,
                                                                               ifelse(x <=
                                                                                        23, 11.9,
                                                                                      ifelse(x <=
                                                                                               24, 10.8,
                                                                                             ifelse(
                                                                                               x <= 25, 9.6,
                                                                                               ifelse(x <=
                                                                                                        26, 8.5,
                                                                                                      ifelse(x <=
                                                                                                               27, 7.3,
                                                                                                             ifelse(
                                                                                                               x <= 28, 6.1,
                                                                                                               ifelse(x <=
                                                                                                                        29, 5.4,
                                                                                                                      ifelse(x <=
                                                                                                                               30, 5,
                                                                                                                             ifelse(
                                                                                                                               x <= 31, 4.7,
                                                                                                                               ifelse(x <=
                                                                                                                                        32, 4.3,
                                                                                                                                      ifelse(x <=
                                                                                                                                               33, 4,
                                                                                                                                             ifelse(
                                                                                                                                               x <= 34, 3.6,
                                                                                                                                               ifelse(x <=
                                                                                                                                                        35, 3.3,
                                                                                                                                                      ifelse(x <=
                                                                                                                                                               36, 3,
                                                                                                                                                             ifelse(
                                                                                                                                                               x <= 37, 2.6,
                                                                                                                                                               ifelse(x <=
                                                                                                                                                                        39, 2.0,
                                                                                                                                                                      ifelse(x <=
                                                                                                                                                                               41, 1.4,
                                                                                                                                                                             ifelse(
                                                                                                                                                                               x <= 43, 0.8,
                                                                                                                                                                               ifelse(x <= 45, 0.2, 0)
                                                                                                                                                                             )))
                                                                                                                                                             )))
                                                                                                                                             )))
                                                                                                                             )))
                                                                                                             )))
                                                                                             )))
                                                                             ))
                                                                    ))
                                                           ))
                                                  ))
                                         ))
                                ))
                       ))))
}

#recode to reflect IMUs
inf_score <- inf %>%
  mutate_at(vars(contains("inf_new")), .funs = ~(recodeInf(.)))
#add back on county fips and state and county
inf_score$x5_digit_fips_code <- county$x5_digit_fips_code_17
inf_score$state <- county$state_abbreviation_17
inf_score$countyname <- county$name_17
###############################################################################
#older adults
old <- county %>%
  select_at(vars(contains("percent_65_and"))) %>%
  select_at(vars(contains("raw")))

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

old <- old %>%
  #multiply by 100 to get %
  mutate_at(vars(contains("percent")), .funs = ~(100*as.numeric(.))) %>%   
  mutate_at(vars(contains("percent")), .funs = ~(oldRecode(.)))

#add back on county fips and state and county
old$x5_digit_fips_code <- county$x5_digit_fips_code_17
old$state <- county$state_abbreviation_17
old$countyname <- county$name_17

pov <- pov %>%
  rename(x5_digit_fips_code = fips)
################################################################
#finally, link all these back together
imu <- primaryCare %>%
  right_join(pov) %>%
  right_join(inf_score) %>%
  right_join(old)

#make composite score
imu <- imu %>%
  mutate(imu17 = percent_65_and_older_raw_value_17 + pv_17 + inf_new_17 + primary_care_physicians_raw_value_17,
         imu16 = percent_65_and_older_raw_value_16 + pv_16 + inf_new_16 + primary_care_physicians_raw_value_16,
         imu15 = percent_65_and_older_raw_value_15 + pv_15 + inf_new_15 + primary_care_physicians_raw_value_15,
         imu14 = percent_65_and_older_raw_value_14 + pv_14 + inf_new_14 + primary_care_physicians_raw_value_14,
         imu13 = percent_65_and_older_raw_value_13 + pv_13 + inf_new_13 + primary_care_physicians_raw_value_13,
         imu12 = percent_65_and_older_raw_value_12 + pv_12 + inf_new_12 + primary_care_physicians_raw_value_12,
         imu11 = percent_65_and_older_raw_value_11 + pv_11 + inf_new_11 + primary_care_physicians_raw_value_11)


imu <- imu %>%
  select(x5_digit_fips_code, countyname,
         imu17, imu16, imu15, imu14, imu13, imu12, imu11)

plot(imu$imu17)
plot(imu$imu16)
plot(imu$imu15)
plot(imu$imu14)
plot(imu$imu13)
plot(imu$imu12)
plot(imu$imu11)

#write it out
write_csv(imu, "data\\feb28_imu.csv")
###############################################

# mortality data ----------------------------------------------------------

##################################################################################
#mortality data

#read in all cause mortality data
mort1 <- read.delim("heartdata\\all99.txt")
mort2 <- read.delim("heartdata\\all07.txt")
mort3 <- read.delim("heartdata\\all12.txt")
mort <- rbind(mort1, mort2, mort3)

#make it one row per county instead of multiple rows
mort.pivot <- mort %>%
  select(-Notes)%>%
  drop_na(County, County.Code, Year, Year.Code,
          Deaths, Crude.Rate, Crude.Rate.Standard.Error, 
          Age.Adjusted.Rate, Age.Adjusted.Rate.Standard.Error,
          X..of.Total.Deaths) %>%
  pivot_wider(id_cols = c("County", "County.Code"),
              names_from = "Year",
              values_from = c("Deaths", "Crude.Rate", "Crude.Rate.Standard.Error", 
                              "Age.Adjusted.Rate","Population", "X..of.Total.Deaths"))

can1 <- read.delim("heartdata\\can99.txt")
can2 <- read.delim("heartdata\\can03.txt")
can3 <- read.delim("heartdata\\can06.txt")
can4 <- read.delim("heartdata\\can12.txt")

#cancer pivoted
can.pivot <- rbind(can1, can2, can3, can4) %>%
  select(-Notes)%>%
  drop_na(County, County.Code, Year, Year.Code,
          Deaths, Crude.Rate, Crude.Rate.Standard.Error, 
          Age.Adjusted.Rate, Age.Adjusted.Rate.Standard.Error,
          X..of.Total.Deaths) %>%
  pivot_wider(id_cols = c("County", "County.Code"),
              names_from = "Year",
              values_from = c("Deaths", "Crude.Rate", "Crude.Rate.Standard.Error", 
                              "Age.Adjusted.Rate","Population", "X..of.Total.Deaths"))


heart1 <- read.delim("heartdata\\heart99.txt")
heart2 <- read.delim("heartdata\\hear02.txt")
heart3 <- read.delim("heartdata\\heart06.txt")
heart4 <- read.delim("heartdata\\heart10.txt")


#heart pivoted
heart.pivot <- rbind(heart1, heart2, heart3, heart4) %>%
  select(-Notes)%>%
  drop_na(County, County.Code, Year, Year.Code,
          Deaths, Crude.Rate, Crude.Rate.Standard.Error, 
          Age.Adjusted.Rate, Age.Adjusted.Rate.Standard.Error,
          X..of.Total.Deaths) %>%
  pivot_wider(id_cols = c("County", "County.Code"),
              names_from = "Year",
              values_from = c("Deaths", "Crude.Rate", "Crude.Rate.Standard.Error", 
                              "Age.Adjusted.Rate","Population", "X..of.Total.Deaths"))

#attach to names what the deaths refer to
names(heart.pivot) <- paste0(names(heart.pivot), "h")
names(can.pivot) <- paste0(names(can.pivot), "c")
names(mort.pivot) <- paste0(names(mort.pivot), "a")

#now put them altogether: rates are per 100,000
all.mort <- mort.pivot %>% 
  full_join(heart.pivot, by = c("County.Codea"= "County.Codeh")) %>%
  full_join(can.pivot, by = c("County.Codea" = "County.Codec"))

#clean up data a bit
all.mort <- all.mort %>%
  rename(County.Code = County.Codea,
         County = Countya) %>%
  select(-Countyh, Countyc)

#get rid off the commas in all.mort data
all.mort$County <- str_remove_all(all.mort$County, pattern = ",")

#make sure the fips code is 5 digits
all.mort$x5_digit_fips_code <- ifelse(nchar(all.mort$County.Code)==4,
                                      paste0("0", all.mort$County.Code),
                                      all.mort$County.Code)

table(all.mort$x5_digit_fips_code %in% imu$x5_digit_fips_code)

#now merge them together
merged.data <- imu %>%
  full_join(all.mort)

#get rid of unreliable designations
#replace all the unreliable texts
merged.data <- merged.data %>%
  mutate_at(vars(contains("Age.Adjusted.Rate")),
            .funs = as.character)%>% 
  mutate_at(vars(contains("Age.Adjusted.Rate")),
            .funs = ~(str_replace_all(., pattern = "\\(Unreliable\\)", replacement = ""))) %>% 
  mutate_at(vars(contains("Age.Adjusted.Rate")),
            .funs = as.numeric)

#do same in crude rates
merged.data <- merged.data %>%
  mutate_at(vars(contains("Crude.Rate")),
            .funs = as.character)%>% 
  mutate_at(vars(contains("Crude.Rate")),
            .funs = ~(str_replace_all(., pattern = "\\(Unreliable\\)", replacement = ""))) %>% 
  mutate_at(vars(contains("Crude.Rate")),
            .funs = as.numeric)

#check coding
table(str_detect(merged.data$Age.Adjusted.Rate_1999a, pattern = "Unreliable"))

#crude mort
plot(merged.data$Crude.Rate_2011c, merged.data$imu11)
plot(merged.data$Crude.Rate_2011h, merged.data$imu11)
plot(merged.data$Crude.Rate_2011a, merged.data$imu11)

#age adjusted
plot(merged.data$Age.Adjusted.Rate_2011c, merged.data$imu11)
plot(merged.data$Age.Adjusted.Rate_2011h, merged.data$imu11)
plot(merged.data$Age.Adjusted.Rate_2011a, merged.data$imu11)

#write this to csv
write_csv(merged.data, path = "data\\merged_mort_feb28.csv")

# ###################################################################
hrsa <- readxl::read_xlsx("data\\county_IMU_scores_from_hrsa.xlsx")

county.hrsa <- hrsa[hrsa$`Service Area` %in% merged.data$countyname,]
oth <- merged.data[merged.data$countyname %in% hrsa$`Service Area`,]

county.hrsa$County <- paste(county.hrsa$`Service Area`,
                            county.hrsa$State)

compare <- county.hrsa %>%
  right_join(merged.data,
            by = c("County"))
compare$sub <- compare$imu17 -compare$`Index of Medical Underservice Score`

compare$newimu17 <- ifelse(!is.na(compare$`Index of Medical Underservice Score`),
                           compare$`Index of Medical Underservice Score`,
                           compare$imu17)
plot(compare$newimu17)

compare$newimu16 <- ifelse(!is.na(compare$`Index of Medical Underservice Score`),
                           compare$imu16 - compare$sub,
                           compare$imu16)
compare$newimu15 <- ifelse(!is.na(compare$`Index of Medical Underservice Score`),
                           compare$imu15 - compare$sub,
                           compare$imu15)
compare$newimu14 <- ifelse(!is.na(compare$`Index of Medical Underservice Score`),
                           compare$imu14 - compare$sub,
                           compare$imu14)


plot(compare$Age.Adjusted.Rate_2014a, compare$newimu14)
anal <- compare
anal$treatment <- ifelse(is.na(anal$`Service Area`), 0, 1)


# RDD ---------------------------------------------------------------------

library(tidyverse)
library(rdd)
library(rddtools)
library(rddapp)
#remotes::install_github(repo = "MatthieuStigler/RDDtools", subdir = "RDDtools")

# Unadjusted Models -------------------------------------------------------
#read in data
merge.mortb <- anal
merge.mortb <- janitor::clean_names(merge.mortb)

merge.mortb <- merge.mortb[is.na(merge.mortb$designation_type) | 
                            (merge.mortb$designation_type !=
                               "Medically Underserved Area – Governor’s Exception" &
                               merge.mortb$designation_type !=
                               "Medically Underserved Population – Governor’s Exception"),]

other.pivot<- read_csv("data\\otherpivot.csv")

table(other.pivot$county %in% merge.mortb$countyname)
other.pivot$County <- paste(other.pivot$county, other.pivot$state)

merge.mortb <- merge.mortb %>%
  left_join(other.pivot, by = c("county" = "County"))

#countyhealth for smoking
smoke <- read_csv('C:\\Users\\Owner\\OneDrive\\Documents\\Spring2020\\Causal_Inf\\Health_Shortage_Data\\healthRankings\\county14.csv')
smoke <- smoke[,c("Adult.smoking.Value", "FIPS","County", "statecode", "State")]
smoke$county <- paste(smoke$County, smoke$State)

merge.mortb <- merge.mortb %>%
  left_join(smoke, by = "county")

merge.mortb <- merge.mortb[merge.mortb$newimu14 > 25,]
################################################
#wrong <- merge.mortb[merge.mortb$treatment == 0 & merge.mortb$newimu14<62,]
#wrongb <- merge.mortb[merge.mortb$treatment == 1 & merge.mortb$newimu14<62,]

#2014 was year in which new IMU scoring came out
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2014a)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2014c)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2014h)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2015a)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2015c)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2015h)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2016a)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2016c)
plot(merge.mortb$newimu14, merge.mortb$age_adjusted_rate_2016h)

#all cause mortality in 2014
###########################################################
ac.14 <- RDestimate(age_adjusted_rate_2014a ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.14)
plot(ac.14)

ac.14c <- RDestimate(crude_rate_2014a ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.14c)
plot(ac.14c)
#################################################################
#heartdz mortality in 2014
###########################################################
hd.14 <- RDestimate(age_adjusted_rate_2014h ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.14)
plot(hd.14)

hd.14c <- RDestimate(crude_rate_2014h ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.14c)
plot(hd.14c)
#################################################################
#cancer mortality in 2014
###########################################################
ca.14 <- RDestimate(age_adjusted_rate_2014c ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.14)
plot(ca.14)

ca.14c <- RDestimate(crude_rate_2014c ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.14c)
plot(ca.14c)

##################################################################
#2015: All cause
###########################################################
ac.15 <- RDestimate(age_adjusted_rate_2015a ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.15)
plot(ac.15)

ac.15c <- RDestimate(crude_rate_2015a ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.15c)
plot(ac.15c)
#################################################################
#heartdz mortality in 2015
###########################################################
hd.15 <- RDestimate(age_adjusted_rate_2015h ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.15)
plot(hd.15)

hd.15c <- RDestimate(crude_rate_2015h ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.15c)
plot(hd.15c)
#################################################################
#cancer mortality in 2015
###########################################################
ca.15 <- RDestimate(age_adjusted_rate_2015c ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.15)
plot(ca.15)

ca.15c <- RDestimate(crude_rate_2015c ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.15c)
plot(ca.15c)

##################################################################
##################################################################
#2016: All cause
###########################################################
ac.16 <- RDestimate(age_adjusted_rate_2016a ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.16)
plot(ac.16)

ac.16c <- RDestimate(crude_rate_2016a ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.16c)
plot(ac.16c)
#################################################################
#heartdz mortality in 2016
###########################################################
hd.16 <- RDestimate(age_adjusted_rate_2016h ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.16)
plot(hd.16)

hd.16c <- RDestimate(crude_rate_2016h ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(hd.16c)
plot(hd.16c)
#################################################################
#cancer mortality in 2015
###########################################################
ca.16 <- RDestimate(age_adjusted_rate_2016c ~ newimu14 + treatment, data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.16)
plot(ca.16)

ca.16c <- RDestimate(crude_rate_2016c ~ newimu14 + treatment, data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.16c)
plot(ca.16c)

##################################################################################
#make adjustments for county obesity rate, uninsurance rate, and unemployment
merge.mortb <- janitor::clean_names(merge.mortb)

ac.142 <- RDestimate(age_adjusted_rate_2014a ~ newimu14 + treatment | unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.142)
plot(ac.142)

ac.14c2 <- RDestimate(crude_rate_2014a ~ newimu14 + treatment | unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ac.14c2)
plot(ac.14c2)
#################################################################
#heartdz mortality in 2014
###########################################################
hd.14 <- RDestimate(age_adjusted_rate_2014h ~ newimu14 + treatment | unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.14)
plot(hd.14)

hd.14c <- RDestimate(crude_rate_2014h ~ newimu14 + treatment| unemployment_rate_2014 +
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
ca.14 <- RDestimate(age_adjusted_rate_2014c ~ newimu14 + treatment| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.14)
plot(ca.14)

ca.14c <- RDestimate(crude_rate_2014c ~ newimu14 + treatment| unemployment_rate_2014 +
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
ac.15 <- RDestimate(age_adjusted_rate_2015a ~ newimu14 + treatment| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014 + adult_smoking_value, 
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.15)
plot(ac.15)

ac.15c <- RDestimate(crude_rate_2015a ~ newimu14 + treatment| unemployment_rate_2014 +
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
hd.15 <- RDestimate(age_adjusted_rate_2015h ~ newimu14 + treatment| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.15)
plot(hd.15)

hd.15c <- RDestimate(crude_rate_2015h ~ newimu14 + treatment| unemployment_rate_2014 +
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
ca.15 <- RDestimate(age_adjusted_rate_2015c ~ newimu14 + treatment| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.15)
plot(ca.15)

ca.15c <- RDestimate(crude_rate_2015c ~ newimu14 + treatment| unemployment_rate_2014 +
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
ac.16 <- RDestimate(age_adjusted_rate_2016a ~ newimu14 + treatment| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ac.16)
plot(ac.16)

ac.16c <- RDestimate(crude_rate_2016a ~ newimu14 + treatment| unemployment_rate_2014 +
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
hd.16 <- RDestimate(age_adjusted_rate_2016h ~ newimu14 + treatment| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(hd.16)
plot(hd.16)

hd.16c <- RDestimate(crude_rate_2016h ~ newimu14 + treatment| unemployment_rate_2014 +
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
ca.16 <- RDestimate(age_adjusted_rate_2016c ~ newimu14 + treatment| unemployment_rate_2014 +
                      adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                    data = merge.mortb,
                    subset = NULL, cutpoint = 62, bw = NULL,
                    kernel = "triangular", se.type = "HC1", cluster = NULL,
                    verbose = TRUE, model = TRUE, frame = FALSE)
summary(ca.16)
plot(ca.16)

ca.16c <- RDestimate(crude_rate_2016c ~ newimu14 + treatment| unemployment_rate_2014 +
                       adult_obesity_2012_2014 + uninsured_2014+ adult_smoking_value,
                     data = merge.mortb,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
summary(ca.16c)
plot(ca.16c)


###################################################
#McCrary Density tests
`IMU Score 2014` <- merge.mortb$newimu14
plot(y =merge.mortb$adult_obesity_2012_2014, x= merge.mortb$newimu14)
plot(y =merge.mortb$adult_smoking_value, x= merge.mortb$newimu14)
plot(y =merge.mortb$uninsured_2014, x= merge.mortb$newimu14)
plot(y =merge.mortb$unemployment_rate_2014, x= merge.mortb$newimu14)

DCdensity(merge.mortb$newimu14, 62, bw = 11.9)
rddapp::dc_test(`IMU Score 2014`, 62, bw = 11.9,)

DCdensity(merge.mortb$newimu14, 62, bw = 9.5)
DCdensity(merge.mortb$newimu14, 62, bw = 8)

