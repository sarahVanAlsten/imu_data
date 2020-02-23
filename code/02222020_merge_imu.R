#################################
#Sarah Van Alsten
#Created: Feb 22, 2020
#Purpose: Join AHRF and IMU data
#Packages: tidyverse
################################
library(tidyverse)

#read in data
ahrf.imu <- read_csv( "data\\ahrf_imu.csv")

#read in data + clean names
imu <- readxl::read_xlsx(path = "data\\county_IMU_scores_from_hrsa.xlsx")
imu <- janitor::clean_names(imu)

#calculate an IMU score for each county, at least in years 10, 15, and 17
ahrf.imu <- ahrf.imu %>%
  rowwise() %>%
  mutate(imu17 = pcp17weight + pop65perc17 + infmort1317 + povperc17,
         imu15 = pcp15weight + pop65perc15 + infmort1115 + povperc15,
         imu10 = pcp10weight + pop65perc10 + infmort0610 + povperc10)


#plot to check
plot(ahrf.imu$imu17)
plot(ahrf.imu$imu15)
plot(ahrf.imu$imu10)

head(ahrf.imu$statefips)
head(ahrf.imu$countyfips)

#try and make columns so we can merge the datasets
imu$countyname2 <- str_replace(imu$service_area, "Service Area", "County")
ahrf.imu$countyname2 <- paste(as.character(ahrf.imu$countyname), "County")

#subset the ones that are acutally counties and not just service areas
is.county <- imu[stringr::str_detect(imu$service_area, pattern = "County"),]

#remove low income designations which prevent matching
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Inc - ")
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Inc -")
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "LI-") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "LI -") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Inc/ M F W -")  
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Inc/ M F W-") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Inc/MFW - ") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low-income/mfw Of ") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Income - ") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Update MUA 01648") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Income Of ") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low-Income Population of") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low-Income") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Inc / M F W -") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Hispanic Population -") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Low Income-") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "Gov MUP") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "MUP - LI") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "(GOV)") 
is.county$countyname2 <- str_remove_all(is.county$countyname2, pattern = "()")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Blounty", "Blount")

#if it has a space at beginning get rid of that space
is.county$countyname2 <- ifelse(substr(is.county$countyname2, 1, 1) == " ",
                                substr(is.county$countyname2, 2, nchar(is.county$countyname2)),
                                is.county$countyname2)
#and remove any spaces at the end
is.county$countyname2 <- ifelse(substr(is.county$countyname2, 
                                       nchar(is.county$countyname2),nchar(is.county$countyname2)) == " ",
                                substr(is.county$countyname2, 1, nchar(is.county$countyname2)-1),
                                is.county$countyname2)

#subset the ones with a match
is.county.in <- is.county[is.county$countyname2 %in% ahrf.imu$countyname2,]
#make a name that includes county and state
is.county.in$name3 <- paste(is.county.in$countyname2, is.county.in$state)
ahrf.imu$name3 <- paste(ahrf.imu$countyname2, ahrf.imu$stateabb)


#check coding
table(is.county.in$name3 %in% ahrf.imu$name3)
plot(is.county$index_of_medical_underservice_score)
table(ahrf.imu$countyname2 %in% is.county.in$countyname2)


#now create one merged data set with the counties in ahrf
merged <- ahrf.imu %>% 
  full_join(is.county.in, by = "name3")

merged$new_imu17 <- ifelse(is.na(merged$index_of_medical_underservice_score),
                           merged$imu17,
                           merged$index_of_medical_underservice_score)
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

#now merge this in with the other data
merge.mort <- merged %>%
  left_join(all.mort, by = c("name3" = "County"))

merge.mort$Age.Adjusted.Rate_1999a <- str_remove_all(
  as.character(merge.mort$Age.Adjusted.Rate_1999a), pattern = "(Unreliable)")

#replace all the unreliable texts
merge.mort <- merge.mort %>%
  mutate_at(vars(contains("Age.Adjusted.Rate")),
            .funs = as.character)%>% 
  mutate_at(vars(contains("Age.Adjusted.Rate")),
            .funs = ~(str_replace_all(., pattern = " (Unreliable)", replacement = ""))) %>% 
  mutate_at(vars(contains("Age.Adjusted.Rate")),
          .funs = as.numeric)

#do same in crude rates
merge.mort <- merge.mort %>%
  mutate_at(vars(contains("Crude.Rate")),
            .funs = as.character)%>% 
  mutate_at(vars(contains("Crude.Rate")),
            .funs = ~(str_replace_all(., pattern = " (Unreliable)", replacement = ""))) %>% 
  mutate_at(vars(contains("Crude.Rate")),
            .funs = as.numeric)

#check coding
table(str_detect(merge.mort$Age.Adjusted.Rate_1999a, pattern = "Unreliable"))
table(str_detect(merge.mort$Crude.Rate_1999a, pattern = "Unreliable"))

#rough plots
plot(merge.mort$Age.Adjusted.Rate_2010c, merge.mort$imu10)
plot(merge.mort$Age.Adjusted.Rate_2010h, merge.mort$imu10)
plot(merge.mort$Age.Adjusted.Rate_2010a, merge.mort$imu10)

#crude mort
plot(merge.mort$Crude.Rate_2010c, merge.mort$imu10)
plot(merge.mort$Crude.Rate_2010h, merge.mort$imu10)
plot(merge.mort$Crude.Rate_2010a, merge.mort$imu10)

#write this to csv
write_csv(merge.mort, path = "data\\merged_mort.csv")


# ###################################################################
# rm(can1)
# rm(can4)
# rm(can3)
# rm(can2)
# rm(heart.pivot)
# rm(can.pivot)
# rm(heart1)
# rm(heart2)
# rm(heart3)
# rm(heart4)
# rm(mort)
# rm(mort.pivot)
# rm(mort1)
# rm(mort2)
# rm(mort3)
