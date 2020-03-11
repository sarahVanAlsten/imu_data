#################################
#Sarah Van Alsten
#Created: Feb 22, 2020
#Purpose: Join AHRF and IMU data
#Packages: tidyverse
################################
library(tidyverse)

#read in data
#ahrf.imu <- read_csv( "data\\ahrf_imu.csv")
ahrf.imu <- read_csv( "data\\ahrf_imu23.csv")

#read in data + clean names
imu <- readxl::read_xlsx(path = "data\\county_IMU_scores_from_hrsa.xlsx")
imu <- janitor::clean_names(imu)

#calculate an IMU score for each county, at least in years 10, 15, and 17
# ahrf.imu <- ahrf.imu %>%
#   rowwise() %>%
#   mutate(imu17 = pcp17weight + pop65perc17 + infmort1317 + povperc17,
#          imu16 = pcp16weight + pop65perc16 + infmort1216 + povperc16,
#          imu15 = pcp15weight + pop65perc15 + infmort1115 + povperc15,
#          imu14 = pcp14weight + pop65perc14 + infmort1014 + povperc14,
#          imu13 = pcp13weight + pop65perc13 + infmort0913 + povperc13,
#          imu12 = pcp12weight + pop65perc12 + infmort0812 + povperc12,
#          imu11 = pcp11weight + pop65perc11 + infmort0711 + povperc11,
#          imu10 = pcp10weight + pop65perc10 + infmort0610 + povperc10)

ahrf.imu <- ahrf.imu %>%
  rowwise() %>%
  mutate(imu17 = pcp17weight + pop65perc17 + infmort1317 + povperc17,
         imu16 = pcp16weight + pop65perc16 + infmort1216 + povperc16,
         imu15 = pcp15weight + pop65perc15 + infmort1115 + povperc15,
         imu14 = pcp14weight + pop65perc14 + infmort1014 + povperc14,
         imu13 = pcp13weight + pop65perc13 + infmort0913 + povperc13,
         imu12 = pcp12weight + pop65perc12 + infmort0610 + povperc12,
         imu11 = pcp11weight + pop65perc11 + infmort0509 + povperc11,
         imu10 = pcp10weight + pop65perc10 + infmort0408 + povperc10)


#how many NAs
ahrf.imu %>%
  summarise_at(vars(contains("infmort")), .funs = ~(sum(is.na(.)))) %>%
  colSums()

#plot to check
plot(ahrf.imu$imu17)
plot(ahrf.imu$imu16)
plot(ahrf.imu$imu15)
plot(ahrf.imu$imu14)
plot(ahrf.imu$imu13)
plot(ahrf.imu$imu12)
plot(ahrf.imu$imu11)
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
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Hempsted", "Hempstead")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Delores", "Dolores")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Dekalb", "DeKalb")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Brandford", "Bradford")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Mcduffie", "McDuffie")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Meriweather", "Meriwether")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Broown", "Brown")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "West Cabell", "Cabell")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Mcintosh", "McIntosh")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Knox  County", "Knox County")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "CHAVES", "Chaves")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Southern Crook County", "Crook County")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "MUP", "")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = " MUA", "")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Mccone", "McCone")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Shenandoah County-MUA", "Shenandoah County")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Yellow Medicine County/Taunton", "Yellow Medicine County")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Greene County SA", "Greene County")
is.county$countyname2 <- str_replace_all(is.county$countyname2, pattern = "Huron County (Governor's)", "Huron County")


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
notin <- is.county[!is.county$countyname2 %in% ahrf.imu$countyname2,]
notin.area <- notin[notin$designation_type == "Medically Underserved Area",]

#make a name that includes county and state
is.county.in$name3 <- paste(is.county.in$countyname2, is.county.in$state)
ahrf.imu$name3 <- paste(ahrf.imu$countyname2, ahrf.imu$stateabb)


#check coding
table(is.county.in$name3 %in% ahrf.imu$name3)
plot(is.county$index_of_medical_underservice_score)


#now create one merged data set with the counties in ahrf
merged <- ahrf.imu %>% 
  left_join(is.county.in, by = "name3")

merged$new_imu17 <- ifelse(is.na(merged$index_of_medical_underservice_score),
                           merged$imu17,
                           merged$index_of_medical_underservice_score)

table(is.na(merged$imu10))
table(is.na(merged$imu11))
table(is.na(merged$imu12))
table(is.na(merged$imu13))
table(is.na(merged$imu14))
table(is.na(merged$imu15))
table(is.na(merged$imu16))
table(is.na(merged$imu17))
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

merged$name3 <- ifelse(grepl("\\(CA\\)",x= merged$name3),
                      gsub("\\(CA\\) County","Census Area", merged$name3), 
                      merged$name3)

merged$name3 <- ifelse(grepl("\\(B\\)",x= merged$name3),
                       gsub("\\(B\\) County","Borough", merged$name3), 
                       merged$name3)

merged$name3 <- str_replace_all(merged$name3, pattern = "(B) County", "Borough")
merged$name3 <- str_replace(string = merged$name3, pattern = "(CA) County", replacement = "Census Area")

#table(merged$name3 %in% all.mort$County)

#now merge this in with the other data
merge.mort <- merged %>%
  left_join(all.mort, by = c("name3" = "County"))

# rates <- merge.mort %>%
#   select_at(vars(contains("Rate")))
# 
# rates <- rates %>%
#   mutate_all(.funs = as.numeric)
# 
# rates$countyname <- merge.mort$countyname
# write_csv(rates, "data\\rates.csv")

#replace all the unreliable texts
merge.mort <- merge.mort %>%
  mutate_at(vars(contains("Age.Adjusted.Rate")),
            .funs = as.character)%>% 
  mutate_at(vars(contains("Age.Adjusted.Rate")),
            .funs = ~(str_replace_all(., pattern = "\\(Unreliable\\)", replacement = ""))) %>% 
  mutate_at(vars(contains("Age.Adjusted.Rate")),
          .funs = as.numeric)

#do same in crude rates
merge.mort2 <- merge.mort %>%
  mutate_at(vars(contains("Crude.Rate")),
            .funs = as.character)%>% 
  mutate_at(vars(contains("Crude.Rate")),
            .funs = ~(str_replace_all(., pattern = "\\(Unreliable\\)", replacement = ""))) %>% 
  mutate_at(vars(contains("Crude.Rate")),
            .funs = as.numeric)

#check coding
table(str_detect(merge.mort$Age.Adjusted.Rate_1999a, pattern = "Unreliable"))
table(str_detect(merge.mort2$Crude.Rate_2010a, pattern = "Unreliable"))

#rough plots
plot(merge.mort$Age.Adjusted.Rate_2010c, merge.mort$imu10)
plot(merge.mort$Age.Adjusted.Rate_2010h, merge.mort$imu10)
plot(merge.mort$Age.Adjusted.Rate_2010a, merge.mort$imu10)

#crude mort
plot(merge.mort2$Crude.Rate_2010c, merge.mort2$imu10)
plot(merge.mort2$Crude.Rate_2010h, merge.mort2$imu10)
plot(merge.mort2$Crude.Rate_2010a, merge.mort2$imu10)

#write this to csv
write_csv(merge.mort, path = "data\\merged_mort.csv")
write_csv(merge.mort2, path = "data\\merged_mort2_0310.csv")

str_detect(merge.mort$Crude.Rate_2010a, pattern = "Unreli")
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


