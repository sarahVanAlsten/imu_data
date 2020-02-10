#Sarah Van Alsten
# February 8, 2020
#Connect HPSA data to NHSC
library(tidyverse)
library(httr)
library(sas7bdat)

#read data from HRSA directly into R
#physician training programs
httr::GET("https://data.hrsa.gov//DataDownload/DD_Files/BPMH_HOME_DASHBOARD_SUMMARY_INFORMATION.xlsx",
          write_disk(tf <- tempfile(fileext = ".xlsx")))
bpmh.summ <- readxl::read_xlsx(tf)

httr::GET("https://data.hrsa.gov//DataDownload/DD_Files/BPMH_SITES_INFORMATION.xlsx",
          write_disk(tf <- tempfile(fileext = ".xlsx")))
bpmh.sites <- readxl::read_xlsx(tf)

httr::GET("https://data.hrsa.gov//DataDownload/DD_Files/BPMH_PARTICIPANT_SUMMARY_INFORMATION.xlsx",
          write_disk(tf <- tempfile(fileext = ".xlsx")))
bpmh.part <- readxl::read_xlsx(tf)

#nhsc job opportunities + sites
httr::GET("https://data.hrsa.gov//DataDownload/DD_Files/HPOL_9.xlsx",
          write_disk(tf <- tempfile(fileext = ".xlsx")))
nhsc.job <- readxl::read_xlsx(tf)

httr::GET("https://data.hrsa.gov//DataDownload/DD_Files/NHSC%20Site_10.xlsx",
          write_disk(tf <- tempfile(fileext = ".xlsx")))
nhsc.site <- readxl::read_xlsx(tf)

#primary care HPSAs
httr::GET('https://data.hrsa.gov/DataDownload/DD_Files/BCD_HPSA_FCT_DET_PC.xlsx',
          write_disk(tf <- tempfile(fileext = ".xlsx")))
hpsa.pc <- readxl::read_xlsx(tf)

#mental health HPSAs
httr::GET('https://data.hrsa.gov/DataDownload/DD_Files/BCD_HPSA_FCT_DET_MH.xlsx',
          write_disk(tf <- tempfile(fileext = ".xlsx")))
hpsa.mh <- readxl::read_xlsx(tf)

#rename columns to get rid of annoying spaces!
mylist <-
  lapply(list(hpsa.mh, hpsa.pc, nhsc.job, nhsc.site, bpmh.part, bpmh.sites, bpmh.summ), 
       FUN = janitor::clean_names)

hpsa.mh <- as.data.frame(mylist[1])
hpsa.pc <- as.data.frame(mylist[2])
nhsc.job <- as.data.frame(mylist[3])
nhsc.site <- as.data.frame(mylist[4])
bpmh.sites <- as.data.frame(mylist[6])
bpmh.part <- as.data.frame(mylist[5])
bpmh.summ <- as.data.frame(mylist[7])

#remove unnecessary extra stuff
rm(mylist)
rm(temp)
rm(tf)


#glance at names
names(hpsa.mh)
names(nhsc.job)
names(nhsc.site)

#how many rural jobs available
table(nhsc.job$rural_status)
table(nhsc.site$rural_status)

#vacancies at the sites/advertised jobs
table(nhsc.job$vacancy_category)
table(nhsc.job$vacancy_discipline)
table(nhsc.job$vacancy_type)

#if any vacancies exist at the designated site
nhsc.site <- nhsc.site %>%
  mutate(any_vacancy = ifelse(vacancies > 0, 1, 0))

#separate out all associated HPSAs into separate columns
nhsc <- nhsc.site %>% 
  separate(col = hps_as_associated_with_site, into = c(paste0("hpsa", 1:6)))

#now, separate out so each HPSA is its own row
nhsc <- nhsc %>%
  pivot_longer(cols = c(paste0("hpsa", 1:6)),
               names_to = "hpsa_num",
               values_to = "hpsa_code")

#drop the unused rows
nhsc <- nhsc %>%
  filter(!is.na(hpsa_code))

#merge it with the health professional shortage areas
merged <- nhsc %>% full_join(hpsa.pc, by = c("hpsa_code" = "hpsa_source_identification_number"))
mergedMH <- nhsc %>% full_join(hpsa.mh, by = c("hpsa_code" = "hpsa_source_identification_number"))

#only keep rural ones
rural <- merged %>% filter(rural_status == "Yes")

#pending ones
pending <- merged %>% filter(hpsa_status_code == "P")

#withdrawn ones
withdrawn <- merged %>% filter(hpsa_status_code == "W")

#how does # vacancies correspond with need
table(rural$vacancies, rural$primary_care_hpsa_score)
table(rural$vacancies, rural$mental_health_hpsa_score)

#check data types
class(rural$hpsa_designation_date)
class(withdrawn$hpsa_degree_of_shortage)
class(rural$hpsa_degree_of_shortage)
class(rural$hpsa_score)

#make numeric
withdrawn$hpsa_degree_of_shortage <- as.numeric(withdrawn$hpsa_degree_of_shortage)
pending$hpsa_degree_of_shortage <- as.numeric(pending$hpsa_degree_of_shortage)
rural$hpsa_degree_of_shortage <- as.numeric(rural$hpsa_degree_of_shortage)

table(withdrawn$hpsa_degree_of_shortage)
table(pending$hpsa_degree_of_shortage)
table(rural$hpsa_degree_of_shortage)

#keep primary care vs MH separate
rural.pc <- rural %>% filter(!is.na(primary_care_hpsa_score))
rural.mh <- rural %>% filter(!is.na(mental_health_hpsa_score))
##############################################################################
#read in county health data

imu <- readxl::read_xlsx(path = "data\\county_IMU_scores_from_hrsa.xlsx")
imu <- janitor::clean_names(imu)

countyhealth <- read_csv(file = "data\\countyhealth.csv")
table(countyhealth$countycode_10 %in% rural.pc$county_or_county_equivalent_federal_information_processing_standard_code)

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
###################################################################
rm(can1)
rm(can4)
rm(can3)
rm(can2)
rm(heart.pivot)
rm(can.pivot)
rm(heart1)
rm(heart2)
rm(heart3)
rm(heart4)
rm(mort)
rm(mort.pivot)
rm(mort1)
rm(mort2)
rm(mort3)
