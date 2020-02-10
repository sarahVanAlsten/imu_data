########################################################
#Author: Sarah Van Alsten
#Date created: January 24, 2020
#Purpose: use the weighted ranking data to calculate the complete IMU scor
#And determine those below cutoff of 62
#Packages used: tidyverse
##################################################

infant <- read.csv("data//infant_imu.csv")
pcp <- read.csv("data//pcp_imu.csv")
pov <- read.csv("data//pov_imu.csv")
elder <- read.csv("data//elder_imu.csv")

#check classes of merge vars
class(infant$fips)
class(elder$fips)
class(pov$fips) #factor
class(pcp$fips)

pov$fips <- as.integer(as.character(pov$fips))

#get rid of county cols in 3 of them so we don't get repeats
pov <- pov %>% select(-county)
infant <- infant %>% select(-county)
pcp <- pcp %>% select(-county)

#merge them all into one
all.imu <- pcp %>%
  full_join(elder, by = "fips") %>%
  full_join(infant, by = "fips") %>%
  full_join(pov, by = "fips")


#rename v053 and v129 to more descriptive things
all.imu <- all.imu %>%
  rename_at(vars(contains("v129")), .funs = ~(str_replace(., "v129", "inf_mort"))) %>%
  rename_at(vars(contains("v053")), .funs = ~(str_replace(., "v053", "elder"))) %>%
  rename_at(vars(contains("rawvalue")), .funs = ~(str_remove_all(., "rawvalue_")))


#now I need to add all 4 diff IMU types by year. 
#would be easiest to pivot longer, where each row is a county/year pair
imu.pivot <- all.imu %>%
  pivot_longer(-c(county, fips), names_to = "measure", values_to = "score") %>%
  #now extract year from the measure name
  mutate(year = substr(measure, nchar(measure)-1, nchar(measure))) %>%
  #remove digits from measure they are no longer needed 
  mutate_at(vars(contains("measure")), .funs = ~(str_remove_all(., pattern = "\\d")))


#now its very long. want to make it a little longer
#so instead of county/measure/year it's just county/year pair
imu.long <- imu.pivot %>%
  filter(!is.na(county)) %>%
  pivot_wider(id_cols = c(county, fips, year, measure),
              names_from = c(year), values_from = c(score))


imu.long <- imu.long %>%
  group_by(fips, county) %>%
  mutate(tot_mua09 = sum(`09`, na.rm =T),
         tot_mua10 = sum(`10`, na.rm =T),
         tot_mua11 = sum(`11`, na.rm =T),
         tot_mua12 = sum(`12`, na.rm =T),
         tot_mua13 = sum(`13`, na.rm =T),
         tot_mua14 = sum(`14`, na.rm =T),
         tot_mua15 = sum(`15`, na.rm =T),
         tot_mua16 = sum(`16`, na.rm =T),
         tot_mua17 = sum(`17`, na.rm =T),
         tot_nas09 = sum(is.na(`09`)),
         tot_nas10 = sum(is.na(`10`)),
         tot_nas11 = sum(is.na(`11`)),
         tot_nas12 = sum(is.na(`12`)),
         tot_nas13 = sum(is.na(`13`)),
         tot_nas14 = sum(is.na(`14`)),
         tot_nas15 = sum(is.na(`15`)),
         tot_nas16 = sum(is.na(`16`)),
         tot_nas17 = sum(is.na(`17`)))
      #year 18 is missing on most of them and is thus not useful to calculate


#don't keep 09 and 18 years, lots missing for those
mini <- imu.long %>%
  ungroup()%>%
  select(-`09`) %>%
  select(-`18`) %>%
  select(-tot_mua09)%>%
  select(- tot_nas09)

#try to keep counties with 0 missing values in later years 15/16/17
mini.comp <- mini %>%
  filter(tot_nas16 == 0)
  
#filter out US
mini.comp <- mini.comp %>%
  filter(county != "United States")

#below the MUA of 62. We'll later have to take a look at how many missing vals
low.mua <- imu.long %>%
  ungroup()%>%
  select(-`09`) %>%
  select(-`18`) %>%
  select(-tot_mua09)%>%
  select(- tot_nas09)
  filter(tot_mua11 < 62)



