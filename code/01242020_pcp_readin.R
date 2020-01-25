########################################################
#Author: Sarah Van Alsten
#Date created: January 24, 2020
#Read in countyhealth files to calculate pcp rates
#Then recode to weighted value for IMU calculation
#Packages used: tidyverse
##################################################
library(tidyverse)
library(httr)
library(janitor)

countyhealth <- read_csv("data//countyhealth.csv")

#other_data_1 is ratio of pop to PCP
pcp <- countyhealth %>%
  select_at(vars(contains("v004_other_data")))

#metric is based on provider:1000 pop; divide cols by 1000
pcp <- pcp %>%
  mutate_all(.funs = ~(as.numeric(.)/1000))

#some are negative, which means there were 0 PCPs. this is fine since
#it should eval to lowest IMU weight = greatest need

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

pcp_imu <- pcp

#rename columns
pcp_imu <- pcp_imu %>%
  rename_all(.funs = ~(str_replace_all(., "v004_other_data_1_", "pcp_rate")))

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate17[i] <- weight_pcp(pcp_imu$pcp_rate17[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate16[i] <- weight_pcp(pcp_imu$pcp_rate16[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate15[i] <- weight_pcp(pcp_imu$pcp_rate15[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate14[i] <- weight_pcp(pcp_imu$pcp_rate14[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate13[i] <- weight_pcp(pcp_imu$pcp_rate13[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate12[i] <- weight_pcp(pcp_imu$pcp_rate12[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate11[i] <- weight_pcp(pcp_imu$pcp_rate11[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate10[i] <- weight_pcp(pcp_imu$pcp_rate10[i])
}

for (i in 1:nrow(pcp_imu)){
  pcp_imu$pcp_rate09[i] <- weight_pcp(pcp_imu$pcp_rate09[i])
}

#reattatch fips and county
pcp_imu$fips <- countyhealth$fipscode_09
pcp_imu$county <- countyhealth$county_11

#also note that these files come out every 2 years. I carry forward the number
#of physicians for a two year period then, rather than annually

#write out the file
write_csv(pcp_imu, "data//pcp_imu.csv")
