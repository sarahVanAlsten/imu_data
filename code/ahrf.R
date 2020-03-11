#########################################
#Sarah Van Alsten
#FEBRUARY 9, 2019
#Read in the Area health resource file
#and use it to code IMU scores for US
#counties, which may be used for RD
#in mortality rates

#Packages: tidyverse, sas7bdat, readxl
########################################
library(tidyverse)
library(sas7bdat)

#AHRF

ahrf <- read.sas7bdat(file = "C:\\Users\\Owner\\Desktop\\AHRF_2018-2019_SAS\\ahrf2019.sas7bdat")

#read in names from master file
ahrf.colnames <- readxl::read_xlsx("heartdata\\ahrfnames.xlsx")

ahrf.colnames <- ahrf.colnames %>%
  filter(!is.na(COLCOL) & !(COLCOL == "COLCOL"))

#nhsc sites
names(ahrf) <- ahrf.colnames$COLCOL
names(ahrf) <- paste0("f", names(ahrf))


ahrf <- ahrf %>%
  rename(statefips = f12200123,
         countyfips = f12400126,
         stateabb = f6500066,
         ruralcode = f34400345,
         persistentpov = f35800358,
         active_md_nf17 = f75100755,
         active_md_nf16 = f75600760,
         active_md_nf15 = f76100765,
         active_md_nf14 = f76600770,
         active_md_nf13 = f77100775,
         active_md_nf12 = f77600780,
         active_md_nf11 = f78100785,
         active_md_nf10 = f78600790,
         active_md_nf05 = f79100795,
         active_md_nf00 = f79600800,
         active_md_nf90 = f80100805,
         active_md_nf80 = f80600810,
         active_md_nf70 = f81100815,
         active_md_nf60 = f81600820,
         psych_pc17 = f456404567,
         psych_pc15 = f456804571,
         psych_pc10 = f457204576,
         tothosp17 = f1145611458,
         tothosp15 = f1145911461,
         tothosp10 = f1146211464,
         prev_stays16 = f1180511809,
         prev_stays15 = f1181011814,
         prev_stays14 = f1181511819,
         prev_stays13 = f1182011824,
         prev_stays12 = f1182511829,
         rurclin18 = f1361713619,
         rurclin17 = f1396413966,
         rurclin16 = f1396713969,
         rurclin15 = f1397013972,
         rurclin14 = f1397313975,
         rurclin13 = f1397613980,
         rurclin12 = f1398113985,
         rurclin11 = f1398613990,
         rurclin10 = f1399113995,
        nhsc_sites19 = f1415714159,
         nhsc_sites18 = f1416014162,
         nhsc_sites17 = f1416314165,
         nhsc_sites16 = f1416614168,
         nhsc_sites15 = f1416914171,
        nhsc_wpc19 = f1419314195,
        nhsc_wpc18 = f1419614198,
        nhsc_wpc17 = f1419914201,
        nhsc_wpc16 = f1420214204,
        nhsc_wpc15 = f1420514207,
        nhsc_wpc14 = f1420814210,
        nhsc_wpc13 = f1421114213,
        nhsc_wmh19 = f1423514237,
        nhsc_wmh18 = f1423814240,
        nhsc_wmh17 = f1424114243,
        nhsc_wmh16 = f1424414246,
        nhsc_wmh15 = f1424714249,
        nhsc_wmh14 = f1425014252,
        nhsc_wmh13 = f1425314255,
        nhsc_pcp19 = f1429114295,
        nhsc_pcp18 = f1429614300,
        nhsc_pcp17 = f1430114305,
        nhsc_pcp16 = f1430614310,
        nhsc_pcp15 = f1431114315,
        nhsc_pcp14 = f1431614320,
        nhsc_pcp13 = f1432114325,
        nhsc_mhp19 = f1436114365,
        nhsc_mhp18 = f1436614370,
        nhsc_mhp17 = f1437114375,
        nhsc_mhp16 = f1437614380,
        nhsc_mhp15 = f1438114385,
        nhsc_mhp14 = f1438614390,
        nhsc_mhp13 = f1439114395,
        popest18 = f1641716424,
        popest17 = f1642516432,
        popest16 = f1643316440,
        popest15 = f1644116448,
        popest14 = f1644916456,
        popest13 = f1645716464,
        popest12 = f1646516472,
        popest11 = f1647316480,
        popest10 = f1648116488,
        popest09 = f1648916496,
        popest08 = f1649716504,
        popest07 = f1650516512,
        popest06 = f1651316520,
        popest05 = f1652116528,
        popest00 = f1652916536,
        pop6517 = f1869018696, 
        pop6516 = f1869718703, 
        pop6515 = f1870418710, 
        pop6514 = f1871118717, 
        pop6513 = f1871818724, 
        pop6512 = f1872518731, 
        pop6511 = f1873218738, 
        pop6510 = f1873918745,
        infmort1317 = f2185121856, 
         infmort1216 = f2185721862,
         infmort1115 = f2186321868,
         infmort1014 = f2186921874,
         infmort0913 = f2187521880,
         infmort0812 = f2188121886,
         infmort0711 = f2188721892,
         infmort0610 = f2189321898,
         infmort0509 = f2189921904,
         infmort0408 = f2190521910,
         infmort0307 = f2191121916,
         infmort0206 = f2191721922,
         infmort0105 = f2192321928,
         infmort9600 = f2192921934,
         sui1517 = f2244922453,
         sui1416 = f2245422458,
         sui1315 = f2245922463,
         sui0810 = f2246422468,
         povperc17 = f2407824081,
         povperc16 = f2408224085,
         povperc15 = f2408624089,
         povperc14 = f2409024093,
         povperc13 = f2409424097,
         povperc12 = f2409824101,
         povperc11 = f2410224105,
         povperc10 = f2410624109)


ahrf <- ahrf %>%
  rename(hpsa_pc19 = f47000470,
         hpsa_pc18 = f47100471,
         hpsa_pc17 = f47200472,
         hpsa_pc16 = f47300473,
         hpsa_pc15 = f47400474,
         hpsa_pc10 = f47500475,
         hpsa_mh19 = f48200482,
         hpsa_mh18 = f48300483,
         hpsa_mh17 = f48400484,
         hpsa_mh16 = f48500485,
         hpsa_mh15 = f48600486,
         hpsa_mh10 = f48700487,
         percwhite10 = f1884118844,
         medhhinc17 = f2319523200,
         medhhinc16 = f2320123206,
         medhhinc15 = f2320723212,
         medhhinc14 = f2321323218,
         medhhinc13 = f2321923224,
         medhhinc12 = f2322523230,
         medhhinc11 = f2323123236,
         medhhinc10 = f2323723242,
         percunins17 = f2520025203, 
         percunins16 = f2520425207, 
         percunins15 = f2520825211, 
         percunins14 = f2521225215, 
         percunins13 = f2521625219, 
         percunins12 = f2522025223, 
         percunins11 = f2522425227, 
         percunins10 = f2522825231,
         perclths1317 = f2935329356,
         perclths1115 = f2935729360,
         unemprate18 = f3087830880,
         unemprate17 = f3088130883,
         unemprate16 = f3088430886,
         unemprate15 = f3088730889,
         unemprate14 = f3089030892,
         unemprate13 = f3089330895,
         unemprate12 = f3089630898,
         unemprate11 = f3089930901,
         unemprate10 = f3090230904)

#need to get FTE for all PCPs in the county
#this comes as sum of OBGYN (1.9), Fam Prac (1.4), Int Med (1.8), Pediatrics (1.4), and Other PC (1.6)
ahrf <- ahrf %>%
  rename(genprac17 = f135101353, 
         genprac15 = f135401356, 
         genprac10 = f137701379,
         dfammed17 = f142101424, 
         dfammed15 = f142501428, 
         dfammed10 = f142901432,
         intmed17 = f202902032, 
         intmed15 = f2023220236, 
         intmed10 = f203702041, 
         ped17 = f232302325, 
         ped15 = f232602328, 
         ped10 = f223502239,
         obgyn17 = f287702880, 
         obgyn15 = f288102884, 
         obgyn10 = f288502889)

ahrf <- ahrf %>% rename(
  countyname = f6700091)

#non fed primary care not including hosp residents and over 75 yrs
ahrf <- ahrf %>%
  rename(
    primcare17 = f55800561,
    primcare16 = f56200565,
    primcare15 = f56600569,
    primcare14 = f57000573,
    primcare13 = f57400577,
    primcare12 = f57800581,
    primcare11 = f58200585,
    primcare10 = f58600589)


#select the columns I just renamed to make this go faster
#names that don't start with F
ahrf.sub <- ahrf[ , ! (substr(names(ahrf), 1, 1) == "f")]

# ahrf.sub<- ahrf.sub %>%
#   rename(obgyn10 = obgyn13,
#          ped10 = ped13,
#          intmed10 = intmed13,
#          dfammed10 = dfammed13)

#create an index of medical underservice for each county

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
ahrf.imu <- ahrf.sub %>%
  mutate_at(vars(contains("infmort")), .funs = ~(recodeInf(.)))

#poverty recode for IMU
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
ahrf.imu <- ahrf.imu %>%
  mutate_at(vars(contains("povperc")), .funs = ~(povRecode(.)))

#make pop over 65 percent vars
ahrf.imu <- ahrf.imu %>%
  mutate(pop65perc10 = pop6510/popest10,
         pop65perc11 = pop6511/popest11,
         pop65perc12 = pop6512/popest12,
         pop65perc13 = pop6513/popest13,
         pop65perc14 = pop6514/popest14,
         pop65perc15 = pop6515/popest15,
         pop65perc16 = pop6516/popest16,
         pop65perc17 = pop6517/popest17)

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

ahrf.imu <- ahrf.imu %>%
  #multiply by 100 to get %
  mutate_at(vars(contains("pop65perc")), .funs = ~(100*(.))) %>%   
  mutate_at(vars(contains("pop65perc")), .funs = ~(oldRecode(.)))


#now have to ratio of PCP to pop
#this comes as sum of OBGYN (1.9), Fam Prac (1.4), Int Med (1.8), Pediatrics (1.4), and Other PC (1.6)
#first multiply to get FTE for each speciatly
# ahrf.imu <- ahrf.imu %>%
#   mutate_at(vars(contains("obgyn")), .funs = ~(1.9*(.))) %>%
#   mutate_at(vars(contains("dfam")), .funs = ~(1.4*(.))) %>%  
#   mutate_at(vars(contains("intmed")), .funs = ~(1.8*(.))) %>%
#   mutate_at(vars(contains("ped")), .funs = ~(1.9*(.))) %>%
#   mutate_at(vars(contains("genp")), .funs = ~(1.6*(.)))

#add up to get total PCPs per county
ahrf.imu <- ahrf.imu %>%
  rowwise() %>% #don't sum full columns!
  mutate(pcp17 = sum(obgyn17, dfammed17, intmed17, ped17, na.rm = T),
         pcp15 = sum(obgyn15, dfammed15, intmed15, ped15, na.rm =T),
         pcp10 = sum(obgyn10, dfammed10, intmed10, ped10, na.rm = T))


#to get pcp ratio, divide by totpop/1000
ahrf.imu <- ahrf.imu %>%
  mutate(#pcpratio17 = pcp17/(popest17/1000),
         #pcpratio15 = pcp15/(popest15/1000),
         #pcpratio10 = pcp10/(popest10/1000),
         pcpratio17 = primcare17/(popest17/1000),
         pcpratio16 = primcare16/(popest16/1000),
         pcpratio15 = primcare15/(popest15/1000),
         pcpratio14 = primcare14/(popest14/1000),
         pcpratio13 = primcare13/(popest13/1000),
         pcpratio12 = primcare12/(popest12/1000),
         pcpratio11 = primcare11/(popest11/1000),
         pcpratio10 = primcare10/(popest10/1000)
         )

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
ahrf.imu$pcp11weight <- 999
ahrf.imu$pcp12weight <- 999
ahrf.imu$pcp13weight <- 999
ahrf.imu$pcp14weight <- 999
ahrf.imu$pcp15weight <- 999
ahrf.imu$pcp16weight <- 999
ahrf.imu$pcp17weight <- 999
ahrf.imu$pcp10weight <- 999

#get pcp imu weights
for (i in 1:nrow(ahrf.imu)) {
  ahrf.imu$pcp10weight[i] <- weight_pcp(ahrf.imu$pcpratio10[i])
  ahrf.imu$pcp11weight[i] <- weight_pcp(ahrf.imu$pcpratio11[i])
  ahrf.imu$pcp12weight[i] <- weight_pcp(ahrf.imu$pcpratio12[i])
  ahrf.imu$pcp13weight[i] <- weight_pcp(ahrf.imu$pcpratio13[i])
  ahrf.imu$pcp14weight[i] <- weight_pcp(ahrf.imu$pcpratio14[i])
  ahrf.imu$pcp15weight[i] <- weight_pcp(ahrf.imu$pcpratio15[i])
  ahrf.imu$pcp16weight[i] <- weight_pcp(ahrf.imu$pcpratio16[i])
  ahrf.imu$pcp17weight[i] <- weight_pcp(ahrf.imu$pcpratio17[i])
}
#############################################

#write this file out
write_csv(ahrf.imu, "data\\ahrf_imu.csv")
write_csv(ahrf, "data\\ahrf_full.csv")
write_csv(ahrf.imu, "data\\ahrf_imu23.csv")
