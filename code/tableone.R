#make a table one to show weighted scoring criteria
pcp_score <- c(0.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65,
               .7, .75, .8, .85, .9, .95, 1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.26)


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

pcp_weights <- sapply(X = pcp_score, FUN = weight_pcp, simplify = "vector")
pcp_score[26] <- ">1.25"

inf_score <- c(8:37, 39, 41, 43, 45)

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

inf_weights <- recodeInf(inf_score)

old_score <- c(7:31)
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

old_weights <- oldRecode(old_score)
old_score[25] <- ">30"
?seq
pov_score <- c(.1, 1, seq(4, 50, 2))
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

pov_weights <- povRecode(pov_score)

tab1 <- cbind(inf_score, inf_weights, c(old_score, rep("--",9)), 
              c(old_weights, rep("--",9)),
              c(pcp_score, rep("--",8)), c(pcp_weights, rep("--",8)),
              c(pov_score, rep("--",8)), c(pov_weights, rep("--",8)))
tab1[26,7] <- "50 +"
tab1 <- as.data.frame(tab1)
names(tab1) <- c("Infant Mortality Rate", "Infant Mortality Score",
                 "Percent Population Over 65", "Over 65 Score",
                 "Primary Care to Population Ratio", "Primary Care Ratio Score",
                 "Percent Population in Poverty", "Poverty Score")

tab1[,c(1,3,5,7)] <- sapply(tab1[,c(1,3,5,7)], function(x) paste0("\u2264", x), USE.NAMES=FALSE)

tab1[,c(1,3,5,7)] <- sapply(tab1[,c(1,3,5,7)], function(x) ifelse(str_detect(x, "--"), "--",x))
tab1[25,3] <- ">30"
tab1[26,5] <- ">1.25"
tab1[26,7] <- ">48"
tab1[34,1] <- ">43"

tab1$`Infant Mortality Score` <- ifelse(nchar(as.character(tab1$`Infant Mortality Score`))<3,
                                        paste0(as.character(tab1$`Infant Mortality Score`), ".0"),
                                        as.character(tab1$`Infant Mortality Score`))

tab1$`Over 65 Score` <- ifelse(nchar(as.character(tab1$`Over 65 Score`))<3 &
                                 !str_detect(as.character(tab1$`Over 65 Score`), "--"),
                                        paste0(as.character(tab1$`Over 65 Score`), ".0"),
                                        as.character(tab1$`Over 65 Score`))

tab1$`Primary Care Ratio Score` <- ifelse(nchar(as.character(tab1$`Primary Care Ratio Score`))<3 &
                                 !str_detect(as.character(tab1$`Primary Care Ratio Score`), "--"),
                               paste0(as.character(tab1$`Primary Care Ratio Score`), ".0"),
                               as.character(tab1$`Primary Care Ratio Score`))

tab1$`Poverty Score` <- ifelse(nchar(as.character(tab1$`Poverty Score`))<3 &
                                 !str_detect(as.character(tab1$`Poverty Score`), "--"),
                               paste0(as.character(tab1$`Poverty Score`), ".0"),
                               as.character(tab1$`Poverty Score`))




names(tab1) <- c(rep(c("Value", "Score"),4))
library(kableExtra)
tab1 %>% kableExtra::kable("latex",booktabs = T) %>%
  # here you can add the vertical line, in my example, for all the columns
  kableExtra::column_spec (1:7,border_left = T, border_right = T) %>%
  kableExtra::kable_styling("striped") %>%
  add_header_above(c("Infant Mortality" = 2, "Percent  Over 65" = 2,
                     "Primary Care Ratio" = 2, "Percent  in Poverty" = 2))%>%
  footnote(general = "Infant Mortality from CDC WONDER, Percent Over 65 from ACS\nPrimary Care Ratio from AHRF, Percent in Poverty from ACS",
           number = c("Infant Mortality Rate = Infant Deaths per 1000 Live Births",
                      "PCP Ratio = Total FTE Non-federal Primary Care Providers per 1000 Population",
                      "Poverty = At or Below Federal Poverty Level"))
  

