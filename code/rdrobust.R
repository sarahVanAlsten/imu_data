#Write a function that will make a plot of the RDD using various bandwidths
#(including the IK bandwidth)
#also see flexibility of model specification usign iv approach given this is a fuzzy rd

library(tidyverse)
library(rdd)
library(rdrobust)
merge.mortb <- read_csv("data\\final_IMU_analysis_data.csv")
merge.mortb <- janitor::clean_names(merge.mortb)
merge.mortb <- merge.mortb[!(merge.mortb$treatment == 1 & merge.mortb$newimu14 > 62),]

set.seed(48104)
# RD Effect for main outcome variable  ==> Local linear polynomial estimation with optimal bandwidth
ac.a.1 <- rdrobust(merge.mortb$age_adjusted_rate_2015a, 
                merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                c = 62, all=TRUE)
ac.a.2 <- rdrobust(merge.mortb$age_adjusted_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2)
ac.a.3 <- rdrobust(merge.mortb$age_adjusted_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3)
hd.a.1 <- rdrobust(merge.mortb$age_adjusted_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE)
hd.a.2 <- rdrobust(merge.mortb$age_adjusted_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2)
hd.a.3 <- rdrobust(merge.mortb$age_adjusted_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3)
ca.a.1 <- rdrobust(merge.mortb$age_adjusted_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE)
ca.a.2 <- rdrobust(merge.mortb$age_adjusted_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2)
ca.a.3 <- rdrobust(merge.mortb$age_adjusted_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3)

ac.c.1 <- rdrobust(merge.mortb$crude_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE)
ac.c.2 <- rdrobust(merge.mortb$crude_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2)
ac.c.3 <- rdrobust(merge.mortb$crude_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3)
hd.c.1 <- rdrobust(merge.mortb$crude_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE)
hd.c.2 <- rdrobust(merge.mortb$crude_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2)
hd.c.3 <- rdrobust(merge.mortb$crude_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3)
ca.c.1 <- rdrobust(merge.mortb$crude_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE)
ca.c.2 <- rdrobust(merge.mortb$crude_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2)
ca.c.3 <- rdrobust(merge.mortb$crude_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3)


#create a nice table of results
outcome <- c(rep(c(rep("All Cause",3), rep("Heart Disease", 3), rep("Cancer", 3)), 2))

polys <- c(rep(1:3,6))

bandwidths <- c(ac.a.1$bws[1,1], ac.a.2$bws[1,1], ac.a.3$bws[1,1],
                hd.a.1$bws[1,1], hd.a.2$bws[1,1], hd.a.3$bws[1,1],
                ca.a.1$bws[1,1], ca.a.2$bws[1,1], ca.a.3$bws[1,1],
                ac.c.1$bws[1,1], ac.c.2$bws[1,1], ac.c.3$bws[1,1],
                hd.c.1$bws[1,1], hd.c.2$bws[1,1], hd.c.3$bws[1,1],
                ca.c.1$bws[1,1], ca.c.2$bws[1,1], ca.c.3$bws[1,1])

tau.rdd <- c(ac.a.1$Estimate[2], ac.a.2$Estimate[2], ac.a.3$Estimate[2],
             hd.a.1$Estimate[2], hd.a.2$Estimate[2], hd.a.3$Estimate[2],
             ca.a.1$Estimate[2], ca.a.2$Estimate[2], ca.a.3$Estimate[2],
             ac.c.1$Estimate[2], ac.c.2$Estimate[2], ac.c.3$Estimate[2],
             hd.c.1$Estimate[2], hd.c.2$Estimate[2], hd.c.3$Estimate[2],
             ca.c.1$Estimate[2], ca.c.2$Estimate[2], ca.c.3$Estimate[2])

ci.conv <- c(paste0(round(ac.a.1$Estimate[2]- 1.96*ac.a.1$se[1],2), " - ", round(ac.a.1$Estimate[2]+ 1.96*ac.a.1$se[1],2)),
             paste0(round(ac.a.2$Estimate[2]- 1.96*ac.a.2$se[1],2), " - ", round(ac.a.2$Estimate[2]+ 1.96*ac.a.2$se[1],2)),
             paste0(round(ac.a.3$Estimate[2]- 1.96*ac.a.1$se[1],2), " - ", round(ac.a.3$Estimate[2]+ 1.96*ac.a.1$se[1],2)),
             paste0(round(hd.a.1$Estimate[2]- 1.96*hd.a.1$se[1],2), " - ", round(hd.a.1$Estimate[2]+ 1.96*hd.a.1$se[1],2)),
             paste0(round(hd.a.2$Estimate[2]- 1.96*hd.a.2$se[1],2), " - ", round(hd.a.2$Estimate[2]+ 1.96*hd.a.2$se[1],2)),
             paste0(round(hd.a.3$Estimate[2]- 1.96*hd.a.1$se[1],2), " - ", round(hd.a.3$Estimate[2]+ 1.96*hd.a.1$se[1],2)),
             paste0(round(ca.a.1$Estimate[2]- 1.96*ca.a.1$se[1],2), " - ", round(ca.a.1$Estimate[2]+ 1.96*ca.a.1$se[1],2)),
             paste0(round(ca.a.2$Estimate[2]- 1.96*ca.a.2$se[1],2), " - ", round(ca.a.2$Estimate[2]+ 1.96*ca.a.2$se[1],2)),
             paste0(round(ca.a.3$Estimate[2]- 1.96*ca.a.1$se[1],2), " - ", round(ca.a.3$Estimate[2]+ 1.96*ca.a.1$se[1],2)),
             paste0(round(ac.c.1$Estimate[2]- 1.96*ac.c.1$se[1],2), " - ", round(ac.c.1$Estimate[2]+ 1.96*ac.c.1$se[1],2)),
             paste0(round(ac.c.2$Estimate[2]- 1.96*ac.c.2$se[1],2), " - ", round(ac.c.2$Estimate[2]+ 1.96*ac.c.2$se[1],2)),
             paste0(round(ac.c.3$Estimate[2]- 1.96*ac.c.1$se[1],2), " - ", round(ac.c.3$Estimate[2]+ 1.96*ac.c.1$se[1],2)),
             paste0(round(hd.c.1$Estimate[2]- 1.96*hd.c.1$se[1],2), " - ", round(hd.c.1$Estimate[2]+ 1.96*hd.c.1$se[1],2)),
             paste0(round(hd.c.2$Estimate[2]- 1.96*hd.c.2$se[1],2), " - ", round(hd.c.2$Estimate[2]+ 1.96*hd.c.2$se[1],2)),
             paste0(round(hd.c.3$Estimate[2]- 1.96*hd.c.1$se[1],2), " - ", round(hd.c.3$Estimate[2]+ 1.96*hd.c.1$se[1],2)),
             paste0(round(ca.c.1$Estimate[2]- 1.96*ca.c.1$se[1],2), " - ", round(ca.c.1$Estimate[2]+ 1.96*ca.c.1$se[1],2)),
             paste0(round(ca.c.2$Estimate[2]- 1.96*ca.c.2$se[1],2), " - ", round(ca.c.2$Estimate[2]+ 1.96*ca.c.2$se[1],2)),
             paste0(round(ca.c.3$Estimate[2]- 1.96*ca.c.1$se[1],2), " - ", round(ca.c.3$Estimate[2]+ 1.96*ca.c.1$se[1],2)))


ci.robu <- c(paste0(round(ac.a.1$Estimate[2]- 1.96*ac.a.1$se[3],2), " - ", round(ac.a.1$Estimate[2]+ 1.96*ac.a.1$se[3],2)),
             paste0(round(ac.a.2$Estimate[2]- 1.96*ac.a.2$se[3],2), " - ", round(ac.a.2$Estimate[2]+ 1.96*ac.a.2$se[3],2)),
             paste0(round(ac.a.3$Estimate[2]- 1.96*ac.a.1$se[3],2), " - ", round(ac.a.3$Estimate[2]+ 1.96*ac.a.1$se[3],2)),
             paste0(round(hd.a.1$Estimate[2]- 1.96*hd.a.1$se[3],2), " - ", round(hd.a.1$Estimate[2]+ 1.96*hd.a.1$se[3],2)),
             paste0(round(hd.a.2$Estimate[2]- 1.96*hd.a.2$se[3],2), " - ", round(hd.a.2$Estimate[2]+ 1.96*hd.a.2$se[3],2)),
             paste0(round(hd.a.3$Estimate[2]- 1.96*hd.a.1$se[3],2), " - ", round(hd.a.3$Estimate[2]+ 1.96*hd.a.1$se[3],2)),
             paste0(round(ca.a.1$Estimate[2]- 1.96*ca.a.1$se[3],2), " - ", round(ca.a.1$Estimate[2]+ 1.96*ca.a.1$se[3],2)),
             paste0(round(ca.a.2$Estimate[2]- 1.96*ca.a.2$se[3],2), " - ", round(ca.a.2$Estimate[2]+ 1.96*ca.a.2$se[3],2)),
             paste0(round(ca.a.3$Estimate[2]- 1.96*ca.a.1$se[3],2), " - ", round(ca.a.3$Estimate[2]+ 1.96*ca.a.1$se[3],2)),
             paste0(round(ac.c.1$Estimate[2]- 1.96*ac.c.1$se[3],2), " - ", round(ac.c.1$Estimate[2]+ 1.96*ac.c.1$se[3],2)),
             paste0(round(ac.c.2$Estimate[2]- 1.96*ac.c.2$se[3],2), " - ", round(ac.c.2$Estimate[2]+ 1.96*ac.c.2$se[3],2)),
             paste0(round(ac.c.3$Estimate[2]- 1.96*ac.c.1$se[3],2), " - ", round(ac.c.3$Estimate[2]+ 1.96*ac.c.1$se[3],2)),
             paste0(round(hd.c.1$Estimate[2]- 1.96*hd.c.1$se[3],2), " - ", round(hd.c.1$Estimate[2]+ 1.96*hd.c.1$se[3],2)),
             paste0(round(hd.c.2$Estimate[2]- 1.96*hd.c.2$se[3],2), " - ", round(hd.c.2$Estimate[2]+ 1.96*hd.c.2$se[3],2)),
             paste0(round(hd.c.3$Estimate[2]- 1.96*hd.c.1$se[3],2), " - ", round(hd.c.3$Estimate[2]+ 1.96*hd.c.1$se[3],2)),
             paste0(round(ca.c.1$Estimate[2]- 1.96*ca.c.1$se[3],2), " - ", round(ca.c.1$Estimate[2]+ 1.96*ca.c.1$se[3],2)),
             paste0(round(ca.c.2$Estimate[2]- 1.96*ca.c.2$se[3],2), " - ", round(ca.c.2$Estimate[2]+ 1.96*ca.c.2$se[3],2)),
             paste0(round(ca.c.3$Estimate[2]- 1.96*ca.c.1$se[3],2), " - ", round(ca.c.3$Estimate[2]+ 1.96*ca.c.1$se[3],2)))

#make plots to pick ones to use
a1<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 1, title = "Age Adjusted All-Cause: Linear",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)
a2<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2, title = "Age Adjusted All-Cause: Quadratic",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)
a3<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3,title = "Age Adjusted All-Cause: Cubic",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)

h1<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 1, title = "Age Adjusted Heart Disease: Linear",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)
h2<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2, title = "Age Adjusted Heart Disease: Quadratic",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)
h3<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3,title = "Age Adjusted Heart Disease: Cubic",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)

c1<- 
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 1, title = "Age Adjusted Cancer: Linear",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)
c2<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2, title = "Age Adjusted Heart Cancer: Quadratic",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)
c3<-
  rdrobust::rdplot(merge.mortb$age_adjusted_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3,title = "Age Adjusted Heart Cancer: Cubic",
                   x.label = "2014 IMU Score", y.label = "Age Adjusted Rate", ci = 95)

a4<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 1, title = "Crude All-Cause: Linear",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)
a5<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2, title = "Crude All-Cause: Quadratic",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)
a6<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015a, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3,title = "Crude All-Cause: Cubic",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)

h4<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 1, title = "Crude Heart Disease: Linear",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)
h5<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2, title = "Crude Heart Disease: Quadratic",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)
h6<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015h, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3,title = "Crude Heart Disease: Cubic",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)
c4<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 1, title = "Crude Cancer: Linear",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)
c5<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 2, title = "Crude Heart Cancer: Quadratic",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)
c6<-
  rdrobust::rdplot(merge.mortb$crude_rate_2015c, 
                   merge.mortb$newimu14, fuzzy = merge.mortb$treatment,
                   c = 62, all=TRUE, p = 3,title = "Crude Heart Cancer: Cubic",
                   x.label = "2014 IMU Score", y.label = "Crude Rate", ci = 95)


c5$vars_bins %>%
  mutate(predicted_y = ifelse(rdplot_mean_bin <= 62,
                              c5$coef[2,1]*-1*rdplot_mean_bin+c5$coef[1,2],
                              c5$coef[2,2]*rdplot_mean_bin +c5$coef[1,1])) %>%
  filter(!is.nan(rdplot_ci_r) & !is.nan(rdplot_mean_y) & !is.na(rdplot_ci_r) &
           rdplot_N != 1) %>%
  ggplot(aes(x = rdplot_mean_bin, y = rdplot_mean_y)) + 
  geom_point() +
  geom_ribbon(aes(ymin = rdplot_ci_l, ymax = rdplot_ci_r), alpha = .1) + theme_bw() +
  geom_vline(xintercept = 62, linetype = "dashed", color = "darkgrey") +
  geom_line(color= ifelse(c5$vars_poly$rdplot_x <= 62, "red", "blue"),
            aes(x=rdplot_x, y= rdplot_y), data = c5$vars_poly) +
  labs(x = "2014 Index of Medical Score", y = "Mortality Rate") +
  ggtitle("Crude Cancer Mortality: Quadratic")
