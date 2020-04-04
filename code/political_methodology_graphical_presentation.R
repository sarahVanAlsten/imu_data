#Write a function that will make a plot of the RDD using various bandwidths
#(including the IK bandwidth)
#also see flexibility of model specification usign iv approach given this is a fuzzy rd

library(tidyverse)
library(rdd)
merge.mortb <- read_csv("data\\final_IMU_analysis_data.csv")
merge.mortb <- janitor::clean_names(merge.mortb)
merge.mortb <- merge.mortb[!(merge.mortb$treatment == 1 & merge.mortb$newimu14 > 62),]

make_rdd_plot_data <- function(outcome, data = merge.mortb, covars = NULL){
  formula <- paste(outcome, "~ newimu14 + treatment")
  if (!is.null(covars)){
    formula <- paste(formula, "|", paste(covars, collapse = "+"))
  }
  rdd1 <- RDestimate(formula = formula,
                     data = data,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
  ik.bw <- rdd1$bw
  tempframe <- as.data.frame(
    cbind(append(1:23,ik.bw),
                     rep(1,24),
                     rep(1,24),
                     rep(1,24),
                     rep(1,24),
                     rep(1,24))
  )
  names(tempframe) <- c("bw", "estimate", "lower", "upper", "pvalue", "nObs")
  tempframe[24,] <- c(ik.bw[1],
                      rdd1$est[1],
                      (rdd1$est[1]- 1.96*rdd1$se[1]),
                      (rdd1$est[1]+ 1.96*rdd1$se[1]),
                      rdd1$p[1],
                      rdd1$obs[1])
  for (i in 1:23){
    rd <- RDestimate(formula = formula,
                     data = data,
                     subset = NULL, cutpoint = 62, bw = i,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
    tempframe[i,] <- c(i,
                       rd$est[1],
                       (rd$est[1]- 1.96*rd$se[1]),
                       (rd$est[1]+ 1.96*rd$se[1]),
                       rd$p[1],
                       rd$obs[1])
  }

return(tempframe)  
}

make_rdd_plot_data2 <- function(outcome, data = merge.mortb, covars = NULL){
  formula <- paste(outcome, "~ newimu14")
  if (!is.null(covars)){
    formula <- paste(formula, "|", paste(covars, collapse = "+"))
  }
  rdd1 <- RDestimate(formula = formula,
                     data = data,
                     subset = NULL, cutpoint = 62, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
  ik.bw <- rdd1$bw
  tempframe <- as.data.frame(
    cbind(append(1:23,ik.bw),
          rep(1,24),
          rep(1,24),
          rep(1,24),
          rep(1,24),
          rep(1,24))
  )
  names(tempframe) <- c("bw", "estimate", "lower", "upper", "pvalue", "nObs")
  tempframe[24,] <- c(ik.bw[1],
                      rdd1$est[1],
                      (rdd1$est[1]- 1.96*rdd1$se[1]),
                      (rdd1$est[1]+ 1.96*rdd1$se[1]),
                      rdd1$p[1],
                      rdd1$obs[1])
  for (i in 1:23){
    rd <- RDestimate(formula = formula,
                     data = data,
                     subset = NULL, cutpoint = 62, bw = i,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
    tempframe[i,] <- c(i,
                       rd$est[1],
                       (rd$est[1]- 1.96*rd$se[1]),
                       (rd$est[1]+ 1.96*rd$se[1]),
                       rd$p[1],
                       rd$obs[1])
  }
  
  return(tempframe)  
}


all.cause.14 <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014a")
all.cause.14 <- all.cause.14[4:24,]

ac1 <-
all.cause.14 %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 13.26, linetype = "dashed") +
  ggtitle("Age-Standardized")+
  geom_text(aes(label = paste(nObs)), nudge_y =  500, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23)) + theme(panel.grid.minor.x = element_blank())

all.cause.14.c <- make_rdd_plot_data(outcome = "crude_rate_2014a")
all.cause.14.c <- all.cause.14.c[4:24,]

ac2 <-
all.cause.14.c %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 13.81, linetype = "dashed") +
  ggtitle("Crude")+
  geom_text(aes(label = paste(nObs)), nudge_y =  500, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
################################################################################
heart.14 <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014h")
heart.14 <- heart.14[4:24,]

h1 <-
heart.14 %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 11.12, linetype = "dashed") +
  ggtitle("Age-Standardized")+
  geom_text(aes(label = paste(nObs)), nudge_y =  250, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

heart.14.c <- make_rdd_plot_data(outcome = "crude_rate_2014h")
heart.14.c <- heart.14.c[4:24,]

h2<-
heart.14.c %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 13.22, linetype = "dashed") +
  ggtitle("Crude")+
  geom_text(aes(label = paste(nObs)), nudge_y =  350, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
####################################################################################
can.14 <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014c")
can.14 <- can.14[4:24,]

can1<-
can.14 %>%
  filter(bw > 7) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 7.3, linetype = "dashed") +
  ggtitle("Age-Standardized")+
  geom_text(aes(label = paste(nObs)), nudge_y =  400, check_overlap = TRUE, angle = 60) +
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

can.14.c <- make_rdd_plot_data(outcome = "crude_rate_2014c")
can.14.c <- can.14.c[1:24,]

can2<-
can.14.c %>%
  filter(bw > 7) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 10.41, linetype = "dashed") +
  ggtitle("Crude") +
  geom_text(aes(label = paste(nObs)), nudge_y =  500, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())



# 
#  plot(can.14.c$bw, can.14.c$estimate,
#       ylim = c(-1000,1000), col = "black")
#  lines(can.14.c$lower,lty=3)
#  lines(can.14.c$upper,lty=3)
#  # Finally, we include an additional axis in the plot
#  axis(3, at=c(can.14.c$bw), labels=c(can.14.c$nObs), 
#       cex=.6, col="grey50",
#      lwd = 0.5, padj=1, line=1, cex.axis=.7, col.axis="grey50")
#  
#  mtext("Number of observations", side=3, col="grey50", cex=.7, adj=0)


library(patchwork)
#patches <- (ac1 + ac2) /(h1 + h2) /(can1+can2)
# patches + 
#   plot_annotation(
#   title = 'Effect of NHSC Participation on County-Level Mortality Rate')

#all cause plot
ac.all <- ac1 / ac2
ac.all + plot_annotation(title = "Effect of NHSC on County-Level All-Cause Mortality Rate")

#heart dz plot
hd.all <- h1 / h2
hd.all + plot_annotation(title = "Effect of NHSC on County-Level Heart Disease Mortality Rate")

#cancer plot
can.all <- can1 / can2
can.all + plot_annotation(title = "Effect of NHSC on County-Level Cancer Mortality Rate")
#################################################################################################
#make plots for adjusted estimates
adj.all <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014a",
                              covars = c("adult_obesity_2012_2014",
                                         "adult_smoking_value",
                                         "uninsured_2014",
                                         "unemployment_rate_2014",
                                         "physical_inactivity_2012_2014"))
all.adj1<- 
adj.all %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 14.846, linetype = "dashed") +
  ggtitle("Age-Standardized (Covariate Adjusted)") +
  geom_text(aes(label = paste(nObs)), nudge_y =  500, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

crude.all <- make_rdd_plot_data(outcome = "crude_rate_2014a",
                              covars = c("adult_obesity_2012_2014",
                                         "adult_smoking_value",
                                         "uninsured_2014",
                                         "unemployment_rate_2014"))
all.adj2<- 
  crude.all %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 12.91, linetype = "dashed") +
  ggtitle("Age-Standardized (Covariate Adjusted)") +
  geom_text(aes(label = paste(nObs)), nudge_y =  500, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

adj.all.c <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014c",
                              covars = c("adult_obesity_2012_2014",
                                         "adult_smoking_value",
                                         "uninsured_2014",
                                         "unemployment_rate_2014"))
can2 <- 
  adj.all.c %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 7.394, linetype = "dashed") +
  ggtitle("Age-Standardized (Covariate Adjusted)") +
  geom_text(aes(label = paste(nObs)), nudge_y =  100, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())


can.unadj <- make_rdd_plot_data(outcome = "crude_rate_2014c",
                                covars = c("adult_obesity_2012_2014",
                                           "adult_smoking_value",
                                           "uninsured_2014",
                                           "unemployment_rate_2014"))
can3 <- 
  can.unadj %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 10.70, linetype = "dashed") +
  ggtitle("Age-Standardized (Covariate Adjusted)") +
  geom_text(aes(label = paste(nObs)), nudge_y =  130, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

adj.heart <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014h",
                                covars = c("adult_obesity_2012_2014",
                                           "adult_smoking_value",
                                           "uninsured_2014",
                                           "unemployment_rate_2014"))
heart.adj2 <- 
  adj.heart %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 11.7, linetype = "dashed") +
  ggtitle("Age-Standardized (Covariate Adjusted)") +
  geom_text(aes(label = paste(nObs)), nudge_y =  100, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

unadj.heart <- make_rdd_plot_data(outcome = "crude_rate_2014h",
                                covars = c("adult_obesity_2012_2014",
                                           "adult_smoking_value",
                                           "uninsured_2014",
                                           "unemployment_rate_2014"))
heart.adj.crude <- 
  unadj.heart %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 13.17, linetype = "dashed") +
  ggtitle("Age-Standardized (Covariate Adjusted)") +
  geom_text(aes(label = paste(nObs)), nudge_y =  100, check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

unadj.heart <- cbind(unadj.heart, rep("Covariate Adjusted", nrow(unadj.heart)))
heart.14.c <- cbind(heart.14.c, rep("Unadjusted", nrow(heart.14.c)))
names(unadj.heart)[7] <- "Adjustment"
names(heart.14.c)[7] <- "Adjustment"

heart.all <- rbind(unadj.heart, heart.14.c)
heart.all %>%
  mutate(unadjObs = ifelse(Adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
         adjObs = ifelse(Adjustment != "Unadjusted", nObs, "")) %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate, color = factor(Adjustment))) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_vline(xintercept = 13.17, linetype = "dashed", col = "#F8766D") +
  geom_vline(xintercept = 13.22, linetype = "dashed", col = "#00BFC4") +
  ggtitle("Age-Standardized (Covariate Adjusted)") +
  geom_vline(xintercept = 9, color = "white") +
  geom_text(aes(label = paste(adjObs)), nudge_y = 200, nudge_x = -.2,
            check_overlap = TRUE, angle = 60)+
  geom_text(aes(label = paste(unadjObs)), nudge_y = -150, nudge_x = -.2,
            check_overlap = FALSE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# ###############################################################################################
# #do data for the complete cases set
# comp.data <- merge.mortb[!is.na(merge.mortb$uninsured_2014) &
#                            !is.na(merge.mortb$physical_inactivity_2012_2014) &
#                            !is.na(merge.mortb$adult_obesity_2012_2014) &
#                            !is.na(merge.mortb$unemployment_rate_2014) &
#                            !is.na(merge.mortb$adult_smoking_value),]
# 
# #make plots for adjusted estimates
# all.age.adj <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014a",
#                               covars = c("adult_obesity_2012_2014",
#                                          "adult_smoking_value",
#                                          "uninsured_2014",
#                                          "unemployment_rate_2014",
#                                          "physical_inactivity_2012_2014"),
#                               data = comp.data)
# all.age.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
# all.age.un <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014a",
#                                   data = comp.data)
# all.age.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))
# 
# 
# rbind(all.age.un, all.age.adj) %>%
#   mutate(unadjObs = ifelse(adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
#          adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
#   geom_vline(xintercept = 13.824786	, linetype = "dashed", col = "darkgrey") +
#   ggtitle("Age-Standardized All-Cause Mortality") +
#   geom_vline(xintercept = 5, color = "white") +
#   geom_text(aes(label = paste(unadjObs)), nudge_y = 150,
#             check_overlap = TRUE, angle = 60, color = "black")+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 
#   
# 
# all.crude.adj <- make_rdd_plot_data(outcome = "crude_rate_2014a",
#                                   covars = c("adult_obesity_2012_2014",
#                                              "adult_smoking_value",
#                                              "uninsured_2014",
#                                              "unemployment_rate_2014",
#                                              "physical_inactivity_2012_2014"),
#                                   data = comp.data)
# all.crude.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
# 
# all.crude.un <- make_rdd_plot_data(outcome = "crude_rate_2014a",
#                                  data = comp.data)
# all.crude.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))
# 
# rbind(all.crude.un, all.crude.adj) %>%
#   mutate(unadjObs = ifelse(adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
#          adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
#   geom_vline(xintercept = 12.298432, linetype = "dashed", col = "darkgrey") +
#   ggtitle("Crude All-Cause Mortality") +
#   geom_vline(xintercept = 6, color = "white") +
#   geom_text(aes(label = paste(unadjObs)), nudge_y = 220,
#             check_overlap = TRUE, angle = 60, color = "black")+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 
# 
# ###############################################################
# heart.age.adj <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014h",
#                                   covars = c("adult_obesity_2012_2014",
#                                              "adult_smoking_value",
#                                              "uninsured_2014",
#                                              "unemployment_rate_2014",
#                                              "physical_inactivity_2012_2014"),
#                                   data = comp.data)
# heart.age.un <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014h",
#                                  data = comp.data)
# heart.age.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
# heart.age.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))
# 
# 
# rbind(heart.age.un, heart.age.adj) %>%
#   mutate(unadjObs = ifelse(adjustment == "Unadjusted" , nObs, ""),
#          adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
#   geom_vline(xintercept = 10.845402, linetype = "dashed", col = "darkgrey") +
#   ggtitle("Age-Standardized Heart Disease Mortality") +
#   geom_vline(xintercept = 9, color = "white") +
#   geom_text(aes(label = paste(unadjObs)), nudge_y = 100,
#             check_overlap = TRUE, angle = 60, color = "black")+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 
# 
# 
# heart.crude.adj <- make_rdd_plot_data(outcome = "crude_rate_2014h",
#                                     covars = c("adult_obesity_2012_2014",
#                                                "adult_smoking_value",
#                                                "uninsured_2014",
#                                                "unemployment_rate_2014",
#                                                "physical_inactivity_2012_2014"),
#                                     data = comp.data)
# heart.crude.un <- make_rdd_plot_data(outcome = "crude_rate_2014h",
#                                    data = comp.data)
# heart.crude.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
# heart.crude.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))
# 
# rbind(heart.crude.un, heart.crude.adj) %>%
#   mutate(unadjObs = ifelse(adjustment == "Unadjusted" , nObs, ""),
#          adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
#   geom_vline(xintercept = 12.82, linetype = "dashed", col = "darkgrey") +
#   ggtitle("Crude Heart Disease Mortality") +
#   geom_vline(xintercept = 9, color = "white") +
#   geom_text(aes(label = paste(nObs)), nudge_y = 100,
#             check_overlap = TRUE, angle = 60, color = "black")+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 
# ##########################################################################
# canc.age.adj <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014c",
#                                   covars = c("adult_obesity_2012_2014",
#                                              "adult_smoking_value",
#                                              "uninsured_2014",
#                                              "unemployment_rate_2014",
#                                              "physical_inactivity_2012_2014"),
#                                   data = comp.data)
# canc.age.un <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014c",
#                                  data = comp.data)
# canc.age.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
# canc.age.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))
# 
# rbind(canc.age.un, canc.age.adj) %>%
#   mutate(unadjObs = ifelse(adjustment == "Unadjusted" , nObs, ""),
#          adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
#   geom_vline(xintercept = 7.405439	, linetype = "dashed", col = "darkgrey") +
#   ggtitle("Age-Standardized Cancer Mortality") +
#   geom_vline(xintercept = 5, color = "white") +
#   geom_text(aes(label = paste(unadjObs)), nudge_y = 50,
#             check_overlap = TRUE, angle = 60, color = "black")+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 
# 
# canc.crude.adj <- make_rdd_plot_data(outcome = "crude_rate_2014c",
#                                     covars = c("adult_obesity_2012_2014",
#                                                "adult_smoking_value",
#                                                "uninsured_2014",
#                                                "unemployment_rate_2014",
#                                                "physical_inactivity_2012_2014"),
#                                     data = comp.data)
# canc.crude.un <- make_rdd_plot_data(outcome = "crude_rate_2014c",
#                                    data = comp.data)
# 
# canc.crude.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
# canc.crude.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))
# 
# rbind(canc.crude.un, canc.crude.adj) %>%
#   mutate(unadjObs = ifelse(adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
#          adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
#   geom_vline(xintercept = 10.18, linetype = "dashed", col = "darkgrey") +
#   ggtitle("Crude Cancer Mortality") +
#   geom_vline(xintercept = 5, color = "white") +
#   geom_text(aes(label = paste(nObs)), nudge_y = 50,
#             check_overlap = TRUE, angle = 60, color = "black")+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 
# #####################################################################################
# 
# all.adj1<- 
#   adj.all %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_vline(xintercept = 14.846, linetype = "dashed") +
#   ggtitle("Age-Standardized (Covariate Adjusted)") +
#   geom_text(aes(label = paste(nObs)), nudge_y =  500, check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# crude.all <- make_rdd_plot_data(outcome = "crude_rate_2014a",
#                                 covars = c("adult_obesity_2012_2014",
#                                            "adult_smoking_value",
#                                            "uninsured_2014",
#                                            "unemployment_rate_2014",
#                                            "physical_inactivity_2012_2014"),
#                                 data = comp.data)
# all.adj2<- 
#   crude.all %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_vline(xintercept = 12.91, linetype = "dashed") +
#   ggtitle("Age-Standardized (Covariate Adjusted)") +
#   geom_text(aes(label = paste(nObs)), nudge_y =  500, check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# adj.all.c <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014c",
#                                 covars = c("adult_obesity_2012_2014",
#                                            "adult_smoking_value",
#                                            "uninsured_2014",
#                                            "unemployment_rate_2014",
#                                            "physical_inactivity_2012_2014"),
#                                 data = comp.data)
# can2 <- 
#   adj.all.c %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_vline(xintercept = 7.394, linetype = "dashed") +
#   ggtitle("Age-Standardized (Covariate Adjusted)") +
#   geom_text(aes(label = paste(nObs)), nudge_y =  100, check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# 
# can.unadj <- make_rdd_plot_data(outcome = "crude_rate_2014c",
#                                 covars = c("adult_obesity_2012_2014",
#                                            "adult_smoking_value",
#                                            "uninsured_2014",
#                                            "unemployment_rate_2014",
#                                            "physical_inactivity_2012_2014"),
#                                 data = comp.data)
# can3 <- 
#   can.unadj %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_vline(xintercept = 10.70, linetype = "dashed") +
#   ggtitle("Age-Standardized (Covariate Adjusted)") +
#   geom_text(aes(label = paste(nObs)), nudge_y =  130, check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# adj.heart <- make_rdd_plot_data(outcome = "age_adjusted_rate_2014h",
#                                 covars = c("adult_obesity_2012_2014",
#                                            "adult_smoking_value",
#                                            "uninsured_2014",
#                                            "unemployment_rate_2014",
#                                            "physical_inactivity_2012_2014"),
#                                 data = comp.data)
# heart.adj2 <- 
#   adj.heart %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_vline(xintercept = 11.7, linetype = "dashed") +
#   ggtitle("Age-Standardized (Covariate Adjusted)") +
#   geom_text(aes(label = paste(nObs)), nudge_y =  100, check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# unadj.heart <- make_rdd_plot_data(outcome = "crude_rate_2014h",
#                                   covars = c("adult_obesity_2012_2014",
#                                              "adult_smoking_value",
#                                              "uninsured_2014",
#                                              "unemployment_rate_2014",
#                                              "physical_inactivity_2012_2014"),
#                                   data = comp.data)
# heart.adj.crude <- 
#   unadj.heart %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_vline(xintercept = 13.17, linetype = "dashed") +
#   ggtitle("Age-Standardized (Covariate Adjusted)") +
#   geom_text(aes(label = paste(nObs)), nudge_y =  100, check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# unadj.heart <- cbind(unadj.heart, rep("Covariate Adjusted", nrow(unadj.heart)))
# heart.14.c <- cbind(heart.14.c, rep("Unadjusted", nrow(heart.14.c)))
# names(unadj.heart)[7] <- "Adjustment"
# names(heart.14.c)[7] <- "Adjustment"
# 
# heart.all <- rbind(unadj.heart, heart.14.c)
# heart.all %>%
#   mutate(unadjObs = ifelse(Adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
#          adjObs = ifelse(Adjustment != "Unadjusted", nObs, "")) %>%
#   filter(!is.nan(lower)) %>%
#   filter(nObs >1) %>%
#   ggplot(aes(x = bw, y = estimate, color = factor(Adjustment))) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_vline(xintercept = 13.17, linetype = "dashed", col = "#F8766D") +
#   geom_vline(xintercept = 13.22, linetype = "dashed", col = "#00BFC4") +
#   ggtitle("Age-Standardized (Covariate Adjusted)") +
#   geom_vline(xintercept = 9, color = "white") +
#   geom_text(aes(label = paste(adjObs)), nudge_y = 200, nudge_x = -.2,
#             check_overlap = TRUE, angle = 60)+
#   geom_text(aes(label = paste(unadjObs)), nudge_y = -150, nudge_x = -.2,
#             check_overlap = FALSE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# 
# 
# #############################################################################
# #See diff in covariates at diff bandwidths
# covariate.test <- make_rdd_plot_data(outcome = "adult_obesity_2012_2014",
#                                 data = comp.data)
# 
# covariate.test%>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_text(aes(label = paste(nObs)), nudge_y = .1, nudge_x = -.2,
#             check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# 
# make_rdd_plot_data(outcome = "adult_smoking_value",
#                    data = comp.data) %>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
#             check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# make_rdd_plot_data(outcome = "uninsured_2014",
#                    data = comp.data)%>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
#             check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# 
# make_rdd_plot_data(outcome = "unemployment_rate_2014",
#                    data = comp.data)%>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
#             check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# 
# make_rdd_plot_data(outcome = "physical_inactivity_2012_2014",
#                    data = comp.data)%>%
#   ggplot(aes(x = bw, y = estimate)) + geom_point() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
#   theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
#   geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
#             check_overlap = TRUE, angle = 60)+
#   xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())
# 
# 
# #look at how the results would be if we used normal linear regression with out the triangular kernels
# #############################################################
# merge.mortb$transformed_imu <- merge.mortb$newimu14 - 62
# merge.mortb$transformed_imu_sq <- merge.mortb$transformed_imu^2
# merge.mortb$transformed_imu_cub <- merge.mortb$transformed_imu^3
# 
# linear.mod <- lm(age_adjusted_rate_2014a ~ transformed_imu + treatment,
#                  data = merge.mortb)
# summary(linear.mod)
# 
# #plot it
# plot(x = merge.mortb$transformed_imu ,y=  merge.mortb$age_adjusted_rate_2014a, 
#      col = factor(merge.mortb$transformed_imu > 0),
#      xlab = "Margin", ylab = "Age Adjusted Rate")
# curve(expr = -4.966*x + 852.68 - 2.84, -62, 0, add = T, col = "yellow")
# curve(expr = -4.966*x + 852.68, 0, 38, add = T, col = "black")
# 
# 
# linear.mod2 <- lm(age_adjusted_rate_2014a ~ transformed_imu*treatment,
#                  data = merge.mortb)
# summary(linear.mod2)
# 
# #install.packages("devtools")
# #devtools::install_github("cardiomoon/ggiraphExtra")
# library(ggiraphExtra)
# 
# #plot it
# ggPredict(linear.mod2, se=TRUE, interactive=FALSE)
# 
# linear.mod3 <- lm(age_adjusted_rate_2014a ~ transformed_imu_sq*treatment + transformed_imu*treatment,
#                   data = merge.mortb)
# summary(linear.mod3)
# 
# #plot it
# plot(x = merge.mortb$transformed_imu ,y=  merge.mortb$age_adjusted_rate_2014a, 
#      col = factor(merge.mortb$transformed_imu > 0),
#      xlab = "Margin", ylab = "Age Adjusted Rate")
# curve(expr = -3.946745*x + 858.62000 + 32.73795- 3.94674*x-0.07205*x^2+ 0.0696*x^2 +3.69977*x,
#       -62, 0, add = T, col = "yellow")
# curve(expr = -3.94674*x + 858.62000- 3.94674*x -0.07205*x^2 ,
#       0, 38, add = T, col = "black")
# 
# 
# linear.mod3 <- lm(age_adjusted_rate_2014a ~ transformed_imu_sq*treatment +
#                     transformed_imu*treatment + transformed_imu_cub*treatment,
#                   data = merge.mortb)
# summary(linear.mod3)
# 
# #plot it
# plot(x = merge.mortb$transformed_imu ,y=  merge.mortb$age_adjusted_rate_2014a, 
#      col = factor(merge.mortb$transformed_imu > 0),
#      xlab = "Margin", ylab = "Age Adjusted Rate")
# curve(expr = 848.434588 + 31.887071+ -3.548567*x +0.056447*x^2-0.004991*x^3  -0.558779*x^2+ 
#         -2.025323*x-0.006530*x^3,
#       -62, 0, add = T, col = "yellow")
# curve(expr = 848.434588 -3.548567*x +0.056447*x^2-0.004991*x^3 ,
#       0, 38, add = T, col = "black")
# 
# 
# rd.mod4 <- RDestimate(age_adjusted_rate_2014a ~ transformed_imu + treatment,
#                   data = merge.mortb)
# summary(rd.mod4)
# plot(rd.mod4)
###############################################################################################
#do data for the complete cases set
comp.data <- merge.mortb[!is.na(merge.mortb$uninsured_2015) &
                           !is.na(merge.mortb$physical_inactivity_2012_2014) &
                           !is.na(merge.mortb$adult_obesity_2013_2015) &
                           !is.na(merge.mortb$unemployment_rate_2015) &
                           !is.na(merge.mortb$adult_smoking_value),]

#make plots for adjusted estimates
all.age.adj <- make_rdd_plot_data(outcome = "age_adjusted_rate_2015a",
                                  covars = c("adult_obesity_2013_2015",
                                             "adult_smoking_value",
                                             "uninsured_2015",
                                             "unemployment_rate_2015",
                                             "physical_inactivity_2012_2014"),
                                  data = comp.data)
all.age.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
all.age.un <- make_rdd_plot_data(outcome = "age_adjusted_rate_2015a",
                                 data = comp.data)
all.age.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))


rbind(all.age.un, all.age.adj) %>%
  mutate(unadjObs = ifelse(adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
         adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
  geom_vline(xintercept = 14.84	, linetype = "dashed", col = "darkgrey") +
  ggtitle("Age-Standardized All-Cause Mortality") +
  geom_vline(xintercept = 5, color = "white") +
  geom_text(aes(label = paste(unadjObs)), nudge_y = 150,
            check_overlap = TRUE, angle = 60, color = "black")+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) +
  ylim(c(-500,500))


all.crude.adj <- make_rdd_plot_data(outcome = "crude_rate_2015a",
                                    covars = c("adult_obesity_2013_2015",
                                               "adult_smoking_value",
                                               "uninsured_2015",
                                               "unemployment_rate_2015",
                                               "physical_inactivity_2012_2014"),
                                    data = comp.data)
all.crude.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))

all.crude.un <- make_rdd_plot_data(outcome = "crude_rate_2015a",
                                   data = comp.data)
all.crude.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))

rbind(all.crude.un, all.crude.adj) %>%
  mutate(unadjObs = ifelse(adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
         adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
  geom_vline(xintercept = 14.7, linetype = "dashed", col = "darkgrey") +
  ggtitle("Crude All-Cause Mortality") +
  geom_vline(xintercept = 5, color = "white") +
  geom_text(aes(label = paste(unadjObs)), nudge_y = 220,
            check_overlap = TRUE, angle = 60, color = "black")+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) +
  ylim(c(-500,500))

###############################################################
heart.age.adj <- make_rdd_plot_data(outcome = "age_adjusted_rate_2015h",
                                    covars = c("adult_obesity_2013_2015",
                                               "adult_smoking_value",
                                               "uninsured_2015",
                                               "unemployment_rate_2015",
                                               "physical_inactivity_2012_2014"),
                                    data = comp.data)
heart.age.un <- make_rdd_plot_data(outcome = "age_adjusted_rate_2015h",
                                   data = comp.data)
heart.age.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
heart.age.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))


rbind(heart.age.un, heart.age.adj) %>%
  mutate(unadjObs = ifelse(adjustment == "Unadjusted" , nObs, ""),
         adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
  geom_vline(xintercept = 9.795384, linetype = "dashed", col = "darkgrey") +
  ggtitle("Age-Standardized Heart Disease Mortality") +
  geom_vline(xintercept = 5, color = "white") +
  geom_text(aes(label = paste(unadjObs)), nudge_y = 100,
            check_overlap = TRUE, angle = 60, color = "black")+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 


heart.crude.adj <- make_rdd_plot_data(outcome = "crude_rate_2015h",
                                      covars = c("adult_obesity_2013_2015",
                                                 "adult_smoking_value",
                                                 "uninsured_2015",
                                                 "unemployment_rate_2015",
                                                 "physical_inactivity_2012_2014"),
                                      data = comp.data)
heart.crude.un <- make_rdd_plot_data(outcome = "crude_rate_2015h",
                                     data = comp.data)
heart.crude.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
heart.crude.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))

rbind(heart.crude.un, heart.crude.adj) %>%
  mutate(unadjObs = ifelse(adjustment == "Unadjusted" , nObs, ""),
         adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
  geom_vline(xintercept = 12.678273, linetype = "dashed", col = "darkgrey") +
  ggtitle("Crude Heart Disease Mortality") +
  geom_vline(xintercept = 5, color = "white") +
  geom_text(aes(label = paste(unadjObs)), nudge_y = 100,
            check_overlap = TRUE, angle = 60, color = "black")+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) + ylim (-100, 250)
##########################################################################
canc.age.adj <- make_rdd_plot_data(outcome = "age_adjusted_rate_2015c",
                                   covars = c("adult_obesity_2013_2015",
                                              "adult_smoking_value",
                                              "uninsured_2015",
                                              "unemployment_rate_2015",
                                              "physical_inactivity_2012_2014"),
                                   data = comp.data)
canc.age.un <- make_rdd_plot_data(outcome = "age_adjusted_rate_2015c",
                                  data = comp.data)
canc.age.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
canc.age.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))

rbind(canc.age.un, canc.age.adj) %>%
  mutate(unadjObs = ifelse(adjustment == "Unadjusted" , nObs, ""),
         adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
  geom_vline(xintercept = 10.730936, linetype = "dashed", col = "darkgrey") +
  ggtitle("Age-Standardized Cancer Mortality") +
  geom_vline(xintercept = 5, color = "white") +
  geom_text(aes(label = paste(unadjObs)), nudge_y = 50,
            check_overlap = TRUE, angle = 60, color = "black")+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 

canc.crude.adj <- make_rdd_plot_data(outcome = "crude_rate_2015c",
                                     covars = c("adult_obesity_2013_2015",
                                                "adult_smoking_value",
                                                "uninsured_2015",
                                                "unemployment_rate_2015",
                                                "physical_inactivity_2012_2014"),
                                     data = comp.data)
canc.crude.un <- make_rdd_plot_data(outcome = "crude_rate_2015c",
                                    data = comp.data)

canc.crude.adj$adjustment <- rep("Adjusted", nrow(all.age.adj))
canc.crude.un$adjustment <- rep("Unadjusted", nrow(all.age.adj))

rbind(canc.crude.un, canc.crude.adj) %>%
  mutate(unadjObs = ifelse(adjustment == "Unadjusted" & as.numeric(nObs) !=1720, nObs, ""),
         adjObs = ifelse(adjustment != "Unadjusted", nObs, "")) %>%
  filter(!is.nan(lower)) %>%
  filter(nObs >1) %>%
  ggplot(aes(x = bw, y = estimate, color = factor(adjustment))) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .01) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE", color = "Covariate Adjusted?")+
  geom_vline(xintercept = 12.142145	, linetype = "dashed", col = "darkgrey") +
  ggtitle("Crude Cancer Mortality") +
  geom_vline(xintercept = 5, color = "white") +
  geom_text(aes(label = paste(nObs)), nudge_y = 50,
            check_overlap = TRUE, angle = 60, color = "black")+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank()) 
#####################################################################################

#See diff in covariates at diff bandwidths
make_rdd_plot_data(outcome = "adult_obesity_2013_2015",
                                     data = comp.data)%>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.1) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_text(aes(label = paste(nObs)), nudge_y = .1, nudge_x = -.2,
            check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())


make_rdd_plot_data(outcome = "adult_smoking_value",
                   data = comp.data) %>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
            check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())

make_rdd_plot_data(outcome = "uninsured_2015",
                   data = comp.data)%>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
            check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())


make_rdd_plot_data(outcome = "unemployment_rate_2015",
                   data = comp.data)%>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
            check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())


make_rdd_plot_data(outcome = "physical_inactivity_2013_2015",
                   data = comp.data)%>%
  ggplot(aes(x = bw, y = estimate)) + geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =.3) +
  theme_bw() + labs(x = "Absolute Distance From Cutoff", y = "LATE")+
  geom_text(aes(label = paste(nObs)), nudge_y = .10, nudge_x = -.2,
            check_overlap = TRUE, angle = 60)+
  xlim(c(5,23))+ theme(panel.grid.minor.x = element_blank())


#look at how the results would be if we used normal linear regression without the triangular kernels
#############################################################
merge.mortb$transformed_imu <- merge.mortb$newimu14 - 62
merge.mortb$transformed_imu_sq <- merge.mortb$transformed_imu^2
merge.mortb$transformed_imu_cub <- merge.mortb$transformed_imu^3

linear.mod <- lm(age_adjusted_rate_2015a ~ transformed_imu + treatment,
                 data = merge.mortb)
summary(linear.mod)

#plot it
plot(x = merge.mortb$transformed_imu ,y=  merge.mortb$age_adjusted_rate_2015a, 
     col = factor(merge.mortb$transformed_imu > 0),
     xlab = "Margin", ylab = "Age Adjusted Rate")
curve(expr = -4.966*x + 852.68 - 2.84, -62, 0, add = T, col = "yellow")
curve(expr = -4.966*x + 852.68, 0, 38, add = T, col = "black")


linear.mod2 <- lm(age_adjusted_rate_2015a ~ transformed_imu*treatment,
                  data = merge.mortb)
summary(linear.mod2)

#install.packages("devtools")
#devtools::install_github("cardiomoon/ggiraphExtra")
library(ggiraphExtra)

#plot it
ggPredict(linear.mod2, se=TRUE, interactive=FALSE)

linear.mod3 <- lm(age_adjusted_rate_2015a ~ transformed_imu_sq*treatment + transformed_imu*treatment,
                  data = merge.mortb)
summary(linear.mod3)

#plot it
plot(x = merge.mortb$transformed_imu ,y=  merge.mortb$age_adjusted_rate_2015a, 
     col = factor(merge.mortb$transformed_imu > 0),
     xlab = "Margin", ylab = "Age Adjusted Rate")
curve(expr = -3.946745*x + 858.62000 + 32.73795- 3.94674*x-0.07205*x^2+ 0.0696*x^2 +3.69977*x,
      -62, 0, add = T, col = "yellow")
curve(expr = -3.94674*x + 858.62000- 3.94674*x -0.07205*x^2 ,
      0, 38, add = T, col = "black")


linear.mod3 <- lm(age_adjusted_rate_2015a ~ transformed_imu_sq*treatment +
                    transformed_imu*treatment + transformed_imu_cub*treatment,
                  data = merge.mortb)
summary(linear.mod3)

#plot it
plot(x = merge.mortb$transformed_imu ,y=  merge.mortb$age_adjusted_rate_2015a, 
     col = factor(merge.mortb$transformed_imu > 0),
     xlab = "Margin", ylab = "Age Adjusted Rate")
curve(expr = 848.434588 + 31.887071+ -3.548567*x +0.056447*x^2-0.004991*x^3  -0.558779*x^2+ 
        -2.025323*x-0.006530*x^3,
      -62, 0, add = T, col = "yellow")
curve(expr = 848.434588 -3.548567*x +0.056447*x^2-0.004991*x^3 ,
      0, 38, add = T, col = "black")


rd.mod4 <- RDestimate(age_adjusted_rate_2015a ~ transformed_imu + treatment,
                      data = merge.mortb)
summary(rd.mod4)
plot(rd.mod4)

#################################################################################
#test with false cutoff values as a placebo
make_rdd_plot_data_placebo  <- function(outcome, data = merge.mortb, covars = NULL, cutoff){
  formula <- paste(outcome, "~ newimu14 + treatment")
  if (!is.null(covars)){
    formula <- paste(formula, "|", paste(covars, collapse = "+"))
  }
  rdd1 <- RDestimate(formula = formula,
                     data = data,
                     subset = NULL, cutpoint = cutoff, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
  ik.bw <- rdd1$bw
  tempframe <- as.data.frame(
    cbind(append(1:23,ik.bw),
          rep(1,24),
          rep(1,24),
          rep(1,24),
          rep(1,24),
          rep(1,24))
  )
  names(tempframe) <- c("bw", "estimate", "lower", "upper", "pvalue", "nObs")
  tempframe[24,] <- c(ik.bw[1],
                      rdd1$est[1],
                      (rdd1$est[1]- 1.96*rdd1$se[1]),
                      (rdd1$est[1]+ 1.96*rdd1$se[1]),
                      rdd1$p[1],
                      rdd1$obs[1])
  for (i in 1:23){
    rd <- RDestimate(formula = formula,
                     data = data,
                     subset = NULL, cutpoint = 62, bw = i,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = TRUE, frame = FALSE)
    tempframe[i,] <- c(i,
                       rd$est[1],
                       (rd$est[1]- 1.96*rd$se[1]),
                       (rd$est[1]+ 1.96*rd$se[1]),
                       rd$p[1],
                       rd$obs[1])
  }
  
  return(tempframe)  
}

plac.cut <- 
purrr::map_df(.x = seq(40, 90, 1), 
               .f = ~make_rdd_plot_data_placebo(outcome = "age_adjusted_rate_2014a",
                                               data = merge.mortb, covars = NULL,
                                               cutoff = .x))

?RDestimate
