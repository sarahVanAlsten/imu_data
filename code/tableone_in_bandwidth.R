#restrict to counties within IK bandwidth to create a table 1 of descriptive statistics
#62 +/- 11.9

#in.bw <- merge.mortb[merge.mortb$imu14 <= (73.9) & merge.mortb$imu14 >= 50.1,]
in.bw <- merge.mortb[merge.mortb$newimu14 <= (62+10.11949) & merge.mortb$newimu14 >= (62-10.11949),]

in.bw$treated <- ifelse(in.bw$newimu14<=62, 1, 0)

#number observations
table(in.bw$treated)
table(in.bw$treated, in.bw$treatment)

#median(14.700201, 10.730936, 9.795384, 14.842001, 12.142145, 12.678273)

library(tableone)
in.bw <- janitor::clean_names(in.bw)
names(in.bw)
in.bw <- in.bw[in.bw$designation_type != "Medically Underserved Population" |
                 is.na(in.bw$designation_type),]

print(
  CreateTableOne(vars = c("uninsured_2014","physical_inactivity_2012_2014",
                        "unemployment_rate_2014", "adult_obesity_2012_2014",
                        "adult_smoking_value","population_2014a",
                        "percent_65_and_older_raw_value_14",
                        "primary_care_physicians_raw_value_14",
                        "pv_14", "inf_new_14", "treatment"),
               data = in.bw,
               strata = "treated")
  #,
  # nonnormal = c("uninsured_2014","physical_inactivity_2012_2014",
  #               "unemployment_rate_2014", "adult_obesity_2012_2014",
  #               "adult_smoking_value","population_2014a",
  #               "percent_65_and_older_raw_value_14",
  #               "primary_care_physicians_raw_value_14",
  #               "pv_14", "inf_new_14")
  )
in.bw$percent_non_hispanic_white_raw_value_14 <-
  as.numeric(as.character(in.bw$percent_non_hispanic_white_raw_value_14))
in.bw$percent_hispanic_raw_value_14 <-
  as.numeric(as.character(in.bw$percent_hispanic_raw_value_14))
in.bw$percent_non_hispanic_african_american_raw_value_14 <-
  as.numeric(as.character(in.bw$percent_non_hispanic_african_american_raw_value_14))

CreateTableOne(vars = c("percent_non_hispanic_white_raw_value_14",
                        "percent_hispanic_raw_value_14", 
                        "percent_non_hispanic_african_american_raw_value_14"),
               data = in.bw, strata = "treated")

in.bw$percent_rural_raw_value_14 <-
  as.numeric(as.character(in.bw$percent_rural_raw_value_14))
in.bw$high_school_graduation_raw_value_14 <-
  as.numeric(as.character(in.bw$high_school_graduation_raw_value_14))
in.bw$uninsured_adults_raw_value_14 <-
  as.numeric(as.character(in.bw$uninsured_adults_raw_value_14))

CreateTableOne(vars = c("percent_rural_raw_value_14",
                        "high_school_graduation_raw_value_14", 
                        "uninsured_adults_raw_value_14"),
               data = in.bw, strata = "treated")

t.test(percent_65_and_older_raw_value_14~ treated, data = in.bw)

DCdensity(comp.data$imu14, cutpoint = 62, bw = 10.11)
`IMU Score 2014`<- merge.mortb$newimu14
rddapp::dc_test(`IMU Score 2014`, 62, bw = 10.11)

