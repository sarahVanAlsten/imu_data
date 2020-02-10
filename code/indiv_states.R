
al <- all.imu %>%
  filter(substr(as.character(fips), 1,1)== 1 & nchar(fips)==4) %>%
  group_by(county, fips)%>%
  select_at(vars(contains("inf"))) %>%
  ungroup()


al[grepl("Bull", al$county), 4:9] <- 20.5
al[grepl("Butl", al$county), 4:9] <- 23.2
al[grepl("Cherok", al$county), 4:9] <- 23.2
al[grepl("Choct", al$county), 4:9] <- 18.5
al[grepl("Coos", al$county), 4:9] <- 16.4
al[grepl("Perry", al$county), 4:9] <- 23.2
al[grepl("Monroe", al$county), 4:9] <- 24
al[grepl("Wash", al$county), 4:9] <- 24.8
al[grepl("Sum", al$county), 3:9] <- 26
al[grepl("Mare", al$county), 3:9] <- 26
al[grepl("Henry", al$county) |grepl("Rand", al$county) |grepl("Faye", al$county) |
     grepl("Clay", al$county) | grepl("Cleb", al$county), 4:9] <- 26

all.imu[which(substr(as.character(all.imu$fips), 1,1)== 1 & nchar(all.imu$fips)==4),
        21:27] <- al[,3:9]

wa <- all.imu %>% 
  filter(substr(as.character(fips),1,2) == 53)
#asotin, jeff had same as state avg of 4.7 deaths, or an IMU score of 26; 
#lewis+ pend orielle = 25.6

wa[grepl("Asotin", wa$county) |grepl("Jeff", wa$county)| grepl("Lewis", wa$county), 21:37] <- 26
wa[grepl("Pend", wa$county), 21:37] <- 25.6
wa[grepl("Pacific", wa$county), 21:37] <- 26
wa[grepl("Kittias", wa$county), 21:37] <- 25.6
wa[grepl("Whitm", wa$county), 21:37] <- 26

all.imu[which(substr(as.character(all.imu$fips), 1,2)== 53),
        21:37] <- wa[,21:37]

###################################################################
#ak: nw = 12, yk = 105, others <8

miss.mort <- imu.long %>%
  filter(measure == "inf_mort_")%>%
  filter(tot_nas13 ==1)

#focus on those missing JUST inf mort. wobt worry about other for now

#basically, if any of the inf morts are not missing, carry that value forward. do
#a range of values to do sensitiy analyses
miss.mort <- miss.mort %>%
  rowwise() %>%
  mutate(yrsMissed = is.na(`09`)+ is.na(`10`)+ is.na(`11`) + is.na(`12`)+
           is.na(`13`)+ is.na(`14`)+ is.na(`15`)+ is.na(`16`)+ is.na(`17`))

table(miss.mort$yrsMissed)

no.miss.mort <- imu.long %>%
  filter(measure == "inf_mort_")%>%
  filter(tot_nas13 ==0)


write_csv(all.imu, "data//all_imu_entry.csv")
