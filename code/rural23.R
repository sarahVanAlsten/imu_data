
hpsa <- readxl::read_xlsx("C:\\Users\\Owner\\OneDrive\\Documents\\Spring2020\\Causal_Inf\\Health_Shortage_Data\\BCD_HPSA_FCT_DET_PrimaryCare.xlsx")

hpsa <- janitor::clean_names(hpsa)
#not withdrawn ones
hpsa <- hpsa[hpsa$hpsa_status_code != "W",]
#only single counties
hpsa <- hpsa[hpsa$hpsa_component_type_code == "SCTY",]

plot(hpsa$hpsa_score)
table(hpsa$hpsa_status_code)
plot(hpsa$hpsa_score)
table(hpsa$hpsa_score < 15)
