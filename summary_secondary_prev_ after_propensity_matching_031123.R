#######################################
#Comparing 2nd prevention after AMI from discharge, LMR 30 days and 12 months in propensity matched population
#Rachel Forster
#12.07.2023
########################################
library(tidyverse)
library(knitr)
library(openxlsx)
###################################
setwd("F:/Forskningsprosjekter/PDB 2890 - Cardiovascular disea_/Forskningsfiler/RAFO/WP4_CVDafterPCa/data_analysis")
data <- read_rds("propensity_matched_working_population_x5_031123.RData")

setwd("F:/Forskningsprosjekter/PDB 2890 - Cardiovascular disea_/Forskningsfiler/RAFO/WP4_CVDafterPCa/data_analysis/summary_statistics_and_logistic_regression_results_proprensity_matched_population")

data1 <- data %>% 
  filter(!doedunderopphold==1)
summary(as.factor(data1$pca_pop))
#8927, 1812, pca

#################### Summary stats  ##############
aspirin_30d<- data1 %>% 
  group_by(pca_pop,aspirin_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(aspirin_30d, format = "markdown")
write.xlsx(aspirin_30d,"aspirin_30d.xlsx")

aspirin_12m<- data1 %>% 
  filter(aspirin_disch_30d ==1) %>%
  filter(time_to_death_mi>365|is.na(time_to_death_mi)) %>% 
  group_by(pca_pop,aspirin_1year_afterAMI) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(aspirin_12m, format = "markdown")
write.xlsx(aspirin_12m,"aspirin_12m.xlsx")
####
p2y12_30d<- data1 %>% 
  group_by(pca_pop,p2y12_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(p2y12_30d, format = "markdown")
write.xlsx(p2y12_30d,"p2y12_30d.xlsx")

p2y12_12m<- data1 %>% 
  filter(p2y12_disch_30d ==1) %>% 
  filter(time_to_death_mi>365|is.na(time_to_death_mi)) %>% 
  group_by(pca_pop,p2y12_1year_afterAMI) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(p2y12_12m, format = "markdown")
write.xlsx(p2y12_12m,"p2y12_12m.xlsx")

####
double_30d<- data1 %>% 
  group_by(pca_pop,double_antiplatelet_disch_30) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(double_30d, format = "markdown")
write.xlsx(double_30d,"double_antiplatelet_30d.xlsx")

double_12m<- data1 %>% 
  filter(double_antiplatelet_disch_30 ==1) %>% 
  filter(time_to_death_mi>365|is.na(time_to_death_mi)) %>% 
  group_by(pca_pop,double_antiplatelet_1year_afterAMI) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(double_12m, format = "markdown")
write.xlsx(double_12m,"double_antiplatelet_12m.xlsx")

####
lipidlowering_30d<- data1 %>% 
  group_by(pca_pop,statin_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(lipidlowering_30d, format = "markdown")
write.xlsx(lipidlowering_30d,"lipidlowering_30d.xlsx")

lipidlowering_12m<- data1 %>% 
  filter(statin_disch_30d ==1) %>% 
  filter(time_to_death_mi>365|is.na(time_to_death_mi)) %>% 
  group_by(pca_pop,statin_1year_afterAMI) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(lipidlowering_12m, format = "markdown")
write.xlsx(lipidlowering_12m,"lipidlowering_12m.xlsx")

####
ace_arb_30d<- data1 %>% 
  group_by(pca_pop,bp_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(ace_arb_30d, format = "markdown")
write.xlsx(ace_arb_30d,"ace_arb_30d.xlsx")

ace_arb_12m<- data1 %>% 
  filter(bp_disch_30d ==1) %>% 
  filter(time_to_death_mi>365|is.na(time_to_death_mi)) %>% 
  group_by(pca_pop,bp_1year_afterAMI) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(ace_arb_12m, format = "markdown")
write.xlsx(ace_arb_12m,"ace_arb_12m.xlsx")
####
betablocker_30d<- data1 %>% 
  group_by(pca_pop,betablocker_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(betablocker_30d, format = "markdown")
write.xlsx(betablocker_30d,"betablocker_30d.xlsx")

betablocker_12m<- data1 %>% 
  filter(betablocker_disch_30d == 1) %>% 
  filter(time_to_death_mi>365|is.na(time_to_death_mi)) %>% 
  group_by(pca_pop,betablocker_1year_afterAMI) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(betablocker_12m, format = "markdown")
write.xlsx(betablocker_12m,"betablocker_12m.xlsx")


#### Summary stats by PCa treatment group ####
summary(as.factor(data$primarytreatment))
#1 AS
#2 ADT
#3 RP
#4 ADT+RP
#5 RT
#6 ADT+RT
#7 No treatment

#Active surveillance
#ADT with or without curative radiotherapy or prostatectomy
#Radiotherapy or prostatectomy without ADT
#No treatment¨

as <- data1 %>% 
  filter(primarytreatment == 1)
ADT <-  data1 %>% 
  filter(primarytreatment == 2|primarytreatment== 4|primarytreatment==6)
RP_RT <- data1 %>% 
  filter(primarytreatment == 3|primarytreatment==5)
notrx <- data1 %>% 
  filter(primarytreatment ==7)

p2y12_30d<- as %>% 
  group_by(pca_pop,p2y12_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(p2y12_30d, format = "markdown")

p2y12_30d<- ADT %>% 
  group_by(pca_pop,p2y12_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(p2y12_30d, format = "markdown")

p2y12_30d<- RP_RT %>% 
  group_by(pca_pop,p2y12_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(p2y12_30d, format = "markdown")

p2y12_30d<- notrx %>% 
  group_by(pca_pop,p2y12_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(p2y12_30d, format = "markdown")

####
double_30d<- as %>% 
  group_by(pca_pop,double_antiplatelet_disch_30) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(double_30d, format = "markdown")

double_30d<- ADT %>% 
  group_by(pca_pop,double_antiplatelet_disch_30) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(double_30d, format = "markdown")

double_30d<- RP_RT %>% 
  group_by(pca_pop,double_antiplatelet_disch_30) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(double_30d, format = "markdown")
double_30d<- notrx %>% 
  group_by(pca_pop,double_antiplatelet_disch_30) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(double_30d, format = "markdown")
####
lipidlowering_30d<- as %>% 
  group_by(pca_pop,statin_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(lipidlowering_30d, format = "markdown")

lipidlowering_30d<- ADT %>% 
  group_by(pca_pop,statin_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(lipidlowering_30d, format = "markdown")

lipidlowering_30d<- RP_RT %>% 
  group_by(pca_pop,statin_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(lipidlowering_30d, format = "markdown")
lipidlowering_30d<- notrx %>% 
  group_by(pca_pop,statin_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(lipidlowering_30d, format = "markdown")
####
ace_arb_30d<- as %>% 
  group_by(pca_pop,bp_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(ace_arb_30d, format = "markdown")

ace_arb_30d<- ADT %>% 
  group_by(pca_pop,bp_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(ace_arb_30d, format = "markdown")
ace_arb_30d<- RP_RT %>% 
  group_by(pca_pop,bp_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(ace_arb_30d, format = "markdown")
ace_arb_30d<- notrx %>% 
  group_by(pca_pop,bp_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(ace_arb_30d, format = "markdown")

####
betablocker_30d<- as %>% 
  group_by(pca_pop,betablocker_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(betablocker_30d, format = "markdown")

betablocker_30d<- ADT %>% 
  group_by(pca_pop,betablocker_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()
kable(betablocker_30d, format = "markdown")
betablocker_30d<- RP_RT %>% 
  group_by(pca_pop,betablocker_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(betablocker_30d, format = "markdown")
betablocker_30d<- notrx %>% 
  group_by(pca_pop,betablocker_disch_30d) %>% 
  summarize(
    count = n()
  ) %>% 
  group_by(pca_pop) %>% 
  mutate(
    percent = round(count / sum(count) * 100, 1)
  ) %>% 
  ungroup()

kable(betablocker_30d, format = "markdown")
