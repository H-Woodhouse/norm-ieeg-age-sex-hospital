################################################################################

## Script to use UCLH example patients to compare new methods to the older
## methods in P.N. Taylor, Brain, 2022



#### PRELMINIARIES #############################################################

# packages
library(dplyr)   # dataframe manipulation

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_m = read.csv("Data/Preprocessing/ROI1_RBP_mirrored.csv") 

# test patients
uclh_data = BPdata_m %>% filter(Pat_ID %in% c("UCLH_1216", "UCLH_910"))

# normative map patients
norm_map_data = BPdata_m %>% filter(Pat_ID!="UCLH_1216") %>% filter(Pat_ID!="UCLH_910")

# bands
bands = c("delta", "theta", "alpha", "beta", "gamma")



#### OLD METHODS ###############################################################

# mean and SD of normative RBP in each region
norm_map_summaries = norm_map_data %>%
  select(c(ROI_R, paste0(bands, "BP"))) %>%
  group_by(ROI_R) %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))

# z scores for uclh
# attach the normative mean and sd so its in the same df
uclh_data_z = left_join(uclh_data, norm_map_summaries, by="ROI_R")
# compute the z score per fb
uclh_data_z = uclh_data_z %>% 
  mutate(delta_zscore = abs((deltaBP - deltaBP_mean)/deltaBP_sd)) %>%
  mutate(theta_zscore = abs((thetaBP - thetaBP_mean)/thetaBP_sd)) %>%
  mutate(alpha_zscore = abs((alphaBP - alphaBP_mean)/alphaBP_sd)) %>%
  mutate(beta_zscore  = abs((betaBP  - betaBP_mean) /betaBP_sd))  %>%
  mutate(gamma_zscore = abs((gammaBP - gammaBP_mean)/gammaBP_sd)) %>%
  select(-c(paste0(bands,"BP_mean"), paste0(bands,"BP_sd")))
  
  

#### NEW METHODS ###############################################################

