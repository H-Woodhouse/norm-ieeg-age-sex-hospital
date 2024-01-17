################################################################################

# Script to check that the age range in each ROI is not affecting results
# Likely for Supplementary



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_p = read.csv("Data/ROI1_relBP_pooled.csv")
age_coeffs = read.csv("Output/age_ROI_stats.csv")

band = c("delta","theta","alpha","beta","gamma")



#### DATA FRAME CREATION #######################################################

# calculate 0.05 and 0.95 percentiles and subtract to give rang/spread of
# central 90% of ages
ROI_ages = BPdata_p %>% 
  group_by(ROI_R) %>%
  reframe(age_range_l=quantile(Age,0.05), age_range_u=quantile(Age,0.95),
          inter90range = age_range_u-age_range_l) %>%
  select(ROI_R,inter90range)

# select just coefs and ROI Index from ROI_stats df
age_coeffs = age_coeffs %>%
  select(ROI_index,paste0(band,"_coef")) %>%
  rename(ROI_R=ROI_index)

# combine
ROI_ages = left_join(ROI_ages,age_coeffs)
