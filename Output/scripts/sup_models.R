################################################################################

## Script to produce required material for supplementary
## Namely: symmetry checking, parcellation checking



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)   # plots & dataframes
library(lme4)        # mixed models

# switch: ROI1 or ROI2?

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
#BPdata = read.csv("Data/Preprocessing/ROI1_RBP_full.csv")
BPdata = read.csv("Data/Preprocessing/ROI2_RBP_full.csv")

# load ROI info from DB
#ROI_info=read.csv("Data/ROI1_info.csv",header = T)
ROI_info=read.csv("Data/ROI2_info.csv",header = T)

# if ROI2, drop L/R accumbens, thalamus proper, palidum (ROI 1,4,7,8,11,14)
BPdata = BPdata %>%
  filter(!(ROI_index  %in% c(1,4,7,8,11,14)))



#### CHECKING SYMMETRY IS VALID & RESULTS HOLD IN UNMIRRORED ROI1/2 ############

## this is repeating the brain coefficients figure without folding/pooling data
## do once in ROI1 and repeat in ROI2 

## DATA ##

# band names
band = c("delta", "theta", "alpha", "beta", "gamma")

# create index for ROIs for looping
BPdata = BPdata %>%
  mutate(ROI_loop=as.numeric(factor(ROI_index))) %>%
  relocate(ROI_loop, .after = ROI_index) %>%
  arrange(ROI_index)

# list of ROIs & stats, put into empty df
regions=unique(BPdata$ROI_index)
N=length(regions)
stats=c(paste0(band,"_coef"),paste0(band,"_singular"))
age_coefs=tibble(ROI=regions,ROI_index=1:N)
age_coefs[c(stats)]=NA

## MODEL ##

# extracting age coefficient for all ROI / FB
for (fb in band) {
  
  # model formula for particular FB
  model = formula(paste0(fb,"BP~Age+(1|Hospital)"))
  
  # running model for that band in every ROI 
  for (i in 1:N) {
    
    # model
    mod = lmer(model, data = BPdata[BPdata$ROI_loop==i,])
    
    # pulling out coefficient of age
    age_coefs[i,paste0(fb,"_coef")] = coef(mod)$Hospital[1,"Age"]
    
    # binary variable indicating singularity
    age_coefs[i,paste0(fb,"_singular")] = as.numeric(isSingular(mod))
  }
}

# how many singular when not pooled
# 28 ROI1, 61 ROI2
sum(age_coefs[,8:12])

## PLOT DF ##

# format for matlab 
ROI_info = ROI_info %>% rename(ROI=ROI_index)
age_coefs = left_join(age_coefs,ROI_info)
age_coefs = age_coefs %>%
  select(c(ROI_name,paste0(band,"_coef"),x,y,z)) %>%
  rename(names=ROI_name)

# export
write.csv(age_coefs, "Output/sup-results/b_age_coeffs_ROI2_SUP.csv", row.names=FALSE)

rm(mod,fb,i,model,stats)


