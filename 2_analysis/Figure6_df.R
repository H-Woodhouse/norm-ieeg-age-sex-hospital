################################################################################

## Script to run age only LMMs investigate the effect of age on log rel BP in
## each band at the regional level, with hospital as a random effect
## Sex has now been dropped
## Relevant model summaries extracted and saved
## Regional-level summaries plot produced



#### PRELIMINARIES #############################################################

# packages 
library(tidyverse)   # dataframes
library(lme4)        # mixed models

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site/")
BPdata_m= read.csv("1_data/ROI1_mirrored_RBP.csv")        # mirrored data
spr = read.csv("1_data/ROI1_mirrored_subj_per_ROI.csv")   # subj per ROI
hpr = read.csv("1_data/ROI1_mirrored_hosp_per_ROI.csv")   # hosp per ROI

# band names
band = c("delta", "theta", "alpha", "beta", "gamma")



#### DATA SET UP ###############################################################

# create index for ROIs for looping
# using the labels from right hemisphere / RH in mirrored data
BPdata_m = BPdata_m %>%
  mutate(ROI_index=as.numeric(factor(ROI_R))) %>%
  relocate(ROI_index, .after = ROI_R)

# list of ROIs 
regions_RH=unique(BPdata_m$ROI_R)
N=length(regions_RH)

# stats to collect/column names
stats=c("coef","SE","singular", "none0_CI")
stats_all=c(paste0("delta_",stats), paste0("theta_",stats),paste0("alpha_",stats),
            paste0("beta_",stats),paste0("gamma_",stats))


# empty df to fill with stats
mod_summaries=tibble(ROI_R=regions_RH,ROI_index=1:N)
mod_summaries[c(stats_all)]=NA



#### FITTING MODELS & EXTRACTING STATS #########################################

# extracting stats for all ROI / FB
for (fb in band) {
  
  # model formula for particular FB
  model = formula(paste0(fb,"BP~Age+(1|Hospital)"))
  
  # running model for that band in every ROI 
  for (i in 1:N) {
    
    # model
    mod = lmer(model, data = BPdata_m[BPdata_m$ROI_index==i,])
    
    # pulling out coef summaries (value, se)
    mod_summaries[i,paste0(fb,"_coef")]     = coef(mod)$Hospital[1,"Age"]
    mod_summaries[i,paste0(fb,"_SE")]       = summary(mod)$coefficients["Age","Std. Error"]
    
    # binary variable indicating singularity
    mod_summaries[i,paste0(fb,"_singular")] = as.numeric(isSingular(mod))
    
    # binary variable indicating whether the 95% CI on beta_age contains 0
    mod_summaries[i,paste0(fb,"_none0_CI")]  = as.numeric(sign(confint(mod)["Age",1])==sign(confint(mod)["Age",2]))
    
  }
}



#### ATTACH NO OF PATIENTS AND SITES PER REGION ################################

mod_summaries = mod_summaries %>% left_join(spr) %>% 
  dplyr::rename(no.subj=n) %>% 
  relocate(no.subj, .after = ROI_index) %>%
  left_join(hpr) %>%
  dplyr::rename(no.hosp=n) %>%
  relocate(no.hosp, .after = no.subj) %>%
  select(-ROI_index) %>%
  dplyr::rename(ROI_index=ROI_R)


#### SAVE ######################################################################

# save resulting table 
write.csv(mod_summaries,"3_output/regional_age_model_stats.csv", row.names = F)

