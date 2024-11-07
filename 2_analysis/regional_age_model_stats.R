################################################################################

## Script to run the age model to investigate the effect of age on log rel BP in
## each band at the regional level, with hospital as a random effect
##
## Relevant model summaries extracted and saved to be used in 
## FIGURE 5B
## FIGURE 6



#### PRELIMINARIES #############################################################

# packages 
library(tidyverse)   # dataframes
library(lme4)        # mixed models

# set working directory to script location (2_analysis)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# folder for storing results (included in all scripts with outputs -> warnings off)
dir.create("../3_output", showWarnings = F)

# data
BPdata_m = read.csv("../1_data/ROI1_mirrored_RBP.csv")            # mirrored data
subj_pr = read.csv("../1_data/ROI1_mirrored_subj_per_ROI.csv")    # subjects per ROI
ROI1_info = read.csv("../1_data/ROI1_info.csv")                   # ROI info (eg xyz)

# drop the low sampled regions not considered in our work
ROI1_info = ROI1_info %>%
  filter(!Area %in% c("Pallidum","Thalamus-Proper","Accumbens-area"))

# band names
band = c("delta", "theta", "alpha", "beta", "gamma")



################################################################################

# begin by focusing on getting the statistics we need in the right hemisphere
# using our mirrored data


#### DATA SET UP ###############################################################

# list of right hemisphere ROIs 
regions_RH=unique(BPdata_m$ROI_R)
N=length(regions_RH)

# stats to collect/column names
stats=c("coef","SE","singular", "none0_CI")
stats_all=c(paste0("delta_",stats), paste0("theta_",stats),paste0("alpha_",stats),
            paste0("beta_",stats),paste0("gamma_",stats))

# empty data frame to fill with model statistics
mod_summaries=tibble(ROI_R=regions_RH,ROI_index=1:N)
mod_summaries[c(stats_all)]=NA



#### FITTING MODELS & EXTRACTING STATISTICS ####################################

# create index for ROIs for looping
BPdata_m = BPdata_m %>%
  mutate(ROI_index=as.numeric(factor(ROI_R))) %>%
  relocate(ROI_index, .after = ROI_R)


# extracting statistics for the age model all ROIs and bands
for (fb in band) {
  
  print(paste0("Fitting regional models in ", fb, " (band ", match(fb,band), "/5)"))
  
  # model formula for particular band
  model = formula(paste0(fb,"BP~Age+(1|Hospital)"))
  
  # running model for that band in every ROI 
  for (i in 1:N) {
    
    # model
    mod = lmer(model, data = BPdata_m[BPdata_m$ROI_index==i,])
    
    # pulling out beta_age summaries (value, standard error)
    mod_summaries[i,paste0(fb,"_coef")]     = coef(mod)$Hospital[1,"Age"]
    mod_summaries[i,paste0(fb,"_SE")]       = summary(mod)$coefficients["Age","Std. Error"]
    
    # binary variable indicating singularity
    mod_summaries[i,paste0(fb,"_singular")] = as.numeric(isSingular(mod))
    
    # binary variable indicating whether the 95% CI on beta_age contains 0
    CI = confint(mod, method="Wald")["Age",]
    mod_summaries[i,paste0(fb,"_none0_CI")]  = as.numeric(sign(CI[1])==sign(CI[2]))
    
  }
}

# attach number of subjects per region
mod_summaries = mod_summaries %>% left_join(subj_pr) %>% 
  rename(no.subj=n) %>% 
  relocate(no.subj, .after = ROI_index) %>%
  select(-ROI_index) %>%
  rename(ROI_index=ROI_R)

rm(mod,subj_pr,CI,fb,i,model,stats,stats_all)



################################################################################

# now 'reflect' back across the midline, effectively copying values into their
# corresponding left hemisphere region (predominently for Figure 5B)


#### CREATING WHOLE BRAIN DATA FRAME ###########################################

# attach model statistics to regional information
region_summaries=left_join(ROI1_info,mod_summaries)

# 'mirror across the midline' to fill in gaps
# checking the symmetric regions have the same name
empty_rows=as.integer(row.names(region_summaries[region_summaries$Hemisphere=="Left",]))
for (i in empty_rows) {
  if (region_summaries$Cortical[i]=="SC" & region_summaries$Area[i]==region_summaries$Area[i+4]) {
    region_summaries[i,paste0(band,"_coef")]=region_summaries[i+4,paste0(band, "_coef")]
    region_summaries[i,paste0(band,"_SE")]=region_summaries[i+4,paste0(band, "_SE")]
    region_summaries[i,paste0(band,"_singular")]=region_summaries[i+4,paste0(band, "_singular")]
    region_summaries[i,paste0(band,"_none0_CI")]=region_summaries[i+4,paste0(band, "_none0_CI")]
    region_summaries[i,"no.subj"]=region_summaries[i+4,"no.subj"]
  }
  else if (region_summaries$Cortical[i]=="C" & region_summaries$Area[i]==region_summaries$Area[i+34]) {
    region_summaries[i,paste0(band,"_coef")]=region_summaries[i+34,paste0(band,"_coef")]
    region_summaries[i,paste0(band,"_SE")]=region_summaries[i+34,paste0(band, "_SE")]
    region_summaries[i,paste0(band,"_singular")]=region_summaries[i+34,paste0(band, "_singular")]
    region_summaries[i,paste0(band,"_none0_CI")]=region_summaries[i+34,paste0(band, "_none0_CI")]
    region_summaries[i,"no.subj"]=region_summaries[i+34,"no.subj"]
  }
}

rm(empty_rows,i,mod_summaries,ROI1_info)

# save resulting data frame 
write.csv(region_summaries,"../3_output/regional_age_model_stats_TEST.csv", row.names = F)




# for plotting script
region_summaries_RH = na.omit(region_summaries) %>% 
  arrange(no.subj) %>% 
  mutate(Area=factor(Area,levels=Area))
