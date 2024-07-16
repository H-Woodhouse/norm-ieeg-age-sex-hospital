################################################################################

## Script to run linear mixed models at the whole brain level to investigate
## the optimal fixed effect structure (age,sex,both,neither) of the LMM and to
## explore the importance of age and sex for explaining variation in log RBP
## all with Site as a random effect



#### PRELIMINARIES #############################################################

# packages 
library(dplyr)         # data frames
library(lme4)          # mixed models
library(performance)   # calculates lots of model comparison stats


# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site/")
BPdata_full = read.csv("Data/Preprocessing/ROI1_RBP_full.csv")

band = c("delta","theta","alpha","beta","gamma")



#### METRIC NOTES ##############################################################

# if looking at model on it's own, REML=T, if comparing 2 mods, REML = F

# LRT: high p = no diff between models = additional variable did nothing

# A/BIC, smaller is better, larger difference is better



#### MODEL PERFORMANCE & EFFECT IMPORTANCE ASSESSMENTS #########################

# loop over bands
# for each band, fit all models under consideration 
# extract and store model assessments, and effects assessments

# df for assessing fix/ran effects
# (model assessments will be stored in list in for loop, one per band)
R2_ICC = data.frame(matrix(ncol=7,nrow=5))
colnames(R2_ICC) = c("Band","R2_m_full","ICC_full","R2_m_age","ICC_age","R2_m_sex","ICC_sex")

for (i in 1:length(band)) {
  
  ## MODELS
  
  # model with each possible fixed effects structure
  # REML = F, comparing across structures
  mod_null = lmer(formula(paste0(band[i],"BP~        (1|Hospital)")), data=BPdata_full, REML = F)
  mod_age  = lmer(formula(paste0(band[i],"BP~Age+    (1|Hospital)")), data=BPdata_full, REML = F)
  mod_sex  = lmer(formula(paste0(band[i],"BP~Sex+    (1|Hospital)")), data=BPdata_full, REML = F)
  mod_full = lmer(formula(paste0(band[i],"BP~Age+Sex+(1|Hospital)")), data=BPdata_full, REML = F)
  
  ## MODEL EVALUATION
  
  # list for storing model assessment for this band
  model_assess = list()
  
  # BIC and AIC
  model_assess$IC = compare_performance(mod_null, mod_age, mod_sex, mod_full, estimator="ML", metrics = c("AIC","BIC"))[,c(1,3,5)]
  
  # likelihood ratio -- null v effect and effect v full
  model_assess$LRT=list(null_age=anova(mod_null,mod_age),
                        null_sex=anova(mod_null,mod_sex),
                        age_full=anova(mod_age,mod_full),
                        sex_full=anova(mod_sex,mod_full))
  
  # profiled confidence interval (excludes null)
  model_assess$CI_prof=list(age=confint(mod_age),sex=confint(mod_sex),full=confint(mod_full))
  
  # give frequency band specific name
  assign(paste0(band[i],"_model_assess"),model_assess)
  
  ## EFFECTS EVALUATION
  
  R2_ICC$Band[i] = band[i]
  
  # full model 
  R2_ICC$R2_m_full[i] = r2_nakagawa(mod_full)$R2_marginal
  R2_ICC$ICC_full[i]  = icc(mod_full)$ICC_adjusted
  
  # age model 
  R2_ICC$R2_m_age[i] = r2_nakagawa(mod_age)$R2_marginal
  R2_ICC$ICC_age[i]  = icc(mod_age)$ICC_adjusted
  
  # sex model
  R2_ICC$R2_m_sex[i] = r2_nakagawa(mod_sex)$R2_marginal
  R2_ICC$ICC_sex[i]  = icc(mod_sex)$ICC_adjusted
  
}

# convert effects stats to % and reduce dp
R2_ICC = R2_ICC %>% mutate_if(is.numeric, round, digits=4)
R2_ICC = R2_ICC %>% mutate_if(is.numeric, function(x) x*100)

# get rid of things which now just correspond to gamma (last in loop)
rm(mod_null,mod_age,mod_sex,mod_full,model_assess)


#### INVESTIGATING ALPHA INTERACTION ###########################################

alpha_full = lmer(alphaBP ~ Age + Sex + (1|Hospital), data = BPdata_full)
alpha_interact = lmer(alphaBP ~ Age + Sex + Age*Sex + (1|Hospital), data = BPdata_full)

compare_performance(alpha_full, alpha_interact,estimator="ML", metrics = c("AIC","BIC"))[,c(1,3,5)]
anova(alpha_full,alpha_interact)
confint(alpha_full)
confint(alpha_interact)

