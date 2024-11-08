################################################################################

## Script to fit and assess linear mixed models at the whole brain level
##
## METHODS 2.5:
## investigate the optimal fixed effect structure of the LMM, either null/none, 
## age, sex, both or interaction
##
## RESULS 3.1-3: 
## quantify the importance of age, sex, hospital for explaining variation in
## log RBP all with hospital as a random effect, age & sex fixed effects



#### PRELIMINARIES #############################################################

# packages 
library(dplyr)         # data frames
library(lme4)          # mixed models
library(performance)   # calculates model comparison stats

# set working directory to script location (2_analysis)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# data
BPdata_full = read.csv("../1_data/ROI1_wholebrain_RBP.csv")



#### METRIC NOTES ##############################################################

# if looking at model on it's own, REML=T, if comparing 2 mods, REML = F

# LRT: high p = no diff between models = additional variable did nothing

# A/BIC, smaller is better, larger difference is better



#### MODEL PERFORMANCE & EFFECT IMPORTANCE ASSESSMENTS #########################

# loop over bands, for each band, fit all models under consideration 
# extract and store model & effects assessments
# model assessments will be stored in list, one per band
# effect assessments will be stored in a data frame 

# effects data frame
R2_ICC = data.frame(matrix(ncol=9,nrow=5))
colnames(R2_ICC) = c("Band","R2_m_int","ICC_int","R2_m_full","ICC_full","R2_m_age","ICC_age","R2_m_sex","ICC_sex")

# for looping
band = c("delta","theta","alpha","beta","gamma")


for (i in 1:length(band)) {
  
  print(paste0("Assessing ", band[i], " (band ", i, "/5)"))
  
  ## MODELS
  
  # model with each possible fixed effects structure
  # REML = F, comparing across structures
  mod_null = lmer(formula(paste0(band[i],"BP~                (1|Hospital)")), data=BPdata_full, REML = F)
  mod_age  = lmer(formula(paste0(band[i],"BP~Age+            (1|Hospital)")), data=BPdata_full, REML = F)
  mod_sex  = lmer(formula(paste0(band[i],"BP~Sex+            (1|Hospital)")), data=BPdata_full, REML = F)
  mod_full = lmer(formula(paste0(band[i],"BP~Age+Sex+        (1|Hospital)")), data=BPdata_full, REML = F)
  mod_int  = lmer(formula(paste0(band[i],"BP~Age+Sex+Age*Sex+(1|Hospital)")), data=BPdata_full, REML = F)
  
  ## MODEL EVALUATION
  
  # list for storing model assessment for this band
  model_assess = list()
  
  # BIC and AIC
  model_assess$IC = compare_performance(mod_null, mod_age, mod_sex, mod_full,mod_int, estimator="ML", metrics = c("AIC","BIC"))[,c(1,3,5)]
  
  # likelihood ratio -- null v effect and effect v full
  model_assess$LRT=list(null_age=anova(mod_null,mod_age),
                        null_sex=anova(mod_null,mod_sex),
                        age_full=anova(mod_age,mod_full),
                        sex_full=anova(mod_sex,mod_full),
                        full_int=anova(mod_full,mod_int))
  
  # profiled confidence interval (excludes null)
  model_assess$CI_prof=list(age=confint(mod_age),sex=confint(mod_sex),full=confint(mod_full),int=confint(mod_int))
  
  # give frequency band specific name
  assign(paste0(band[i],"_model_assess"),model_assess)
  
  ## EFFECTS EVALUATION
  
  R2_ICC$Band[i] = band[i]
  
  # interaction model
  R2_ICC$R2_m_int[i]  = r2_nakagawa(mod_int)$R2_marginal
  R2_ICC$ICC_int[i]   = icc(mod_int)$ICC_adjusted
  
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

# convert effects stats to percentages and reduce decimal places
R2_ICC = R2_ICC %>% mutate_if(is.numeric, round, digits=4)
R2_ICC = R2_ICC %>% mutate_if(is.numeric, function(x) x*100)

# get rid of loop specifics/last loop run
rm(mod_null,mod_age,mod_sex,mod_full,mod_int,model_assess,i,band)


