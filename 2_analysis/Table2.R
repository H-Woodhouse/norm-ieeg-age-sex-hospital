################################################################################

## Script to fit Age only model with random hosp effect on the whole brain level
## 
## TABLE 2:
## Extract and save regression coefficients and confidence intervals



##### PRELIMINARIES ############################################################

# packages
library(lme4)      # mixed effect models

# set working directory to script location (2_analysis)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# folder for storing results (included in all scripts with outputs -> warnings off)
dir.create("../3_output", showWarnings = F)

# full (not-mirrored) data 
BPdata_full=read.csv("../1_data/ROI1_wholebrain_RBP.csv")

# bands
band = c("delta", "theta", "alpha", "beta", "gamma")



#### MODELLING AGE AND EXTRACTING STATS ########################################

# empty data frame to fill with statistics
age_stats=data.frame(matrix(nrow=5,ncol=4))
colnames(age_stats)=c("Band","coeff","lower95ci","upper95ci")

# run model and store statistics
for (i in 1:5) {
  
  print(paste0("Fitting model for ", band[i], " (band ", i, "/5)"))
  
  # model formula for particular FB
  model = formula(paste0(band[i],"BP~Age+(1|Hospital)"))
  
  # model
  age_mod = lmer(model, data = BPdata_full)
  
  # band
  age_stats[i,"Band"] = band[i]
  
  # extract coefs
  age_stats[i,"coeff"] = coef(age_mod)$Hospital[1,"Age"]
  
  # extract 95% CI
  CI = confint(age_mod)["Age",]
  age_stats[i,"lower95ci"] = CI[1]
  age_stats[i,"upper95ci"] = CI[2]
  
}

rm(CI,i,model,age_mod)


#### SAVE ######################################################################

# save resulting table 
write.csv(age_stats,"../3_output/wholebrain_age_model_stats.csv", row.names = F)



