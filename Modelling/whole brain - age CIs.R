################################################################################

## Script to fit Age only model with random site effect on the whole brain level
## Extract and save regression coefs and CIs ready for plotting


##### PRELIMINARIES ############################################################

# packages
library(lme4)      # mixed effect models

# database ROI level data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site/")
BPdata_full=read.csv("Data/Preprocessing/ROI1_RBP_full.csv")

# bands
band = c("delta", "theta", "alpha", "beta", "gamma")



#### MODELLING AGE AND EXTRACTING STATS ########################################

age_stats=data.frame(matrix(nrow=5,ncol=4))
colnames(age_stats)=c("Band","coeff","lower95ci","upper95ci")

for (i in 1:5) {
  
  # model formula for particular FB
  model = formula(paste0(band[i],"BP~Age+(1|Hospital)"))
  
  # model
  age_mod = lmer(model, data = BPdata_full)
  
  # band
  age_stats[i,"Band"] = band[i]
  
  # extract coefs
  age_stats[i,"coeff"] = coef(age_mod)$Hospital[1,"Age"]
  
  # extract 95% CI
  age_stats[i,"lower95ci"] = confint(age_mod)["Age",][1]
  age_stats[i,"upper95ci"] = confint(age_mod)["Age",][2]
  
}



#### SAVE ######################################################################

# save resulting table 
write.csv(age_stats,"Output/age_wholebrain_stats_new.csv", row.names = F)



