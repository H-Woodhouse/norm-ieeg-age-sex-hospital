################################################################################

## Script to 'zoom in' on the middle temporal region in each band
## 
## FIGURE 5A: 
## show how RBP is changing with age across bands in a densely populated region



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)   # data frames & ggplot 
library(lme4)        # linear mixed models
library(ggpubr)      # arranging plots

# set working directory to script location (2_analysis)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# folder for storing results (included in all scripts with outputs -> warnings off)
dir.create("../3_output", showWarnings = F)

# mirrored RBP data
BPdata_m = read.csv("../1_data/ROI1_mirrored_RBP.csv")     

# plot settings (ROI plots)
theme_set(theme_classic())
options(scipen=999) # no sci notation

# band
band=c("delta","theta","alpha","beta","gamma")



#### AGE MODEL IN MIDDLE TEMP FOR EACH FB ######################################

# function to extract data for one ROI using region label
extract_ROI_data = function(ROI_label) {
  BPdata_m %>%
    filter(ROI_R==ROI_label) %>%
    select(-c(Sex, ROI_R))
}

# chosen ROI: middle temporal (label=62), highest populated region
# to view a different region, find region labels in 1_data/ROI1_info.csv
BPdata_regional = extract_ROI_data(62)

# clear
rm(BPdata_m)

# set up mini table to record if the confidence interval on beta_age contains 0
# in each band
conf_int_0 = tibble(band=band, CI_0=NA)


# plotting model directly uses ggplot uses OLS - no grouping/random effect
# diff approach ->

for (i in 1:5) {
  
  # model formula for particular FB
  model = formula(paste0(band[i],"BP~Age+(1|Hospital)"))
  
  # model
  age_mod = lmer(model, data = BPdata_regional)
  
  # fitted values
  BPdata_regional[,paste0("AgeFit.", band[i])] = predict(age_mod, re.form = NA)
  
  # binary indicator of whether confidence interval on beta_age contains 0
  CI = confint(age_mod)["Age",]
  if (sign(CI[1])!=sign(CI[2])) { 
    conf_int_0[i,]$CI_0 = 0 
  } else if (sign(CI[1])==sign(CI[2])) {
      conf_int_0[i,]$CI_0 = sign(CI[1])
      }
}

rm(CI,i,model)


#### PLOTTING ##################################################################

# function to plot fitted line over data points in each frequency band
regional_plot = function(RBP_band,fit_band,label,linecol) {
  BPdata_regional %>% 
    ggplot(aes(x=Age, y=.data[[RBP_band]])) + ylab(substitute("RBP("*label*")")) +
    geom_point(col = "darkgrey",size=0.5) + theme(aspect.ratio=1) +
    geom_line(aes(y = .data[[fit_band]]),col=linecol,linewidth=1)
}


# check conf_int_0 to choose appropriate line colours
# choosing blue for -1 (negative relationship), red for 1 (positive
# relationship), black for 0 (no relationship) here

# plot for each band and combine
regional_scatter_all_bands =
  ggarrange(regional_plot("deltaBP","AgeFit.delta",delta,"blue"),
            regional_plot("thetaBP","AgeFit.theta",theta,"blue"),
            regional_plot("alphaBP","AgeFit.alpha",alpha,"red"),
            regional_plot("betaBP" ,"AgeFit.beta" ,beta ,"red"),
            regional_plot("gammaBP","AgeFit.gamma",gamma,"black"),
            ncol=5, nrow=1, align = "v")

# view
regional_scatter_all_bands

# save
# change file name if changing region
pdf("../3_output/midtemp_scatter1.pdf",width = 8, height = 2)
regional_scatter_all_bands
dev.off()

