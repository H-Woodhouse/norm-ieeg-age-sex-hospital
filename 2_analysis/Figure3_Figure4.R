################################################################################

## Script to to produce figures to demonstrate effect importance visually
##
## FIGURE 3:
## Fit the age model in an example band (delta) then pull out 3 example
## hospitals (UCL, Jefferson, Mayo) to demonstrate the hospital offset
## 
## FIGURE 4:
## Fit the full model in all bands then plot fitted line for each sex to
## demonstrate lack of sex effect



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)     # data frames & ggplot 
library(lme4)          # linear mixed models
library(ggeffects)     # for plotting (predict_response function)
library(RColorBrewer)  # more colour palettes
library(ggpubr)        # arranging plots
theme_set(theme_classic())

# set working directory to script location (2_analysis)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# folder for storing results (included in all scripts with outputs -> warnings off)
dir.create("../3_output", showWarnings = F)

# data
BPdata = read.csv("../1_data/ROI1_wholebrain_RBP.csv") 

band=c("delta","theta","alpha","beta","gamma")



#### PLOTTING HOSPITAL EFFECT (FIGURE 3) #######################################

# run the age model in delta and extract regression coefficients
age_mod_del = lmer(deltaBP ~ Age + (1|Hospital), data = BPdata)
age_model_coefs = coef(age_mod_del)$Hospital %>% 
  rename(Intercept = `(Intercept)`, Slope = Age) %>% 
  rownames_to_column("Hospital")


# keeping three reasonably well populated hospitals with similar age range
ages = BPdata %>% filter(Hospital %in% c("UCLH", "RAMM","RAMJ")) %>%
  select(Pat_ID,Hospital,Age) %>%
  distinct()
# check
ggplot(aes(x=Age,fill=Hospital),data=ages) + geom_histogram(binwidth = 1)
ggplot(aes(x=Age),data=ages) + geom_histogram(binwidth = 1) + facet_grid(vars(Hospital))


# suitable data frame for plotting
# include data and model & retain only selected hospitals
# for visualisation purposes, drop singular point that is an outlier when using
# only these hospitals
hosp_effects_df = left_join(BPdata,age_model_coefs,by="Hospital") %>% 
  filter(deltaBP<0.5) %>% 
  filter(Hospital %in% c("UCLH", "RAMM","RAMJ")) %>%
  select(c(Hospital, Pat_ID, Age, Sex, Intercept, Slope, deltaBP))

# plot
hosp_effects_plot =
  ggplot(aes(x=Age, y=deltaBP, col=Hospital), data=hosp_effects_df) +
  geom_point(na.rm = T, alpha = 0.75) +
  geom_abline(aes(intercept=Intercept, slope=Slope, colour=Hospital),lwd = 1.5) +
  theme(legend.position = "top", aspect.ratio = 1) + ylab(expression("RBP("*delta*")")) +
  scale_color_manual(values = brewer.pal(3, "Accent"), labels=c("Jefferson Hosp.","Mayo Clinic","University College London Hosp."))

# view
hosp_effects_plot

#save
pdf("../3_output/hospital_effect.pdf",width=6,height = 6)
hosp_effects_plot
dev.off()


# for publication, three example icEEG segments will be shown next to the plot
# (added in Illustrator) to complete figure 3
# the below finds their subject IDs (looking for same age/sex)
hosp_vis_df %>% filter(Age==33 & Sex=="M") %>% select(Pat_ID,Hospital,Age,Sex) %>% distinct()

rm(age_model_coefs, ages, age_mod_del)


#### PLOTTING SEX EFFECT #######################################################


# run the full model in all bands and extract fitted values
for (i in 1:length(band)) {
  
  # band-specific model
  mod = formula(paste0(band[i],"BP~Age+Sex+(1|Hospital)"))
  model = lmer(mod, data = BPdata)
  
  # fitted values
  pred = predict_response(model, terms = c("Age", "Sex")) %>% rename(Age=x, Sex=group)
  
  # rename
  assign(paste0(band[i],"_pred"),pred)
  
}

rm(mod,i,pred,model,band)


# function to plot fitted line per sex overlaid on data points in one band 
sex_plot = function(RBP_band,label,pred_df) {
  ggplot(aes(y=.data[[RBP_band]],x=Age, col=Sex),data=BPdata,) +
    ylab(substitute("RBP("*label*")")) +
    geom_point(alpha=0.05, size=0.5) +
    geom_line(aes(x=Age,y=predicted,group=Sex,col=Sex),lwd=1, data=pred_df, inherit.aes = F) +
    theme(legend.position = "none", aspect.ratio = 1) +
    scale_color_manual(values=c("hotpink1","cornflowerblue"))
}


# combine to create figure for all bands with shared legend
sex_effects_plot = 
  ggarrange(sex_plot("deltaBP",delta,delta_pred),
            sex_plot("thetaBP",theta,theta_pred),
            sex_plot("alphaBP",alpha,alpha_pred),
            sex_plot("betaBP" ,beta ,beta_pred ),
            sex_plot("gammaBP",gamma,gamma_pred),
            ncol=5, nrow=1, common.legend = T,legend = "top", align = "v")

# view
sex_effects_plot

# save
pdf("../3_output/sex_effect.pdf",width = 8, height = 2)
sex_effects_plot
dev.off()
