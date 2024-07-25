################################################################################

# Script to check that the age range in each ROI is not affecting results
# also that there is no correlation between our normative values and other
# features such as age of onset
# For Supplementary



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)            # data frames & ggplot
library(ggpubr)               # add stats (corr coef) to ggplot
library(gridExtra)            # combining ggplots
library(lme4)                 # mixed models

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_f = read.csv("Data/Preprocessing/ROI1_RBP_full.csv")
BPdata_m = read.csv("Data/Preprocessing/ROI1_RBP_mirrored.csv")
age_coeffs = read.csv("Output/age_ROI_stats.csv")

band = c("delta","theta","alpha","beta","gamma")



#### AGE RANGE CHECK - DATA FRAME CREATION #####################################

# calculate 0.05 and 0.95 percentiles and subtract to give rang/spread of
# central 90% of ages
ROI_ages = BPdata_m %>% 
  group_by(ROI_R) %>%
  reframe(age_range_l=quantile(Age,0.1), age_range_u=quantile(Age,0.9),
          inter90range = age_range_u-age_range_l) %>%
  select(ROI_R,inter90range)

# select just coefs and ROI Index from ROI_stats df
age_coeffs = age_coeffs %>%
  select(ROI_index,paste0(band,"_coef")) %>%
  rename(ROI_R=ROI_index)

# combine
ROI_ages = left_join(ROI_ages,age_coeffs)



#### AGE RANGE CHECK - PLOTTING ################################################

# plot settings
options(scipen=999)           # no sci notation
theme_set(theme_minimal(base_size = 20))

# additions to all plots
gg_all = list(
  geom_point(), xlab("10-90th percentile age range"), ylab(expression(italic(hat(b)[age]))),
  theme(plot.title = element_text(hjust = 0.5),panel.grid.minor.y = element_blank())
)

# each band 
del=ggplot(data = ROI_ages, aes(x=inter90range,y=delta_coef)) + ggtitle(expression(delta*"-band")) +
  stat_cor(method="spearman", label.x=37, label.y=-0.0017) + gg_all

the=ggplot(data = ROI_ages, aes(x=inter90range,y=theta_coef)) + ggtitle(expression(theta*"-band")) +
  stat_cor(method="spearman", label.x=37, label.y=-0.0006) + gg_all

alp=ggplot(data = ROI_ages, aes(x=inter90range,y=alpha_coef)) + ggtitle(expression(alpha*"-band")) +
  stat_cor(method="spearman", label.x=34, label.y=0.00015) + gg_all

bet=ggplot(data = ROI_ages, aes(x=inter90range,y=beta_coef)) + ggtitle(expression(beta*"-band")) +
  stat_cor(method="spearman", label.x=37, label.y=0.0009) + gg_all

gam=ggplot(data = ROI_ages, aes(x=inter90range,y=gamma_coef)) + ggtitle(expression(gamma*"-band")) +
  stat_cor(method="spearman", label.x=37, label.y=0.0007) + gg_all

# combine
pdf("Output/sup-results/ROI_ages_SUP.pdf", width=15,height=13)
grid.arrange(del,the,alp,bet,gam)
dev.off()



#### COVARIATE CHECK ###########################################################

# extract uclh (has all additional features)
uclh = BPdata_f %>% filter(Hospital=="UCLH")

# empty df
corr_check = tibble(band=band,Age_onset_corr=NA,Age_onset_p=NA, type_p=NA, lobe_p=NA, side_p=NA, pathology_p=NA)

# extract corr and p vlaues
for (i in 1:5) {
  
  # age onset is not normal -> spearmans
  corr_check[i,"Age_onset_corr"]=cor.test(uclh[,paste0(band[i],"BP")], uclh$Age_onset, method = "spearman", exact = F)$estimate
  corr_check[i,"Age_onset_p"]   =cor.test(uclh[,paste0(band[i],"BP")], uclh$Age_onset, method = "spearman", exact = F)$p.value
  
  # kruskal test for categorical
  # type
  type = formula(paste0(band[i],"BP~op_type"))
  corr_check[i,"type_p"] = kruskal.test(type, data = uclh)$p.value
  
  # lobe
  lobe = formula(paste0(band[i],"BP~op_lobe"))
  corr_check[i,"lobe_p"] = kruskal.test(lobe, data = uclh)$p.value
  
  # side
  side = formula(paste0(band[i],"BP~op_side"))
  corr_check[i,"side_p"] = kruskal.test(side, data = uclh)$p.value
  
  # pathology
  pathology = formula(paste0(band[i],"BP~op_pathology"))
  corr_check[i,"pathology_p"] = kruskal.test(pathology, data = uclh)$p.value

}

ggplot(data = uclh, aes(x=op_type,y=deltaBP)) + geom_violin()



#### CORR CHECK 2 ##############################################################

BPdata_onset = BPdata_f %>% drop_na(Age_onset)

del_age = lmer(gammaBP~Age_onset+(1|Hospital),data=BPdata_onset, REML = F)
del_onset = lmer(gammaBP~Age+Age_onset+(1|Hospital),data=BPdata_onset, REML = F)

anova(del_age,del_onset)
cor.test(BPdata_onset$Age,BPdata_onset$Age_onset)
