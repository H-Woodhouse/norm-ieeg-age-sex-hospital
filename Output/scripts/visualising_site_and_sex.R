################################################################################

## Script to plot 3 example sites to demonstrate the site intercept offset and
## lack of sex effect visually


#### PRELIMINARIES #############################################################

# packages
library(tidyverse)   # data frames & ggplot 
library(lme4)        # linear mixed models
library(ggpubr)      # arranging plots
theme_set(theme_classic())

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_p = read.csv("Data/ROI1_relBP_pooled.csv")   

# keeping three reasonably large sites with similar age range
BPdata_site_eff = BPdata_p %>% filter(Site %in% c("UCLH", "RAMM","RAMJ")) 
ages = BPdata_site_eff %>%  select(Patient_no,Site,Age) %>% distinct()
# check
ggplot(aes(x=Age,fill=Site),data=ages) + geom_histogram(binwidth = 1)
ggplot(aes(x=Age),data=ages) + geom_histogram(binwidth = 1) + facet_grid(vars(Site))

# drop singular outlier
BPdata_site_eff = BPdata_site_eff %>% filter(deltaBP<0.5)


#### MODELLING & PLOTTING MAIN #################################################

# run on whole brain, using the bands optimal model (all sites?)
# plot varying intercepts

reduced_mod = lmer(deltaBP ~ Age + (1|Site), data = BPdata_site_eff)

cols = brewer.pal(3, "Accent")

# easy but no way to get points on top 
plot_model(reduced_mod, type="pred", terms=c("Age","Site"),ci.lvl = NA, pred.type = "re", title = "") +
  geom_point(aes(x=Age,y=deltaBP,col=Site),data=BPdata_site_eff, alpha=0.6,inherit.aes = FALSE) +
  theme(aspect.ratio = 1, legend.position = "top") + ylab(expression("RBP("*delta*")")) + 
  scale_color_brewer(palette = "Accent", labels=c("Jefferson Hosp.","Mayo Clinic","University College London Hosp."))

# fitted values, dont extend past site age range
BPdata_site_eff$fit = predict(reduced_mod)
ggplot(data = BPdata_site_eff) + 
  geom_point(aes(x=Age,y=deltaBP,col=Site), alpha=0.4) +
  geom_line(aes(x=Age,y=fit,col=Site),lwd=1) +
  theme(aspect.ratio = 1, legend.position = "top") + ylab(expression("RBP("*delta*")")) + 
  scale_color_brewer(palette = "Accent", labels=c("Jefferson Hosp.","Mayo Clinic","University College London Hosp."))

# manual ablines, line not included in legend
coefs = coef(reduced_mod)[[1]]
ggplot(data = BPdata_site_eff) + 
  geom_point(aes(x=Age,y=deltaBP,col=Site), alpha=0.4) +
  geom_abline(intercept = coefs["RAMJ","(Intercept)"],slope=coefs["RAMJ","Age"], lwd=1,col=cols[1]) +
  geom_abline(intercept = coefs["RAMM","(Intercept)"],slope=coefs["RAMM","Age"], lwd=1,col=cols[2]) +
  geom_abline(intercept = coefs["UCLH","(Intercept)"],slope=coefs["UCLH","Age"], lwd=1,col=cols[3]) +
  theme(aspect.ratio = 1, legend.position = "top") + ylab(expression("RBP("*delta*")")) + 
  scale_color_manual(values = cols, labels=c("Jefferson Hosp.","Mayo Clinic","University College London Hosp."))


#### MODELLING & PLOTTING SUPPLEMENTARY ########################################

# extension to all sites and bands 

# 15 colour palette
c15 = c("dodgerblue2","#E31A1C","green4","#6A3D9A","#FF7F00","black", "gold1","skyblue2",
        "#FB9A99","palegreen2", "#CAB2D6", "#FDBF6F","gray70", "khaki2", "maroon")

# each model on all sites
age_mod_del = lmer(deltaBP ~ Age + (1|Site), data = BPdata_p)
age_mod_the = lmer(thetaBP ~ Age + (1|Site), data = BPdata_p)
age_mod_alp = lmer(alphaBP ~ Age + (1|Site), data = BPdata_p)
age_mod_bet = lmer(betaBP ~ Age + (1|Site), data = BPdata_p)
age_mod_gam = lmer(gammaBP ~ Age + (1|Site), data = BPdata_p)

# each plot
del = plot_model(age_mod_del, type="pred", terms=c("Age","Site"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
      theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*delta*")"))
the = plot_model(age_mod_the, type="pred", terms=c("Age","Site"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1, legend.position = "none") + ylab(expression("RBP("*theta*")"))
alp = plot_model(age_mod_alp, type="pred", terms=c("Age","Site"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*alpha*")"))
bet = plot_model(age_mod_bet, type="pred", terms=c("Age","Site"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*beta*")"))
gam = plot_model(age_mod_gam, type="pred", terms=c("Age","Site"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*gamma*")"))

# experiment ot add legend as 6th plot to fill white space
#legend =  ggplot(BPdata_p, aes(x = Age, y = deltaBP, color = Site))+
#  geom_point()+
#  lims(x = c(0,0), y = c(0,0))+
#  theme_void()+
#  theme(legend.position = c(0.5,0.5),
#        legend.key.size = unit(1, "cm"),
#        legend.text = element_text(size =  12),
#        legend.title = element_text(size = 15, face = "bold"))+
#  guides(colour = guide_legend(override.aes = list(size=8))) +
#  scale_color_manual(values = c15)

ggarrange(del,the,alp,bet,gam, ncol=2, nrow=3, common.legend = T,legend = "top")

#### SEGMENTS TO LOOK AT #######################################################

BPdata_site_eff %>% filter(Age==20 & Sex=="M") %>% select(Patient_no,Site,Age,Sex) %>% distinct()


#### SEX EFFECT ################################################################

full_mod_del = lmer(deltaBP ~ Age + Sex + (1|Site), data = BPdata_p)
full_mod_the = lmer(thetaBP ~ Age + Sex + (1|Site), data = BPdata_p)

plot_model(full_mod_del, type="pred", terms=c("Age","Sex"),ci.lvl = NA, title="") +
  geom_point(aes(x=Age,y=deltaBP,col=Sex),data=BPdata_p, alpha=0.6,inherit.aes = FALSE) +
  theme(aspect.ratio = 1, legend.position = "top") + ylab(expression("RBP("*delta*")"))
