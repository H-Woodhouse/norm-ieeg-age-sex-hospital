################################################################################

## Script to plot 3 example sites to demonstrate the site intercept offset and
## lack of sex effect visually


#### PRELIMINARIES #############################################################

# packages
library(tidyverse)     # data frames & ggplot 
library(lme4)          # linear mixed models
library(sjPlot)        # plotting lme4 results
library(RColorBrewer)  # more colour palettes
library(ggpubr)        # arranging plots
theme_set(theme_classic())

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata = read.csv("Data/Preprocessing/ROI1_RBP_full.csv")   

# keeping three reasonably large sites with similar age range
ages = BPdata %>% filter(Hospital %in% c("UCLH", "RAMM","RAMJ"))  %>%  select(Pat_ID,Hospital,Age) %>% distinct()
# check
ggplot(aes(x=Age,fill=Hospital),data=ages) + geom_histogram(binwidth = 1)
ggplot(aes(x=Age),data=ages) + geom_histogram(binwidth = 1) + facet_grid(vars(Hospital))



#### MODELLING & PLOTTING MAIN #################################################

# model on whole brain, using the bands optimal model & all sites
age_mod_del = lmer(deltaBP ~ Age + (1|Hospital), data = BPdata)

# if plot using fitted values, don't extend past site age range, not perfectly straight
# if plot using plot_model, points have to be on top of lines -> manual ablines

# correct hospitals for geom point and drop extreme value (amongst this hosp subset)
BPdata_hosp_vis = BPdata %>% 
  filter(deltaBP<0.5) %>% 
  filter(Hospital %in% c("UCLH", "RAMM","RAMJ"))

cols = brewer.pal(3, "Accent")
coefs = coef(age_mod_del)[[1]]

ggplot(data = BPdata_hosp_vis) + 
  geom_point(aes(x=Age,y=deltaBP,col=Hospital), alpha=0.4) +
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
age_mod_del = lmer(deltaBP ~ Age + (1|Hospital), data = BPdata)
age_mod_the = lmer(thetaBP ~ Age + (1|Hospital), data = BPdata)
age_mod_alp = lmer(alphaBP ~ Age + (1|Hospital), data = BPdata)
age_mod_bet = lmer(betaBP  ~ Age + (1|Hospital), data = BPdata)
age_mod_gam = lmer(gammaBP ~ Age + (1|Hospital), data = BPdata)

# each plot
del = plot_model(age_mod_del, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
      theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*delta*")"))
the = plot_model(age_mod_the, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1, legend.position = "none") + ylab(expression("RBP("*theta*")"))
alp = plot_model(age_mod_alp, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*alpha*")"))
bet = plot_model(age_mod_bet, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*beta*")"))
gam = plot_model(age_mod_gam, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "", colors = c15) +
  theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*gamma*")"))

# experiment to add legend as 6th plot to fill white space
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

BPdata_hosp_vis %>% filter(Age==24 & Sex=="M") %>% select(Pat_ID,Hospital,Age,Sex) %>% distinct()


#### SEX EFFECT ################################################################

full_mod_del = lmer(deltaBP ~ Age + Sex + (1|Site), data = BPdata_p)
full_mod_the = lmer(thetaBP ~ Age + Sex + (1|Site), data = BPdata_p)

plot_model(full_mod_del, type="pred", terms=c("Age","Sex"),ci.lvl = NA, title="") +
  geom_point(aes(x=Age,y=deltaBP,col=Sex),data=BPdata_p, alpha=0.6,inherit.aes = FALSE) +
  theme(aspect.ratio = 1, legend.position = "top") + ylab(expression("RBP("*delta*")"))
