################################################################################

## Script to plot 3 example hospitals to demonstrate the hospital intercept 
## offset and demonstrate lack of sex effect visually



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)     # data frames & ggplot 
library(lme4)          # linear mixed models
library(sjPlot)        # plotting lme4 results
library(ggeffects)     # for plotting
library(RColorBrewer)  # more colour palettes
library(ggpubr)        # arranging plots
theme_set(theme_classic())

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-hospital")
BPdata = read.csv("1_data/ROI1_wholebrain_RBP.csv")   

# run the age model in all fb
age_mod_del = lmer(deltaBP ~ Age + (1|Hospital), data = BPdata)
age_mod_the = lmer(thetaBP ~ Age + (1|Hospital), data = BPdata)
age_mod_alp = lmer(alphaBP ~ Age + (1|Hospital), data = BPdata)
age_mod_bet = lmer(betaBP  ~ Age + (1|Hospital), data = BPdata)
age_mod_gam = lmer(gammaBP ~ Age + (1|Hospital), data = BPdata)

# run the full model in all fb
full_mod_del = lmer(deltaBP ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_the = lmer(thetaBP ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_alp = lmer(alphaBP ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_bet = lmer(betaBP  ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_gam = lmer(gammaBP ~ Age + Sex + (1|Hospital), data = BPdata)



#### PLOTTING HOSPITAL EFFECT (DELTA AND 3 EXAMPLE HOSPS.) #####################

# keeping three reasonably large sites with similar age range
ages = BPdata %>% filter(Hospital %in% c("UCLH", "RAMM","RAMJ"))  %>%  select(Pat_ID,Hospital,Age) %>% distinct()
# check
ggplot(aes(x=Age,fill=Hospital),data=ages) + geom_histogram(binwidth = 1)
ggplot(aes(x=Age),data=ages) + geom_histogram(binwidth = 1) + facet_grid(vars(Hospital))
rm(ages)

# extract coefs
age_model_coefs = coef(age_mod_del)$Hospital %>% 
  rename(Intercept = `(Intercept)`, Slope = Age) %>% 
  rownames_to_column("Hospital")

# suitable df for plotting, the retain selected hospitals
# for visualisation purpose drop point that's outlier when using only these hospitals
BPdata_hosp_vis = left_join(BPdata,age_model_coefs,by="Hospital") %>% 
  filter(deltaBP<0.5) %>% 
  filter(Hospital %in% c("UCLH", "RAMM","RAMJ")) %>%
  select(c(Hospital, Pat_ID, Age, Sex, Intercept, Slope, deltaBP))
rm(age_model_coefs)

# plot
cols = brewer.pal(3, "Accent")
pdf("3_output/hospital_effect.pdf",width=6,height = 6)
ggplot(aes(x=Age, y=deltaBP, col=Hospital), data=BPdata_hosp_vis) +
  geom_point(na.rm = T, alpha = 0.75) +
  geom_abline(aes(intercept=Intercept, slope=Slope, colour=Hospital),lwd = 1.5) +
  theme(legend.position = "none", aspect.ratio = 1) + ylab(expression("RBP("*delta*")")) +
  scale_color_manual(values = cols)
dev.off()


# segments to find and add to plot
BPdata_hosp_vis %>% filter(Age==33 & Sex=="M") %>% select(Pat_ID,Hospital,Age,Sex) %>% distinct()



#### PLOTTING HOSPITAL EFFECT (ALL HOPSITALS AND BANDS, SUPPLEMENTARY) #########

# extension to all sites and bands 

# full hosp names
names_full = c("Beijing Tiantan Hosp.", "Great Ormond St. Hosp.", "University of Iowa Hosp.","Columbia University*",
              "Dartmouth University*","Emory University*","Jefferson Hosp.*","Mayo Clinic*",
              "NINDS*","University of Pennsylvania*","UT Southwestern*","University of Washington*",
              "SickKids", "University College London Hosp.", "University of Wisconsin-Madison")


# each plot
del = plot_model(age_mod_del, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "") +
      theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*delta*")")) + scale_color_discrete(labels=names_full)
the = plot_model(age_mod_the, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "") +
    theme(aspect.ratio = 1, legend.position = "none") + ylab(expression("RBP("*theta*")")) + scale_color_discrete(labels=names_full)
alp = plot_model(age_mod_alp, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "") +
    theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*alpha*")")) + scale_color_discrete(labels=names_full)
bet = plot_model(age_mod_bet, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "") +
    theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*beta*")")) + scale_color_discrete(labels=names_full)
gam = plot_model(age_mod_gam, type="pred", terms=c("Age","Hospital"),ci.lvl = NA, pred.type = "re", title = "") +
    theme(aspect.ratio = 1,legend.position = "none") + ylab(expression("RBP("*gamma*")")) + scale_color_discrete(labels=names_full)

ggarrange(del,the,alp,bet,gam, ncol=2, nrow=3, common.legend = T,legend = "left")



#### PLOTTING SEX EFFECT #######################################################


del_pred=predict_response(full_mod_del, terms = c("Age", "Sex")) %>% rename(Age=x, Sex=group)
the_pred=predict_response(full_mod_the, terms = c("Age", "Sex")) %>% rename(Age=x, Sex=group)
alp_pred=predict_response(full_mod_alp, terms = c("Age", "Sex")) %>% rename(Age=x, Sex=group)
bet_pred=predict_response(full_mod_bet, terms = c("Age", "Sex")) %>% rename(Age=x, Sex=group)
gam_pred=predict_response(full_mod_gam, terms = c("Age", "Sex")) %>% rename(Age=x, Sex=group)

gg_all = list(geom_point(alpha=0.05, size=0.5),theme(legend.position = "none", aspect.ratio = 1),
              scale_color_manual(values=c("hotpink1","cornflowerblue")))

del_sex=ggplot(aes(y=deltaBP,x=Age, col=Sex),data=BPdata) + ylab(expression("RBP("*delta*")")) +
  gg_all + geom_line(aes(x=Age,y=predicted,group=Sex,col=Sex),lwd=1, data=del_pred, inherit.aes = F)
the_sex=ggplot(aes(y=thetaBP,x=Age, col=Sex),data=BPdata) + ylab(expression("RBP("*theta*")")) +
  gg_all + geom_line(aes(x=Age,y=predicted,group=Sex,col=Sex),lwd=1, data=the_pred, inherit.aes = F)
alp_sex=ggplot(aes(y=alphaBP,x=Age, col=Sex),data=BPdata) + ylab(expression("RBP("*alpha*")")) +
  gg_all + geom_line(aes(x=Age,y=predicted,group=Sex,col=Sex),lwd=1, data=alp_pred, inherit.aes = F)
bet_sex=ggplot(aes(y=betaBP,x=Age, col=Sex),data=BPdata) + ylab(expression("RBP("*beta*")")) +
  gg_all + geom_line(aes(x=Age,y=predicted,group=Sex,col=Sex),lwd=1, data=bet_pred, inherit.aes = F)
gam_sex=ggplot(aes(y=gammaBP,x=Age, col=Sex),data=BPdata) + ylab(expression("RBP("*gamma*")")) +
  gg_all + geom_line(aes(x=Age,y=predicted,group=Sex,col=Sex),lwd=1, data=gam_pred, inherit.aes = F)

pdf("3_output/sex_effect.pdf",width = 8, height = 2)
ggarrange(del_sex,the_sex,alp_sex,bet_sex,gam_sex, ncol=5, nrow=1, common.legend = T,legend = "top")
dev.off()

