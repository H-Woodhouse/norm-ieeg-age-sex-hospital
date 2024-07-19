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



#### MODELLING & PLOTTING DELTA TO VISUALISE HOSPITAL EFFECT ###################

# keeping three reasonably large sites with similar age range
ages = BPdata %>% filter(Hospital %in% c("UCLH", "RAMM","RAMJ"))  %>%  select(Pat_ID,Hospital,Age) %>% distinct()
# check
ggplot(aes(x=Age,fill=Hospital),data=ages) + geom_histogram(binwidth = 1)
ggplot(aes(x=Age),data=ages) + geom_histogram(binwidth = 1) + facet_grid(vars(Hospital))

# model on whole brain, using the bands optimal model & all sites
age_mod_del = lmer(deltaBP ~ Age + (1|Hospital), data = BPdata)

# extract coefs
age_model_coefs = coef(age_mod_del)$Hospital %>% 
  rename(Intercept = `(Intercept)`, Slope = Age) %>% 
  rownames_to_column("Hospital")

# suitable df for plotting, the retain selected hospitals
BPdata_hosp_vis = left_join(BPdata,age_model_coefs,by="Hospital") %>% 
  filter(deltaBP<0.5) %>% 
  filter(Hospital %in% c("UCLH", "RAMM","RAMJ")) %>%
  select(c(Hospital, Age, Sex, Intercept, Slope, deltaBP))

# plot
cols = brewer.pal(3, "Accent")
pdf("Output/hospital_effects.pdf",width=6,height = 6)
ggplot(aes(x=Age, y=deltaBP, col=Hospital), data=BPdata_hosp_vis) +
  geom_point(na.rm = T, alpha = 0.75) +
  geom_abline(aes(intercept=Intercept, slope=Slope, colour=Hospital),lwd = 1.5) +
  theme(legend.position = "none", aspect.ratio = 1) + ylab(expression("RBP("*delta*")")) +
  scale_color_manual(values = cols)
dev.off()


# segments to find and add to plot
BPdata_hosp_vis %>% filter(Age==33 & Sex=="M") %>% select(Pat_ID,Hospital,Age,Sex) %>% distinct()



#### MODELLING & PLOTTING SUPPLEMENTARY ########################################

# extension to all sites and bands 

# 15 colour palette - retain same for Jefferson, UCL, Mayo
c15 = c("dodgerblue2","#E31A1C","green4","#6A3D9A","#FF7F00","black", "#7fc97f","#beaed4",
        "#FB9A99","gold2", "gray70", "palegreen2","#00ffff", "#FDC086", "maroon")

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



#### SEX EFFECT -- OPTION 1 ####################################################

# bands
bands = c("delta","theta","alpha","beta","gamma")
fb_colors=c("#3366ff","#bf80ff","#ff00bf","#3de600","#00cccc")
fb_lab=c(expression(delta),expression(theta),expression(alpha),expression(beta),expression(gamma))

# models
full_mod_del = lmer(deltaBP ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_the = lmer(thetaBP ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_alp = lmer(alphaBP ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_bet = lmer(betaBP  ~ Age + Sex + (1|Hospital), data = BPdata)
full_mod_gam = lmer(gammaBP ~ Age + Sex + (1|Hospital), data = BPdata)

# fitted
del_pred=predict_response(full_mod_del, terms = c("Sex"))
the_pred=predict_response(full_mod_the, terms = c("Sex"))
alp_pred=predict_response(full_mod_alp, terms = c("Sex"))
bet_pred=predict_response(full_mod_bet, terms = c("Sex"))
gam_pred=predict_response(full_mod_gam, terms = c("Sex"))

# data frame
sex_mod_coefs = data.frame(matrix(ncol=3,nrow=10))
colnames(sex_mod_coefs) = c("Sex", "band", "pred")
sex_mod_coefs$Sex = rep(c("M","F"),5)
sex_mod_coefs$band = rep(bands,each=2)

# add fitted
sex_mod_coefs$pred[1:2]=del_pred$predicted
sex_mod_coefs$pred[3:4]=the_pred$predicted
sex_mod_coefs$pred[5:6]=alp_pred$predicted
sex_mod_coefs$pred[7:8]=bet_pred$predicted
sex_mod_coefs$pred[9:10]=gam_pred$predicted

sex_mod_coefs$band = factor(sex_mod_coefs$band, levels = bands)

ggplot(aes(x=Sex,y=pred, group=band, col=band),col=band, data=sex_mod_coefs) + 
  geom_point() + geom_line() + ylab("RBP( )") + scale_color_manual(values = fb_colors, labels=fb_lab) +
  theme(legend.title=element_blank(), legend.position = c(0.1,0.2))


#### SEX EFFECT -- OPTION 2 ####################################################


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

#del_sex = plot_model(full_mod_del, type="pred", terms=c("Age","Sex"),ci.lvl = NA, title="") +
#  geom_point(aes(x=Age,y=deltaBP,col=Sex),data=BPdata, alpha=0.2,inherit.aes = FALSE) +
#  theme(aspect.ratio = 1, legend.position = "top") + ylab(expression("RBP("*delta*")"))

#plot(del_pred, show_ci = F, show_data = T)

pdf("Output/sex_effect.pdf",width = 8, height = 2)
ggarrange(del_sex,the_sex,alp_sex,bet_sex,gam_sex, ncol=5, nrow=1, common.legend = T,legend = "top")
dev.off()

