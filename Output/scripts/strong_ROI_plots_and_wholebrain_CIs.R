################################################################################

## Script to 'zoom in' on a couple of well populated ROIs to show how RBP is 
## changing with age
## Also but plot the 95% CIs on b_age for each band at the whole brain level
## Combine these plots for 3 out of 4 pannels involved in Figure 2



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)   # data frames & ggplot 
library(lme4)        # linear mixed models
library(grid)        # combining figures 

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_p = read.csv("Data/ROI1_relBP_pooled.csv")     # for well populated ROIs
CIs = read.csv("Output/age_wholebrain_stats.csv")     # for whole brain CIs


# chosen ROIs: want one +ve and one -ve relationship with RBP & age
# also one cortical and one subcortical

# looking to my ROI-level analysis, will take hippocampus in alpha (+ve)
# this is the highest populated subcortical region 
alp_HP = BPdata_p %>%
  filter(ROI_R==12) %>%
  select(Site, Age, alphaBP)

# take middle temporal in delta (-ve) highest populated coritcal region
del_MT = BPdata_p %>%
  filter(ROI_R==62) %>%
  select(Site, Age, deltaBP)

# plot settings (ROI plots)
theme_set(theme_classic(base_size = 32))
options(scipen=999) # no sci notation

# greek letters
fb_lab=c(expression(delta),expression(theta),expression(alpha),expression(beta),expression(gamma))

# clear
rm(BPdata_p)


#### MODEL AND PLOT MID TEMP ALPHA #############################################

# plotting model directly uses ggplot uses OLS - no grouping/random effect

# fit the LMM & extract coeffs
alp_HP_LMM = lmer(alphaBP~Age+(1|Site), data=alp_HP)
alp_HP_LMM_coef = summary(alp_HP_LMM)$coefficients

# predict marginal/fixed effect values
alp_HP = alp_HP %>% 
  mutate(fit.age = predict(alp_HP_LMM, re.form = NA))

# plot
b_alp = round(alp_HP_LMM_coef["Age","Estimate"],6) 
HPfig = alp_HP %>%
  ggplot(aes(x = Age, y = alphaBP)) +
  geom_point(col = "#D9D9D9", size=2) +
  geom_line(aes(y = fit.age), col = "#FFB6B6", linewidth = 3) +
  ylab(expression("RBP("*alpha*")")) +
  annotate("text", x=15,y=0.24, col="#FFB6B6",size=12, label=deparse1(bquote(italic(hat(b))[age]==.(b_alp))),parse=T)


#### MODEL AND PLOT HIPPOCAMPUS DELTA ##########################################

# fit the LMM & extract coeffs
del_MT_LMM = lmer(deltaBP~Age+(1|Site), data=del_MT)
del_MT_LMM_coef = summary(del_MT_LMM)$coefficients

# predict marginal/fixed effect values
del_MT = del_MT %>% 
  mutate(fit.age = predict(del_MT_LMM, re.form = NA))

# plot
b_del = round(del_MT_LMM_coef["Age","Estimate"],6)
MTfig = del_MT %>%
  ggplot(aes(x = Age, y = deltaBP)) +
  geom_point(col = "#D9D9D9", size=2) +
  geom_line(aes(y = fit.age), col = "#3A9CFF", linewidth = 3) +
  ylab(expression("RBP("*delta*")")) +
  annotate("text", x=15,y=0.45, col="#3A9CFF",size=12, label=deparse1(bquote(italic(hat(b))[age]==.(b_del))),parse=T)



#### CONFIDENCE INTERVAL PLOT (WHOLE BRAIN) ####################################


# create factor for whole brain
CIs = CIs %>% mutate(Band=factor(Band, levels=Band))

# plot
CIfig = ggplot(data = CIs) +
  geom_vline(xintercept = 0, linewidth = 2, color = "white") +
  geom_vline(xintercept = 0, linetype = 2, col="gray30") +
  geom_segment(mapping = aes(x=lower95ci,xend=upper95ci,y=Band, yend=Band), linewidth=2,lineend="butt") +
  ylab(NULL) + xlab(expression(italic(hat(b)[age]))) + scale_y_discrete(limits=rev, label=rev(fb_lab)) + theme_minimal(base_size = 32) +
  theme(panel.grid.major.y = element_blank(),axis.title.x=element_text(margin=margin(t=10)),axis.text.y = element_text(size=32)) + 
  scale_x_continuous(breaks=c(-0.0008,-0.0004,0,0.0004,0.0008),limits = c(-0.0008,0.0008)) +
  geom_point(aes(x=coeff,y=Band),size=7) 


  
#### COMBINE ###################################################################

grid.newpage()
pdf("Output/wholebrain_CIs_and_MT_HP_scatter.pdf",width = 23.44, height = 39.75)
grid.draw(rbind(ggplotGrob(MTfig),ggplotGrob(HPfig),ggplotGrob(CIfig)))
dev.off()



#### CHECKING ROI PLOTS ########################################################

# sanity check alpha !
#alp_HP %>%
#  ggplot(aes(x = Age, y = alphaBP)) +
#  geom_point(pch = 16, col = "grey") +
#  geom_abline(intercept=alp_HP_LMM_coef["(Intercept)","Estimate"],
#              slope = alp_HP_LMM_coef["Age","Estimate"])


# sanity check delta !
#del_MT %>%
#  ggplot(aes(x = Age, y = deltaBP)) +
#  geom_point(pch = 16, col = "grey") +
#  geom_abline(intercept=del_MT_LMM_coef["(Intercept)","Estimate"],
#              slope = del_MT_LMM_coef["Age","Estimate"])

