################################################################################

# Script to 'zoom in' on a couple of well populated ROIs to show how RBP is 
# changing with age



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)   # data frames & ggplot 
library(lme4)        # linear mixed models

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_p = read.csv("Data/ROI1_relBP_pooled.csv")

# chosen ROIs: want one +ve and one -ve relationship with RBP & age
# also one cortical and one subcortical

# looking to my ROI-level analysis, will hippocampus in alpha (+ve)
# this is the highest populated subcortical region 
alp_HP = BPdata_p %>%
  filter(ROI_R==12) %>%
  select(Site, Age, alphaBP)

# take middle temporal in delta (-ve) highest populated coritcal region
del_MT = BPdata_p %>%
  filter(ROI_R==62) %>%
  select(Site, Age, deltaBP)

# plot settings
theme_set(theme_classic(base_size = 24))


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
pdf("Output/alpha_hipp.pdf", width=11,height=7)
alp_HP %>%
  ggplot(aes(x = Age, y = alphaBP)) +
  geom_point(col = "#D9D9D9") +
  geom_line(aes(y = fit.age), col = "#FFB6B6", linewidth = 1.5) +
  ylab(expression("RBP("*alpha*")")) +
  annotate("text", x=15,y=0.24, col="#FFB6B6",size=6, label=deparse1(bquote(italic(hat(b))[age]==.(b_alp))),parse=T)
dev.off()


#### MODEL AND PLOT HIPPOCAMPUS DELTA ##########################################

# fit the LMM & extract coeffs
del_MT_LMM = lmer(deltaBP~Age+(1|Site), data=del_MT)
del_MT_LMM_coef = summary(del_MT_LMM)$coefficients

# predict marginal/fixed effect values
del_MT = del_MT %>% 
  mutate(fit.age = predict(del_MT_LMM, re.form = NA))

# plot
b_del = round(del_MT_LMM_coef["Age","Estimate"],6)
pdf("Output/delta_midtemp.pdf", width=11,height=7)
del_MT %>%
  ggplot(aes(x = Age, y = deltaBP)) +
  geom_point(col = "#D9D9D9") +
  geom_line(aes(y = fit.age), col = "#3A9CFF", linewidth = 1.5) +
  ylab(expression("RBP("*delta*")")) +
  annotate("text", x=15,y=0.45, col="#3A9CFF",size=6, label=deparse1(bquote(italic(hat(b))[age]==.(b_del))),parse=T)
dev.off()



#### CHECKING ##################################################################

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

