################################################################################

## Script to 'zoom in' on MT in each band to show how RBP is changing with age
## Also plot the 95% CIs on b_age for each band at the whole brain level



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)   # data frames & ggplot 
library(lme4)        # linear mixed models
library(grid)        # combining figures 

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_p = read.csv("Data/ROI1_relBP_pooled.csv")     # example ROIs
CIs = read.csv("Output/age_wholebrain_stats.csv")     # for whole brain CIs


# chosen ROI: will middle temporal, highest populated region 
BPdata_MT = BPdata_p %>%
  filter(ROI_R==62) %>%
  select(-c(Sex, ROI_R))


# plot settings (ROI plots)
theme_set(theme_classic())
options(scipen=999) # no sci notation

# band
band=c("delta","theta","alpha","beta","gamma")

# greek letters
fb_lab=c(expression(delta),expression(theta),expression(alpha),expression(beta),expression(gamma))

# clear
rm(BPdata_p)


#### AGE MODEL IN MIDDLE TEMP FOR EACH FB ######################################

# plotting model directly uses ggplot uses OLS - no grouping/random effect

for (i in 1:5) {
  
  # model formula for particular FB
  model = formula(paste0(band[i],"BP~Age+(1|Site)"))
  
  # model
  age_mod = lmer(model, data = BPdata_MT)
  
  # fitted values
  BPdata_MT[,paste0("AgeFit.", band[i])] = predict(age_mod, re.form = NA) 

}



#### PLOTTING ##################################################################

# each fb plot

gg_all = list(geom_point(col = "darkgrey",size=0.5), theme(aspect.ratio=1))

del = BPdata_MT %>%
  ggplot(aes(x = Age, y = deltaBP)) + gg_all + ylab(expression("RBP("*delta*")")) +
  geom_line(aes(y = AgeFit.delta),col="blue",linewidth=1)

the = BPdata_MT %>%
  ggplot(aes(x = Age, y = thetaBP)) + gg_all + ylab(expression("RBP("*theta*")")) +
  geom_line(aes(y = AgeFit.theta),col="blue",linewidth=1)

alp = BPdata_MT %>%
  ggplot(aes(x = Age, y = alphaBP))+ gg_all + ylab(expression("RBP("*alpha*")")) +
  geom_line(aes(y = AgeFit.alpha),col="red",linewidth=1)

bet = BPdata_MT %>%
  ggplot(aes(x = Age, y = betaBP)) + gg_all + ylab(expression("RBP("*beta*")")) +
  geom_line(aes(y = AgeFit.beta),col="red",linewidth=1)

gam = BPdata_MT %>%
  ggplot(aes(x = Age, y = gammaBP)) + gg_all + ylab(expression("RBP("*gamma*")")) +
  geom_line(aes(y = AgeFit.gamma),col="red",linewidth=1)

# combine 

pdf("Output/MT_scatter.pdf",width = 8, height = 2)
grid.newpage()
grid.draw(cbind(ggplotGrob(del),ggplotGrob(the),ggplotGrob(alp),ggplotGrob(bet),ggplotGrob(gam)))
dev.off()


#### CONFIDENCE INTERVAL PLOT (WHOLE BRAIN) ####################################


# create factor for whole brain
CIs = CIs %>% mutate(Band=factor(Band, levels=Band))

# plot
pdf("Output/wholebrain_CIs.pdf",width = 10, height = 7)
ggplot(data = CIs) +
  geom_hline(yintercept = 0, linewidth = 2, color = "white") +
  geom_hline(yintercept = 0, linetype = 2, col="gray30") +
  geom_segment(mapping = aes(y=lower95ci,yend=upper95ci,x=Band, xend=Band), lineend="butt") +
  xlab(NULL) + ylab(expression(italic(hat(b)[age]))) + scale_x_discrete(label=(fb_lab)) + theme_minimal() +
  theme(panel.grid.major.x = element_blank(),axis.title.y=element_text(margin=margin(t=10))) + 
  scale_y_continuous(breaks=c(-0.0008,-0.0004,0,0.0004,0.0008),limits = c(-0.0008,0.0008)) +
  geom_point(aes(y=coeff,x=Band)) 
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

# code for annotating b_age -- currently not used
#alp_HP_LMM_coef = summary(alp_HP_LMM)$coefficients
#b_alp = round(alp_HP_LMM_coef["Age","Estimate"],6) 
# in plot: + annotate("text", x=15,y=0.24, col="#FFB6B6",size=12, label=deparse1(bquote(italic(hat(b))[age]==.(b_alp))),parse=T)

