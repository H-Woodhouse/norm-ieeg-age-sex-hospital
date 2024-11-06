################################################################################

## Script to 'zoom in' on the middle temporal region in each band to 
## show how RBP is changing with age



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)   # data frames & ggplot 
library(lme4)        # linear mixed models
library(grid)        # combining figures 

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-hospital")
BPdata_m = read.csv("1_data/ROI1_mirrored_RBP.csv")     


# chosen ROI: will middle temporal, highest populated region 
BPdata_MT = BPdata_m %>%
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
rm(BPdata_m)


#### AGE MODEL IN MIDDLE TEMP FOR EACH FB ######################################

# plotting model directly uses ggplot uses OLS - no grouping/random effect
# diff approach ->

for (i in 1:5) {
  
  # model formula for particular FB
  model = formula(paste0(band[i],"BP~Age+(1|Hospital)"))
  
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
  geom_line(aes(y = AgeFit.gamma),col="black",linewidth=1)

# combine 

pdf("3_output/midtemp_scatter.pdf",width = 8, height = 2)
grid.newpage()
grid.draw(cbind(ggplotGrob(del),ggplotGrob(the),ggplotGrob(alp),ggplotGrob(bet),ggplotGrob(gam)))
dev.off()

