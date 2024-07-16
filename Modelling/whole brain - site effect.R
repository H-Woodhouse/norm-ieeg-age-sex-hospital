################################################################################

# Quick investigation into site effects, is anything obvious driving differences

################################################################################

# packages
library(lme4)      # mixed effect models
library(ggplot2)   # plots
library(reshape)   # melt data

# database ROI level data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site/")
BPdata_full=read.csv("Data/Preprocessing/ROI1_RBP_full.csv")

# bands
band = c("delta", "theta", "alpha", "beta", "gamma")

# colours
fb_colors=c("#3366ff","#bf80ff","#ff00bf","#3de600","#00cccc")

# empty df
intercepts = data.frame(matrix(nrow = length(unique(BPdata_full$Hospital)), ncol = length(band)))
intercepts$site=unique(BPdata_full$Hospital)
colnames(intercepts)=c(band,"Hospital")

# models 
delta=lmer(deltaBP~Age+(1|Hospital), data=BPdata_full)
theta=lmer(thetaBP~Age+(1|Hospital), data=BPdata_full)
alpha=lmer(alphaBP~Age+(1|Hospital), data=BPdata_full)
beta=lmer(betaBP~Age+(1|Hospital), data=BPdata_full)
gamma=lmer(gammaBP~Age+(1|Hospital), data=BPdata_full)

# site intercepts
intercepts$delta=coef(delta)$Hospital[,"(Intercept)"] - fixef(delta)[1]
intercepts$theta=coef(theta)$Hospital[,"(Intercept)"] - fixef(theta)[1]
intercepts$alpha=coef(alpha)$Hospital[,"(Intercept)"] - fixef(alpha)[1]
intercepts$beta =coef(beta)$Hospital[,"(Intercept)"]  - fixef(beta)[1]
intercepts$gamma=coef(gamma)$Hospital[,"(Intercept)"] - fixef(gamma)[1]

# melt for ggplot
intercepts = melt(intercepts, id.vars = ("Hospital"))
colnames(intercepts) = c("Hospital","band","intercept")

# plot
ggplot(aes(x=Hospital,y=intercept,color=band,group=band),data=intercepts) +
  geom_line() + geom_hline(yintercept = 0) + scale_color_manual(values = fb_colors) + 
  theme_minimal() + ylab("intercept diff") + facet_wrap(~band,ncol=1) +
  theme(strip.background = element_blank(),strip.text = element_blank(),
        panel.grid.minor = element_blank())

# can break line before facet wrap and drop gamma from df to view all on one plot
