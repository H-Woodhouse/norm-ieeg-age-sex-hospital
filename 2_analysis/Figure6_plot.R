################################################################################

## Script to produce plots for the ROI-level age model outputs
## Requires output 'regional_age_model_stats.csv' from script
## regional_age_model_stats.R
##
## FIGURE 6
## Number of subjects per region, indicator of whether region's 95% confidence
## interval on beta_age contains 0 in each frequency band, regional standard
## error in each frequency band
##
## All formatted in one column of plots, shared x-axis of regions ordered by
## the number of subjects per region



#### PRELIMINARIES #############################################################

# packages
library(tidyverse)                 # data frames & ggplot
library(reshape)                   # melting data frames
library(scales)                    # unit control for plots
library(grid)                      # to combine plots
library(conflicted)                # handle package clashes
conflict_prefer("rename","dplyr")  # use rename from dplyr
conflict_prefer("filter","dplyr")  # use filter from dplyr
theme_set(theme_minimal(base_size = 24))

# set working directory to script location (2_analysis)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# not creating directory 3_output in this script, it should already exist as we
# need tables from it

# import regional summaries
setwd("../3_output")
regional_summaries = read.csv("regional_age_model_stats_TEST.csv")

# colours
fb_colors=c("#3366ff","#bf80ff","#ff00bf","#3de600","#00cccc")

# bands
band = c("delta","theta","alpha","beta","gamma")

# greek letters
fb_lab=c(expression(delta),expression(theta),expression(alpha),expression(beta),expression(gamma))



#### INITIAL DATA FRAMES #######################################################

# not doing any reflecting across the midline for visualisation purposes ->
# return to just right hemisphere
regional_summaries_RH =  regional_summaries %>%
  filter(Hemisphere=="Right")

# just retain what's needed for plotting, convert Area to factor and order the 
# data frame by number of subjects per region
regional_summaries_RH = regional_summaries_RH %>% 
  select(-c(ROI_index,ROI_name,x,y,z,Hemisphere,Cortical)) %>%
  arrange(no.subj) %>% 
  mutate(Area=factor(Area,levels=Area))

rm(regional_summaries)


################################################################################

# Sub plots intentionally have missing axes/oddly sized text ready for comining


#### 1. SUBJECTS PER REGION PLOT ###############################################

# create plot
spr_plot=ggplot(data=regional_summaries_RH) +
  geom_col(aes(x=Area,y=no.subj), fill="grey") +
  ylab("No. of subjects in region") +
  theme(axis.title.y=element_text(margin=margin(r=10)),axis.text.x=element_blank(),
        axis.title.x = element_blank(),panel.grid.major.x = element_blank())

# view
spr_plot


#### 2. COLOR GRID CONFIDENCE INTERVALS & SINGULARITY PLOT #####################

#### DF CREATION

# currently have binary indicators of whether the 95% confidence interval (CI)
# contains 0, and binary indicators of singularity for each band and region

CI_df = regional_summaries_RH %>%
  select(c("Area","no.subj",paste0(band,"_none0_CI"),paste0(band,"_singular")))

# make different indicator for each band (delta: 0s & 1s, theta: 0s and 2s, etc)
# this enables plotting each band in a different colour 
for (i in 2:5) {
  CI_df[,paste0(band[i],"_none0_CI")] = CI_df[,paste0(band[i],"_none0_CI")]*(i)
}

# need to also include the singularity indicator in each band's CI column
# if singular, CI indicator doesn't matter/is overwritten
# assign -1 = singular, then there's still the 0 & n for the CI indicator
CI_df = CI_df %>%
  mutate(delta_none0_CI = if_else(delta_singular==1,-1,delta_none0_CI)) %>%
  mutate(theta_none0_CI = if_else(theta_singular==1,-1,theta_none0_CI)) %>%
  mutate(alpha_none0_CI = if_else(alpha_singular==1,-1,alpha_none0_CI)) %>%
  mutate(beta_none0_CI  = if_else(beta_singular ==1,-1,beta_none0_CI))  %>%
  mutate(gamma_none0_CI = if_else(gamma_singular==1,-1,gamma_none0_CI)) %>%
  select(-c(paste0(band,"_singular")))

# melt for suitable ggplot format and reformat new columns
CI_df = melt(CI_df, id.vars=c("Area","no.subj"))
CI_df = CI_df %>% 
  rename(freqband=variable) %>% mutate(freqband=gsub("_none0_CI","",freqband)) %>%
  mutate(freqband=factor(freqband,levels = band)) %>% rename(indicator=value) %>%
  mutate(indicator=factor(indicator,levels=-1:5))

#### PLOT CREATION 

# factor level order is -1 (singular), 0 (CI has 0), 1:5 (CI is none 0)

# plot
CI_plot=ggplot(data = CI_df) +
  geom_tile(aes(x=Area,y=freqband,fill=indicator),show.legend = F) +
  xlab(NULL) + ylab(NULL) +
  scale_y_discrete(labels=rev(fb_lab), limits=rev) +
  theme(axis.text.x=element_blank(),panel.grid.major.x=element_blank()) +
  scale_fill_manual(values = c("black","white",fb_colors)) +
  annotate("label", x=32.5,y=5,size=4.5,label="Shaded: CI does not contain 0\nWhite: CI contains 0\nBlack: Singular fit")

# view
CI_plot

rm(CI_df, i)


#### 3. STANDARD ERROR LINE PLOT ###############################################

#### DF CREATION

SE_df = regional_summaries_RH %>%
  select(c("Area","no.subj",paste0(band,"_SE")))

# melt for suitable ggplot format and reformat resulting columns
SE_df = melt(SE_df, id.vars=c("Area","no.subj"))
SE_df = SE_df %>% 
  rename(freqband=variable) %>% mutate(freqband=gsub("_SE","",freqband)) %>%
  mutate(freqband=factor(freqband,levels = band)) %>% rename(SE=value)

#### PLOT CREATION 

# plot
SE_plot=ggplot(data=SE_df) +
  geom_line(aes(x=Area,y=SE, group=freqband,color=freqband)) +
  ylab(expression(paste("Standard error of ",italic(hat(b)[age])))) +
  xlab("Region name") +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = fb_colors, labels=fb_lab) +
  theme(axis.text.x=element_text(hjust=0,angle=-90,vjust=0.5),
        legend.title=element_blank(),
        legend.position= c(0.8,0.7),
        legend.background = element_rect(colour="grey"),
        axis.title.x=element_text(margin=margin(t=10)),
        axis.title.y=element_text(margin=margin(r=10)),
        panel.grid.major.x = element_blank())

# view
SE_plot

rm(SE_df)


#### COMBINE! ##################################################################

# combine & view the three subplots
# (sizing may look a little off in R's plotting window, open as pdf from 3_output)
grid.newpage()
grid.draw(rbind(ggplotGrob(spr_plot),ggplotGrob(CI_plot),ggplotGrob(SE_plot)))

# save
pdf("regional_age_model_stats_2.pdf", width = 14.5,height = 18.75)
grid.newpage()
grid.draw(rbind(ggplotGrob(spr_plot),ggplotGrob(CI_plot),ggplotGrob(SE_plot)))
dev.off()


