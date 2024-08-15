################################################################################

## Script to produce plots for the ROI-level age model outputs
## Requires outputs from age_model_stats.R


#### PRELIMINARIES #############################################################

# packages
library(tidyverse)                 # data frames & ggplot
library(reshape)                   # melting data frames
library(scales)                    # unit control for plots
library(grid)                      # to combine plots
library(conflicted)                # handle package clashes
conflict_prefer("rename","dplyr")  # use rename from dplyr
conflict_prefer("filter", "dplyr")
theme_set(theme_minimal(base_size = 24))

# import summaries
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site/3_output/")
mod_summaries = read.csv("regional_age_model_stats.csv")

# load ROI info from DB
ROI1_info=read.csv("/media/b6036780/8TB1/norm-ieeg-age-sex-site/1_data/ROI1_info.csv")

# colours
fb_colors=c("#3366ff","#bf80ff","#ff00bf","#3de600","#00cccc")

# bands
band = c("delta","theta","alpha","beta","gamma")

# greek letters
fb_lab=c(expression(delta),expression(theta),expression(alpha),expression(beta),expression(gamma))



#### INITIAL DATA FRAMES #######################################################

# attach roi & ppr info to region level stats
region_summaries=left_join(ROI1_info,mod_summaries)
region_summaries_RH = na.omit(region_summaries) %>% 
  arrange(no.subj) %>% 
  mutate(Area=factor(Area,levels=Area))

# clear work space
rm(mod_summaries, ROI1_info)

#### SUBJECTS PER REGION PLOT ##################################################

spr=ggplot(data=region_summaries_RH) +
  geom_col(aes(x=Area,y=no.subj), fill="grey") +ylab("No. of subjects in region") +
  theme(axis.title.y=element_text(margin=margin(r=10)),axis.text.x=element_blank(),
        axis.title.x = element_blank(),panel.grid.major.x = element_blank())



#### COLOR GRID CI & SINGULARITY PLOT ##########################################

#### DF CREATION

region_CI = region_summaries_RH %>%
  select(c("Area","no.subj",paste0(band,"_none0_CI"),paste0(band,"_singular")))

# make different indicator for each fb (delta: 0s & 1s, theta: 0s and 2s, etc)
# this enables plotting each band in a different colour 
for (i in 2:5) {
  region_CI[,paste0(band[i],"_none0_CI")] = region_CI[,paste0(band[i],"_none0_CI")]*(i)
}

# need to also include a singularity indicator in each fb column
# if singular, CI indicator doesn't matter/is overwritten
# assign -1 = singular, then there's still the 0 & n for the CI indicator
region_CI = region_CI %>%
  mutate(delta_none0_CI = if_else(delta_singular==1,-1,delta_none0_CI)) %>%
  mutate(theta_none0_CI = if_else(theta_singular==1,-1,theta_none0_CI)) %>%
  mutate(alpha_none0_CI = if_else(alpha_singular==1,-1,alpha_none0_CI)) %>%
  mutate(beta_none0_CI  = if_else(beta_singular ==1,-1,beta_none0_CI))  %>%
  mutate(gamma_none0_CI = if_else(gamma_singular==1,-1,gamma_none0_CI)) %>%
  select(-c(paste0(band,"_singular")))

# melt for suitable ggplot format and reformat new columns
region_CI = melt(region_CI, id.vars=c("Area","no.subj"))
region_CI = region_CI %>% 
  rename(freqband=variable) %>% mutate(freqband=gsub("_none0_CI","",freqband)) %>%
  mutate(freqband=factor(freqband,levels = band)) %>% rename(none0_CI=value) %>%
  mutate(none0_CI=factor(none0_CI,levels=-1:5))

#### PLOT CREATION 

# factor level order is -1 (singular), 0 (CI has 0), 1:5 (CI is none 0)
CI=ggplot(data = region_CI) +
  geom_tile(aes(x=Area,y=freqband,fill=none0_CI),show.legend = F) +
  xlab(NULL) + ylab(NULL) + scale_y_discrete(labels=rev(fb_lab), limits=rev) +
  theme(axis.text.x=element_blank(),panel.grid.major.x=element_blank()) +
  scale_fill_manual(values = c("black","white",fb_colors)) +
  annotate("label", x=32.5,y=5,size=4.5,label="Shaded: CI does not contain 0\nWhite: CI contains 0\nBlack: Singular fit")



#### STANDARD ERROR LINE PLOT ##################################################

#### DF CREATION

region_SE = region_summaries_RH %>%
  select(c("Area","no.subj",paste0(band,"_SE")))

# melt for suitable ggplot format and reformat resulting columns
region_SE = melt(region_SE, id.vars=c("Area","no.subj"))
region_SE = region_SE %>% 
  rename(freqband=variable) %>% mutate(freqband=gsub("_SE","",freqband)) %>%
  mutate(freqband=factor(freqband,levels = band)) %>% rename(SE=value)

#### PLOT CREATION 

SE=ggplot(data=region_SE) +
  geom_line(aes(x=Area,y=SE, group=freqband,color=freqband)) +
  ylab(expression(paste("Standard error of ",italic(hat(b)[age])))) + xlab("Region name") +
  scale_y_continuous(labels = comma) + scale_color_manual(values = fb_colors, labels=fb_lab) +
  theme(axis.text.x=element_text(hjust=0,angle=-90,vjust=0.5),
        legend.title=element_blank(), legend.position = c(0.8,0.7), legend.background = element_rect(colour="grey"),
        axis.title.x=element_text(margin=margin(t=10)),axis.title.y=element_text(margin=margin(r=10)),
        panel.grid.major.x = element_blank())



#### COMBINE! ##################################################################

grid.newpage()
pdf("regional_age_model_stats.pdf", width = 14.5,height = 18.75)
grid.draw(rbind(ggplotGrob(spr),ggplotGrob(CI),ggplotGrob(SE)))
dev.off()



#### MATLAB EXPORT FOR COEF MAPS ###############################################

# df to export to create brain space visualisation in matlab
# need to be mirrored for this

# select stats for matlab (betas)
beta_age = region_summaries %>%
  select(c("ROI_name", "x", "y", "z", "Hemisphere","Cortical","Area",paste0(band,"_coef")))

# mirror 
empty_rows=as.integer(row.names(beta_age[beta_age$Hemisphere=="Left",]))
for (i in empty_rows) {
  if (beta_age$Cortical[i]=="SC" & beta_age$Area[i]==beta_age$Area[i+7]) {
    beta_age[i,paste0(band,"_coef")]=beta_age[i+7,paste0(band, "_coef")]
  }
  else if (beta_age$Cortical[i]=="C" & beta_age$Area[i]==beta_age$Area[i+34]) {
    beta_age[i,paste0(band,"_coef")]=beta_age[i+34,paste0(band,"_coef")]
  }
}

# filter empty rows and give matlab names
beta_age = beta_age %>% 
  na.omit() %>%
  rename(names=ROI_name) %>%
  select(-c(Area,Hemisphere,Cortical))

# save
write.csv(beta_age, "regional_b_age_coeffs.csv", row.names=FALSE)

