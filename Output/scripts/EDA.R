################################################################################

# EDA script for age, sex & site paper



#### PRELIMINARIES #############################################################

# packages 
library(tidyverse)         # data frames and ggplot
library(forestplot)        # forest plot
library(gridExtra)         # combining plots

# data
setwd("/media/b6036780/8TB1/norm-ieeg-age-sex-site")
BPdata_full = read.csv("Data/Preprocessing/ROI1_RBP_full.csv")



#### EDA #######################################################################

# visualise the age/sex distribution across sites

# meta data 
meta = BPdata_full %>% select(Pat_ID,Hospital,Age,Sex) %>% 
  distinct() %>% arrange(Pat_ID)

# basis stats
N=length(unique(BPdata_full$Pat_ID))
meta %>% count(Sex)
meta %>% count(Hospital)
min(meta$Age)
max(meta$Age)
mean(meta$Age)
hist(meta$Age)



#### FOREST PLOT ###############################################################

# full site names
site_full = c("Beijing Tiantan Hosp.", "Great Ormond St. Hosp.", "University of Iowa Hosp.","Columbia University*",
              "Dartmouth University*","Emory University*","Jefferson Hosp.*","Mayo Clinic*",
              "NINDS*","University of Pennsylvania*","UT Southwestern*","University of Washington*",
              "SickKids", "University College London Hosp.", "University of Wisconsin-Madison")

# forest plot data set
forest_df = meta %>%
  group_by(Hospital) %>%
  summarise(n=n(),across(Sex,list(M = ~ sum(. == "M"),F = ~ sum(. == "F"))),
            lower=min(Age),upper=max(Age),mean=mean(Age)) %>%
  unite(M_F, c(Sex_M,Sex_F), sep=":") %>% mutate(Site_full=site_full) %>%
  arrange(Site_full)

# forest plot figure
pdf("Output/subject_meta.pdf",width = 10, height = 8, onefile = FALSE)
forest_df |>
  forestplot(labeltext=c(Site_full,n, M_F), boxsize=0.25, xticks=seq(0,70,10), xlab="Age range with mean",
             zero=NA,fn.ci_norm=fpDrawDiamondCI,col=fpColors(lines="gray50")) |>
  fp_add_lines() |>
  fp_add_header(Site_full="Hospital", n="N", M_F="M:F") |>
  fp_set_style(txt_gp=fpTxtGp(xlab=gpar(cex=1), ticks=gpar(cex=0.75)), align="lcc") |>
  fp_append_row(Site_full="TOTAL",n=N,M_F="261:241",lower=3,upper=66,mean=31.21,is.summary = F)
dev.off()



#### SCATTER PLOTS #############################################################

# see if there's evidence of a non linear relationship
# included in supplementary
theme_set(theme_minimal())
gg_all = list(geom_point(size=.5), theme(aspect.ratio=1))
del=ggplot(data=BPdata_full, aes(x=Age,y=deltaBP)) + gg_all + ylab(expression("RBP("*delta*")"))
the=ggplot(data=BPdata_full, aes(x=Age,y=thetaBP)) + gg_all + ylab(expression("RBP("*theta*")"))
alp=ggplot(data=BPdata_full, aes(x=Age,y=alphaBP)) + gg_all + ylab(expression("RBP("*alpha*")"))
bet=ggplot(data=BPdata_full, aes(x=Age,y=betaBP )) + gg_all + ylab(expression("RBP("*beta*")"))
gam=ggplot(data=BPdata_full, aes(x=Age,y=gammaBP)) + gg_all + ylab(expression("RBP("*gamma*")"))

pdf("Output/sup-results/linear_scatter_SUP.pdf", width = 8, height = 10)
grid.arrange(del,the,alp,bet,gam)
dev.off()
