library(lattice)
library(lme4)
library(plyr)
library(dplyr)
library(tidyr)

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_dominance.R')
source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_biomass.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')

#keep only needed dataframes
rm(list=setdiff(ls(), c('domRR', 'bioRR')))

domBio <- bioRR%>%
  left_join(domRR)%>%
  #keep only NPK and fence factorial experiment
  filter(trt=='Control'|trt=='NPK'|trt=='Fence'|trt=='NPK+Fence')%>%
  #create NPK and fence treatment variables
  mutate(NPK=ifelse(trt=='NPK'|trt=='NPK+Fence', 'NPK', 'Control'), Fence=ifelse(trt=='Fence'|trt=='NPK+Fence', 'Fence', 'Control'))

summary(domBioModel3 <- lme(live_mass_temp_lnRR ~ cover_temp_lnRR + NPK*Fence, random=~1|site_code, data=subset(domBio, year_trt==3)))

summary(domBioModel6 <- lme(live_mass_temp_lnRR ~ cover_temp_lnRR + NPK*Fence, random=~1|site_code, data=subset(domBio, year_trt==6)))