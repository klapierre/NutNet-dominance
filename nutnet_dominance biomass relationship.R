library(lattice)
library(lme4)
library(plyr)
library(dplyr)
library(tidyr)

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_dominance.R')
source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_biomass.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')

coverBio <- trtBio%>%
  left_join(domRR)



coverBioYr3 <- coverBio%>%
  filter(year_trt==3, trt=='NPK')
plot(coverBioYr3$live_mass_lnRR~coverBioYr3$cover_lnRR)

sub <- coverBioYr3%>%
  group_by(site_code, trt)%>%
  summarize(cover_mean_lnRR=mean(cover_lnRR), live_mass_mean_lnRR=mean(live_mass_lnRR))
  
plot(sub$live_mass_mean_lnRR~sub$cover_mean_lnRR)
