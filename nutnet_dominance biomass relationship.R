library(lattice)
library(lme4)
library(plyr)
library(dplyr)
library(tidyr)

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_dominance.R')
source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet-basic-data-script.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')

coverBio <- trtBio%>%
  left_join(domRR)



coverBioYr3 <- coverBio%>%
  filter(year_trt==3, trt=='NPK')
plot(coverBioYr3$live_mass_lnRR~coverBioYr3$cover_lnRR)

sub <- coverBioYr3%>%
  filter(cover_lnRR<5)
plot(sub$live_mass_lnRR~sub$cover_lnRR)
