library(plyr)
library(dplyr)
library(tidyr)

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet-basic-data-script.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')

#pull out biomass data
nutnetBio <- nutnetData%>%
  select(site_code, N, P, K, Exclose, trt, plot, year_trt, year, rich, site_year_rich, MAT, MAP, RAIN_PET, plot_beta, total_mass, live_mass, dead_mass)

###through time calculation (from pre-treatment year)
#pull out pre-treatment data
preTrtBio <- nutnetBio%>%filter(year_trt==0)%>%
  select(site_code, plot, live_mass)
names(preTrtBio)[names(preTrtBio)=="live_mass"] <- "yr0_live_mass"

#pull out experimental data
bioRR <- nutnetBio%>%filter(year_trt>0)%>%
  #merge experimental years with pre-treatment years
  left_join(preTrtBio, by=c('site_code', 'plot'))%>%
  #remove sites without pretreatment data
  filter(!is.na(yr0_live_mass), yr0_live_mass!='NULL')%>%
  #remove plots without treatment year data
  filter(!is.na(live_mass), live_mass!='NULL')%>%
  #calculate change in ANPP from pre-treatment year to each treatment year
  mutate(live_mass=as.numeric(live_mass), yr0_live_mass=as.numeric(yr0_live_mass))%>%
  mutate(live_mass_temp_lnRR=(log(live_mass/yr0_live_mass)))