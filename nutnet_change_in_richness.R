library(vegan)
library(lattice)
library(plyr)
library(dplyr)
library(tidyr)

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet-basic-data-script.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')


#pull out richness data
nutnetRich <- nutnetData%>%
  select(site_code, N, P, K, Exclose, trt, plot, year_trt, year, rich.vegan)

###through time calculation (from pre-treatment year)
#pull out pre-treatment data
preTrtRich <- nutnetRich%>%filter(year_trt==0)%>%
  select(site_code, plot, rich.vegan)
names(preTrtRich)[names(preTrtRich)=="rich.vegan"] <- "yr0_rich"

#pull out experimental data
richRR <- nutnetRich%>%filter(year_trt>0)%>%
  #merge experimental years with pre-treatment years
  left_join(preTrtRich, by=c('site_code', 'plot'))%>%
  #remove sites without pretreatment data
  filter(!is.na(yr0_rich), yr0_rich!='NULL')%>%
  #remove plots without treatment year data
  filter(!is.na(rich.vegan), rich.vegan!='NULL')%>%
  #calculate change in richness from pre-treatment year to each treatment year
  mutate(rich.vegan=as.numeric(rich.vegan), yr0_rich=as.numeric(yr0_rich))%>%
  mutate(rich_temp_lnRR=(log(rich.vegan/yr0_rich)))%>%
  select(-rich.vegan, -yr0_rich)