library(lattice)
library(plyr)
library(dplyr)
library(tidyr)

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet-cover-data.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')


###through time calculation (from pre-treatment year)
#calculate ln response ratio for change in spp cover
coverRR <- trtCoverTime%>%
  mutate(cover_lnRR=(log(rel_cover/pretrt_cover)))

#filter out just the dominant species RR, as determined from pre-trt data
domRR <- coverRR%>%
  group_by(site_code, trt, plot, year)%>%
  mutate(max=ifelse(pretrt_cover==max(pretrt_cover), 1, 0))%>%
  filter(max==1)%>%
  #take average change in dominant spp from pre-treatment for sites with 2+ codominants at yr=0
  #we need to think more about this!
  ungroup()%>%
  select(site_code, year, year_trt, plot, trt, cover_lnRR)%>%
  group_by(site_code, year, year_trt, plot, trt)%>%
  summarise(cover_lnRR=mean(cover_lnRR))