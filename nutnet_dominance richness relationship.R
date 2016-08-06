library(lattice)
library(lme4)
library(plyr)
library(dplyr)
library(tidyr)

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_dominance.R')
source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_richness.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')

#keep only needed dataframes
rm(list=setdiff(ls(), c('domRR', 'richRR')))

#merge dominance and richness data
domRich <- domRR%>%
  left_join(richRR)
