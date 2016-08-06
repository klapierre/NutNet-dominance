library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')


###read in data
nutnetData <- read.csv('comb-by-plot-clim-soil-diversity21-Jun-2016.csv')%>%
  #delete data with no cover
  filter(total_cover!='NULL')%>%
  #drop sites with just 3 plots
  filter(site_code!='ucsc.us', site_code!='elkh.us')%>%
  #get rid of observational only datasets
  filter(experiment_type!='Observational')


#generate site information table
nutnetSite <- nutnetData%>%
  select(site_name, site_code, continent, country, region, managed, burned, grazed, anthropogenic, habitat, elevation, latitude, longitude, first_nutrient_year, site_richness, site_native_richness, site_introduced_richness)%>%
  unique()%>%
  #remove sites with observational only datasets
  filter(first_nutrient_year!='NULL')