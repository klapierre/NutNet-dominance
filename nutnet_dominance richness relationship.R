library(lattice)
library(nlme)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(tidyr)

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=34),
             axis.title.y=element_text(size=40, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=34),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))

###bar graph summary statistics function
#barGraphStats(data=, variable="", byFactorNames=c(""))
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  

source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_dominance.R')
source('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\nutnet_change_in_richness.R')

setwd('C:\\Users\\Kim\\Dropbox\\NutNet\\NutNet-dominance\\NutNet data')

#keep only needed dataframes
rm(list=setdiff(ls(), c('domRR', 'richRR')))


###through time analysis
#merge dominance and richness data
domRich <- domRR%>%
  left_join(richRR)%>%
  #keep only NPK and fence factorial experiment
  filter(trt=='Control'|trt=='NPK'|trt=='Fence'|trt=='NPK+Fence')%>%
  #create NPK and fence treatment variables
  mutate(NPK=ifelse(trt=='NPK'|trt=='NPK+Fence', 'NPK', 'Control'), Fence=ifelse(trt=='Fence'|trt=='NPK+Fence', 'Fence', 'Control'))
  

###mixed model - note, both cover_lnRR and rich_lnRR are fairly normally distributed
#yr=3
summary(domRichModel3 <- lme(rich_lnRR ~ cover_lnRR, random=~1|site_code, data=subset(domRich, year_trt==3), method='REML'))

#yr=6
summary(domRichModel6 <- lme(rich_lnRR ~ cover_lnRR, random=~1|site_code, data=subset(domRich, year_trt==6), method='REML'))

###plots
#yr3
domRichPlot3<- ggplot(data=subset(domRich, year_trt==3), aes(x=cover_lnRR, y=rich_lnRR)) +
  geom_point() +
  xlab('lnRR Dominant Species\nRelative Abundance') +
  ylab('lnRR Richness') +
  coord_cartesian(ylim=c(-2.5, 1.5)) +
  stat_function(fun=function(x){(-0.10377165 + -0.03864927*x)}, size=1, xlim=c(min(domRich$cover_lnRR),max(domRich$cover_lnRR)), colour='black') +
  annotate('text', x=-6, y=1.5, label='(a) Year 3', size=10, hjust='left')

#yr6
domRichPlot6 <- ggplot(data=subset(domRich, year_trt==6), aes(x=cover_lnRR, y=rich_lnRR)) +
  geom_point() +
  xlab('lnRR Dominant Species\nRelative Abundance') +
  ylab('') +
  coord_cartesian(ylim=c(-2.5, 1.5)) +
  stat_function(fun=function(x){(-0.29832165 + -0.08510367*x)}, size=1, xlim=c(min(domRich$cover_lnRR),max(domRich$cover_lnRR)), colour='black') +
  annotate('text', x=-6, y=1.5, label='(b) Year 6', size=10, hjust='left')

pushViewport(viewport(layout=grid.layout(1,2)))
print(domRichPlot3, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(domRichPlot6, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1400x700

###mixed model - with trts as covariates
#yr=3
summary(domRichModel3 <- lme(rich_lnRR ~ cover_lnRR + NPK*Fence, random=~1|site_code, data=subset(domRich, year_trt==3)))

#yr=6
summary(domRichModel3 <- lme(rich_lnRR ~ cover_lnRR + NPK*Fence, random=~1|site_code, data=subset(domRich, year_trt==6)))

###plots - note, just using geom_smooth for convinence now, but need to change to actual model estimates later
#yr3
domRichTrtPlot3 <- ggplot(data=subset(domRich, year_trt==3), aes(x=cover_lnRR, y=rich_lnRR, colour=NPK)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab('lnRR Dominant Species\nRelative Abundance') +
  ylab('lnRR Richness') +
  coord_cartesian(ylim=c(-2.5, 1.5)) +
  annotate('text', x=-6, y=1.5, label='(a) Year 3', size=10, hjust='left')

#yr6
domRichTrtPlot6 <- ggplot(data=subset(domRich, year_trt==6), aes(x=cover_lnRR, y=rich_lnRR, colour=NPK)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab('lnRR Dominant Species\nRelative Abundance') +
  ylab('') +
  coord_cartesian(ylim=c(-2.5, 1.5)) +
  annotate('text', x=-6, y=1.5, label='(b) Year 6', size=10, hjust='left')

pushViewport(viewport(layout=grid.layout(1,2)))
print(domRichTrtPlot3, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(domRichTrtPlot6, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x700

###mixed model - just trt effects
#yr=3, dominance
summary(richModel3 <- lme(cover_lnRR ~ NPK*Fence, random=~1|site_code, data=subset(domRich, year_trt==3)))

#yr=6, dominance
summary(richModel6 <- lme(cover_lnRR ~ NPK*Fence, random=~1|site_code, data=subset(domRich, year_trt==6)))

#yr=3, richness
summary(richModel3 <- lme(rich_lnRR ~ NPK*Fence, random=~1|site_code, data=subset(domRich, year_trt==3)))

#yr=6, richness
summary(richModel6 <- lme(rich_lnRR ~ NPK*Fence, random=~1|site_code, data=subset(domRich, year_trt==6)))

###plots
#yr3, dom response
domTrtPlot3 <- ggplot(data=barGraphStats(data=subset(domRich, year_trt==3), variable="cover_lnRR", byFactorNames=c("trt")), aes(x=trt, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0.2)) +
  xlab('') +
  ylab('lnRR Dominant Species\nRelative Abundance') +
  coord_cartesian(ylim=c(-1.5,0)) +
  annotate('text', x=0.1, y=0, label='(a) Year 3', size=10, hjust='left')

#yr6, dom response
domTrtPlot6 <- ggplot(data=barGraphStats(data=subset(domRich, year_trt==6), variable="cover_lnRR", byFactorNames=c("trt")), aes(x=trt, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0.2)) +
  xlab('') +
  ylab('') +
  coord_cartesian(ylim=c(-1.5,0))+
  annotate('text', x=0.1, y=0, label='(b) Year 6', size=10, hjust='left')

#yr3, rich response
richTrtPlot3 <- ggplot(data=barGraphStats(data=subset(domRich, year_trt==3), variable="rich_lnRR", byFactorNames=c("trt")), aes(x=trt, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0.2)) +
  xlab('') +
  ylab('lnRR Richness\n') +
  coord_cartesian(ylim=c(-0.3,0.1)) +
  annotate('text', x=0.1, y=0.1, label='(c) Year 3', size=10, hjust='left') +
  geom_hline(yintercept=0)

#yr6, rich response
richTrtPlot6 <- ggplot(data=barGraphStats(data=subset(domRich, year_trt==6), variable="rich_lnRR", byFactorNames=c("trt")), aes(x=trt, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0.2)) +
  xlab('') +
  ylab('') +
  coord_cartesian(ylim=c(-0.5,0.1)) +
  annotate('text', x=0.1, y=0.1, label='(d) Year 6', size=10, hjust='left') +
  geom_hline(yintercept=0)


pushViewport(viewport(layout=grid.layout(2,2)))
print(domTrtPlot3, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(domTrtPlot6, vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(richTrtPlot3, vp=viewport(layout.pos.row=2, layout.pos.col=1))
print(richTrtPlot6, vp=viewport(layout.pos.row=2, layout.pos.col=2))
#export at 1800x1800