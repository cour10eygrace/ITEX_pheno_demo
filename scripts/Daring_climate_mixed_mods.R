#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(car)
library(lme4)
library(lmerTest)
library(AICcmodavg)

#load data
load('data/DLphen_dem_climate.Rdata')
load(file="data/Mixed_mods_df.Rdata")

#add in season lags 
flower_open<-left_join(flower_open, seas_clim)
flower_bud<-left_join(flower_bud, seas_clim)
#deal with summer_lag for year 0 (2001) replace with current year Summer
flower_open<-mutate(flower_open, Summer_lag=if_else(is.na(Summer_lag), Summer, Summer_lag))


semdat<-select(flower_open, species, year, plantid, plot, treatment, phen, trait, value, doy, GDD, GDD_lag, sfDOY,  phen2, trait2, Spring, Fall, Summer, 
               Spring_lag, Summer_lag, Fall_lag, Summer_diff) 
semdat<-subset(semdat, treatment=="CTL") #only keep control data - look at OTCs separately 

#look at general relationships- what do we expect?
#change in temps over time 
ggplot(semdat,
       aes(x=as.numeric(year), y=Summer))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  xlab("Year")+ ylab("Growing Season temp (C)")

summary(lm(Summer~as.numeric(year), semdat))
#0.116*20

#temporal trend in snow free much weaker but also higher variation (by sites)
ggplot(semdat,
       aes(x=as.numeric(year), y=sfDOY))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  xlab("Year")+ ylab("Snow free DOY")

#doy~Spring negative
ggplot(semdat,
       aes(x=Spring, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(semdat$Spring,semdat$doy) #-0.23

#doy~Summer negative
ggplot(semdat,
       aes(x=Summer, y=doy, fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  ylab("Flowering DOY")+ xlab("Growing Season temp (C)")

cor.test(semdat$Summer,semdat$doy) #-0.4

#doy~Fall_lag negative (weaker)
ggplot(semdat,
       aes(x=Fall_lag, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(semdat$doy, semdat$Fall_lag) #-0.15

#doy~Summer_lag negative (weaker)
ggplot(semdat,
       aes(x=Summer_lag, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(semdat$doy, semdat$Summer_lag) #-0.13

#doy~sfdoy positive
ggplot(semdat,
       aes(x=sfDOY, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm')
cor.test(semdat$doy, semdat$sfDOY) #0.4


ggplot(semdat,
       aes(x=sfDOY, y=doy, fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  ylab("Flowering DOY")+ xlab("Snow free DOY")

#best for DOY overall: SFdoy, Spring and Summer 
#are they highly correlated?? 
cor.test(semdat$Spring, semdat$Summer) 
cor.test(semdat$Spring, semdat$sfDOY) 
cor.test(semdat$sfDOY, semdat$Summer) 
#not too bad ~0.4-0.5

#general patterns
#snow free does not affect fruit
ggplot(fruitdat,
       aes(x=sfDOY, y=value))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

#warmer Spring= no relationship
ggplot(fruitdat,
       aes(x=Spring, y=value))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

#warmer Summer= more fruit
ggplot(fruitdat,
       aes(x=Summer, y=value))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(fruitdat$value, fruitdat$Summer)# 0.15

#warmer previous fall= more fruit 
ggplot(fruitdat,
       aes(x=Fall_lag, y=value))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(fruitdat$value, fruitdat$Fall_lag)# 0.2

#warmer previous summer= more fruit 
ggplot(fruitdat,
       aes(x=Summer_lag, y=value))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(fruitdat$value, fruitdat$Summer_lag)#0.22

ggplot(fruitdat,
       aes(x=Summer_lag, y=log(value), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  ylab("Fruit number")+ xlab("Growing Season temp (C)") 


#warmer previous spring= more fruit 
ggplot(fruitdat,
       aes(x=Spring_lag, y=value))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(fruitdat$value, fruitdat$Spring_lag)#0.17

#previous fall, summer and spring all about the same 
hist(fruitdat$doy)
hist(log(fruitdat$value))


#lmers
#flowering DOY
doy1<-lmer(doy~ Summer+ (1|species:plantid) + (1|year) + (1|species), semdat)
summary(doy1)
doy1x<-lmer(doy~ Summer_lag+ (1|species:plantid) + (1|year) + (1|species), semdat)
summary(doy1x)
doy1y<-lmer(doy~ sfDOY+ (1|species:plantid) + (1|year) + (1|species), semdat)
summary(doy1y)
doy2<-lmer(doy~ Summer+ Summer_lag + (1|species:plantid) + (1|year) + (1|species), semdat)
summary(doy2)
doy2x<-lmer(doy~ Summer+ sfDOY + (1|species:plantid) + (1|year) + (1|species), semdat)
summary(doy2x)
doy2y<-lmer(doy~ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), semdat)
summary(doy2y)
doy3<-lmer(doy~ Summer+ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), semdat)
summary(doy3)

#model selection
aictab(cand.set = c(doy1, doy1x, doy1y, doy2, doy2x, doy2y, doy3), 
       modnames = c("doy1", "doy1x", "doy1y", "doy2", "doy2x", "doy2y", "doy3"))
#Summer, Summer + Summer_lag, Summer+ SF, Summer + Summer_lag + SF all equal 
#use most simple model =summer only

#flower number 
flowdat<-subset(semdat, trait2=="num_flowers"& value>0)
hist(flowdat$value)
hist(log(flowdat$value))

flow1<-lmer(log(value)~ Summer+ (1|species:plantid) + (1|year) + (1|species), flowdat)
summary(flow1)
flow1x<-lmer(log(value)~ Summer_lag+ (1|species:plantid) + (1|year) + (1|species), flowdat)
summary(flow1x)
flow1y<-lmer(log(value)~ sfDOY+ (1|species:plantid) + (1|year) + (1|species), flowdat)
summary(flow1y)
flow2<-lmer(log(value)~ Summer+ Summer_lag + (1|species:plantid) + (1|year) + (1|species), flowdat)
summary(flow2)
flow2x<-lmer(log(value)~ Summer+ sfDOY + (1|species:plantid) + (1|year) + (1|species), flowdat)
summary(flow2x)
flow2y<-lmer(log(value)~ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), flowdat)
summary(flow2y)
flow3<-lmer(log(value)~ Summer+ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), flowdat)
summary(flow3)

aictab(cand.set = c(flow1, flow1x, flow1y, flow2, flow2x, flow2y, flow3), 
       modnames = c("flow1", "flow1x", "flow1y","flow2", "flow2x", "flow2y", "flow3"))
#Summer only best model delta aic >2 


#fruit number 
fruitdat<-subset(semdat, trait2=="num_fruit")
hist(fruitdat$value)
hist(log(fruitdat$value+1))

fruit1<-lmer(log(value+1)~ Summer+ (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit1)
fruit1x<-lmer(log(value+1)~ Summer_lag+ (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit1x)
fruit1y<-lmer(log(value+1)~ sfDOY+ (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit1y)
fruit2<-lmer(log(value+1)~ Summer+ Summer_lag + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit2)
fruit2x<-lmer(log(value+1)~ Summer+ sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit2x)
fruit2y<-lmer(log(value+1)~ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit2y)
fruit3<-lmer(log(value+1)~ Summer+ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit3)

aictab(cand.set = c(fruit1, fruit1x, fruit1y, fruit2, fruit2x, fruit2y, fruit3), 
       modnames = c("fruit1", "fruit1x", "fruit1y", "fruit2", "fruit2x", "fruit2y", "fruit3"))
#Summer lag only best model delta aic >2 
