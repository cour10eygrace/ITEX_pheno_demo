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

#deal with summer_lag for year 0 (2001) replace with current year Summer
flower_open<-mutate(flower_open, Summer_lag=if_else(is.na(Summer_lag), Summer, Summer_lag))


semdat<-select(flower_open, species, year, plantid, treatment, phen, trait, value, doy, sfDOY,  phen2, trait2, Spring, Fall, Summer, 
               Spring_lag, Summer_lag, Fall_lag, Summer_diff) 
semdat<-subset(semdat, treatment=="CTL") #only keep control data - look at OTCs separately 


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
table1A<-aictab(cand.set = c(doy1, doy1x, doy1y, doy2, doy2x, doy2y, doy3), 
       modnames = c("Growing season temp", "Growing season temp (prev)", "Snow free DOY", 
                    "Growing season temp + Growing season temp (prev)", "Growing season temp + Snow free DOY",
                    "Growing season temp (prev) + Snow free DOY", "Growing season temp + Growing season temp (prev) + Snow free DOY"))
table1A$Response<-"Flowering time (DOY)"
#Summer+ SF, Summer + Summer_lag + SF, Summer, Summer + Summer_lag all equal 
#use most simple model summer only & for consistency with flower number (below)
#use summer + summer lag in fruit models 

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

table1B<-aictab(cand.set = c(flow1, flow1x, flow1y, flow2, flow2x, flow2y, flow3), 
       modnames = c("Growing season temp", "Growing season temp (prev)", "Snow free DOY", 
                      "Growing season temp + Growing season temp (prev)", "Growing season temp + Snow free DOY",
                      "Growing season temp (prev) + Snow free DOY", "Growing season temp + Growing season temp (prev) + Snow free DOY"))
table1B$Response<-"Flower number"
#Summer only best model delta aic >2 


#prob fruit 
fruitdat<-subset(semdat, trait2=="num_fruit")%>%mutate(probfruit=ifelse(value==0, 0, 1))
hist(fruitdat$probfruit)

pfruit1<-glmer(probfruit~ Summer+ (1|species:plantid) + (1|year) + (1|species), fruitdat, family = binomial())
summary(pfruit1)
pfruit1x<-glmer(probfruit~ Summer_lag+ (1|species:plantid) + (1|year) + (1|species), fruitdat,  family = binomial())
summary(pfruit1x)
pfruit1y<-glmer(probfruit~ sfDOY+ (1|species:plantid) + (1|year) + (1|species), fruitdat, family = binomial())
summary(pfruit1y)
pfruit2<-glmer(probfruit~ Summer+ Summer_lag + (1|species:plantid) + (1|year) + (1|species), fruitdat, family = binomial())
summary(pfruit2)
pfruit2x<-glmer(probfruit~ Summer+ sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat, family = binomial())
summary(pfruit2x)
pfruit2y<-glmer(probfruit~ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat, family = binomial())
summary(pfruit2y)
pfruit3<-glmer(probfruit~ Summer+ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat, family = binomial())
summary(pfruit3)

table1C<-aictab(cand.set = c(pfruit1, pfruit1x, pfruit1y, pfruit2, pfruit2x, pfruit2y, pfruit3), 
       modnames = c("Growing season temp", "Growing season temp (prev)", "Snow free DOY", 
                     "Growing season temp + Growing season temp (prev)", "Growing season temp + Snow free DOY",
                     "Growing season temp (prev) + Snow free DOY", "Growing season temp + Growing season temp (prev) + Snow free DOY"))
table1C$Response<-"Probability of fruiting"
table1C<-rename(table1C, Res.LL=LL)

#Summer lag only is best model but lots of other models are equivalent. Use summer lag for consistency with fruit number (below)


#fruit number 
fruitdat<-subset(fruitdat, value>0)
hist(fruitdat$value)
hist(log(fruitdat$value))

fruit1<-lmer(log(value)~ Summer+ (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit1)
fruit1x<-lmer(log(value)~ Summer_lag+ (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit1x)
fruit1y<-lmer(log(value)~ sfDOY+ (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit1y)
fruit2<-lmer(log(value)~ Summer+ Summer_lag + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit2)
fruit2x<-lmer(log(value)~ Summer+ sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit2x)
fruit2y<-lmer(log(value)~ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit2y)
fruit3<-lmer(log(value)~ Summer+ Summer_lag +  sfDOY + (1|species:plantid) + (1|year) + (1|species), fruitdat)
summary(fruit3)

table1D<-aictab(cand.set = c(fruit1, fruit1x, fruit1y, fruit2, fruit2x, fruit2y, fruit3), 
       modnames = c("Growing season temp", "Growing season temp (prev)", "Snow free DOY", 
                     "Growing season temp + Growing season temp (prev)", "Growing season temp + Snow free DOY",
                     "Growing season temp (prev) + Snow free DOY", "Growing season temp + Growing season temp (prev) + Snow free DOY"))
table1D$Response<-"Fruit number"

#Summer lag only best model delta aic >2 

#Results table for manuscript 
Table1<-rbind(table1A, table1B, table1C, table1D)
names(Table1)
Table1<-select(Table1, Response,Modnames, AICc, Delta_AICc)%>%rename(Predictors=Modnames)

#write.csv(Table1, "MS_docs/Table1.csv")

#Misc----
##look at general relationships- what do we expect?
#change in temps over time 
#Fig 3a
ggplot(semdat,
       aes(x=as.numeric(year), y=Summer))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  xlab("Year")+ ylab("Growing Season temp (C)")

summary(lm(Summer~as.numeric(year), semdat))
#0.116*21= 2.4 C

#Fig S2a 
ggplot(semdat,
       aes(x=Summer, y=doy, fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  ylab("Flowering DOY")+ xlab("Growing Season temp (C)")

cor.test(semdat$Summer,semdat$doy) #-0.4

#other parameters 
#temporal trend in snow free much weaker but also higher variation (by sites)
ggplot(semdat,
       aes(x=as.numeric(year), y=sfDOY))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  xlab("Year")+ ylab("Snow free DOY")

summary(lm(sfDOY~as.numeric(year), semdat))

#doy~Spring negative
ggplot(semdat,
       aes(x=Spring, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
cor.test(semdat$Spring,semdat$doy) #-0.23


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



