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


#fruit number 
fruitdat<-subset(semdat, trait2=="num_fruit")

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

table1C<-aictab(cand.set = c(fruit1, fruit1x, fruit1y, fruit2, fruit2x, fruit2y, fruit3), 
       modnames = c("Growing season temp", "Growing season temp (prev)", "Snow free DOY", 
                     "Growing season temp + Growing season temp (prev)", "Growing season temp + Snow free DOY",
                     "Growing season temp (prev) + Snow free DOY", "Growing season temp + Growing season temp (prev) + Snow free DOY"))
table1C$Response<-"Fruit number"

#Summer lag only best model delta aic >2 

#Results table for manuscript 
Table1<-rbind(table1A, table1B, table1C)
names(Table1)
Table1<-select(Table1, Response,Modnames, AICc, Delta_AICc)%>%rename(Predictors=Modnames)

write.csv(Table1, "MS_docs/Table1.csv")

#Misc----
##look at general relationships- what do we expect?
#change in temps over time 
#Fig 3a (also in warmingplots.R)
ggplot(semdat,
       aes(x=as.numeric(year), y=Summer))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  xlab("Year")+ ylab("Growing Season temp (C)")

summary(lm(Summer~as.numeric(year), semdat))
#0.116*21= 2.4 C

#Fig S3a 
ggplot(subset(semdat,species!="carex"), 
       aes(x=Summer, y=doy, fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Flowering DOY")+ xlab("Growing Season temp (C)")

cor.test(semdat$Summer,semdat$doy) #-0.4

#other parameters 
#temporal trend in snow free much weaker but also higher variation (by sites)
#Fig S1a
semdatplot<-subset(semdat,species!="carex")
ggplot(semdatplot, 
       aes(x=as.numeric(year), y=sfDOY))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  xlab("Year")+ ylab("Snow free DOY")

mod<-(lmer(sfDOY~as.numeric(year) + (1|year), semdatplot))
summary(mod)
MuMIn::r.squaredGLMM(mod) 

