#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(car)

####RUN MODELS separately by spp---- 
load('data/DLphen_dem_climate.Rdata')
load(file="data/Mixed_mods_df.Rdata")

#add in season lags 
flower_open<-left_join(flower_open, seas_clim)
flower_bud<-left_join(flower_bud, seas_clim)


#try with Iler et al 2017 approach
#detrend demog with climate (and/or year)

#ledum----
ledum<-subset(daring_phen_long, species=="ledum")
ledum<-select(ledum, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value")
#look at distribution of response data 
hist(ledum$num_flowers) #use poisson
hist(ledum$num_fruit)#use poisson
hist(ledum$growth_inc_mm)
hist(log(ledum$growth_inc_mm)) #normal-log

hist(subset(ledum, phen2=="first_flower_open")$doy)#normal 


#flowering time to flower number
detrend.numflow<-glm(value ~ Spring + Fall_lag + sfDOY ,
                    data=subset(flower_open, trait2=="num_flowers" &species=="ledum"), family = "poisson") 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
vif(detrend.numflow) #good

#MuMIn::r.squaredGLMM(detrend.numflow) 

detrend.flow<-lm(doy ~ Spring + Fall_lag + sfDOY ,
                 data=subset(flower_open, trait2=="num_flowers"&species=="ledum")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
#MuMIn::r.squaredGLMM(detrend.flow) 
vif(detrend.flow) #good

#correlations
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #negative relationship but low R square ~4%
cor.test(resid.numflow,resid.flow) #-0.2


#flowering time to fruit number
detrend.numfruit<-glm(value ~ Spring + Fall_lag + sfDOY,
                     data=subset(flower_open, trait2=="num_fruit"&species=="ledum"), family="poisson") 
summary(detrend.numfruit)
resid.numfruit<-residuals(detrend.numfruit)
vif(detrend.numfruit)

detrend.flow2<-lm(doy ~ Spring + Fall_lag +sfDOY,
                  data=subset(flower_open, trait2=="num_fruit"&species=="ledum")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)
  vif(detrend.flow2)

#correlations
plot(resid.numfruit~resid.flow2)
summary(lm(resid.numfruit~resid.flow2)) #negative relationship and low R square ~6%
cor.test(resid.numfruit,resid.flow2) #-0.24

#flowering time to stem growth
detrend.growth<-lm(log(value) ~ Spring + Fall_lag + sfDOY,
                      data=subset(flower_open, trait2=="growth_inc_mm"&species=="ledum"))
summary(detrend.growth)
resid.growth<-residuals(detrend.growth)
vif(detrend.growth)

detrend.flow3<-lm(doy ~ Spring + Fall_lag + sfDOY,
                  data=subset(flower_open, trait2=="growth_inc_mm"&species=="ledum")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)
vif(detrend.flow3)

#correlations
plot(resid.growth~resid.flow3)
summary(lm(resid.growth~resid.flow3)) #negative relationship R square 4%
cor.test(resid.growth,resid.flow3) #negative -0.19


#all these relationships negative initially
#remain negative after controlling for climate- but low overall explanatory power 2-10% 
ggplot(subset(flower_open,species=="ledum"),
       aes(x=doy, y=value, fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+ 
  ylab("")+ xlab("DOY flower open")


#vaccinium----
vac<-subset(daring_phen_long, species=="vaccinium")
vac<-select(vac, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value")

#look at distribution of response data 
hist(vac$num_flowers) #use poisson
hist(vac$num_fruit)#use poisson
hist(vac$growth_inc_mm)
hist(log(vac$growth_inc_mm)) #normal-log

hist(subset(vac, phen2=="first_flower_open")$doy)#normal 

detrend.numflow<-glm(value ~ Spring + Fall_lag + sfDOY ,
                     data=subset(flower_open, trait2=="num_flowers" &species=="vaccinium"), family = "poisson") 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
#MuMIn::r.squaredGLMM(detrend.numflow) 

detrend.flow<-lm(doy ~ Spring + Fall_lag + sfDOY ,
                 data=subset(flower_open, trait2=="num_flowers"&species=="vaccinium")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
#MuMIn::r.squaredGLMM(detrend.flow) 

#correlations
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #negative R2 4%
cor.test(resid.numflow,resid.flow) #-0.2

#flowering time to fruit number
detrend.numfruit<-glm(value ~ Spring + Fall_lag + sfDOY ,
                      data=subset(flower_open, trait2=="num_fruit"&species=="vaccinium"), family="poisson") 
summary(detrend.numfruit)
resid.numfruit<-residuals(detrend.numfruit)

detrend.flow2<-lm(doy ~ Spring + Fall_lag + sfDOY ,
                  data=subset(flower_open, trait2=="num_fruit"&species=="vaccinium")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)

#correlations
plot(resid.numfruit~resid.flow2)
summary(lm(resid.numfruit~resid.flow2)) #weak positive relationship/ NS (p=0.07)
cor.test(resid.numfruit,resid.flow2) #weak positive 0.11

#flowering time to stem growth
detrend.growth<-lm(log(value) ~ Spring + Fall_lag + sfDOY,
                   data=subset(flower_open, trait2=="growth_inc_mm"&species=="vaccinium"))
summary(detrend.growth)
resid.growth<-residuals(detrend.growth)

detrend.flow3<-lm(doy ~ Spring + Fall_lag  + sfDOY ,
                  data=subset(flower_open, trait2=="growth_inc_mm"&species=="vaccinium")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)

#correlations
plot(resid.growth~resid.flow3)
summary(lm(resid.growth~resid.flow3)) #NS
cor.test(resid.growth,resid.flow3) #NS

#only relationship that is retained is # flow 
ggplot(subset(flower_open,species=="vaccinium"),
       aes(x=doy, y=value))+ #fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+
  ylab("")+ xlab("DOY flower open")


#betula----
bet<-subset(daring_phen_long, species=="betula")
bet<-select(bet, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value")

#look at distribution of response data 
hist(bet$num_flowers) #use poisson
hist(bet$leaf_length_mm)
hist(log(bet$leaf_length_mm))#normal-log
hist(bet$growth_inc_mm)
hist(log(bet$growth_inc_mm)) #normal-log

hist(subset(bet, phen2=="first_flower_open")$doy)#normal 

detrend.numflow<-glm(value ~ Spring + Fall_lag + sfDOY, 
                     data=subset(flower_open, trait2=="num_flowers" &species=="betula"), family = "poisson") 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
#MuMIn::r.squaredGLMM(detrend.numflow) 

detrend.flow<-lm(doy ~ Spring + Fall_lag  + sfDOY ,
                 data=subset(flower_open, trait2=="num_flowers"&species=="betula")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
#MuMIn::r.squaredGLMM(detrend.flow) 

#correlations
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #NS
cor.test(resid.numflow,resid.flow) #NS

#flowering time to leaf size
detrend.leafsize<-lm(log(value) ~ GDD + GDD_lag + sfDOY,
                      data=subset(flower_open, trait2=="leaf_length_mm"&species=="betula")) 
summary(detrend.leafsize)
resid.leafsize<-residuals(detrend.leafsize)

detrend.flow2<-lm(doy ~ GDD + GDD_lag + sfDOY ,
                  data=subset(flower_open, trait2=="leaf_length_mm"&species=="betula")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)

#correlations
plot(resid.leafsize~resid.flow2)
summary(lm(resid.leafsize~resid.flow2)) #pos
cor.test(resid.leafsize,resid.flow2) #0.2

#flowering time to stem growth
detrend.growth<-lm(log(value) ~ Spring + Fall_lag + sfDOY,
                   data=subset(flower_open, trait2=="growth_inc_mm"&species=="betula"))
summary(detrend.growth)
resid.growth<-residuals(detrend.growth)

detrend.flow3<-lm(doy ~ Spring + Fall_lag + sfDOY ,
                  data=subset(flower_open, trait2=="growth_inc_mm"&species=="betula")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)

#correlations
plot(resid.growth~resid.flow3)
summary(lm(resid.growth~resid.flow3)) #NS
cor.test(resid.growth,resid.flow3) #NS

#only relationship is leaf size ?
ggplot(subset(flower_open,species=="betula"),
       aes(x=doy, y=value, fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+
  ylab("")+ xlab("DOY flower open")

#salix----
sal<-subset(daring_phen_long, species=="salix")
sal<-select(sal, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value")

#look at distribution of response data 
hist(sal$num_flowers) #use poisson
hist(sal$leaf_length_mm)#normal
hist(sal$repro_size) #normal
hist(sal$growth_inc_mm) 
hist(log(sal$growth_inc_mm)) #normal-log

hist(subset(sal, phen2=="first_flower_open")$doy)#normal-ish 

detrend.numflow<-glm(value ~ GDD + GDD_lag + sfDOY, 
                     data=subset(flower_open, trait2=="num_flowers" &species=="salix"), family = "poisson") 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
#MuMIn::r.squaredGLMM(detrend.numflow) 

detrend.flow<-lm(doy ~ GDD + GDD_lag + sfDOY ,
                 data=subset(flower_open, trait2=="num_flowers"&species=="salix")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
#MuMIn::r.squaredGLMM(detrend.flow) 

#correlations
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #strong negative r2 10%
cor.test(resid.numflow,resid.flow) #negative -0.3

#flowering time to leaf size
detrend.leafsize<-lm(value ~ GDD + sfDOY + GDD_lag ,
                     data=subset(flower_open, trait2=="leaf_length_mm"&species=="salix")) 
summary(detrend.leafsize)
resid.leafsize<-residuals(detrend.leafsize)

detrend.flow2<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="leaf_length_mm"&species=="salix")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)

#correlations
plot(resid.leafsize~resid.flow2)
summary(lm(resid.leafsize~resid.flow2)) #NS
cor.test(resid.leafsize,resid.flow2) #NS

#flowering time to stem growth
detrend.growth<-lm(log(value) ~ GDD + sfDOY + GDD_lag ,
                   data=subset(flower_open, trait2=="growth_inc_mm"&species=="salix"))
summary(detrend.growth)
resid.growth<-residuals(detrend.growth)

detrend.flow3<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="growth_inc_mm"&species=="salix")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)

#correlations
plot(resid.growth~resid.flow3)
summary(lm(resid.growth~resid.flow3)) #NS
cor.test(resid.growth,resid.flow3) #NS


#flowering time to repro size
detrend.repsize<-lm(value ~ GDD + sfDOY + GDD_lag ,
                     data=subset(flower_open, trait2=="repro_size"&species=="salix")) 
summary(detrend.repsize)
resid.repsize<-residuals(detrend.repsize)

detrend.flow4<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="repro_size"&species=="salix")) 
summary(detrend.flow4)
resid.flow4<-residuals(detrend.flow4)

#correlations
plot(resid.repsize~resid.flow4)
summary(lm(resid.repsize~resid.flow4)) #NS
cor.test(resid.repsize,resid.flow4) #NS


#only relationship retained is num flowers
ggplot(subset(flower_open,species=="salix"),
       aes(x=doy, y=value, fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+
  ylab("")+ xlab("DOY flower open")

#eriophorum----
eri<-subset(daring_phen_long, species=="eriophorum")
eri<-select(eri, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value")

#look at distribution of response data 
hist(eri$num_flowers) #use poisson
hist(eri$leaf_length_mm) #normal
hist(eri$diameter_mm)#normal
hist(eri$repro_size)
hist(log(eri$repro_size))#normal-log

hist(subset(eri, phen2=="first_flower_open")$doy)#normalish

detrend.numflow<-glm(value ~ GDD + GDD_lag + sfDOY ,
                     data=subset(flower_open, trait2=="num_flowers" &species=="eriophorum"), family = "poisson") 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
#MuMIn::r.squaredGLMM(detrend.numflow) 

detrend.flow<-lm(doy ~ GDD + GDD_lag + sfDOY ,
                 data=subset(flower_open, trait2=="num_flowers"&species=="eriophorum")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
#MuMIn::r.squaredGLMM(detrend.flow) 

#correlations
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #NS
cor.test(resid.numflow,resid.flow) #NS

#flowering time to leaf length
detrend.leafsize<-lm(value ~ GDD + sfDOY + GDD_lag ,
                   data=subset(flower_open, trait2=="leaf_length_mm"&species=="eriophorum"))
summary(detrend.leafsize)
resid.leafsize<-residuals(detrend.leafsize)

detrend.flow2<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="leaf_length_mm"&species=="eriophorum")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)

#correlations
plot(resid.leafsize~resid.flow2)
summary(lm(resid.leafsize~resid.flow2)) #positive but weird gap in residuals 
cor.test(resid.leafsize,resid.flow2) #positive relationship 

#flowering time to diameter
detrend.diam<-lm(value ~ GDD + sfDOY + GDD_lag ,
                     data=subset(flower_open, trait2=="diameter_mm"&species=="eriophorum"))
summary(detrend.diam)
resid.diam<-residuals(detrend.diam)

detrend.flow3<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="diameter_mm"&species=="eriophorum")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)

#correlations
plot(resid.diam~resid.flow3)
summary(lm(resid.diam~resid.flow3)) #positive but weird gap in residuals 
cor.test(resid.diam,resid.flow3) #positive relationship 

#flowering time to repro size
detrend.repsize<-lm(log(value) ~ GDD + sfDOY + GDD_lag ,
                 data=subset(flower_open, trait2=="repro_size"&species=="eriophorum"))
summary(detrend.repsize)
resid.repsize<-residuals(detrend.repsize)

detrend.flow4<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="repro_size"&species=="eriophorum")) 
summary(detrend.flow4)
resid.flow4<-residuals(detrend.flow4)

#correlations
plot(resid.repsize~resid.flow4)
summary(lm(resid.repsize~resid.flow4)) #NS
cor.test(resid.repsize,resid.flow4) #NS

#leaf size, diameter positive 
ggplot(subset(flower_open,species=="eriophorum"),
       aes(x=doy, y=value, fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+
  ylab("")+ xlab("DOY flower open")

#carex----
car<-subset(daring_phen_long, species=="carex")
car<-select(car, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value")

#look at distribution of response data 
hist(car$leaf_length_mm) #normal
hist(car$repro_size)#normal

hist(subset(car, phen2=="first_flower_open")$doy)#normalish

#flowering time to leaf length
detrend.leafsize<-lm(value ~ GDD + sfDOY + GDD_lag ,
                     data=subset(flower_open, trait2=="leaf_length_mm"&species=="carex"))
summary(detrend.leafsize)
resid.leafsize<-residuals(detrend.leafsize)

detrend.flow2<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="leaf_length_mm"&species=="carex")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)

#correlations
plot(resid.leafsize~resid.flow2)
summary(lm(resid.leafsize~resid.flow2)) #NS 
cor.test(resid.leafsize,resid.flow2) #NS

#flowering time to repro size
detrend.repsize<-lm(log(value) ~ GDD + sfDOY + GDD_lag ,
                    data=subset(flower_open, trait2=="repro_size"&species=="carex"))
summary(detrend.repsize)
resid.repsize<-residuals(detrend.repsize)

detrend.flow4<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="repro_size"&species=="carex")) 
summary(detrend.flow4)
resid.flow4<-residuals(detrend.flow4)

#correlations
plot(resid.repsize~resid.flow4)
summary(lm(resid.repsize~resid.flow4)) #negative strong 10% r2
cor.test(resid.repsize,resid.flow4) #-0.32


#repro size negative
ggplot(subset(flower_open,species=="carex"),
       aes(x=doy, y=value, fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+
  ylab("")+ xlab("DOY flower open")


#oxytropis----
oxy<-subset(daring_phen_long, species=="oxytropis")
oxy<-select(oxy, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value")

#look at distribution of response data 
hist(oxy$num_flowers) #use poisson
hist(oxy$num_fruit)#use poisson
hist(oxy$diameter_mm) #normal

hist(subset(oxy, phen2=="first_flower_open")$doy)#normalish

detrend.numflow<-glm(value ~ GDD + GDD_lag + sfDOY ,
                     data=subset(flower_open, trait2=="num_flowers" &species=="oxytropis"), family = "poisson") 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)

detrend.flow<-lm(doy ~ GDD + GDD_lag + sfDOY  ,
                 data=subset(flower_open, trait2=="num_flowers"&species=="oxytropis")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)

#correlations
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #weak negative
cor.test(resid.numflow,resid.flow) #weak negative

#flowering time to fruit number
detrend.numfruit<-glm(value ~ GDD + sfDOY + GDD_lag ,
                      data=subset(flower_open, trait2=="num_fruit"&species=="oxytropis"), family="poisson") 
summary(detrend.numfruit)
resid.numfruit<-residuals(detrend.numfruit)

detrend.flow2<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="num_fruit"&species=="oxytropis")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)

#correlations
plot(resid.numfruit~resid.flow2)
summary(lm(resid.numfruit~resid.flow2)) #NS
cor.test(resid.numfruit,resid.flow2) #NS

#flowering time to diameter
detrend.growth<-lm(log(value) ~ GDD + sfDOY + GDD_lag ,
                   data=subset(flower_open, trait2=="diameter_mm"&species=="oxytropis"))
summary(detrend.growth)
resid.growth<-residuals(detrend.growth)

detrend.flow3<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="diameter_mm"&species=="oxytropis")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)

#correlations
plot(resid.growth~resid.flow3)
summary(lm(resid.growth~resid.flow3)) #NS
cor.test(resid.growth,resid.flow3) #NS

#only relationship that is retained is # flow (p=0.09)
ggplot(subset(flower_open,species=="oxytropis"),
       aes(x=doy, y=value, fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+
  ylab("")+ xlab("DOY flower open")

#saxifraga----
sax<-subset(daring_phen_long, species=="saxifraga")
sax<-select(sax, -trait, -phen)%>%pivot_wider(names_from = "trait2", values_from = "value", values_fn=mean) #take mean of 47 diameter measurements that have 2 values 

#look at distribution of response data 
hist(sax$num_flowers) #use poisson
hist(sax$diameter_mm)#normal
hist(subset(sax, phen2=="first_flower_open")$doy)#normal

detrend.numflow<-glm(value ~ GDD + GDD_lag + sfDOY ,
                     data=subset(flower_open, trait2=="num_flowers" &species=="saxifraga"), family = "poisson") 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
#MuMIn::r.squaredGLMM(detrend.numflow) 

detrend.flow<-lm(doy ~ GDD + GDD_lag + sfDOY ,
                 data=subset(flower_open, trait2=="num_flowers"&species=="saxifraga")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
#MuMIn::r.squaredGLMM(detrend.flow) 

#correlations
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #negative r2 15%
cor.test(resid.numflow,resid.flow) #-0.39

#flowering time to diameter
detrend.diam<-lm(value ~ GDD + sfDOY + GDD_lag ,
                 data=subset(flower_open, trait2=="diameter_mm"&species=="saxifraga"))
summary(detrend.diam)
resid.diam<-residuals(detrend.diam)

detrend.flow3<-lm(doy ~ GDD + sfDOY + GDD_lag  ,
                  data=subset(flower_open, trait2=="diameter_mm"&species=="saxifraga")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)

#correlations
plot(resid.diam~resid.flow3)
summary(lm(resid.diam~resid.flow3)) #NS
cor.test(resid.diam,resid.flow3) #NS

#num flowers negative 
ggplot(subset(flower_open,species=="saxifraga"),
       aes(x=doy, y=value, fill=treatment))+
  geom_point(aes(colour=treatment), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+
  ylab("")+ xlab("DOY flower open")






detrend.numflow<-lmer(log(value+1) ~ GDD + sfDOY + GDD_lag + year + (1|species) + (1|species:plantid) ,
                  control = lmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")), 
                  data=subset(flower_open, trait2=="num_flowers")) 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
MuMIn::r.squaredGLMM(detrend.numflow) 

detrend.numflow<-lmer(log(value+1) ~   year + (1|species) + (1|plantid)  ,
                      control = lmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")), 
                      data=subset(flower_open, trait2=="num_flowers")) 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
MuMIn::r.squaredGLMM(detrend.numflow) 

#looks weird--some of these are number of flowering stalks...others are number of flowers 
ggplot(data=subset(flower_open, trait2=="num_flowers"), aes(x=log(value+1), y=GDD))+ 
  geom_point()
#may need to run at species level... 

#looks weird--some of these are number of fruiting stalks...others are number of fruit
ggplot(data=subset(flower_open, trait2=="num_fruit"), aes(x=log(value+1), y=GDD))+ 
  geom_point()
#may need to run at species level... 


#try with season avg/lags 
detrend.numflow2<-lmer(log(value+1) ~ Spring + Summer + Fall + Spring_lag + Summer_lag+ Fall_lag + sfDOY + year+
                        (1|species),  data=subset(flower_open, trait2=="num_flowers")) 

summary(detrend.numflow2)                       
#resid.numflow2<-residuals(detrend.numflow2)
MuMIn::r.squaredGLMM(detrend.numflow2) #GDD model better

#detrend phen (flowering) with climate 
#much more responsive 
detrend.flow<-lmer(scale(doy) ~   GDD + sfDOY + GDD_lag + (1|species)  , 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")), 
                   data=subset(flower_open, trait2=="num_flowers")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
MuMIn::r.squaredGLMM(detrend.flow)


#correlations
#flowering time to flower number
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #weak negative relationship but low R square ~2%
cor.test(resid.numflow,resid.flow) #negative




#detrend climate
detrend.gdd<-lmer(GDD ~  year + (1|species)  ,
                  control = lmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")), 
                  data=subset(flower_open, trait2=="num_flowers")) 
summary(detrend.gdd)
resid.gdd<-residuals(detrend.gdd)

detrend.sf<-lmer(sfDOY ~  year + (1|species)  ,
                 control = lmerControl(optimizer= "optimx",
                                       optCtrl  = list(method="nlminb")), 
                 data=subset(flower_open, trait2=="num_flowers")) 
summary(detrend.sf)
resid.sf<-residuals(detrend.sf)



#correlations
plot(resid.flow~resid.sf)
summary(lm(resid.flow~resid.sf)) #weak positive relationship  ~very low R square 
cor.test(resid.flow,resid.sf) #weak positive 

plot(resid.numflow~resid.sf)
summary(lm(resid.numflow~resid.sf)) #weak positive relationship  ~very low R square 
cor.test(resid.numflow,resid.sf) #weak positive 


#NUM FRUIT
#GDDs
detrend.numfruit<-lmer(log(value+1) ~  GDD +  sfDOY + GDD_lag+ year + (1|species)  ,
                      control = lmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")), 
                      data=subset(flower_open, trait2=="num_fruit")) 
summary(detrend.numfruit)
resid.numfruit<-residuals(detrend.numfruit)
MuMIn::r.squaredGLMM(detrend.numfruit)


#try with season avg/lags 
detrend.numfruit2<-lmer(log(value+1) ~ Spring + Summer + Fall + Spring_lag + Summer_lag+ Fall_lag + sfDOY +
                       + (1|species) + (1|year),  data=subset(flower_open, trait2=="num_fruit")) 
summary(detrend.numfruit2)                       
MuMIn::r.squaredGLMM(detrend.numfruit2)#GDD model better


#detrend phenology-use fruiting dataset here 
detrend.flow2<-lmer(scale(doy) ~  GDD + sfDOY + GDD_lag+ year + (1|species) , 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")), 
                   data=subset(flower_open, trait2=="num_fruit")) 
summary(detrend.flow2)
resid.flow2<-residuals(detrend.flow2)
MuMIn::r.squaredGLMM(detrend.flow2)

#Correlations
#flowering time to fruit number
plot(resid.numfruit~resid.flow2)
summary(lm(resid.numfruit~resid.flow2)) #weak negative relationship  ~very low R square 
cor.test(resid.numfruit,resid.flow2) #weak negative correlation 



#REP SIZE
#GDDs
detrend.repsize<-lmer(log(value+1) ~  GDD +  sfDOY + GDD_lag+ year + (1|species)  ,
                       control = lmerControl(optimizer= "optimx",
                                             optCtrl  = list(method="nlminb")), 
                       data=subset(flower_open, trait2=="repro_size")) 
summary(detrend.repsize)
resid.repsize<-residuals(detrend.repsize)
MuMIn::r.squaredGLMM(detrend.repsize)


#detrend phenology
detrend.flow3<-lmer(scale(doy) ~  GDD + sfDOY + GDD_lag+ year + (1|species) , 
                    control = lmerControl(optimizer= "optimx",
                                          optCtrl  = list(method="nlminb")), 
                    data=subset(flower_open, trait2=="repro_size")) 
summary(detrend.flow3)
resid.flow3<-residuals(detrend.flow3)
MuMIn::r.squaredGLMM(detrend.flow3)

#Correlations
#flowering time to rep size
plot(resid.repsize~resid.flow3)
summary(lm(resid.repsize~resid.flow3)) #NS 
cor.test(resid.repsize,resid.flow3) #NS


#Diameter (size)
#GDDs
detrend.size<-lmer(log(value+1) ~  GDD +  sfDOY + GDD_lag+ year + (1|species)  ,
                      control = lmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")), 
                      data=subset(flower_open, trait2=="diameter_mm")) 
summary(detrend.size)
resid.size<-residuals(detrend.size)
MuMIn::r.squaredGLMM(detrend.size)


#detrend phenology
detrend.flow4<-lmer(scale(doy) ~  GDD + sfDOY + GDD_lag+ year + (1|species) , 
                    control = lmerControl(optimizer= "optimx",
                                          optCtrl  = list(method="nlminb")), 
                    data=subset(flower_open, trait2=="diameter_mm")) 
summary(detrend.flow4)
resid.flow4<-residuals(detrend.flow4)
MuMIn::r.squaredGLMM(detrend.flow4)

#Correlations
#flowering time to rep size
plot(resid.size~resid.flow4)
summary(lm(resid.size~resid.flow4)) #NS 
cor.test(resid.size,resid.flow4) #NS

#Growth (stem-shrubs)
#GDDs
detrend.growth<-lmer(log(value+1) ~  GDD +  sfDOY + GDD_lag+ year + (1|species)  ,
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")), 
                   data=subset(flower_open, trait2=="growth_inc_mm")) 
summary(detrend.growth)
resid.growth<-residuals(detrend.growth)
MuMIn::r.squaredGLMM(detrend.growth) #high compared to others 


#detrend phenology
detrend.flow5<-lmer(scale(doy) ~  GDD + sfDOY + GDD_lag+ year + (1|species) , 
                    control = lmerControl(optimizer= "optimx",
                                          optCtrl  = list(method="nlminb")), 
                    data=subset(flower_open, trait2=="growth_inc_mm")) 
summary(detrend.flow5)
resid.flow5<-residuals(detrend.flow5)
MuMIn::r.squaredGLMM(detrend.flow5)

#Correlations
#flowering time to rep size
plot(resid.growth~resid.flow5) #outliers... need to deal with that??
hist(resid.flow5)
residflow5<-subset(resid.flow5>-0.4)
summary(lm(resid.growth~resid.flow5)) #NS 
cor.test(resid.size,resid.flow4) #NS


#Leaf length 
#GDDs
detrend.leafsize<-lmer(log(value+1) ~  GDD +  sfDOY + GDD_lag+ year + (1|species)  ,
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")), 
                   data=subset(flower_open, trait2=="leaf_length_mm")) 
summary(detrend.leafsize)
resid.leafsize<-residuals(detrend.leafsize)
MuMIn::r.squaredGLMM(detrend.leafsize)


#detrend phenology
detrend.flow6<-lmer(scale(doy) ~  GDD + sfDOY + GDD_lag+ year + (1|species) , 
                    control = lmerControl(optimizer= "optimx",
                                          optCtrl  = list(method="nlminb")), 
                    data=subset(flower_open, trait2=="leaf_length_mm")) 
summary(detrend.flow6)
resid.flow6<-residuals(detrend.flow6)
MuMIn::r.squaredGLMM(detrend.flow6)

#Correlations
#flowering time to leaf size
plot(resid.leafsize~resid.flow6)
summary(lm(resid.leafsize~resid.flow6)) #neg relationship- highly significant-low r square 
cor.test(resid.leafsize,resid.flow6) #neg 


#RUN SEMs----  
library(lme4)
library(lmerTest)
#try in brms 
#https://rpubs.com/jebyrnes/brms_bayes_sem
library(brms)
library(rstan)
rstan_options(disable_march_warning = TRUE)
rstan_options(mc.cores = parallel::detectCores())

#select data 
semdat<-select(flower_open, species, year, plantid, treatment, phen, trait, value, doy, GDD, GDD_lag, sfDOY,  phen2, trait2, Spring, Fall, Summer) 

hist(semdat$GDD) #bit left skewed...
hist(semdat$GDD_lag) #bit left skewed...
hist(semdat$sfDOY) #normal 
hist(semdat$doy) #normal



#num fruit----
#organize and scale data 
fruitdat<-subset(semdat, trait2=="num_fruit")
fruitdat$GDD<-scale(fruitdat$GDD) 
fruitdat$GDD_lag<-scale(fruitdat$GDD_lag) 
fruitdat$sfDOY<-scale(fruitdat$sfDOY) 

hist(fruitdat$doy) #normal
fruitdat$doy<-scale(fruitdat$doy) 
hist(fruitdat$doy) #normal ish

hist(fruitdat$value)
fruitdat<-subset(fruitdat, value<40) #remove large outlier 
hist(fruitdat$value)
fruitdat$value<-scale(log(fruitdat$value+1)) 
hist(fruitdat$value)#right skewed...try with skew normal

#fruitdat<-subset(fruitdat, !is.na(fruitdat$GDD_lag))#remove NAs for model convergence 

#brms model structure
#here species random effects account for both location across landscape & differences in biology eg GDD/phen/traits (how to separate?)
#mod1 <- bf(GDD ~ year + (1|species))  + gaussian()
#mod2 <- bf(sfDOY ~ year + (1|species))  + gaussian() 
#mod2.5 <- bf(sfDOY ~ year + (1|species) + (1|species:plantid))  + gaussian() 
#mod2 <- bf(sfDOY ~ year + (1|species))  + gaussian() 
#mod3 <- bf(GDD_lag ~ year + (1|species))  + gaussian()
mod4<- bf(doy~ GDD + sfDOY + GDD_lag + (1|species))  + gaussian()
mod4.5<- bf(doy~ GDD + sfDOY + GDD_lag+ (1|species) + (1|species:plantid))  + gaussian()
mod5 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (1|species))  + skew_normal()
mod5.5 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (1|species) + (1|species:plantid)) + skew_normal()
#include random slopes by species? might not need since already in above model(s)
mod5.75 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (doy|species) + (1|species:plantid)) + skew_normal() 

#run mods
fruitmod<-brm(mod4+ mod5 + set_rescor(FALSE),
              data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
fruitmod <- add_criterion(fruitmod, "loo") #for model comparison if needed (loo-cv)
save(fruitmod, file="data/BRMS_SEM_output/fruitnumber.Rdata")
summary(fruitmod)

#with individual level random effects 
fruitmod_indiv0<-brm(mod4.5+ mod5.5 + set_rescor(FALSE),
              data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
fruitmod_indiv0 <- add_criterion(fruitmod_indiv0, "loo") #for model comparison if needed (loo-cv)
save(fruitmod_indiv0, file="data/BRMS_SEM_output/fruitnumber_indiv0.Rdata")
summary(fruitmod_indiv0)

#with individual level random effects and random slopes by species 
fruitmod_indiv<-brm(mod4.5+ mod5.75 + set_rescor(FALSE),
                    data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
fruitmod_indiv <- add_criterion(fruitmod_indiv, "loo") #for model comparison if needed (loo-cv)
save(fruitmod_indiv, file="data/BRMS_SEM_output/fruitnumber_indiv.Rdata")
summary(fruitmod_indiv)

priorx <- c(set_prior(prior = 'normal(0,2)', class='b'), 	
            # global slope, int belongs to a normal distribution centered around 0
            set_prior(prior = 'normal(0,2)', class='Intercept'))
# random slopes, intercepts
#set_prior(prior = 'cauchy(0,2)', class='sd', group="species"))	#doesn't like my sd priors syntax.... 


loo(fruitmod, fruitmod_indiv, fruitmod_indiv0) #fruitmod_indiv best 
plot(loo(fruitmod_indiv))
#post checks for best model 
#plot model parameters -make sure no overlap between betas (multicollinearity)
mcmc_plot(fruitmod_indiv, 
          type = "intervals", 
          prob = .5, 
          prob_outer = .95,
          point_est = "median") 

pp_check(fruitmod_indiv, resp="value") #looks good 
pp_check(fruitmod_indiv, resp="doy") #off- skew normal didn't fix


bayesplot::mcmc_pairs(as.matrix(fruitmod_indiv),pars = c("b_doy_GDD", "b_doy_GDD_lag", "b_doy_sfDOY", 
                                                        "b_value_GDD", "b_value_GDD_lag", "b_value_sfDOY", "b_value_doy"))

#check convergence of chains- good except for sd params (prob want to run more iterations in final mods)
plot(fruitmod_indiv)


#num flowers----
flowdat<-subset(semdat, trait2=="num_flowers")
#scale all- add 1 becuase of lognormal dist for count data (below)
flowdat$GDD<-scale(flowdat$GDD)+1
flowdat$GDD_lag<-scale(flowdat$GDD_lag)+1
flowdat$sfDOY<-scale(flowdat$sfDOY)+1
flowdat$doy<-scale(flowdat$doy)+1

hist(flowdat$doy) #normal

hist(flowdat$value)
hist(log(flowdat$value+1)) #bit left skewed
#flowdat$value<-scale(log(flowdat$value+ 1)) #try with skew normal
flowdat$value<-scale(flowdat$value) + 1 #try with log normal
hist(flowdat$value)

#brms model structure
#here species random effects account for both location across landscape & differences in biology eg GDD/phen/traits (how to separate?)
mod4<- bf(doy~ GDD + sfDOY + GDD_lag+  (1|species))  + gaussian()
mod4.5<- bf(doy~ GDD + sfDOY + GDD_lag+ (1|species) + (1|species:plantid)) + gaussian()
mod5 <- bf(value~  GDD + sfDOY + GDD_lag+ doy+ (1|species))  + lognormal() 
mod5.5 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (1|species) + (1|species:plantid)) + lognormal()
mod5.75 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (doy|species) + (1|species:plantid)) +lognormal() 

#run mods
flowmod<-brm(mod4+ mod5 + set_rescor(FALSE),
              data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
flowmod <- add_criterion(flowmod, "loo") #for model comparison if needed (loo-cv)
save(flowmod, file="data/BRMS_SEM_output/flownumber.Rdata")
summary(flowmod)

#inividual random effects 
flowmod_indiv0<-brm(mod4.5+ mod5.5 + set_rescor(FALSE),
                   data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
flowmod_indiv0 <- add_criterion(flowmod_indiv0, "loo") #for model comparison if needed (loo-cv)
save(flowmod_indiv0, file="data/BRMS_SEM_output/flownumber_indiv0.Rdata")
summary(flowmod_indiv0)

#individual random effects & random species slopes 
flowmod_indiv<-brm(mod4.5+ mod5.75 + set_rescor(FALSE),
                    data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
flowmod_indiv <- add_criterion(flowmod_indiv, "loo") #for model comparison if needed (loo-cv)
save(flowmod_indiv, file="data/BRMS_SEM_output/flownumber_indiv.Rdata")
summary(flowmod_indiv)

loo(flowmod, flowmod_indiv0, flowmod_indiv)#flowmod indiv mod better 

#post checks for best model 
#plot model parameters -look for overlap between betas (multicollinearity)
mcmc_plot(flowmod_indiv, 
          type = "intervals", 
          prob = .5, 
          prob_outer = .95,
          point_est = "median") 

pp_check(flowmod_indiv, resp="value") #good
pp_check(flowmod_indiv, resp="doy") #good

parnames(flowmod_indiv)
bayesplot::mcmc_pairs(as.matrix(flowmod_indiv),pars = c("b_doy_GDD", "b_doy_GDD_lag", "b_doy_sfDOY", 
                      "b_value_GDD", "b_value_GDD_lag", "b_value_sfDOY", "b_value_doy"))


#growth----
growdat<-subset(semdat, trait2=="growth_inc_mm")
growdat$GDD<-scale(growdat$GDD)
growdat$GDD_lag<-scale(growdat$GDD_lag)
growdat$sfDOY<-scale(growdat$sfDOY)
growdat$year<-scale(as.numeric(growdat$year))
growdat$doy<-scale(growdat$doy)

hist(growdat$doy) #normal

hist(growdat$value)
hist(log(growdat$value+1)) #normal 
growdat$value<-scale(log(growdat$value+ 1)) 
hist(growdat$value)

#brms model structure
#here species random effects account for both location across landscape & differences in biology eg GDD/phen/traits (how to separate?)
mod4<- bf(doy~ GDD + sfDOY + GDD_lag+  (1|species))  + gaussian()
mod4.5<- bf(doy~ GDD + sfDOY + GDD_lag+ (1|species) + (1|species:plantid)) + gaussian()
mod5 <- bf(value~  GDD + sfDOY + GDD_lag+ doy+ (doy|species))  + gaussian()
mod5.5 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (doy|species) + (1|species:plantid)) + gaussian()
mod5.75 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (doy|species) + (1|species:plantid)) +gaussian() 

growmod<-brm(mod4+ mod5 + set_rescor(FALSE),
             data = growdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)

growmod <- add_criterion(growmod, "loo") #for model comparison if needed (loo-cv)
save(growmod, file="data/BRMS_SEM_output/stemgrowth.Rdata")
summary(growmod)

#individual random effects 
growmod_indiv0<-brm(mod4.5+ mod5.5 + set_rescor(FALSE),
                   data = growdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
growmod_indiv0 <- add_criterion(growmod_indiv0, "loo") #for model comparison if needed (loo-cv)
save(growmod_indiv0, file="data/BRMS_SEM_output/stemgrowth_indiv0.Rdata")
summary(growmod_indiv0)

#individual random effects plus species random slopes 
growmod_indiv<-brm(mod4.5+ mod5.75 + set_rescor(FALSE),
                    data = growdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
growmod_indiv <- add_criterion(growmod_indiv, "loo") #for model comparison if needed (loo-cv)
save(growmod_indiv, file="data/BRMS_SEM_output/stemgrowth_indiv.Rdata")
summary(growmod_indiv)

loo(growmod, growmod_indiv0, growmod_indiv)#indiv mod better 

#plot model parameters -make sure no overlap between betas (multicollinearity)
mcmc_plot(growmod, 
          type = "intervals", 
          prob = .5, 
          prob_outer = .95,
          point_est = "median") 

#post checks
pp_check(growmod, resp="value") #looks good
pp_check(growmod, resp="doy") #looks weird...misses a bit 
summary(growmod_indiv)

summary(testmod6)

#leaf length----
leafdat<-subset(semdat, trait2=="leaf_length_mm")
leafdat$GDD<-scale(leafdat$GDD)
leafdat$GDD_lag<-scale(leafdat$GDD_lag)
leafdat$sfDOY<-scale(leafdat$sfDOY)

hist(leafdat$doy) #normal-ish -slightly left skewed
leafdat$doy<-scale(leafdat$doy)

hist(leafdat$value)
hist(log(leafdat$value+1)) #normal 
leafdat$value<-scale(log(leafdat$value+ 1)) 
hist(leafdat$value) #RIGHT skewed

#brms model structure
#here species random effects account for both location across landscape & differences in biology eg GDD/phen/traits (how to separate?)
mod4<- bf(doy~ GDD + sfDOY + GDD_lag+  (1|species))  + gaussian()
mod4.5<- bf(doy~ GDD + sfDOY + GDD_lag+ (1|species) + (1|species:plantid)) + gaussian()
mod5 <- bf(value~  GDD + sfDOY + GDD_lag+ doy+ (doy|species))  + gaussian()
mod5.5 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (doy|species) + (1|species:plantid)) + gaussian()
mod5.75 <- bf(value~ GDD + sfDOY + GDD_lag+ doy+ (doy|species) + (1|species:plantid)) +skew_normal() 


leafmod<-brm( mod4+ mod5 + set_rescor(FALSE),
             data = leafdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(leafmod, file="data/BRMS_SEM_output/leaflength.Rdata")

summary(leafmod)

leafmod_indiv<-brm(mod4.5+ mod5.75 + set_rescor(FALSE),
                   data = leafdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(leafmod_indiv, file="data/BRMS_SEM_output/leaflength_indiv.Rdata")
pp_check(leafmod_indiv, resp="value") #looks good
pp_check(leafmod_indiv, resp="doy") #looks good

summary(leafmod_indiv)

#repro size----
reproszdat$GDD<-scale(reproszdat$GDD)
reproszdat$GDD_lag<-scale(reproszdat$GDD_lag)
reproszdat$sfDOY<-scale(reproszdat$sfDOY)

hist(reproszdat$doy) #normal-ish -slightly left skewed
reproszdat$doy<-scale(reproszdat$doy)

reproszdat<-subset(semdat, trait2=="repro_size")
hist(reproszdat$value)
hist(log(reproszdat$value+1))
reproszdat$value<-scale(log(reproszdat$value+1))#slightly right skewed-skew normal 

reproszmod<-brm(mod1 + mod2 + mod3+ mod4+ mod5 + set_rescor(FALSE),
             data = reproszdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(reproszmod, file="data/BRMS_SEM_output/reprosize.Rdata")

summary(reproszmod)

reproszmod_indiv<-brm(mod4.5+ mod5.75 + set_rescor(FALSE),
                   data = reproszdat, control = list(adapt_delta=0.99, max_treedepth = 13), cores=3, chains=3, iter=2000)
save(reproszmod_indiv, file="data/BRMS_SEM_output/reprosize_indiv.Rdata")

summary(reproszmod_indiv)

#diameter
diamdat<-subset(semdat, trait2=="diameter_mm")
diamdat$GDD<-scale(diamdat$GDD)
diamdat$GDD_lag<-scale(diamdat$GDD_lag)
diamdat$sfDOY<-scale(diamdat$sfDOY)

hist(diamdat$doy) #normal
diamdat$doy<-scale(diamdat$doy)

hist(diamdat$value)
hist(log(diamdat$value+1))#skew normal 
diamdat$value<-scale(log(diamdat$value+1))#+2  

diammod<-brm(mod1 + mod2 + mod3+ mod4+ mod5 + set_rescor(FALSE),
                data = diamdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(diammod, file="data/BRMS_SEM_output/diameter.Rdata")

summary(diammod)

diammod_indiv<-brm(mod4.5+ mod5.75 + set_rescor(FALSE),
                      data = diamdat, control = list(adapt_delta=0.99, max_treedepth = 13), cores=3, chains=3, iter=2000)
save(diammod_indiv, file="data/BRMS_SEM_output/diameter_indiv.Rdata")

summary(diammod_indiv)



#other mixed mods----
library(lme4)
library(lmerTest)

###TRY EHRLEN AND VALDES 2020 approach 
#seeds/fitness (std-yr)~ffd (std-yr) x clim + flow # + (1|plantid)
#Here issue is we are throwing away A LOT of data becuause not that many individuals have 2+ traits recorded plus phenology 
#in same year. plus model singularity/convergence issues 1/5/23

#pull traits back wide
#pull wide, take average of diameter measurements for sax (n=47 obs)
daring_phen_wide<-select(daring_phen_long, -phen2, -trait2)%>%pivot_wider(names_from = "trait", values_from = "value", values_fn = mean)

oxy<-subset(daring_phen_wide,species=="oxytropis")
ledum<-subset(daring_phen_wide,species=="ledum")
vacc<-subset(daring_phen_wide,species=="vaccinium")
betula<-subset(daring_phen_wide,species=="betula")
salix<-subset(daring_phen_wide,species=="salix")
carex<-subset(daring_phen_wide,species=="carex")
sax<-subset(daring_phen_wide,species=="saxifraga")

#ledum
ledum_mod1<-glmer(num_fruit_per_stalk~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                  control = glmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")),
                  subset(ledum, phen=="first_flower_bud"), family = "poisson")
summary(ledum_mod1)#negative interaction- does not converge-try brms 

#run as regular glm 
ledum_mod1<-glm(num_fruit_per_stalk~DOY_stdsf*scale(GDD) + num_flowers_per_stalk, #+ (1|plantid), 
                #  control = glmerControl(optimizer= "optimx",
                 #                        optCtrl  = list(method="nlminb")),
                  subset(ledum, phen=="first_flower_bud"), family = "poisson")
summary(ledum_mod1)#negative interaction


ledum_mod1.5<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                 control = lmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")),
                subset(ledum, phen=="first_flower_bud"))
summary(ledum_mod1.5)#NS- singular fit 


ledum_mod2<-glmer(num_fruit_per_stalk~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                  subset(ledum, phen=="first_flower_open"), family = "poisson")
summary(ledum_mod2) #NS -singular fit 

ledum_mod2.5<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                   subset(ledum, phen=="first_flower_open"))
summary(ledum_mod2.5)#NS- singular fit


ledum_mod3<-glmer(num_fruit_per_stalk~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                  subset(ledum, phen=="first_flower_shed"), family = "poisson")
summary(ledum_mod3) #NS singular fit 

ledum_mod3.5<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                   subset(ledum, phen=="first_flower_shed"))
summary(ledum_mod3.5)#won't run 

ledum_mod4<-glmer(num_fruit_per_stalk~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                  subset(ledum, phen=="last_flower_shed"), family = "poisson")
summary(ledum_mod4) #NS-singular fit 

ledum_mod4.5<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                   subset(ledum, phen=="last_flower_shed"))
summary(ledum_mod4.5)#won't run 

ledum_mod5<-glmer(num_fruit_per_stalk~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                  subset(ledum, phen=="first_fruit_visible"), family = "poisson")
summary(ledum_mod5) #NS- singular fit 

ledum_mod5.5<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD) + num_flowers_per_stalk + (1|plantid), 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                   subset(ledum, phen=="first_fruit_visible"))
summary(ledum_mod5.5)#won't run 

#vaccinium
vac_mod1<-glmer(num_fruit~DOY_stdsf*scale(GDD)+ num_flowers+ (1|plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                  subset(vacc, phen=="first_flower_bud"), family = "poisson")
summary(vac_mod1)#negative interaction 


vac_mod1.5<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD)+ num_flowers + (1|plantid), 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                 subset(vacc, phen=="first_flower_bud"))
summary(vac_mod1.5)#NS-singular fit


##oxytropis-small # observations 
oxy<-subset(oxy, !is.na(num_fruit_1))

oxy_mod1<-glmer(num_fruit~DOY_stdsf*scale(GDD) + (1|plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")),
                  subset(oxy, phen=="first_flower_bud"), family = "poisson")
summary(oxy_mod1)#positive interaction



#salix- small # of obs (~35)
sal_mod1<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD)+ num_catkins + (1|plantid) , 
                control = lmerControl(optimizer= "optimx",
                                       optCtrl  = list(method="nlminb")),
                subset(salix, phen=="first_stigma"))
summary(sal_mod1)#negative interaction -singular fit 

sal_mod2<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD)+ num_catkins + (1|plantid), 
               control = lmerControl(optimizer= "optimx",
                                     optCtrl  = list(method="nlminb")),
               subset(salix, phen=="pollen_shed"))
summary(sal_mod2)#NS-singular fit 

#betula 
bet_mod1<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD)+ num_male_catkins + (1|plantid), 
               control = lmerControl(optimizer= "optimx",
                                     optCtrl  = list(method="nlminb")),
               subset(betula, phen=="first_catkin_female"))
summary(bet_mod1)#NS


#try hybrid strategy
#pull wide, take average of diameter measurements for sax
daring_phen_wide2<-select(daring_phen_long, -trait, -phen)%>% pivot_wider(names_from = "trait2", values_from = "value", values_fn = mean)


library(lme4)
fruit_mod1<-glmer(num_fruit~DOY_stdsf*scale(GDD) + num_flowers + (1|species) + (1|species:plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method='nlminb')),
                  subset(daring_phen_wide, phen2=="first_flower_bud") , family = "poisson")
summary(fruit_mod1)#negative interaction


fruit_mod2<-glmer(num_fruit~DOY_stdsf*scale(GDD) + num_flowers + (1|species) + (1|species:plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method='nlminb')),
                  subset(daring_phen_wide, phen2=="first_flower_open") , family = "poisson")
summary(fruit_mod2)#NS-singular fit

fruit_mod3<-glmer(num_fruit~DOY_stdsf*scale(GDD) + num_flowers + (1|species) + (1|species:plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method='nlminb')),
                  subset(daring_phen_wide, phen2=="first_fruit_visible") , family = "poisson")
summary(fruit_mod3)#NS-singular fit-only 2 spp


growth_mod1<-lmer(growth_inc_mm~DOY_stdsf*scale(GDD)+ num_flowers + (1|species) + (1|species:plantid) , 
                  control = lmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")),
                  subset(daring_phen_wide, phen2=="first_flower_bud"))
summary(growth_mod1)#NS 



fruit_mod2<-glmer(num_fruit~DOY_stdsf*scale(GDD) + num_flowers + (1|species:plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method='L-BFGS-B')),
                  subset(daring_phen_wide, phen2=="first_flower_open") , family = "poisson")
summary(fruit_mod2)#negative interaction


fruit_mod1<-glmer(~DOY_stdsf*scale(GDD) + num_flowers + (1|species:plantid), 
                  control = glmerControl(optimizer= "optimx",
                                         optCtrl  = list(method='L-BFGS-B')),
                  subset(daring_phen_wide, phen2=="first_flower_bud") , family = "poisson")
summary(fruit_mod1)#negative interaction


#prob going to need brms here- lmer not converging  
library(brms)
library(rstan)
rstan_options(disable_march_warning = TRUE)

fruit_mod1<-bf(num_fruit~DOY_stdsf*scale(GDD) + num_flowers + (1|species) + (1|plantid)) 
fit_fruit_mod1<- brm(fruit_mod1, data = daring_phen_wide, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=100, family=poisson)

#not really resolving issues from above... 


#pattern is negative relationships b/w phenology & temp on fitness individually but positive interaction
#issues:
#doy phen and hobo temp are highly correlated >0.75
#adding in the lag effects changes or removes phenology relationships, should this be in a two step model?
#how to calculate/average temps? 
#does this answer whether phenology is repsonding to temp?? doesn't really--> support for Iler approach 

mod.numflow<-lmer(log(value+1) ~ GDD*scale(doy) + GDD_lag + (1|species)+ (1|year), 
                  control = lmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")), 
                  data=subset(flower_open, trait2=="num_flowers")) 

summary(mod.numflow) #7 spp -none for carex
#negative effect of phenology, neg effect temp (weak), pos interaction (weak)
#earlier flowering more flowers
#cooler temps more flowers 
#above patterns weakened when doy flowering is warmer

mod.numfruit<-lmer(log(value+1) ~ DOY_stdsf * hobo_temp_C + log(value_lag+1) + (1|species) + (1|year), 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")), 
                   data=subset(flower_open, trait2=="num_fruit")) 
summary(mod.numfruit) #singular fit with year random effect
#also adding in the lag effect term changes the direction of the 
# phenology relationship to positive but it's clearly negative from graph...

mod.veggrowth<-lmer(log(value+1) ~ DOY_stdsf * hobo_temp_C + log(value_lag+1) + (1|species) + (1|year), 
                    control = lmerControl(optimizer= "optimx",
                                          optCtrl  = list(method="nlminb")), 
                    data=subset(flower_open, trait2=="growth_inc_mm")) 
summary(mod.veggrowth)#4 spp -shrubs-neg effect temp, neg effect phen, pos interaction temp x phenology
#cooler temps=more growth 
#early season flowering=more growth
#above patterns weakened when doy flowering is warmer

mod.repsize<-lmer(log(value+1) ~ DOY_stdsf * av_air_temp_C + log(value_lag+1) + (1|species) + (1|year), 
                  control = lmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")), 
                  data=subset(flower_open, trait2=="repro_size")) 
summary(mod.repsize)#3 spp 
#not sure how to interpet this... 

#early season flowering under warm temps bad for veg growth, num flowers
#late season flowering under warm temps good for reproductive growth
#fitness tradeoffs (repro vs. veg) through phenology, dictated by climate warming 

mod.leafsize<-lmer(log(value+1) ~ DOY_stdsf * hobo_temp_C + log(value_lag+1) + (1|species) + (1|year), 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")), 
                   data=subset(flower_open, trait2=="leaf_length_mm")) 
summary(mod.leafsize)#Nothing 

mod.diam<-lmer(log(value+1) ~ DOY_stdsf * hobo_temp_C + log(value_lag+1) + (1|species) + (1|year), 
               control = lmerControl(optimizer= "optimx",
                                     optCtrl  = list(method="nlminb")), 
               data=subset(flower_open, trait2=="diameter_mm")) 
summary(mod.diam)#Nothing 



#has the flowering date gotten earlier over time? yes
ggplot(semdat,
       aes(x=as.numeric(year), y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

#more so in OTCs 
ggplot(semdat,
       aes(x=as.numeric(year), y=doy, fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

ggplot(semdat,
       aes(x=as.numeric(year), y=value,  fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm')+ facet_wrap(~trait2, scales='free') 

ggplot(semdat,
       aes(x=as.numeric(year), y=GDD))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

ggplot(semdat,
       aes(x=as.numeric(year), y=Spring))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

ggplot(semdat,
       aes(x=as.numeric(year), y=Summer))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
ggplot(semdat,
       aes(x=as.numeric(year), y=Fall))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

ggplot(semdat,
       aes(x=as.numeric(year), y=sfDOY))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 

ggplot(semdat,
       aes(x=Spring, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
ggplot(semdat,
       aes(x=Fall_lag, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
ggplot(semdat,
       aes(x=sfDOY, y=doy))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') 
