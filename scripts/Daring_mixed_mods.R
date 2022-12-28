library(dplyr)
library (tidyr)
library(tidyverse)
load('data/DLphen_dem_climate.Rdata')

#Lag effects----
#determine prev years' demog for lag effects 
#check for gaps in observations 
#if > 1 year since last observed this individual, put NA for lag value
daring_phen_lag<-select(daring_phen_long, -phen, -doy, -av_air_temp_C, -sfDOY)%>%distinct(.)%>%
  group_by(species, plantid, treatment, trait)%>%arrange(year)%>%
  mutate(year=as.numeric(year), value_lag=lag(value), year_lag=lag(year))%>%
  mutate(lagtime=year-year_lag)%>%
  mutate(value_lag=if_else(lagtime>1, NA_real_, value_lag))

#GDD lags 
#determine prev years' GDD for enviro lag effects 
names(daring_phen_long)
GDD_lag<-select(daring_phen_long, species, plantid, treatment, phen, year, GDD)%>%distinct(.)%>%
  group_by(species, plantid, treatment, phen)%>%arrange(year)%>%
  mutate(year=as.numeric(year), GDD_lag=lag(GDD), year_lag=lag(year))%>%
  mutate(lagtime=year-year_lag)%>%
  mutate(GDD_lag=if_else(lagtime>1, NA_real_, GDD_lag))%>%select(-year_lag, -lagtime)%>%
  distinct(.)


#merge back with phen data 
daring_phen_lag$year<-as.character(as.integer(daring_phen_lag$year))
daring_phen_long<-left_join(daring_phen_long, select(daring_phen_lag,  -year_lag, -lagtime))

GDD_lag$year<-as.character(as.integer(GDD_lag$year))
daring_phen_long<-left_join(daring_phen_long, GDD_lag)

#raw values
plot(daring_phen_long$value~daring_phen_long$value_lag)
cor.test(daring_phen_long$value, daring_phen_long$value_lag)
#logged 
plot(log(daring_phen_long$value+1)~(daring_phen_long$value_lag+1))
cor.test(log(daring_phen_long$value+1),log(daring_phen_long$value_lag+1))
#highly correlated...but we know this... 

#final cleaning
#load plot numbers
#only for spp with OTCs!!
plots<-read.csv("data/Daring_raw_data/plantidmaps.csv")%>%rename(plantid=plant_id, treatment=otc_treatment)
daring_phen_long<-left_join(daring_phen_long, plots)
daring_phen_long$plot<-as.character(as.numeric(daring_phen_long$plot))
names(daring_phen_long)

#take off numbers after traits
daring_phen_long$trait<-substring(daring_phen_long$trait,1,nchar(daring_phen_long$trait)-2)

#add back in info about phenology and trait order 
#traitsDL<-read.csv("data/traits_DL.csv")%>%select(species, treatment,trait_new,trait_order)%>%rename(trait=trait_new)%>%
#  distinct(.)
#daring_phen2_long<-left_join(daring_phen_long, traitsDL)


#reclassifying to first flower open and first flower bud equivalents
phen_check<-select(daring_phen_long, species, phen, treatment)%>%distinct(.)
phen_check$plantid<-NULL
phen_check$year<-NULL
phen_check<-distinct(phen_check)

#not sure if these are all correct?
#salix does not seem to have a flower 'bud' equivalaent
##SLOW!!! 
daring_phen_long<-mutate(daring_phen_long, phen2=case_when(
  species=="salix"& phen=="first_stigma"~"first_flower_open", 
  species=="betula"& phen=="first_stigma"~"first_flower_open", 
  species=="carex"& phen=="first_anther"~"first_flower_open", 
  species=="eriophorum"& phen=="first_anther"~"first_flower_open", 
  species=="betula"& phen=="first_catkin_male"~"first_flower_bud", 
  species=="carex" & phen=="first_stigma"~"first_flower_bud"))
daring_phen_long<-mutate(daring_phen_long, 
                         phen2=if_else(is.na(phen2), phen, phen2))

#for simplicity start with flower open & bud
flower_open<-subset(daring_phen_long, phen2=="first_flower_open")
flower_bud<-subset(daring_phen_long, phen2=="first_flower_bud")


#plot
ggplot(flower_open,
      aes(x=DOY_stdsf, y=log(value+1), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
 geom_smooth(method='lm') +
 scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
theme_bw()+  facet_wrap(~trait+phen2,scales = "free")+ 
ylab("trait value (log)")+ xlab("days past snowmelt")

#ggplot(flower_bud,
 #      aes(x=DOY_stdsf, y=log(value+1), fill=species))+
#  geom_point(aes(colour=species), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
 # geom_smooth(method='lm') +
#  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
#  theme_bw()+  facet_wrap(~trait+phen2,scales = "free")+ 
#  ylab("trait value (log)")+ xlab("days past snowmelt")

#filter outliers
#hist(flower_open$DOY_stdsf) #normally distributed 
#x<-mean(flower_open$DOY_stdsf, na.rm=T)
#y<-sd(flower_open$DOY_stdsf, na.rm=T)
#outlier<-x+3*y
#flower_open<-filter(flower_open, DOY_stdsf<outlier)

#hist(flower_bud$DOY_stdsf)#left skewed- needs log
#x<-mean(flower_bud$DOY_stdsf, na.rm=T)
#y<-sd(flower_open$DOY_stdsf, na.rm=T)
#outlier<-x+3*y
#flower_bud<-filter(flower_bud, DOY_stdsf<outlier)
#log transform
#hist(log(flower_bud$DOY_stdsf+5))


#reclassifying to num flowers, num fruits, repro_size equivalents
##SLOW!! 
daring_phen_long<-mutate(daring_phen_long, trait2=case_when(
  trait=="num_flowers"|trait=="num_flowering_stalks"|trait=="num_flowers_per_stalk"|trait=="num_flowering_stalks"|
    trait=="num_catkins"|trait=="num_female_catkins"~"num_flowers", 
  trait=="num_fruit"|trait=="num_fruit_per_stalk"~"num_fruit", 
  trait=="flowering_stalk_length_mm"|trait=="flowering_stalk_length_mm_early"|
    trait=="length_mature_female_catkins_mm"~"repro_size", TRUE~NA_character_))

daring_phen_long<-mutate(daring_phen_long, 
                         trait2=if_else(is.na(trait2), trait, trait2))

#now select only standardized traits
flower_open<-subset(daring_phen_long, phen2=="first_flower_open")
flower_open<-subset(flower_open, trait2=="num_flowers"|trait2=="num_fruit"|trait2=="repro_size"|
                      trait2=="diameter_mm"|trait=="growth_inc_mm"|trait=="leaf_length_mm")
flower_bud<-subset(daring_phen_long, phen2=="first_flower_bud")
flower_bud<-subset(flower_bud, trait2=="num_flowers"|trait2=="num_fruit"|trait2=="repro_size"|
                     trait2=="diameter_mm"|trait=="growth_inc_mm"|trait=="leaf_length_mm")


ggplot(flower_open,
       aes(x=DOY_stdsf, y=log(value+1), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
#  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+ 
  ylab("trait value (log)")+ xlab("days past snowmelt")

ggplot(flower_open,
       aes(x=DOY, y=log(value+1), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+ 
  ylab("trait value (log)")+ xlab("DOY flower open")

ggplot(flower_bud,
       aes(x=DOY_stdsf, y=log(value+1), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
 geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+ 
  ylab("trait value (log)")+ xlab("days past snowmelt")



save(flower_open, flower_bud, file="data/Mixed_mods_df.Rdata")
#load(file="data/Mixed_mods_df.Rdata")

####RUN MODELS---- 
library(lme4)
library(lmerTest)

load(file="data/Mixed_mods_df.Rdata")

#add in season lags 
flower_open<-left_join(flower_open, seas_clim)
flower_bud<-left_join(flower_bud, seas_clim)



#pattern is negative relationships b/w phenology & temp on fitness individually but positive interaction
#issues:
#doy phen and hobo temp are highly correlated >0.75
#adding in the lag effects changes or removes phenology relationships, should this be in a two step model?
#how to calculate/average temps? 
#does this answer whether phenology is repsonding to temp?? somewhat required

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

ggplot(flower_open,
       aes(x=DOY_stdsf, y=log(value+1)))+
  geom_point(aes(colour=hobo_temp_C), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 2)) + #cubic spline
  geom_smooth(method='lm') + scale_color_viridis_c()+
  #scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+ 
  ylab("trait value (log)")+ xlab("days past snowmelt")


#try with Iler et al 2017 approach
#detrend demog with climate and/or year (year as fixed or random effect doesn't change much...)

#NUM FLOWERS
#GDDs
detrend.numflow<-lmer(log(value+1) ~  GDD +  sfDOY + GDD_lag+ year + (1|species)  ,
                  control = lmerControl(optimizer= "optimx",
                                        optCtrl  = list(method="nlminb")), 
                  data=subset(flower_open, trait2=="num_flowers")) 
summary(detrend.numflow)
resid.numflow<-residuals(detrend.numflow)
MuMIn::r.squaredGLMM(detrend.numflow) 


#try with season avg/lags 
detrend.numflow2<-lmer(log(value+1) ~ Spring + Summer + Fall + Spring_lag + Summer_lag+ Fall_lag + sfDOY + year+
                        (1|species),  data=subset(flower_open, trait2=="num_flowers")) 

summary(detrend.numflow2)                       
#resid.numflow2<-residuals(detrend.numflow2)
MuMIn::r.squaredGLMM(detrend.numflow2) #GDD model better

#detrend phen (flowering) with climate 
#much more responsive 
detrend.flow<-lmer(scale(doy) ~  GDD + sfDOY + GDD_lag+  year + (1|species) , 
                   control = lmerControl(optimizer= "optimx",
                                         optCtrl  = list(method="nlminb")), 
                   data=subset(flower_open, trait2=="num_flowers")) 
summary(detrend.flow)
resid.flow<-residuals(detrend.flow)
MuMIn::r.squaredGLMM(detrend.flow)


#correlations
#flowering time to flower number
plot(resid.numflow~resid.flow)
summary(lm(resid.numflow~resid.flow)) #weak positive relationship  ~very low R square 
cor.test(resid.numflow,resid.flow) #weak positive 


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


     