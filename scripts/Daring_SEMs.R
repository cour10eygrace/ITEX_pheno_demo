#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(car)

specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

####RUN MODELS separately by spp---- 
load('data/DLphen_dem_climate.Rdata')
load(file="data/Mixed_mods_df.Rdata")

#add in season lags 
flower_open<-left_join(flower_open, seas_clim)
flower_bud<-left_join(flower_bud, seas_clim)
#deal with summer_lag for year 0 (2001) replace with current year Summer
flower_open<-mutate(flower_open, Summer_lag=if_else(is.na(Summer_lag), Summer, Summer_lag))

#RUN SEMs----  
library(lme4)
library(lmerTest)
library(brms)
library(rstan)
rstan_options(disable_march_warning = TRUE)
rstan_options(mc.cores = parallel::detectCores())

#select data 
semdat<-select(flower_open, species, year, plantid, plot, treatment, phen, trait, value, doy, GDD, GDD_lag, sfDOY,  phen2, trait2, Spring, Fall, Summer, 
               Spring_lag, Summer_lag, Fall_lag) 
semdat<-subset(semdat, treatment=="CTL") #only keep control data - look at OTCs separately 


#look at general relationships- what do we expect?
#change in temps over time 
ggplot(semdat,
       aes(x=as.numeric(year), y=Summer))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm") + theme_bw()+
  xlab("Year")+ ylab("Growing Season temp (C)")

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
  geom_smooth(method='lm') + 
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

#num fruit----
fruitdat<-subset(semdat, trait2=="num_fruit")
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

#by spp
#ledum 
fruitdat_ledum<-subset(fruitdat, species=="ledum")
fruitdat_ledum$Summer<-scale(fruitdat_ledum$Summer) 
fruitdat_ledum$sfDOY<-scale(fruitdat_ledum$sfDOY) 
fruitdat_ledum$Summer_lag<-scale(fruitdat_ledum$Summer_lag) 

hist(fruitdat_ledum$doy) #normal
fruitdat_ledum$doy<-scale(fruitdat_ledum$doy) 
hist(fruitdat_ledum$doy) #normal 

hist(fruitdat_ledum$value) #normal 
fruitdat_ledum<-subset(fruitdat_ledum, value>0) #only individuals that did fruit 
fruitdat_ledum$value<-scale(fruitdat_ledum$value)
hist(fruitdat_ledum$value)#normal

mod1<- bf(doy~  Summer + sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag + doy + (1|plantid) + (1|year)) + gaussian()

fruitmod_ledum<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = fruitdat_ledum, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(fruitmod_ledum)
pp_check(fruitmod_ledum, resp="doy") #looks good
pp_check(fruitmod_ledum, resp="value") #looks good 

save(fruitmod_ledum, file="data/BRMS_SEM_output/fruitnumber_ledum.Rdata")
loo(fruitmod_ledum)
loo_R2(fruitmod_ledum)
summary(fruitmod_ledum)

bayesplot::mcmc_pairs(as.matrix(fruitmod_ledum),pars = c( "b_doy_sfDOY", "b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
                                                         #  "b_doy_Spring","b_value_sfDOY", "b_value_Fall_lag", ))

bayestestR::ci(fruitmod_ledum, method="ETI", ci=c(0.85,0.9,0.95))

#vaccinium- too few observations?? (98-borderline) 
fruitdat_vacc<-subset(fruitdat, species=="vaccinium"& value>0)
fruitdat_vacc$Summer<-scale(fruitdat_vacc$Summer) 
fruitdat_vacc$sfDOY<-scale(fruitdat_vacc$sfDOY) 
fruitdat_vacc$Summer_lag<-scale(fruitdat_vacc$Summer_lag) 

hist(scale(log(fruitdat_vacc$doy))) #normal
fruitdat_vacc$doy<-scale(fruitdat_vacc$doy) 
hist(fruitdat_vacc$doy) #normal 

hist(fruitdat_vacc$value) #left skew
fruitdat_vacc$value<-scale(log(fruitdat_vacc$value))
hist(fruitdat_vacc$value)

mod1<- bf(doy~  Summer + sfDOY + (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer_lag + doy + (1|plantid) + (1|year)) + skew_normal()

fruitmod_vacc<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = fruitdat_vacc, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=10000)
plot(fruitmod_vacc)
pp_check(fruitmod_vacc, resp="doy") #looks ok
pp_check(fruitmod_vacc, resp="value") #looks bad

save(fruitmod_vacc, file="data/BRMS_SEM_output/fruitnumber_vacc.Rdata")
loo(fruitmod_vacc)
loo_R2(fruitmod_vacc)#very bad... 
summary(fruitmod_vacc)

bayesplot::mcmc_pairs(as.matrix(fruitmod_vacc),pars = c( "b_doy_sfDOY", "b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
#  "b_doy_Spring","b_value_sfDOY", "b_value_Fall_lag", ))

bayestestR::ci(fruitmod_vacc, method="ETI", ci=c(0.85,0.9,0.95))


#oxytropis- too few observations (68 only)
fruitdat_oxy<-subset(fruitdat, species=="oxytropis"& value>0)

#num flowers----
flowdat<-subset(semdat, trait2=="num_flowers")

#by species
#ledum
flowdat_ledum<-subset(flowdat, species=="ledum")
#scale vars 
flowdat_ledum$Summer<-scale(flowdat_ledum$Summer) 
flowdat_ledum$sfDOY<-scale(flowdat_ledum$sfDOY) 
flowdat_ledum$Summer_lag<-scale(flowdat_ledum$Summer_lag) 

hist(flowdat_ledum$doy) #normal
flowdat_ledum$doy<-scale(flowdat_ledum$doy) 
hist(flowdat_ledum$doy) #normal

hist(flowdat_ledum$value) #normal 
flowdat_ledum<-subset(flowdat_ledum, value>0) #only individuals that did flower 
flowdat_ledum<-subset(flowdat_ledum, value<40)#remove outlier 
hist(flowdat_ledum$value)
flowdat_ledum$value<-scale(flowdat_ledum$value)
hist(flowdat_ledum$value)#normal

mod1<- bf(doy~  Summer + sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag + doy + (1|plantid) + (1|year)) + gaussian()

flowmod_ledum<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = flowdat_ledum, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(flowmod_ledum)
pp_check(flowmod_ledum, resp="doy") #looks good
pp_check(flowmod_ledum, resp="value") #looks good 

save(flowmod_ledum, file="data/BRMS_SEM_output/flownumber_ledum.Rdata")
loo(flowmod_ledum) #looks good 
loo_R2(flowmod_ledum)
summary(flowmod_ledum)

bayesplot::mcmc_pairs(as.matrix(flowmod_ledum),pars = c( "b_doy_sfDOY", "b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
#some collinearity here...how to deal with this??
post<-posterior_samples(flowmod_ledum)

post %>% 
  select(b_doy_sfDOY, b_doy_Summer) %>% 
  cor() #not bad 0.33

post %>% 
  select(b_value_doy, b_value_Summer_lag) %>% 
  cor()#not bad 0.34


bayestestR::ci(flowmod_ledum, method="ETI", ci=c(0.85,0.9,0.95))

#vaccinium 
flowdat_vacc<-subset(flowdat, species=="vaccinium" & value>0) 
#scale predictors 
flowdat_vacc$Summer<-scale(flowdat_vacc$Summer) 
flowdat_vacc$sfDOY<-scale(flowdat_vacc$sfDOY) 
flowdat_vacc$Summer_lag<-scale(flowdat_vacc$Summer_lag) 

hist(flowdat_vacc$doy) #normal ish 
flowdat_vacc$doy<-scale(flowdat_vacc$doy)
hist(flowdat_vacc$doy) #normal ish 

hist(flowdat_vacc$value) #left skew
hist(log(flowdat_vacc$value)) #normal ish- 
flowdat_vacc$value<-scale(log(flowdat_vacc$value)) 
hist(flowdat_vacc$value) #skew normal?

mod1<- bf(doy~  Summer + sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag + doy + (1|plantid) + (1|year)) + skew_normal()

flowmod_vacc<-brm(mod1+ mod2 + set_rescor(FALSE),
                   data = flowdat_vacc, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)

  
plot(flowmod_vacc)
pp_check(flowmod_vacc, resp="doy") #meh..about the same as skew normal 
pp_check(flowmod_vacc, resp="value") #looks good 

save(flowmod_vacc, file="data/BRMS_SEM_output/flownumber_vacc.Rdata")
loo(flowmod_vacc)#good
loo_R2(flowmod_vacc)
summary(flowmod_vacc)

bayestestR::ci(flowmod_vacc, method="ETI", ci=c(0.85,0.9,0.95))

#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_vacc),pars = c( "b_doy_sfDOY", "b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
#  "b_doy_Spring","b_value_sfDOY", "b_value_Fall_lag", ))
post<-posterior_samples(flowmod_vacc)

post %>% 
  select(b_doy_sfDOY, b_doy_Summer) %>% 
  cor() #0.49 #these might need to run these separately.. 

post %>% 
  select(b_value_doy, b_value_Summer_lag) %>% 
  cor()#0.43- ehhh


sfmod<-brm(doy~ sfDOY + (1|plantid) + (1|year), 
data = flowdat_vacc, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
summary(sfmod)
#there is still huge error for SFDOY even when the only predictor in the model 
#so multicollinearity isn't what's driving the large error in that parameter estimate in main model 
#so can probably ignore it 

#salix
flowdat_sal<-subset(flowdat, species=="salix" & value>0) 
#scale predictors
flowdat_sal$Summer<-scale(flowdat_sal$Summer) +1
flowdat_sal$Summer_lag<-scale(flowdat_sal$Summer_lag) +1
flowdat_sal$sfDOY<-scale(flowdat_sal$sfDOY) +1
mean(flowdat_sal$sfDOY)#mean =1 
sd(flowdat_sal$sfDOY)#mean =1 

hist(flowdat_sal$doy) #normal
flowdat_sal$doy<-scale(flowdat_sal$doy) +1
hist(flowdat_sal$doy) 
flowdat_sal<-subset(flowdat_sal, doy>-2)#remove outlier 
hist(flowdat_sal$doy) #normal

hist(flowdat_sal$value) #left skewed
hist(log(flowdat_sal$value))#hmm
flowdat_sal$value<-log(flowdat_sal$value)
hist((flowdat_sal$value)) #exnormal?

mean(flowdat_sal$value)#1.3- add 1 to all scaled vars above 
sd(flowdat_sal$value) #~1

#try transform tukey??? doesn't work -use negbin 
#library(rcompanion)
#flowdat_sal_tuk<-transformTukey(flowdat_sal$value)
#hist(flowdat_sal_tuk)
#hist(scale(flowdat_sal_tuk))

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + gaussian()
#try setting some better priors for doy bc pp check looks bad ??
#priorx <- c(set_prior(prior = 'normal(0,2)', class='b', resp = "doy"), 	
            # global slope, int belongs to a normal distribution centered around 0
#            set_prior(prior = 'normal(0,2)', class='Intercept', resp="doy"))

flowmod_sal<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_sal, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000,
                 save_pars = save_pars(all = TRUE))
# random slopes, intercepts
#set_prior(prior = 'cauchy(0,2)', class='sd', group="species"))	#doesn't like my sd priors syntax.... 

plot(flowmod_sal)
pp_check(flowmod_sal, resp="doy") #looks bad
pp_check(flowmod_sal, resp="value") #looks ok

save(flowmod_sal, file="data/BRMS_SEM_output/flownumber_sal.Rdata")
loo(flowmod_sal)#looks good
loo_R2(flowmod_sal)
summary(flowmod_sal)


bayestestR::ci(flowmod_sal, method="ETI", ci=c(0.85,0.9,0.95))

#betula
flowdat_bet<-subset(flowdat, species=="betula" & value>0) 
#scale predictors 
flowdat_bet$Summer<-scale(flowdat_bet$Summer) 
flowdat_bet$Summer_lag<-scale(flowdat_bet$Summer_lag)
flowdat_bet$sfDOY<-scale(flowdat_bet$sfDOY)
#scale responses 
hist(flowdat_bet$doy) #normal
flowdat_bet$doy<-scale(flowdat_bet$doy)
hist(flowdat_bet$doy) #normal?

hist(flowdat_bet$value) #left skewed
hist(log(flowdat_bet$value))#hmm
hist(scale(log(flowdat_bet$value)))#hmm
flowdat_bet$value<-scale(log(flowdat_bet$value)) 
hist(flowdat_bet$value) #skew normal??

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + skew_normal()

flowmod_bet<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_bet, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))

plot(flowmod_bet)
pp_check(flowmod_bet, resp="doy") #looks good
pp_check(flowmod_bet, resp="value") #looks good

save(flowmod_bet, file="data/BRMS_SEM_output/flownumber_bet.Rdata")
loo(flowmod_bet)  
loo_R2(flowmod_bet)
summary(flowmod_bet)

bayestestR::ci(flowmod_bet, method="ETI", ci=c(0.85,0.9,0.95))

#saxifraga 
flowdat_sax<-subset(flowdat, species=="saxifraga" & value>0) 
#scale predictors 
flowdat_sax$Summer<-scale(flowdat_sax$Summer)
flowdat_sax$Summer_lag<-scale(flowdat_sax$Summer_lag)
flowdat_sax$sfDOY<-scale(flowdat_sax$sfDOY)
#scale responses 
hist(flowdat_sax$doy) #normal
flowdat_sax$doy<-scale(flowdat_sax$doy)
hist(flowdat_sax$doy) #normal?

hist(flowdat_sax$value) #left skewed
hist(log(flowdat_sax$value))#hmm
hist(scale(log(flowdat_sax$value)))#hmm
flowdat_sax$value<-scale(log(flowdat_sax$value)) 
hist(flowdat_sax$value) #skew normal??

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + gaussian()

flowmod_sax<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_sax, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))

plot(flowmod_sax)
pp_check(flowmod_sax, resp="doy") #looks good
pp_check(flowmod_sax, resp="value") #looks good

save(flowmod_sax, file="data/BRMS_SEM_output/flownumber_sax.Rdata")
loo(flowmod_sax)  #good
loo_R2(flowmod_sax)
summary(flowmod_sax)

bayestestR::ci(flowmod_sax, method="ETI", ci=c(0.85,0.9,0.95))

#oxytropis
flowdat_oxy<-subset(flowdat, species=="oxytropis" & value>0) 
hist(scale(flowdat_oxy$Summer))
#scale predictors 
flowdat_oxy$Summer<-scale(flowdat_oxy$Summer) 
flowdat_oxy$Summer_lag<-scale(flowdat_oxy$Summer_lag)
flowdat_oxy$sfDOY<-scale(flowdat_oxy$sfDOY)
#scale responses 
hist(flowdat_oxy$doy) 
flowdat_oxy$doy<-scale(flowdat_oxy$doy)
hist(flowdat_oxy$doy) #normal?

hist(flowdat_oxy$value) #left skewed
hist(log(flowdat_oxy$value))#hmm
hist(scale(log(flowdat_oxy$value)))#hmm
flowdat_oxy$value<-scale(log(flowdat_oxy$value)) 
hist(flowdat_oxy$value) #skew normal??

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) +gaussian()

flowmod_oxy<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_oxy, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))

plot(flowmod_oxy)
pp_check(flowmod_oxy, resp="doy") 
pp_check(flowmod_oxy, resp="value") 

save(flowmod_oxy, file="data/BRMS_SEM_output/flownumber_oxy.Rdata")
loo(flowmod_oxy)  
loo_R2(flowmod_oxy)
summary(flowmod_oxy)

bayestestR::ci(flowmod_oxy, method="ETI", ci=c(0.85,0.9,0.95))


#eriophorum 
flowdat_eri<-subset(flowdat, species=="eriophorum" & value>0) 
flowdat_eri$Summer<-scale(flowdat_eri$Summer) 
flowdat_eri$Summer_lag<-scale(flowdat_eri$Summer_lag) 
flowdat_eri$sfDOY<-scale(flowdat_eri$sfDOY) 

hist(flowdat_eri$doy) 
#flowdat_eri<-subset(flowdat_eri, doy<170)#remove outlier 
flowdat_eri$doy<-scale(flowdat_eri$doy)
hist(flowdat_eri$doy) 
flowdat_eri<-subset(flowdat_eri, doy<3)#remove outlier 
hist(flowdat_eri$doy) 

hist(flowdat_eri$value) #left skewed
hist(log(flowdat_eri$value))#normal 
flowdat_eri$value<-scale(log(flowdat_eri$value))
hist(flowdat_eri$value)#normal 

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + gaussian()

flowmod_eri<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_eri, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))
plot(flowmod_eri)
pp_check(flowmod_eri, resp="doy") #looks good
pp_check(flowmod_eri, resp="value") #looks good

save(flowmod_eri, file="data/BRMS_SEM_output/flownumber_eri.Rdata")
loo(flowmod_eri)
loo_R2(flowmod_eri)
summary(flowmod_eri)

bayestestR::ci(flowmod_eri, method="ETI", ci=c(0.85,0.9,0.95))


#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_eri),pars = c( "b_doy_sfDOY", "b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
#  "b_doy_Spring","b_value_sfDOY", "b_value_Fall_lag", ))



#repro size----
#by spp
reproszdat<-subset(semdat, trait2=="repro_size")
#eriophorum
reproszdat_eri<-subset(reproszdat, species=="eriophorum")
reproszdat_eri$Summer<-scale(reproszdat_eri$Summer) 
reproszdat_eri$Summer_lag<-scale(reproszdat_eri$Summer_lag) 
reproszdat_eri$sfDOY<-scale(reproszdat_eri$sfDOY) 

hist(reproszdat_eri$doy) #normal
reproszdat_eri$doy<-scale(reproszdat_eri$doy) 
hist(reproszdat_eri$doy) #normal

hist(reproszdat_eri$value) #left skewed
hist(log(reproszdat_eri$value)) #normal

reproszdat_eri$value<-scale(log(reproszdat_eri$value)) #log transform
hist(reproszdat_eri$value)#skew normal 

#remove problematic observation from LOO (obs #118)
reproszmod_eri$data[118,]
loo_obs<-reproszdat_eri[130,]
head(loo_obs)#check
reproszdat_eri<-anti_join(reproszdat_eri, loo_obs)

mod1<- bf(doy~ Summer +   sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag + doy+ (1|plantid) + (1|year)) + gaussian()

reproszmod_eri<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = reproszdat_eri, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                    save_pars = save_pars(all = TRUE))
plot(reproszmod_eri)
pp_check(reproszmod_eri, resp="doy") #looks ok
pp_check(reproszmod_eri, resp="value") #looks ok 

save(reproszmod_eri, file="data/BRMS_SEM_output/reprosize_eri.Rdata")
loo(reproszmod_eri)
loox<-loo(reproszmod_eri, reloo=T)
loo_R2(reproszmod_eri, robust = T)
bayes_R2(reproszmod_eri)
summary(reproszmod_eri)

bayestestR::ci(reproszmod_eri, method="ETI", ci=c(0.85,0.9,0.95))

#carex
reproszdat_car<-subset(reproszdat, species=="carex")
reproszdat_car$Summer<-scale(reproszdat_car$Summer) 
reproszdat_car$Summer_lag<-scale(reproszdat_car$Summer_lag) 
reproszdat_car$sfDOY<-scale(reproszdat_car$sfDOY) 

hist(reproszdat_car$doy) #normal
reproszdat_car$doy<-scale(reproszdat_car$doy) 
hist(reproszdat_car$doy) #normal

hist(reproszdat_car$value) #normal
reproszdat_car$value<-scale(reproszdat_car$value) 
hist(reproszdat_car$value)#normal

mod1<- bf(doy~ Summer +   sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag + doy+ (1|plantid) + (1|year)) + gaussian()

reproszmod_car<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = reproszdat_car, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(reproszmod_car)
pp_check(reproszmod_car, resp="doy") #looks ok
pp_check(reproszmod_car, resp="value") #looks ok 

save(reproszmod_car, file="data/BRMS_SEM_output/reprosize_car.Rdata")
loo(reproszmod_car)
loo_R2(reproszmod_car)
summary(reproszmod_car)

bayestestR::ci(reproszmod_car, method="ETI", ci=c(0.85,0.9,0.95))

#salix 
reproszdat_sal<-subset(reproszdat, species=="salix")
reproszdat_sal$Summer<-scale(reproszdat_sal$Summer) 
reproszdat_sal$Summer_lag<-scale(reproszdat_sal$Summer_lag) 
reproszdat_sal$sfDOY<-scale(reproszdat_sal$sfDOY) 

hist(reproszdat_sal$doy) #hmm
reproszdat_sal$doy<-scale(reproszdat_sal$doy)
hist(reproszdat_sal$doy) #normal-ish

hist(reproszdat_sal$value) #normal
reproszdat_sal$value<-scale(reproszdat_sal$value) 
hist(reproszdat_sal$value)#normal 

mod1<- bf(doy~ Summer +   sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag + doy+ (1|plantid) + (1|year)) + gaussian()

reproszmod_sal<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = reproszdat_sal, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(reproszmod_sal)
pp_check(reproszmod_sal, resp="doy") #looks ok
pp_check(reproszmod_sal, resp="value") #looks good

save(reproszmod_sal, file="data/BRMS_SEM_output/reprosize_sal.Rdata")
loo(reproszmod_sal)
loo_R2(reproszmod_sal)
summary(reproszmod_sal)

bayestestR::ci(reproszmod_sal, method="ETI", ci=c(0.85,0.9,0.95))

#diameter----
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


#treatment effects?----

dat<-select(flower_open, species, year, plantid, plot, treatment, phen, trait, value, doy, GDD, GDD_lag, sfDOY,  phen2, trait2, Spring, Fall, Summer, 
            Spring_lag, Summer_lag, Fall_lag) 

dat<-subset(dat, species=="eriophorum"|species=="ledum"|species=="vaccinium") #OTC species
dat<-subset(dat, value!=0) #remove zero flowering/fruiting  

#num fruit 
ggplot(subset(dat,trait2=="num_fruit"), 
       aes(x=doy, y=log(value), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+ theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Num fruit (log)")+ xlab("DOY mature flower")

#num flowers
ggplot(subset(dat,trait2=="num_flowers"), 
       aes(x=doy, y=log(value), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+
  theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

#repro size
ggplot(subset(dat,trait2=="repro_size"), 
       aes(x=doy, y=log(value), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+
  theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Reproductive size (log)")+ xlab("DOY mature flower")




#growth----
growdat<-subset(semdat, trait2=="growth_inc_mm")
#by species 
##ledum 
growdat_ledum<-subset(growdat, species=="ledum")
growdat_ledum$Summer<-scale(growdat_ledum$Summer) 
growdat_ledum$Summer_lag<-scale(growdat_ledum$Summer_lag) 
growdat_ledum$sfDOY<-scale(growdat_ledum$sfDOY) 

hist(growdat_ledum$doy) #normal
growdat_ledum$doy<-scale(growdat_ledum$doy) 
hist(growdat_ledum$doy) #normal ish

hist(growdat_ledum$value) #skew normal?
growdat_ledum$value<-scale(growdat_ledum$value)
hist(growdat_ledum$value)#skew normal 

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + gaussian()

growmod_ledum<-brm(mod1+ mod2 + set_rescor(FALSE),
                   data = growdat_ledum, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(growmod_ledum)
pp_check(growmod_ledum, resp="doy") #looks ok
pp_check(growmod_ledum, resp="value") #looks ok 

save(growmod_ledum, file="data/BRMS_SEM_output/stemgrowth_ledum.Rdata")
summary(growmod_ledum)
loo(growmod_ledum)
loo_R2(growmod_ledum)
bayestestR::ci(growmod_ledum, method="ETI", ci=c(0.85,0.9,0.95))

#vaccinium 
growdat_vacc<-subset(growdat, species=="vaccinium")
growdat_vacc$Summer<-scale(growdat_vacc$Summer) 
growdat_vacc$Summer_lag<-scale(growdat_vacc$Summer_lag) 
growdat_vacc$sfDOY<-scale(growdat_vacc$sfDOY) 

hist(growdat_vacc$doy) #normal
growdat_vacc$doy<-scale(growdat_vacc$doy) 
hist(growdat_vacc$doy) #normal 

hist(growdat_vacc$value) #left skew
hist(log(growdat_vacc$value)) #normal 
growdat_vacc$value<-scale(log(growdat_vacc$value))
hist(growdat_vacc$value)#normal 

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + skew_normal()

growmod_vacc<-brm(mod1+ mod2 + set_rescor(FALSE),
                   data = growdat_vacc, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(growmod_vacc)
pp_check(growmod_vacc, resp="doy") #looks ok
pp_check(growmod_vacc, resp="value") #looks ok 

save(growmod_vacc, file="data/BRMS_SEM_output/stemgrowth_vacc.Rdata")
loo(growmod_vacc)
loo_R2(growmod_vacc)
summary(growmod_vacc)

bayestestR::ci(growmod_vacc, method="ETI", ci=c(0.85,0.9,0.95))

#betula 
growdat_betula<-subset(growdat, species=="betula")
growdat_betula$Spring<-scale(growdat_betula$Summer) 
growdat_betula$Fall_lag<-scale(growdat_betula$Summer_lag) 
growdat_betula$sfDOY<-scale(growdat_betula$sfDOY) 

hist(growdat_betula$doy) #normal
growdat_betula$doy<-scale(growdat_betula$doy) 
hist(growdat_betula$doy) #normal ish

hist(growdat_betula$value)
hist(log(growdat_betula$value)) #skew normal?
growdat_betula$value<-scale(log(growdat_betula$value))
hist(growdat_betula$value)#skew normal 

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + skew_normal()

growmod_betula<-brm(mod1+ mod2 + set_rescor(FALSE),
                  data = growdat_betula, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(growmod_betula)
pp_check(growmod_betula, resp="doy") #looks ok
pp_check(growmod_betula, resp="value") #looks ok 

save(growmod_betula, file="data/BRMS_SEM_output/stemgrowth_betula.Rdata")
loo(growmod_betula)
loo_R2(growmod_betula)

summary(growmod_betula)

bayestestR::ci(growmod_betula, method="ETI", ci=c(0.85,0.9,0.95))


#salix- 
growdat_salix<-subset(growdat, species=="salix")
growdat_salix$Spring<-scale(growdat_salix$Summer) 
growdat_salix$Fall_lag<-scale(growdat_salix$Summer_lag) 
growdat_salix$sfDOY<-scale(growdat_salix$sfDOY) 

hist(growdat_salix$doy) #normal
growdat_salix$doy<-scale(growdat_salix$doy) 
hist(growdat_salix$doy) #normal ish

hist(log(growdat_salix$value)) #skew normal?
growdat_salix$value<-scale(log(growdat_salix$value))
hist(growdat_salix$value)#skew normal 

mod1<- bf(doy~ Summer+  sfDOY + (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + skew_normal()

growmod_salix<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = growdat_salix, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(growmod_salix)
pp_check(growmod_salix, resp="doy") #looks ok
pp_check(growmod_salix, resp="value") #looks ok 

save(growmod_salix, file="data/BRMS_SEM_output/stemgrowth_salix.Rdata")
loo(growmod_salix)
loo_R2(growmod_salix)

summary(growmod_salix)

bayestestR::ci(growmod_salix, method="ETI", ci=c(0.85,0.9,0.95))



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

#by species 
#eriophorum 
leafdat_eri<-subset(leafdat, species=="eriophorum")
leafdat_eri$Spring<-scale(leafdat_eri$Spring) 
leafdat_eri$Fall_lag<-scale(leafdat_eri$Fall_lag) 
leafdat_eri$sfDOY<-scale(leafdat_eri$sfDOY) 

hist(leafdat_eri$doy) #normal
leafdat_eri$doy<-scale(leafdat_eri$doy) 
hist(leafdat_eri$doy) #normal

hist(leafdat_eri$value) #left skewed
leafdat_eri$value<-scale(log(leafdat_eri$value+1)) #log transform
hist(leafdat_eri$value)#skew normal 

mod4.5<- bf(doy~ Spring + Fall_lag +  sfDOY + (1|plantid) + (1|year))  + gaussian()
mod5.5 <- bf(value~ Spring + Fall_lag +  sfDOY + doy+ (1|plantid) + (1|year)) + skew_normal()

leafmod_eri<-brm(mod4.5+ mod5.5 + set_rescor(FALSE),
                 data = leafdat_eri, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(leafmod_eri)
pp_check(leafmod_eri, resp="doy") #looks ok
pp_check(leafmod_eri, resp="value") #looks ok 

save(leafmod_eri, file="data/BRMS_SEM_output/leaflength_eri.Rdata")
loo(leafmod_eri)
loo_R2(leafmod_eri)
summary(leafmod_eri)

bayestestR::ci(leafmod_eri, method="ETI", ci=c(0.85,0.9,0.95))

#OLD CODE----


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
