#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(car)

source("scripts/colorscale.R")

#load datasets
load('data/DLphen_dem_climate.Rdata')
load(file="data/Mixed_mods_df.Rdata")

#merge clim data  
flower_open<-left_join(flower_open, seas_clim)
flower_bud<-left_join(flower_bud, seas_clim)

#select data 
semdat<-select(flower_open, species, year, plantid, plot, treatment, phen, trait, value, doy, GDD, GDD_lag, sfDOY,  phen2, trait2, Spring, Fall, Summer, 
               Spring_lag, Summer_lag, Fall_lag) 
semdat<-subset(semdat, treatment=="CTL") #only keep control data - look at OTCs separately 

#RUN SEMs----  
library(lme4)
library(lmerTest)
library(brms)
library(rstan)
rstan_options(disable_march_warning = TRUE)
rstan_options(mc.cores = parallel::detectCores())

#num flowers----
flowdat<-subset(semdat, trait2=="num_flowers" & value > 0)

#all species together -random intercepts
flowdat$Summer<-scale(flowdat$Summer) 
hist(flowdat$doy) #normal
flowdat$doy<-scale(flowdat$doy) 
hist(flowdat$doy) #normal

hist(flowdat$value) #left skewed
hist(log(flowdat$value)) #pretty normal 
hist(scale(log(flowdat$value))) #pretty normal 
flowdat$value<-scale(log(flowdat$value)) 

mod1<- bf(doy~  Summer +  (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(value~ Summer + doy + (1|species:plantid) + (1|year) + (1|species)) + gaussian()

flowmod<-brm(mod1+ mod2 + set_rescor(FALSE),
             data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(flowmod)
pp_check(flowmod, resp="doy") #
pp_check(flowmod, resp="value") #looks good

save(flowmod, file="data/BRMS_SEM_output/flownumber.Rdata")
loo(flowmod)
loo_R2(flowmod)

parnames(flowmod)
bayesplot::mcmc_pairs(as.matrix(flowmod),pars = c("b_doy_Summer", "b_value_Summer","b_value_doy"))   
vcov(flowmod, correlation=T)%>%round(digits=2) #0.4 correlation -not too bad 

summary(flowmod)

#by species
flowdat<-subset(semdat, trait2=="num_flowers" & value > 0)

#ledum
flowdat_ledum<-subset(flowdat, species=="ledum")
#scale vars 
flowdat_ledum$Summer<-scale(flowdat_ledum$Summer) 
flowdat_ledum$Summer_lag<-scale(flowdat_ledum$Summer_lag) 

hist(flowdat_ledum$doy) #normal
flowdat_ledum$doy<-scale(flowdat_ledum$doy) 
hist(flowdat_ledum$doy) #normal-ish

hist(flowdat_ledum$value) 
flowdat_ledum<-subset(flowdat_ledum, value<40)#remove outlier 
hist(flowdat_ledum$value)
flowdat_ledum$value<-scale(flowdat_ledum$value)
hist(flowdat_ledum$value)#normal

mod1<- bf(doy~  Summer +  (1|plantid) + (1|year))  + gaussian()
#mod2 <- bf(value~ Summer + doy + (1|plantid) + (1|year)) + gaussian()
mod2 <- bf(value~ Summer_lag + doy + (1|plantid) + (1|year)) + gaussian()

flowmod_ledum<-brm(mod1+ mod2 + set_rescor(FALSE),
                   data = flowdat_ledum, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(flowmod_ledum, file="data/BRMS_SEM_output/flownumber_ledum.Rdata")

plot(flowmod_ledum)
pp_check(flowmod_ledum, resp="doy") #looks good
pp_check(flowmod_ledum, resp="value") #looks good 

loo(flowmod_ledum) #looks good 
loo_R2(flowmod_ledum)
summary(flowmod_ledum)

bayesplot::mcmc_pairs(as.matrix(flowmod_ledum),pars = c( "b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
#collinearity here...how to deal with this??
vcov(flowmod_ledum, correlation=T)%>%round(digits=2) #0.68 high,  #0.38 better 

bayestestR::ci(flowmod_ledum, method="ETI", ci=c(0.85,0.9,0.95))

#vaccinium 
flowdat_vacc<-subset(flowdat, species=="vaccinium") 
#scale predictors 
flowdat_vacc$Summer<-scale(flowdat_vacc$Summer) 

hist(flowdat_vacc$doy) #normal ish 
flowdat_vacc$doy<-scale(flowdat_vacc$doy)
hist(flowdat_vacc$doy) #normal ish 

hist(flowdat_vacc$value) #left skew
hist(log(flowdat_vacc$value)) #normal ish- 
flowdat_vacc$value<-scale(log(flowdat_vacc$value)) 
hist(flowdat_vacc$value) #skew normal?

mod1<- bf(doy~  Summer + (1|plantid) + (1|year))  + gaussian()
#mod2 <- bf(value~ Summer + doy + (1|plantid) + (1|year)) + gaussian()
mod2 <- bf(value~ Summer_lag + doy + (1|plantid) + (1|year)) + gaussian()

flowmod_vacc<-brm(mod1+ mod2 + set_rescor(FALSE),
                  data = flowdat_vacc, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(flowmod_vacc, file="data/BRMS_SEM_output/flownumber_vacc.Rdata")

plot(flowmod_vacc)
pp_check(flowmod_vacc, resp="doy") #meh..about the same as skew normal 
pp_check(flowmod_vacc, resp="value") #looks good 

loo(flowmod_vacc)#good
loo_R2(flowmod_vacc)
summary(flowmod_vacc)

bayestestR::ci(flowmod_vacc, method="ETI", ci=c(0.85,0.9,0.95))

#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_vacc),pars = c("b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
vcov(flowmod_vacc, correlation=T)%>%round(digits=2) #0.63 high... 0.39 better 


#salix
flowdat_sal<-subset(flowdat, species=="salix") 
#scale predictors
flowdat_sal$Summer<-scale(flowdat_sal$Summer) +1

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

mod1<- bf(doy~ Summer+ (1|plantid) + (1|year))  + gaussian()
#mod2 <- bf(value~ Summer +doy+ (1|plantid) + (1|year)) + gaussian()
mod2 <- bf(value~ Summer_lag +doy+ (1|plantid) + (1|year)) + gaussian()

#try setting some better priors for doy bc pp check looks bad ??
#priorx <- c(set_prior(prior = 'normal(0,2)', class='b', resp = "doy"), 	
# global slope, int belongs to a normal distribution centered around 0
#            set_prior(prior = 'normal(0,2)', class='Intercept', resp="doy"))

flowmod_sal<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_sal, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000,
                 save_pars = save_pars(all = TRUE))
save(flowmod_sal, file="data/BRMS_SEM_output/flownumber_sal.Rdata")

plot(flowmod_sal)
pp_check(flowmod_sal, resp="doy") #not great
pp_check(flowmod_sal, resp="value") #looks ok

loo(flowmod_sal)#looks ok
loo_R2(flowmod_sal)
summary(flowmod_sal)


bayestestR::ci(flowmod_sal, method="ETI", ci=c(0.85,0.9,0.95))

#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_sal),pars = c("b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
vcov(flowmod_sal, correlation=T)%>%round(digits=2) #0.63 high... 0.22 better


#betula
flowdat_bet<-subset(flowdat, species=="betula") 
#scale predictors 
flowdat_bet$Summer<-scale(flowdat_bet$Summer) 

#scale responses 
hist(flowdat_bet$doy) #normal
flowdat_bet$doy<-scale(flowdat_bet$doy)
hist(flowdat_bet$doy) #normal?

hist(flowdat_bet$value) #left skewed
hist(log(flowdat_bet$value))#hmm
hist(scale(log(flowdat_bet$value)))#hmm
flowdat_bet$value<-scale(log(flowdat_bet$value)) 
hist(flowdat_bet$value) #skew normal??

mod1<- bf(doy~ Summer+ (1|plantid) + (1|year))  + gaussian()
#mod2 <- bf(value~ Summer +  doy+ (1|plantid) + (1|year)) + gaussian()
mod2 <- bf(value~ Summer_lag +  doy+ (1|plantid) + (1|year)) + gaussian()

flowmod_bet<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_bet, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))
save(flowmod_bet, file="data/BRMS_SEM_output/flownumber_bet.Rdata")

plot(flowmod_bet)
pp_check(flowmod_bet, resp="doy") #looks good
pp_check(flowmod_bet, resp="value") #looks ok

loo(flowmod_bet) #ok
loo_R2(flowmod_bet)
summary(flowmod_bet)

bayestestR::ci(flowmod_bet, method="ETI", ci=c(0.85,0.9,0.95))

#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_bet),pars = c("b_doy_Summer", "b_value_doy", "b_value_Summer_lag"))
vcov(flowmod_bet, correlation=T)%>%round(digits=2) #0.64 high... 0.07 


#saxifraga 
flowdat_sax<-subset(flowdat, species=="saxifraga") 
#scale predictors 
flowdat_sax$Summer<-scale(flowdat_sax$Summer)
#scale responses 
hist(flowdat_sax$doy) #normal
flowdat_sax$doy<-scale(flowdat_sax$doy)
hist(flowdat_sax$doy) #normal?

hist(flowdat_sax$value) #left skewed
hist(log(flowdat_sax$value))#hmm
hist(scale(log(flowdat_sax$value)))#hmm
flowdat_sax$value<-scale(log(flowdat_sax$value)) 
hist(flowdat_sax$value) #skew normal??

mod1<- bf(doy~ Summer+  (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer+  doy+ (1|plantid) + (1|year)) + gaussian()

flowmod_sax<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_sax, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))
save(flowmod_sax, file="data/BRMS_SEM_output/flownumber_sax.Rdata")

plot(flowmod_sax)
pp_check(flowmod_sax, resp="doy") #looks good
pp_check(flowmod_sax, resp="value") #looks good

loo(flowmod_sax)  #good
loo_R2(flowmod_sax)
summary(flowmod_sax)

bayestestR::ci(flowmod_sax, method="ETI", ci=c(0.85,0.9,0.95))


#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_sax),pars = c("b_doy_Summer", "b_value_doy", "b_value_Summer"))
vcov(flowmod_sax, correlation=T)%>%round(digits=2) #0.5 borderline -either way... 

#oxytropis
flowdat_oxy<-subset(flowdat, species=="oxytropis" ) 
#scale predictors 
flowdat_oxy$Summer<-scale(flowdat_oxy$Summer) 
hist(scale(flowdat_oxy$Summer))

#scale responses 
hist(flowdat_oxy$doy) 
flowdat_oxy$doy<-scale(log(flowdat_oxy$doy))
hist(flowdat_oxy$doy) #normal?

hist(flowdat_oxy$value) #left skewed
hist(log(flowdat_oxy$value))#hmm
hist(scale(log(flowdat_oxy$value)))#hmm
flowdat_oxy$value<-scale(log(flowdat_oxy$value)) 
hist(flowdat_oxy$value) #normal??

mod1<- bf(doy~ Summer+ (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~ Summer  +doy+ (1|plantid) + (1|year)) +gaussian()

flowmod_oxy<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_oxy, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))
save(flowmod_oxy, file="data/BRMS_SEM_output/flownumber_oxy.Rdata")

plot(flowmod_oxy)
pp_check(flowmod_oxy, resp="doy") 
pp_check(flowmod_oxy, resp="value") #not great 

loo(flowmod_oxy)  #ok
loo_R2(flowmod_oxy)
summary(flowmod_oxy)

bayestestR::ci(flowmod_oxy, method="ETI", ci=c(0.85,0.9,0.95))

#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_oxy),pars = c("b_doy_Summer", "b_value_doy", "b_value_Summer"))
vcov(flowmod_oxy, correlation=T)%>%round(digits=2) #0.46 


#eriophorum 
flowdat_eri<-subset(flowdat, species=="eriophorum") 
#scale data
flowdat_eri$Summer<-scale(flowdat_eri$Summer) 

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

mod1<- bf(doy~ Summer + (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer  doy+ (1|plantid) + (1|year)) + gaussian()

flowmod_eri<-brm(mod1+ mod2 + set_rescor(FALSE),
                 data = flowdat_eri, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                 save_pars = save_pars(all = TRUE))
save(flowmod_eri, file="data/BRMS_SEM_output/flownumber_eri.Rdata")

plot(flowmod_eri)
pp_check(flowmod_eri, resp="doy") #looks good
pp_check(flowmod_eri, resp="value") #looks good

loo(flowmod_eri)
loo_R2(flowmod_eri)
summary(flowmod_eri)

bayestestR::ci(flowmod_eri, method="ETI", ci=c(0.85,0.9,0.95))


#check multicollinearity 
bayesplot::mcmc_pairs(as.matrix(flowmod_eri),pars = c( "b_doy_Summer", "b_value_doy", "b_value_Summer"))
vcov(flowmod_eri, correlation=T)%>%round(digits=2) #0.41 not bad 

#prob fruit----
fruitdat<-subset(semdat, trait2=="num_fruit") #don't remove zeroes because individuals could flower but not fruit 
fruitdat<-mutate(fruitdat, probfruit=ifelse(value==0, 0, 1))
hist(fruitdat$probfruit)

fruitdat$Summer<-scale(fruitdat$Summer)
fruitdat$Summer_lag<-scale(fruitdat$Summer_lag)
hist(fruitdat$Summer)
hist(fruitdat$Summer_lag)

hist(fruitdat$doy) #normal
fruitdat$doy<-scale(fruitdat$doy)
hist(fruitdat$doy, breaks = 20) #bit weird

mod1<- bf(doy~  Summer + Summer_lag + (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(probfruit ~  Summer_lag + doy + (1|species:plantid) + (1|year) + (1|species)) + bernoulli()

pfruitmod<-brm(mod1+ mod2 + set_rescor(FALSE),
              data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(pfruitmod)
pp_check(pfruitmod, resp="doy") #good
pp_check(pfruitmod, resp="probfruit") #good
loo(pfruitmod) #good 
loo_R2(pfruitmod) #good
summary(pfruitmod)

parnames(fruitmod)
bayesplot::mcmc_pairs(as.matrix(pfruitmod),pars = c("b_doy_Summer","b_doy_Summer_lag", "b_probfruit_Summer_lag", "b_probfruit_doy"))   
vcov(pfruitmod, correlation=T)%>%round(digits=2)
bayestestR::ci(pfruitmod, method="ETI", ci=c(0.85,0.9,0.95))

#num fruit----
fruitdat2<-subset(semdat, trait2=="num_fruit"& value>0) #don't remove zeroes because individuals could flower but not fruit 

fruitdat2$Summer<-scale(fruitdat2$Summer)
fruitdat2$Summer_lag<-scale(fruitdat2$Summer_lag)
hist(fruitdat2$Summer)
hist(fruitdat2$Summer_lag)

hist(fruitdat2$doy) #normal
fruitdat2$doy<-scale(fruitdat2$doy)
hist(fruitdat2$doy, breaks = 20) #bit weird

hist(fruitdat2$value, breaks=20) #right skewed
fruitdat<-subset(fruitdat2, value<40) #remove outlier
hist(fruitdat2$value) #right skewed

library(rcompanion)
transformTukey(fruitdat2$value)
hist(log(fruitdat2$value)) 
hist(scale(log(fruitdat2$value))) #looks ok..

fruitdat2$value<-scale(log(fruitdat2$value)) 
hist(fruitdat2$value) #normal ish 

unique(fruitdat2$species)#3 spp 

mod1<- bf(doy~  Summer + Summer_lag + (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(value~  Summer_lag + doy + (1|species:plantid) + (1|year) + (1|species)) + skew_normal()

fruitmod<-brm(mod1+ mod2 + set_rescor(FALSE),
             data = fruitdat2, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)

save(fruitmod, file="data/BRMS_SEM_output/fruitnumber.Rdata")

plot(fruitmod)
pp_check(fruitmod, resp="doy") #good
pp_check(fruitmod, resp="value") #gaussian worse than skew normal

loo(fruitmod) #skew normal bad, gaussian ok 
loo_R2(fruitmod) #seems too high for fruit number 
summary(fruitmod)

parnames(fruitmod)
bayesplot::mcmc_pairs(as.matrix(fruitmod),pars = c("b_doy_Summer","b_doy_Summer_lag", "b_value_Summer_lag", "b_value_doy"))   
vcov(fruitmod, correlation=T)%>%round(digits=2)
bayestestR::ci(fruitmod, method="ETI", ci=c(0.85,0.9,0.95))


#by spp
fruitdat<-subset(semdat, trait2=="num_fruit")
#ledum 
fruitdat_ledum<-subset(fruitdat, species=="ledum")
fruitdat_ledum$Summer<-scale(fruitdat_ledum$Summer) 
fruitdat_ledum$Summer_lag<-scale(fruitdat_ledum$Summer_lag) 

hist(fruitdat_ledum$doy) #normal
fruitdat_ledum$doy<-scale(fruitdat_ledum$doy) 
hist(fruitdat_ledum$doy) #normal 

hist(fruitdat_ledum$value) #normal 
fruitdat_ledum$value<-scale(fruitdat_ledum$value)
hist(fruitdat_ledum$value)#normal

mod1<- bf(doy~  Summer + Summer_lag + (1|plantid) + (1|year))  + gaussian()
mod2 <- bf(value~  Summer_lag  + doy + (1|plantid) + (1|year)) + gaussian()

fruitmod_ledum<-brm(mod1+ mod2 + set_rescor(FALSE),
                    data = fruitdat_ledum, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(fruitmod_ledum)
pp_check(fruitmod_ledum, resp="doy") #looks good
pp_check(fruitmod_ledum, resp="value") #looks good 

save(fruitmod_ledum, file="data/BRMS_SEM_output/fruitnumber_ledum.Rdata")
loo(fruitmod_ledum) #good
loo_R2(fruitmod_ledum) #good
summary(fruitmod_ledum)

parnames(fruitmod_ledum)
bayesplot::mcmc_pairs(as.matrix(fruitmod_ledum),pars = c("b_doy_Summer",   "b_doy_Summer_lag", "b_value_Summer_lag",  "b_value_doy"))   
vcov(fruitmod_ledum, correlation=T)%>%round(digits=2) #no issues 
  
bayestestR::ci(fruitmod_ledum, method="ETI", ci=c(0.85,0.9,0.95))


#vaccinium- too few observations?? (98-borderline) 
fruitdat_vacc<-subset(fruitdat, species=="vaccinium")
fruitdat_vacc$Summer<-scale(fruitdat_vacc$Summer) 
fruitdat_vacc$Summer_lag<-scale(fruitdat_vacc$Summer_lag) 

hist(fruitdat_vacc$doy) 
hist(scale(fruitdat_vacc$doy))
fruitdat_vacc$doy<-scale(fruitdat_vacc$doy) 
hist(fruitdat_vacc$doy) #normal ish

hist(fruitdat_vacc$value) #right skew
fruitdat_vacc$value<-scale(log(fruitdat_vacc$value))
hist(fruitdat_vacc$value)

mod1<- bf(doy~  Summer + Summer_lag+ (1|plantid) + (1|year))  + skew_normal()
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

bayesplot::mcmc_pairs(as.matrix(fruitmod_vacc),pars = c( "b_doy_Summer", "b_doy_Summer_lag", "b_value_doy", "b_value_Summer_lag"))
#  "b_doy_Spring","b_value_sfDOY", "b_value_Fall_lag", ))

bayestestR::ci(fruitmod_vacc, method="ETI", ci=c(0.85,0.9,0.95))


#oxytropis- very few observations (68 only)
fruitdat_oxy<-subset(fruitdat, species=="oxytropis")
fruitdat_oxy$Summer<-scale(fruitdat_oxy$Summer) 
fruitdat_oxy$Summer_lag<-scale(fruitdat_oxy$Summer_lag) 

hist(fruitdat_oxy$doy) #normal
fruitdat_oxy$doy<-scale(fruitdat_oxy$doy) 
hist(fruitdat_oxy$doy) #normal ish

hist(fruitdat_oxy$value) #right skew
fruitdat_oxy<-subset(fruitdat_oxy, value<40) #remove outlier
fruitdat_oxy$value<-scale(log(fruitdat_oxy$value))
hist(fruitdat_oxy$value)

mod1<- bf(doy~  Summer + Summer_lag+ (1|plantid) + (1|year))  + skew_normal()
mod2 <- bf(value~ Summer_lag + doy + (1|plantid) + (1|year)) + skew_normal()

fruitmod_oxy<-brm(mod1+ mod2 + set_rescor(FALSE),
                   data = fruitdat_oxy, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=10000)
plot(fruitmod_oxy)
pp_check(fruitmod_oxy, resp="doy") #looks ok
pp_check(fruitmod_oxy, resp="value") #looks bad

save(fruitmod_oxy, file="data/BRMS_SEM_output/fruitnumber_oxy.Rdata")
loo(fruitmod_oxy)
loo_R2(fruitmod_oxy)#very bad... 
summary(fruitmod_oxy)




#simply check if fruit number and flower number correlate
fruit_test<-subset(semdat, trait2=="num_flowers" | trait2=="num_fruit")%>%
  select(species, year, plantid, value, trait2)%>% pivot_wider(names_from = "trait2", values_from = "value")%>%
  subset(num_flowers>0)

plot(log(fruit_test$num_flowers)~log(fruit_test$num_fruit))
cor.test(fruit_test$num_flowers, fruit_test$num_fruit)

ggplot(fruit_test,
       aes(x=log(num_flowers), y=log(num_fruit)))+
  geom_point( alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+ theme_bw()+
 ylab("Num fruit (log)")+ xlab("Num flowers (log)")


#OLD CODE----
#repro size
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


#treatment effects

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




#growth
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



#leaf length
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
