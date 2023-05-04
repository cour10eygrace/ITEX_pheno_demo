#load libraries
library(tidyverse)
library(car)
library(marginaleffects)
library(ggdist)
library(patchwork)
library(lme4)
library(lmerTest)
library(brms)
library(rstan)

source("scripts/colorscale.R")

#load datasets
load('data/DLphen_dem_climate.Rdata')
load(file="data/Mixed_mods_df.Rdata")

#merge clim data  
flower_open<-left_join(flower_open, seas_clim)

#select data 
semdat<-select(flower_open, species, year, plantid,treatment, phen, trait, value, doy, sfDOY,  phen2, trait2, Spring, Fall, Summer, 
               Spring_lag, Summer_lag, Fall_lag) 
semdat<-subset(semdat, treatment=="CTL") #only keep control data - look at OTCs separately 

semdat<-mutate(semdat, Summer_lag=if_else(is.na(Summer_lag), Summer, Summer_lag))# fill in 2001 with current year 

#RUN SEMs----  
rstan_options(disable_march_warning = TRUE)
rstan_options(mc.cores = parallel::detectCores())

#num flowers----
flowdat<-subset(semdat, trait2=="num_flowers" & value > 0) #must flower to have flower open phenology 

#all species together -random intercepts 
flowdat$Summer<-scale(flowdat$Summer) 
hist(flowdat$Summer) 
flowdat$doy<-scale(flowdat$doy) 
hist(flowdat$doy) 

hist(flowdat$value) 
hist(log(flowdat$value)) 
hist(scale(log(flowdat$value))) 
flowdat$value<-scale(log(flowdat$value)) 
hist(flowdat$value) 

mod1<- bf(doy~  Summer +  (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod1q<- bf(doy~  Summer  + (1|species:plantid) + (1|year) + (1|species))  + gaussian()

mod2 <- bf(value~ Summer + doy + (1|species:plantid) + (1|year) + (1|species)) + gaussian()
mod2q <- bf(value~ Summer + doy + I(doy^2) + (1|species:plantid) + (1|year) + (1|species)) + gaussian()

flowmod<-brm(mod1+ mod2 + set_rescor(FALSE),
             data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(flowmod, file="data/BRMS_SEM_output/flownumber.Rdata")

flowmodq<-brm(mod1q+ mod2q + set_rescor(FALSE),
             data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=10000)
save(flowmodq, file="data/BRMS_SEM_output/flownumber_quad.Rdata")


pp_check(flowmodq, resp="doy") 
pp_check(flowmodq, resp="value") 

loo(flowmodq)
loo_R2(flowmodq)
performance::r2_bayes(flowmodq) #conditional and marginal R2

bayesplot::mcmc_pairs(as.matrix(flowmodq),pars = c("b_doy_Summer", "b_value_Summer","b_value_doy"))   
vcov(flowmodq, correlation=T)%>%round(digits=2) #0.38 correlation -not too bad 

summary(flowmodq)

#loo_compare(loo1, loo2)

#num fruit----
fruitdat<-subset(semdat, trait2=="num_fruit") 
#fruitdat<-subset(fruitdat, value>0) #only if successfully fruited 
fruitdat$Summer<-scale(fruitdat$Summer)
fruitdat$Summer_lag<-scale(fruitdat$Summer_lag)
hist(fruitdat$Summer)
hist(fruitdat$Summer_lag)

hist(fruitdat$doy) 
fruitdat$doy<-scale(fruitdat$doy)
hist(fruitdat$doy, breaks = 20) 

hist(fruitdat$value) 
fruitdat<-subset(fruitdat, value<40) #remove outlier
hist(fruitdat$value) 
hist(log(fruitdat$value)) 
hist(scale(log(fruitdat$value+1))) 
hist(scale(fruitdat$value))

fruitdat$value<-scale(log(fruitdat$value+1))
#fruitdat$value<-scale(log(fruitdat$value)) #without zeroes

hist(fruitdat$value) 

unique(fruitdat$species)#3 spp 

#set weak priors for doy->num fruits 
priorx <- c(set_prior(coef = 'IdoyE2', prior = 'uniform(-100, 0)', resp = "value")) #force quadratic as concave down? 

mod1<- bf(doy~  Summer + Summer_lag + (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod1q<- bf(doy~  Summer + Summer_lag +  (1|species:plantid) + (1|year) + (1|species))  + gaussian()

mod2 <- bf(value~  Summer_lag + doy +  (1|species:plantid) + (1|year) + (1|species)) + skew_normal()
mod2q<- bf(value~  Summer_lag + doy + I(doy^2) + (1|species:plantid) + (1|year) + (1|species)) + skew_normal()

fruitmod<-brm(mod1+ mod2 + set_rescor(FALSE),
             data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, #prior = priorx,  sample_prior = TRUE,  
             save_pars = save_pars(all = TRUE))
save(fruitmod, file="data/BRMS_SEM_output/fruitnumber.Rdata")


fruitmodq<-brm(mod1q+ mod2q + set_rescor(FALSE),
              data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=10000, #prior = priorx,  sample_prior = TRUE,  
              save_pars = save_pars(all = TRUE))

save(fruitmodq, file="data/BRMS_SEM_output/fruitnumber_quad.Rdata")

pp_check(fruitmodq, resp="doy")
pp_check(fruitmodq, resp="value") 

loo(fruitmodq)  
loo_R2(fruitmod)  
performance::r2_bayes(fruitmod) #conditional and marginal R2s
summary(fruitmodq)

parnames(fruitmod)
bayesplot::mcmc_pairs(as.matrix(fruitmod),pars = c("b_doy_Summer","b_doy_Summer_lag", "b_value_Summer_lag", "b_value_doy"))   
vcov(fruitmod, correlation=T)%>%round(digits=2) #0.2-0.37
bayestestR::ci(fruitmod, method="ETI", ci=c(0.85,0.9,0.95))

#look at prior vs posteriors 
#plot(hypothesis(fruitmod, "value_doy < 0")) 



#simply check that fruit number and flower number correlate----
fruit_test<-subset(semdat, trait2=="num_flowers" | trait2=="num_fruit")%>%
  select(species, year, plantid, value, trait2)%>% pivot_wider(names_from = "trait2", values_from = "value")%>%
  subset(num_flowers>0)%>%
  mutate(FFratio=num_fruit/num_flowers)

plot(log(fruit_test$num_flowers)~log(fruit_test$num_fruit+1))
cor.test(fruit_test$num_flowers, fruit_test$num_fruit)

cors<-filter(fruit_test, !is.na(num_fruit))%>% group_by(species)%>%summarize(corr=cor(num_flowers, num_fruit))

FFfig<-ggplot(subset(fruit_test, !is.na(num_fruit)),
       aes(x=log(num_flowers), y=log(num_fruit+1)))+
  geom_point( alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+ theme_bw()+
  ylab("Num fruit (log+1)")+ xlab("Num flowers (log)")
#yes (to varying degrees across spp) but overall clear pattern


#Supp fig 2
ggplot(flowdat, aes(x=doy, y=log(value), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  #facet_wrap(~trait2,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("DOY flower open")

ggplot(flowdat, aes(x=doy, y=log(value)))+
  geom_point(aes(colour=species), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  #facet_wrap(~trait2,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("DOY flower open")

ggplot(fruitdat, aes(x=doy, y=log(value+1)))+
  geom_point(aes(colour=species), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  #facet_wrap(~trait2,scales = "free")+ 
  ylab("Num fruit (log)")+ xlab("DOY flower open")

#ggplot(fruitdat, aes(x=doy, y=probfruit, fill=species))+
#  geom_point(aes(colour=species), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth() +
#  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
#  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
#  theme_bw()+  #facet_wrap(~trait2,scales = "free")+ 
#  ylab("Prob fruit")+ xlab("DOY flower open")


#fruit set-fruit:flower ratio----
fruitdat<-subset(semdat, trait2=="num_fruit")
fruitdat<-left_join(fruitdat, select(fruit_test, treatment, species, year, plantid, FFratio)) #can include zero 

ggplot(fruitdat, aes(x=doy, y=FFratio))+
  geom_point(aes(colour=species), alpha=0.5)+
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+ # facet_wrap(~species,scales = "free")+ 
  ylab("Fruit:flower")+ xlab("DOY flower open")+ ylim(0,4.5)

fruitdat$Summer<-scale(fruitdat$Summer)
fruitdat$Summer_lag<-scale(fruitdat$Summer_lag)
hist(fruitdat$Summer)
hist(fruitdat$Summer_lag)

hist(fruitdat$doy) 
fruitdat$doy<-scale(fruitdat$doy)
hist(fruitdat$doy, breaks = 20) 

hist(fruitdat$FFratio, breaks=20) 

#remove outliers 
fruitdat<-subset(fruitdat, FFratio<6)
hist(fruitdat$FFratio+1) 
hist(log(fruitdat$FFratio+1)) 
hist(scale(log(fruitdat$FFratio+1))) #normal ish 
#fruitdat$FFratio<-scale(log(fruitdat$FFratio+1))
hist(scale(fruitdat$FFratio))
fruitdat$FFratio<-fruitdat$FFratio +1
hist(fruitdat$FFratio) 

#fruitdat$FFratio<-scale(fruitdat$FFratio)

mod1<- bf(doy~  Summer + Summer_lag + (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(FFratio~  Summer_lag + doy +(1|species:plantid) + (1|year) + (1|species)) + lognormal()

testmod<-brm(mod2,  data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000,  
          save_pars = save_pars(all = TRUE))

pp_check(testmod, resp="FFratio")

ratiomod<-brm(mod1+ mod2 + set_rescor(FALSE),
              data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000,  
              save_pars = save_pars(all = TRUE))

save(ratiomod, file="data/BRMS_SEM_output/FFratio.Rdata")

mod1q<- bf(doy~  Summer + Summer_lag  + (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod2q <- bf(FFratio~  Summer_lag + doy + I(doy^2) + (1|species:plantid) + (1|year) + (1|species)) + lognormal()
ratiomodq<-brm(mod1q+ mod2q + set_rescor(FALSE),
              data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000,  
              save_pars = save_pars(all = TRUE))

save(ratiomodq, file="data/BRMS_SEM_output/FFratio_quad.Rdata")


pp_check(ratiomod, resp="doy")
pp_check(ratiomodq=, resp="FFratio")

x<-pp_check(ratiomodq, resp="FFratio", ndraws = 100) #pretty bad 
x<-as.data.frame(x$data$value)

loo(fruitmod)  

loo_R2(ratiomod)  
performance::r2_bayes(ratiomod)
summary(ratiomod)

parnames(ratiomod)
bayesplot::mcmc_pairs(as.matrix(ratiomod),pars = c("b_doy_Summer","b_doy_Summer_lag", "b_value_Summer_lag", "b_value_doy"))   
vcov(ratiomod, correlation=T)%>%round(digits=2) #0.2-0.37
bayestestR::ci(ratiomod, method="ETI", ci=c(0.85,0.9,0.95))



#run SEM w/ both flow # & fruit num - Doesn't really work b/c collinearity 
flowdatx<-select(flowdat, species, year, plantid, treatment, value)%>%rename(numflow=value)
fruitflowdat<-left_join(fruitdat, flowdatx)

fruitflowdat$Summer<-scale(fruitflowdat$Summer)
fruitflowdat$Summer_lag<-scale(fruitflowdat$Summer_lag)

hist(fruitflowdat$doy)
fruitflowdat$doy<-scale(fruitflowdat$doy)

hist(scale(log(fruitflowdat$numflow)))
fruitflowdat$numflow<-scale(log(fruitflowdat$numflow))

hist(fruitflowdat$value)
min(fruitflowdat$value)
hist(scale(log(fruitflowdat$value+1)))
fruitflowdat$value<-scale(log(fruitflowdat$value+1))

mod1<- bf(doy~  Summer + Summer_lag + (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(numflow~  Summer_lag + doy +  (1|species:plantid) + (1|year) + (1|species)) + gaussian()
mod3 <- bf(value~  Summer_lag +  doy + numflow + (1|species:plantid) + (1|year) + (1|species)) + gaussian()

fruitflowmod<-brm(mod1+ mod2+ mod3+ set_rescor(FALSE),
                  data = fruitflowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000, 
                  save_pars = save_pars(all = TRUE))
summary(fruitflowmod)
vcov(fruitflowmod, correlation=T)%>%round(digits=2) 

#prob fruit----
fruitdat<-subset(semdat, trait2=="num_fruit") 
fruitdat<-mutate(fruitdat, probfruit=ifelse(value==0, 0, 1)) #set to binary response 
hist(fruitdat$probfruit)

fruitdat$Summer<-scale(fruitdat$Summer)
fruitdat$Summer_lag<-scale(fruitdat$Summer_lag)
hist(fruitdat$Summer)
hist(fruitdat$Summer_lag)

hist(fruitdat$doy) 
fruitdat$doy<-scale(fruitdat$doy)
hist(fruitdat$doy, breaks = 20) 

mod1<- bf(doy~  Summer + Summer_lag + (1|species:plantid) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(probfruit ~  Summer_lag + doy + (1|species:plantid) + (1|year) + (1|species)) + bernoulli()

pfruitmod<-brm(mod1+ mod2 + set_rescor(FALSE),
               data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(pfruitmod)
pp_check(pfruitmod, resp="doy") 
pp_check(pfruitmod, resp="probfruit") 
loo(pfruitmod) 
loo_R2(pfruitmod) 

summary(pfruitmod)

save(pfruitmod, file="data/BRMS_SEM_output/probfruit.Rdata")

parnames(fruitmod)
bayesplot::mcmc_pairs(as.matrix(pfruitmod),pars = c("b_doy_Summer","b_doy_Summer_lag", "b_probfruit_Summer_lag", "b_probfruit_doy"))   
vcov(pfruitmod, correlation=T)%>%round(digits=2)
bayestestR::ci(pfruitmod, method="ETI", ci=c(0.85,0.9,0.95))


#SEMs by species----
#Flowering 
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

#fruiting 


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

