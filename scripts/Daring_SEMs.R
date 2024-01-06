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
#priorx <- c(set_prior(coef = 'IdoyE2', prior = 'uniform(-100, 0)', resp = "value")) #force quadratic as concave down? 

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

loo(fruitmodq) #some pareto k values >0.7
#assess
loox<-loo(fruitmodq)  
plot(loox, label_points = TRUE)
loo()
#reloo with moment match -fails
#loo2<-loo_moment_match(fruitmodq, loox)
#loo2<-loo(fruitmodq, moment_match = T)  

#reloo full 
#reloox<-reloo(loox, fruitmodq, chains=2)

loo_R2(fruitmodq)  
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


#Supp fig 4
ggplot(flowdat, aes(x=doy, y=log(value), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  #facet_wrap(~trait2,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("DOY flower open")

ggplot(fruitdat, aes(x=doy, y=log(value+1), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=c("#74D944", "#CE50CA", "#5F7FC7"))+ scale_color_manual(values=c("#74D944", "#CE50CA", "#5F7FC7"))+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  #facet_wrap(~trait2,scales = "free")+ 
  ylab("Num fruit (log)")+ xlab("DOY flower open")

