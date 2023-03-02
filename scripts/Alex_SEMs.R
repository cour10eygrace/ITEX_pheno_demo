
#select data 
names(flower_open)

#RUN SEMs----  
library(lme4)
library(lmerTest)
library(brms)
library(rstan)
rstan_options(disable_march_warning = TRUE)
rstan_options(mc.cores = parallel::detectCores())


#phenology by treatment 

ggplot(flowdat, 
       aes(x=species, y=doy, fill=otc_treatment))+ 
  geom_boxplot()+ theme_bw()

ggplot(fruitdat, 
       aes(x=species, y=doy, fill=otc_treatment))+ 
  geom_boxplot()+ theme_bw()


geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+ theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("")+ xlab("DOY mature flower")


#num flowers----
flowdat<-subset(flower_open, trait_simple2=="flower_no"& snow_treatment=="control"& site!="Fert") #no zeroes in dataset

#num flowers 
ggplot(flowdat, 
       aes(x=doy, y=log(value), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+ theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

hist(flowdat$doy) #normal
flowdat$doy<-scale(flowdat$doy) 
hist(flowdat$doy) #normal

hist(flowdat$value) #right skewed
hist(log(flowdat$value)) 

hist(scale(log(flowdat$value)), breaks=20) 
flowdat$value<-scale(log(flowdat$value)) 
flowdat$value<-log(flowdat$value)


mod1<- bf(doy~  otc_treatment +  (1|site:plot) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(value~ otc_treatment + doy + (1|site:plot) + (1|year) + (1|species)) + lognormal()

flowmodOTC<-brm(mod1+ mod2 + set_rescor(FALSE),
             data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(flowmodOTC, file="data/BRMS_SEM_output/flownumberOTC.Rdata")

summary(flowmodOTC)

plot(flowmodOTC)
pp_check(flowmodOTC, resp="doy") #good
pp_check(flowmodOTC, resp="value") #looks good lognormal

loo(flowmodOTC)
loo_R2(flowmodOTC)

vcov(flowmodOTC, correlation=T)%>%round(digits=2) #0.4 correlation -not too bad 
bayestestR::ci(flowmodOTC, method="ETI", ci=c(0.85,0.9,0.95))


#num fruit----

fruitdat<-subset(flower_open, trait_simple2=="fruit_no" & snow_treatment=="control"& site!="Fert")#no zeroes in dataset 

#num fruit
ggplot(fruitdat,  
       aes(x=doy, y=log(value), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+
  theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Num fruit(log)")+ xlab("DOY mature flower")

hist(fruitdat$doy) #normal
fruitdat$doy<-scale(fruitdat$doy) 
hist(fruitdat$doy) #normal

hist(fruitdat$value) #right skewed
hist(log(fruitdat$value)) 

hist(scale(log(fruitdat$value)), breaks=20) 
#fruitdat$value<-scale(log(fruitdat$value)) 
#fruitdat$value<-log(fruitdat$value)

mod1<- bf(doy~  otc_treatment +  (1|site:plot) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(value~ otc_treatment + doy + (1|site:plot) + (1|year) + (1|species)) + lognormal()

fruitmodOTC<-brm(mod1+ mod2 + set_rescor(FALSE),
                data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(fruitmodOTC, file="data/BRMS_SEM_output/fruitnumberOTC.Rdata")

summary(fruitmodOTC)

plot(fruitmodOTC)
pp_check(fruitmodOTC, resp="doy") #good
pp_check(fruitmodOTC, resp="value") #looks ok

loo(fruitmodOTC) #good
loo_R2(fruitmodOTC)


vcov(fruitmodOTC, correlation=T)%>%round(digits=2) #0.36 correlation -not too bad 
bayestestR::ci(fruitmodOTC, method="ETI", ci=c(0.85,0.9,0.95))





