#load libraries 
library(lme4)
library(lmerTest)
library(brms)
library(rstan)
rstan_options(disable_march_warning = TRUE)
rstan_options(mc.cores = parallel::detectCores())

#load data
load(file='data/AFphen_dem_climate.Rdata')

source('scripts/colorscale.R')

#RUN SEMs 
#num flowers----
flowdat<-subset(flower_open, trait_simple2=="flower_no") #no zeroes in dataset
#remove Luzula 1992- weird counts all much higher (possibly were cumsums- Greg)
flowdat<-mutate(flowdat, value=if_else(species=="Luzula"&year==1992, NA_real_, value))


#visualize -Fig S3c
ggplot(flowdat, 
       aes(x=doy, y=log(value), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+ theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

hist(flowdat$doy) 
flowdat$doy<-scale(flowdat$doy) 
hist(flowdat$doy) 

hist(flowdat$value) #right skewed
hist(log(flowdat$value)) #lognormal dist 
hist(scale(log(flowdat$value)))

mod1<- bf(doy~  otc_treatment +  (1|site:plot) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(value~ otc_treatment + doy + (1|site:plot) + (1|year) + (1|species)) + lognormal()

flowmodOTC<-brm(mod1+ mod2 + set_rescor(FALSE),
                data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(flowmodOTC, file="data/BRMS_SEM_output/flownumberOTC.Rdata")
summary(flowmodOTC) 

#w quadratic term 
mod2q <- bf(value~ otc_treatment + doy + I(doy^2) + (1|site:plot) + (1|year) + (1|species)) + lognormal()
flowmodOTCq<-brm(mod1+ mod2q + set_rescor(FALSE),
                 data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=10000)
summary(flowmodOTCq)
save(flowmodOTCq, file="data/BRMS_SEM_output/flownumberOTC_quad.Rdata")

plot(flowmodOTC)
pp_check(flowmodOTCq, resp="doy") #good
pp_check(flowmodOTCq, resp="value") #looks good lognormal

loo(flowmodOTCq)
loo_R2(flowmodOTCq)

vcov(flowmodOTC, correlation=T)%>%round(digits=2) 
bayestestR::ci(flowmodOTC, method="ETI", ci=c(0.85,0.9,0.95))

#loo_compare(loo1, loo2)

#num fruit----
fruitdat<-subset(flower_open, trait_simple2=="fruit_no")
fruitdat<-mutate(fruitdat, value=if_else(species=="Dryas"&year==2003, NA_real_, value)) #only zeroes recorded for fruits this year- seems wrong 

#visualize- Fig S3d
ggplot(subset(fruitdat, species!="Luzula"& species!="Oxyria"), 
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+
  theme_bw()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  ylab("Num fruit(log)")+ xlab("DOY mature flower")

hist(fruitdat$doy) #normal
fruitdat$doy<-scale(fruitdat$doy) 
hist(fruitdat$doy) #normal

hist(fruitdat$value) #right skewed
hist(log(fruitdat$value+1)) #lognormal
fruitdat$value<-(fruitdat$value+1) 
hist(fruitdat$value) #right skewed

mod1<- bf(doy~  otc_treatment +  (1|site:plot) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(value~ otc_treatment + doy  + (1|site:plot) + (1|year) + (1|species)) +lognormal()
#w quadratic term 
mod2q <- bf(value~ otc_treatment + doy + I(doy^2) + (1|site:plot) + (1|year) + (1|species)) + lognormal()


fruitmodOTC<-brm(mod1+ mod2 + set_rescor(FALSE),
                data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
save(fruitmodOTC, file="data/BRMS_SEM_output/fruitnumberOTC.Rdata")

fruitmodOTCq<-brm(mod1+ mod2q + set_rescor(FALSE),
                 data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=10000)
save(fruitmodOTCq, file="data/BRMS_SEM_output/fruitnumberOTC_quad.Rdata")

summary(fruitmodOTCq)

plot(fruitmodOTC)
pp_check(fruitmodOTCq, resp="doy") #good
pp_check(fruitmodOTCq, resp="value") #looks ok

loo(fruitmodOTCq) #good
loo_R2(fruitmodOTCq)

vcov(fruitmodOTC, correlation=T)%>%round(digits=2) 
bayestestR::ci(fruitmodOTC, method="ETI", ci=c(0.85,0.9,0.95))
bayestestR::ci(fruitmodOTCs, method="ETI", ci=c(0.85,0.9,0.95))

#loo_compare(loo1, loo2)


#plot phenology by treatment- Fig S2
ggplot(flowdat, 
       aes(x=species, y=doy, fill=otc_treatment))+ 
  geom_boxplot()+ theme_bw()+ 
    scale_fill_manual(values=specColor)


#simply check that fruit number and flower number correlate----
fruit_testAF<-select(flower_open, species, year, plot, site, value, trait_simple2, otc_treatment)%>% 
  pivot_wider(names_from = "trait_simple2", values_from = "value", values_fn = 'mean')%>% #average 5 duplicates 
  subset(flower_no>0)%>%
  mutate(FFratio=fruit_no/flower_no)

fruit_testAF<-mutate(fruit_testAF, fruit_no=if_else(species=="Dryas"&year==2003, NA_real_, fruit_no))

FFfig2<-ggplot(subset(fruit_testAF, !is.na(fruit_no)),
       aes(x=log(flower_no), y=log(fruit_no+1)))+
  geom_point( alpha=0.5)+
  geom_smooth(method='lm') + facet_wrap(~species, scales="free")+ theme_bw()+
  ylab("Num fruit (log+1)")+ xlab(" ")
#yes (to varying degrees across spp) but overall clear pattern

cors2<-filter(fruit_testAF, !is.na(fruit_no))%>% group_by(species)%>%summarize(corr=cor(flower_no, fruit_no))

ggpubr::ggarrange(FFfig2, FFfig, ncol = 1, common.legend = T)

