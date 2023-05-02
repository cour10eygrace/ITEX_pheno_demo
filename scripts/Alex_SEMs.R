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


#visualize
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
                 data = flowdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
summary(flowmodOTCq)
save(flowmodOTCq, file="data/BRMS_SEM_output/flownumberOTC_quad.Rdata")

plot(flowmodOTC)
pp_check(flowmodOTCq, resp="doy") #good
pp_check(flowmodOTCq, resp="value") #looks good lognormal

loo(flowmodOTCq)
loo_R2(flowmodOTC)

vcov(flowmodOTC, correlation=T)%>%round(digits=2) 
bayestestR::ci(flowmodOTC, method="ETI", ci=c(0.85,0.9,0.95))

#loo_compare(loo1, loo2)

#num fruit----
fruitdat<-subset(flower_open, trait_simple2=="fruit_no")
fruitdat<-mutate(fruitdat, value=if_else(species=="Dryas"&year==2003, NA_real_, value)) #only zeroes recorded for fruits this year- seems wrong 

#visualize
ggplot(fruitdat,  
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
                 data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
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


#plot phenology by treatment 
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




#prob fruit---- 
fruitdat<-mutate(fruitdat, probfruit=ifelse(value==0, 0, 1)) #set to binary response 
hist(fruitdat$probfruit)

hist(fruitdat$doy) 
fruitdat$doy<-scale(fruitdat$doy)
hist(fruitdat$doy, breaks = 20) 

mod1<- bf(doy~  otc_treatment +  (1|site:plot) + (1|year) + (1|species))  + gaussian()
mod2 <- bf(probfruit~ otc_treatment + doy + (1|site:plot) + (1|year) + (1|species)) + bernoulli()

pfruitmod<-brm(mod1+ mod2 + set_rescor(FALSE),
               data = fruitdat, control = list(adapt_delta=0.99, max_treedepth = 12), cores=3, chains=3, iter=2000)
plot(pfruitmod)
pp_check(pfruitmod, resp="doy") 
pp_check(pfruitmod, resp="probfruit") 
loo(pfruitmod) 
loo_R2(pfruitmod) 

ggplot(data=fruitdat, aes(x=otc_treatment, y=probfruit))+
  geom_smooth()

summary(pfruitmod)

save(pfruitmod, file="data/BRMS_SEM_output/probfruit.Rdata")

#RESULTS TABLES----
rm(list=ls()) 
#load SEM outputs
load("data/brms_SEM_output/flownumber.Rdata")
load("data/brms_SEM_output/flownumberOTC.Rdata")
load("data/brms_SEM_output/fruitnumber.Rdata")
load("data/brms_SEM_output/fruitnumberOTC.Rdata")
load("data/brms_SEM_output/probfruit.Rdata")

#create Table 2- mod results 
Table2a<-as.data.frame(summary(flowmod)$fixed)%>%mutate(Model="Flower number")
ci2a<-bayestestR::ci(flowmod, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter"))%>%select(-x, -Effects, -Component)%>%mutate(Model="Flower number")

Table2b<-as.data.frame(summary(pfruitmod)$fixed)%>%mutate(Model="Fruit prob")
ci2b<-bayestestR::ci(pfruitmod, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter", "Param2"))%>%unite(Parameter, Parameter, Param2)%>%
  separate(Parameter, into= "Parameter", sep="_NA")%>%
  select(-x, -Effects, -Component)%>%mutate(Model="Fruit prob")

Table2c<-as.data.frame(summary(fruitmod)$fixed)%>%mutate(Model="Fruit number")
ci2c<-bayestestR::ci(fruitmod, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter", "Param2"))%>%unite(Parameter, Parameter, Param2)%>%
  separate(Parameter, into= "Parameter", sep="_NA")%>%select(-x, -Effects, -Component)%>%mutate(Model="Fruit number")

Table2d<-as.data.frame(summary(flowmodOTC)$fixed)%>%mutate(Model="Flower number")
ci2d<-bayestestR::ci(flowmodOTC, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter"))%>%select(-x, -Effects, -Component)%>%mutate(Model="Flower number")

Table2e<-as.data.frame(summary(fruitmodOTC)$fixed)%>%mutate(Model="Fruit number")
ci2e<-bayestestR::ci(fruitmodOTC, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter"))%>%select(-x, -Effects, -Component)%>%mutate(Model="Fruit number")

#Daring
#only 95% CIs for main text 
Table2<-rbind(Table2a, Table2b, Table2c)%>%mutate(Site="Daring Lake")%>%mutate(Response=rownames(.))%>%
  separate(Response, into= c("Response","Parameter", "Param2"))%>%unite(Parameter, Parameter, Param2)%>%
  separate(Parameter, into= "Parameter", sep="_NA")%>%
  select(Site, Model, Response, Parameter, Estimate, Est.Error, "l-95% CI" , "u-95% CI",Rhat , Bulk_ESS)%>%
  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))

#Full mod results for supplement with 3 levels of CIs
ci2<-rbind(ci2a, ci2b, ci2c)
Table2supp<-rbind(Table2a, Table2b, Table2c)%>%mutate(Site="Daring Lake")%>%mutate(Response=rownames(.))%>%
  separate(Response, into= c("Response","Parameter", "Param2"))%>%unite(Parameter, Parameter, Param2)%>%
  separate(Parameter, into= "Parameter", sep="_NA")%>%
  select(Site, Model, Response, Parameter, Estimate, Est.Error, Rhat , Bulk_ESS)%>%
  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))%>%
  left_join(., ci2)


#Alex 
#only 95% CIs for main text 
Table2x<-rbind(Table2d, Table2e)%>%mutate(Site="Alexandra Fiord")%>%mutate(Response=rownames(.))%>%
  separate(Response, into= c("Response","Parameter"))%>%
  select(Site, Model, Response, Parameter, Estimate, Est.Error, "l-95% CI" , "u-95% CI",Rhat , Bulk_ESS)%>%
  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))

#Full mod results for supplement with 3 levels of CIs
ci2x<-rbind(ci2d, ci2e)
Table2xsupp<-rbind(Table2d, Table2e)%>%mutate(Site="Alexandra Fiord")%>%mutate(Response=rownames(.))%>%
  separate(Response, into= c("Response","Parameter"))%>%
  select(Site, Model, Response, Parameter, Estimate, Est.Error, Rhat , Bulk_ESS)%>%
  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))%>%
  left_join(., ci2x)


#combine

Table2_all<-rbind(Table2, Table2x)
Table2_all_supp<-rbind(Table2supp, Table2xsupp)

write.csv(Table2_all, "MS_docs/Table2.csv")
write.csv(Table2_all_supp, "MS_docs/TableS1.csv")


#create Table 3- group level hyperparameters
Table3a<-as.data.frame(summary(flowmod)$random)%>%mutate(Model="Flower number")
Table3b<-as.data.frame(summary(pfruitmod)$random)%>%mutate(Model="Fruit prob")
Table3c<-as.data.frame(summary(fruitmod)$random)%>%mutate(Model="Fruit number")
Table3d<-as.data.frame(summary(flowmodOTC)$random)%>%mutate(Model="Flower number")
Table3e<-as.data.frame(summary(fruitmodOTC)$random)%>%mutate(Model="Fruit number")

Table3<-rbind(Table3a, Table3b, Table3c)%>%mutate(Site="Daring Lake")%>%mutate(Response=rownames(.))%>%
  select(!contains(c("Bulk","95", "Tail", "Rhat")))%>%select(Site, Model, Response, species.Estimate, species.plantid.Estimate, 
                                                      year.Estimate, species.Est.Error,  species.plantid.Est.Error, year.Est.Error)%>%
  rename(species_plantid.Est.Error= species.plantid.Est.Error, species_plantid.Estimate= species.plantid.Estimate)%>%
  pivot_longer(cols = species.Estimate:year.Estimate, names_to = "Group", values_to = "SD", names_repair = "minimal")%>%
    pivot_longer(cols = species.Est.Error:year.Est.Error, names_to = "Group2", values_to = "Error", names_repair = "minimal")%>%
  separate(Group, into = "Group", sep = ".Estimate", fill="left")%>%separate(Group2, into = "Group2", sep = ".Est.Error", fill="left")%>%
  mutate(keep=if_else(Group==Group2, 1, 0))%>%filter(keep>0)%>%
    separate(Response, into= c("Response1","Response"), fill="left")%>%select(-Response1, -keep, -Group2)

  
Table3x<-rbind(Table3d, Table3e)%>%mutate(Site= "Alexandra Fiord")%>%mutate(Response=rownames(.))%>%
  select(!contains(c("Bulk","95", "Tail", "Rhat")))%>%select(Site, Model, Response, species.Estimate, site.plot.Estimate, 
                                                             year.Estimate, species.Est.Error,  site.plot.Est.Error, year.Est.Error)%>%
  rename(site_plot.Est.Error= site.plot.Est.Error, site_plot.Estimate= site.plot.Estimate)%>%
  pivot_longer(cols = species.Estimate:year.Estimate, names_to = "Group", values_to = "SD", names_repair = "minimal")%>%
  pivot_longer(cols = species.Est.Error:year.Est.Error, names_to = "Group2", values_to = "Error", names_repair = "minimal")%>%
  separate(Group, into = "Group", sep = ".Estimate", fill="left")%>%separate(Group2, into = "Group2", sep = ".Est.Error", fill="left")%>%
  mutate(keep=if_else(Group==Group2, 1, 0))%>%filter(keep>0)%>%
  separate(Response, into= c("Response1","Response"), fill="left")%>%select(-Response1,  -keep, -Group2)

Table3all<-rbind(Table3, Table3x)

write.csv(Table3all, "MS_docs/Table3.csv")
