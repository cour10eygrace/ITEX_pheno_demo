library(dplyr)
library(brms)
library(marginaleffects)
library(ggplot2)
library(ggdist)

#Daring Lake
#load models 
load("data/brms_SEM_output/flownumber_quad.Rdata") 
load("data/brms_SEM_output/fruitnumber_quad_skewnorm.Rdata") 
summary(flowmodq)#quadratic term significant 
summary(fruitmodq) #quadratic term NS

#Flower number----
flowdat<-flowmodq$data
flowmodq$formula

#simulate some new test data from existing values
nd <- with(flowdat, expand.grid(Summer=unique(Summer), 
                                      doy=unique(doy), 
                                    # species:plantid=unique(species:plantid), 
                                    # species=unique(species), 
                                    # year=unique(year), 
                                    value=NA))#%>%
#  slice(which(row_number() %% 200 == 1))#make much smaller n~1000
  
# Predictions
#https://vincentarelbundock.github.io/marginaleffects/articles/brms.html#continuous-predictors
pred<-predictions(flowmodq, nd, resp = "value", re_formula = NA) |> #setting RE=NA here because otherwise error ribbon is weird
  posterior_draws()
#average over all draw
pred<-group_by(pred,estimate)%>%mutate(draw=mean(draw))

#plot
flowpredplot<-ggplot(pred, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  
  stat_lineribbon()+ scale_fill_brewer() +
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  #geom_ribbon(aes(ymin = (conf.low*1.11)+2,
  #                ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Flower #")+ theme_bw()
#between beginning of flowering to doy 190 it is better to flower earlier
#after about DOY 190 it's not any worse to flower later (curve flattens)

flowpredplot


#expected values 
#https://www.andrewheiss.com/blog/2022/09/26/guide-visualizing-types-posteriors/
post_pred<- tidybayes::epred_draws(flowmodq, nd, resp = "value", re_formula =  NA) #simplify the REs
hist(post_pred$.epred)
hist(flowdat$value)

ggplot(post_pred, aes(x = (doy*14)+172, y = (.epred*1.11)+2)) + #backcalculate 
  
  stat_lineribbon()+ scale_fill_brewer() +
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  #geom_ribbon(aes(ymin = (conf.low*1.11)+2,
  #                ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Flower #")+ theme_bw()


#Fruit number----
fruitdat<-fruitmodq$data
fruitmodq$formula

nd <- with(fruitdat, expand.grid(Summer=unique(Summer), 
                                 Summer_lag=unique(Summer_lag), 
                                 doy=unique(doy), 
                                # species:plantid=unique(species:plantid), 
                                #species=unique(species), 
                                #year=unique(year), 
                                value=NA))%>%
  slice(which(row_number() %% 10 == 1))#make much smaller n~1500

pred<-predictions(fruitmodq, nd, resp = "value",  re_formula =  NA) |> #50% CIs 
  posterior_draws()

fruitpredplot<-ggplot(pred, aes(x = (doy*9.25)+182, y = (draw*0.97)+1.9)) + #backcalculate 
  stat_lineribbon() + scale_fill_brewer()+
  #geom_smooth(method='lm', se = T)+
  #geom_ribbon(aes(ymin = (conf.low*0.97)+1.9, #50% CIs 
  #                ymax = (conf.high*0.97)+1.9), alpha=0.2, outline.type = "both")+
  geom_point(data=fruitdat, aes(x = (doy*9.25)+182, y=(value*0.97)+1.9), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Fruit # ")+ theme_bw()

fruitpredplot

#Alex
#load models 
load("data/brms_SEM_output/flownumberOTC_quad.Rdata")
load("data/brms_SEM_output/fruitnumberOTC_quad.Rdata")
summary(flowmodOTCq)#quadratic NS
summary(fruitmodOTCq)#quadratic NS


#Flower number----
flowdat<-flowmodOTCq$data
flowmodOTCq$formula

#simulate some new test data from existing values
nd <- with(flowdat, expand.grid(otc_treatment=unique(otc_treatment), 
                                doy=unique(doy), 
                                # species:plantid=unique(species:plantid), 
                                species=unique(species), 
                                year=unique(year), 
                                value=NA))%>%
  slice(which(row_number() %% 3== 1))#make smaller n~1000


pred<-predictions(flowmodOTCq, nd, resp = "value", re_formula =  NA) |>
  posterior_draws()


flowpredplot2<-ggplot(pred, aes(x = (doy*9)+187, y = log(draw))) + #backcalculate 
  stat_lineribbon() + scale_fill_brewer()+
    #geom_smooth(method='lm', se = T)+
  #geom_ribbon(aes(ymin = log(conf.low),
  #                ymax = log(conf.high)), alpha=0.2, outline.type = "both")+
  geom_point(data=flowdat, aes(x = (doy*9)+187, y=log(value)), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Flower #")+  theme_bw()


flowpredplot2

#fruit number
fruitdat<-fruitmodOTCq$data
fruitmodOTCq$formula

#simulate some new test data from existing values
nd <- with(fruitdat, expand.grid(otc_treatment=unique(otc_treatment), 
                                doy=unique(doy), 
                                # species:plantid=unique(species:plantid), 
                                species=unique(species), 
                                year=unique(year), 
                                value=NA))%>%
slice(which(row_number() %% 3 == 1))#make much smaller n~1000

pred<-predictions(fruitmodOTCq, nd, resp = "value", re_formula =  NA) |>
  posterior_draws()


fruitpredplot2<-ggplot(pred, aes(x = (doy*9)+187, y = log(draw))) + #backcalculate 
  stat_lineribbon() + scale_fill_brewer()+
  #geom_smooth(method='lm', se = T)+
  #geom_ribbon(aes(ymin = log(conf.low),
  #                ymax = log(conf.high)), alpha=0.2, outline.type = "both")+
  geom_point(data=flowdat, aes(x = (doy*9)+187, y=log(value)), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Fruit #")+  theme_bw() + 

fruitpredplot2

ggpubr::ggarrange(flowpredplot, fruitpredplot, flowpredplot2, fruitpredplot2)
