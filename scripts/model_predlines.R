library(dplyr)
library(brms)
library(marginaleffects)
library(ggplot2)
library(ggdist)
source("scripts/colorscale.R")

#Generate predictions on span of values from existing dataset ----

#load models 
#Daring
load("data/brms_SEM_output/flownumber_quad.Rdata") 
load("data/brms_SEM_output/fruitnumber_quad_skewnorm.Rdata") 
summary(flowmodq)#quadratic term significant 
summary(fruitmodq) #quadratic term NS
#Alex
#load models 
load("data/brms_SEM_output/flownumberOTC_quad.Rdata")
load("data/brms_SEM_output/fruitnumberOTC_quad.Rdata")
summary(flowmodOTCq)#quadratic NS
summary(fruitmodOTCq)#quadratic NS

#Flower number DL----
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
#pred<-group_by(pred,estimate)%>%mutate(draw=mean(draw))

#plot
flowpredplot<-ggplot(pred, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  
  stat_lineribbon()+ scale_fill_brewer() +
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  #geom_ribbon(aes(ymin = (conf.low*1.11)+2,
  #                ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+# plot raw data
  labs(x = " ",
       y = "log Flower #")+ theme_bw()
#between beginning of flowering to doy 190 it is better to flower earlier
#after about DOY 190 it's not any worse to flower later (curve flattens)

#flowpredplot


#expected values - not using rn 
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


#Fruit number DL----
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

#fruitpredplot


#Flower number AF----
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
  labs(x = " ",
       y = "log Flower #")+  theme_bw()


#flowpredplot2

#fruit number AF----
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
       y = "log Fruit #")+  theme_bw() 

#fruitpredplot2

#plot all- Fig 5
ggpubr::ggarrange(flowpredplot2,flowpredplot, fruitpredplot2, fruitpredplot,common.legend = T)



#predicted values over NEW climate/phenology data---- 
#Use DL flower mod bc strongest slope and only sig quadratic term
flowdat<-flowmodq$data

#re make original plot 
nd <- with(flowdat, expand.grid(Summer=unique(Summer), 
                                doy=unique(doy), 
                                # species:plantid=unique(species:plantid), 
                                # species=unique(species), 
                                # year=unique(year), 
                                value=NA))#%>%

pred<-predictions(flowmodq, nd, resp = "value", re_formula = NA) |> #setting RE=NA here because otherwise error ribbon is weird
  posterior_draws()#%>%mutate(clim= "Current")

#plot
flowpredplot<-ggplot(pred, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  
  stat_lineribbon()+ scale_fill_brewer() +
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  #geom_ribbon(aes(ymin = (conf.low*1.11)+2,
  #                ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  #geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+
  labs(x = " ",
       y = "log Flower #", title = "Historic")+ theme_bw()


#add 1, 3, 5 C to all summer temps 
ndclim1 <- with(flowdat, expand.grid(Summer=unique(Summer)+1, 
                                     doy=unique(doy), 
                                     value=NA))
ndclim3 <- with(flowdat, expand.grid(Summer=unique(Summer)+3, 
                                     doy=unique(doy), 
                                     value=NA))

ndclim5 <- with(flowdat, expand.grid(Summer=unique(Summer)+5, 
                                doy=unique(doy), 
                                value=NA))#%>%

prednewclim1<-predictions(flowmodq, ndclim1, resp = "value", re_formula = NA) |> 
  posterior_draws() %>%mutate(clim= "+ 1C")
prednewclim3<-predictions(flowmodq, ndclim3, resp = "value", re_formula = NA) |> 
  posterior_draws()%>%mutate(clim= "+ 3C")
prednewclim5<-predictions(flowmodq, ndclim5, resp = "value", re_formula = NA) |> 
  posterior_draws()%>%mutate(clim= "+ 5C")
prednewclim<-rbind(prednewclim1,prednewclim3, prednewclim5)

#plot Fig 6
newclimplot<-ggplot(prednewclim, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  
  stat_lineribbon(alpha=0.2)+ facet_wrap(~clim)+ #scale_fill_brewer() + 
  scale_fill_brewer() + 
    #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  #geom_ribbon(aes(ymin = (conf.low*1.11)+2,
  #                ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  #geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Flower #")+ theme_bw() #+ scale_fill_manual()

newclimplot+
 #add current slope line dotted 
   geom_smooth(data=pred, aes(x = (doy*14)+172, y = (draw*1.11)+2), 
    method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = F, lty=2, col="black")

