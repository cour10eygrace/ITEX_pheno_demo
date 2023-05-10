library(dplyr)
library(brms)
library(marginaleffects)
library(ggplot2)
library(ggdist)
library(tidyr)
source("scripts/colorscale.R")

#Generate predictions on span of values from existing dataset ----

#load models 
#Daring
load("data/brms_SEM_output/flownumber_quad.Rdata") 
load("data/brms_SEM_output/fruitnumber_quad.Rdata") 
#summary(flowmodq)#quadratic term significant 
#summary(fruitmodq) #quadratic term NS
#Alex
#load models 
load("data/brms_SEM_output/flownumberOTC_quad.Rdata")
load("data/brms_SEM_output/fruitnumberOTC_quad.Rdata")
#summary(flowmodOTCq)#quadratic NS
#summary(fruitmodOTCq)#quadratic NS

gc() #free up mem

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
 gc()

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
gc()

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

gc()
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
gc()
#fruitpredplot2

#plot all- Fig 5----
rm(flowmodOTCq)
rm(flowmodq)
rm(fruitmodOTCq)
rm(fruitmodq)

gc()

ggpubr::ggarrange(flowpredplot2,flowpredplot, fruitpredplot2, fruitpredplot,common.legend = T)



#predicted values over NEW climate/phenology data---- 

#Use DL flower mod bc strongest slope in phenology-fitness model and only sig quadratic term
rm(list=ls())
gc()
load("data/brms_SEM_output/flownumber_quad.Rdata") 

flowdat<-flowmodq$data

#first predict doy at AVERAGE summer temp 
nd <- with(flowdat, expand.grid(Summer=mean(Summer), 
                                doy=NA)) 

#generate fitted doys
doy_pred <- fitted(flowmodq, re_formula = NA, newdata = nd,
                     resp = "doy", ndraws=1000,
                     summary = FALSE)
#now predict flow# at AVERAGE summer temp with fitted doys
nd2<-with(flowdat, expand.grid(Summer=mean(Summer), 
                                     doy=as.vector(doy_pred), value=NA)) 
#generate predicted flow#
pred<-predictions(flowmodq, nd2, resp = "value", re_formula = NA) |> #setting RE=NA here because otherwise error ribbon is weird
  posterior_draws()


#re make original plot 
#plot
flowpredplot<-ggplot(pred, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  
  stat_lineribbon()+ scale_fill_brewer() +
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  #geom_ribbon(aes(ymin = (conf.low*1.11)+2,
  #                ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  #geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+
  labs(x = " ",
       y = "log Flower #", title = "Historic")+ theme_bw()

gc()
#add 1, 3, 5 C to all summer temps 
#AVERAGE summer temp +1
nd <- with(flowdat, expand.grid(Summer=mean(Summer)+1, 
                                doy=NA)) 

#generate fitted doys
doy_pred <- fitted(flowmodq, re_formula = NA, newdata = nd,
                   resp = "doy", ndraws=1000,
                   summary = FALSE)
#now predict flow# 
nd2<-with(flowdat, expand.grid(Summer=mean(Summer)+1, 
                               doy=as.vector(doy_pred), value=NA)) 
#generate predicted flow#
pred1C<-predictions(flowmodq, nd2, resp = "value", re_formula = NA) |> #setting RE=NA here because otherwise error ribbon is weird
  posterior_draws()%>%mutate(clim= "+ 1C")
gc()
#AVERAGE summer temp +3
nd <- with(flowdat, expand.grid(Summer=mean(Summer)+3, 
                              doy=NA)) 
#generate fitted doys
doy_pred <- fitted(flowmodq, re_formula = NA, newdata = nd,
                   resp = "doy", ndraws=1000,
                   summary = FALSE)
#now predict flow# 
nd2<-with(flowdat, expand.grid(Summer=mean(Summer)+3, 
                               doy=as.vector(doy_pred), value=NA)) 
#generate predicted flow#
pred3C<-predictions(flowmodq, nd2, resp = "value", re_formula = NA) |> #setting RE=NA here because otherwise error ribbon is weird
  posterior_draws()%>%mutate(clim= "+ 3C")
gc()
#AVERAGE summer temp +5
nd <- with(flowdat, expand.grid(Summer=mean(Summer)+5, 
                                doy=NA)) 
#generate fitted doys
doy_pred <- fitted(flowmodq, re_formula = NA, newdata = nd,
                   resp = "doy", ndraws=1000,
                   summary = FALSE)
#now predict flow# 
nd2<-with(flowdat, expand.grid(Summer=mean(Summer)+5, 
                               doy=as.vector(doy_pred), value=NA)) 
#generate predicted flow#
pred5C<-predictions(flowmodq, nd2, resp = "value", re_formula = NA) |> #setting RE=NA here because otherwise error ribbon is weird
  posterior_draws()%>%mutate(clim= "+ 5C")
gc()
prednewclim<-rbind(pred1C,pred3C, pred5C)

#plot Fig 6

newclimplot<-ggplot(prednewclim, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  
  stat_lineribbon(alpha=0.2)+ facet_wrap(~clim)+ # , scales = "free")+ #scale_fill_brewer() + 
  scale_fill_brewer() + 
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  #geom_ribbon(aes(ymin = (conf.low*1.11)+2,
  #                ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  #geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Flower #")+ theme_bw() #+ scale_fill_manual()

gc()
pdf(file = "MS_docs/Fig6x.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 6) # The height of the plot in inches

newclimplot+
  #add current slope line dotted 
  geom_smooth(data=pred, aes(x = (doy*14)+172, y = (draw*1.11)+2), 
              method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = F, lty=2, col="black")
dev.off()

#save(pred, prednewclim, file="data/BRMS_SEM_output/newclimate_projections.Rdata")
