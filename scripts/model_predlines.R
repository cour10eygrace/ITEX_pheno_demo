library(dplyr)
library(brms)
library(marginaleffects)
library(ggplot2)
library(ggdist)


#load models 
load("data/brms_SEM_output/flownumber.Rdata")
load("data/brms_SEM_output/flownumber_quad.Rdata") #quadratic term significant 
load("data/brms_SEM_output/fruitnumber.Rdata") #quadratic term NS
load("data/brms_SEM_output/fruitnumber_quad_skewnorm.Rdata") 
load("data/brms_SEM_output/FFratio_quad.Rdata")


#Flower number----
flowdat<-flowmodq$data
flowmodq$formula

#simulate some new test data from existing values
nd <- with(flowdat, expand.grid(Summer=unique(Summer), 
                                      doy=unique(doy), 
                                    # species:plantid=unique(species:plantid), 
                                     species=unique(species), 
                                     year=unique(year), 
                                    value=NA))%>%
  slice(which(row_number() %% 2000 == 1))#make much smaller n~100
  
#pull expected values from trained model on test data
post_pred<-posterior_epred(flowmod, nd, resp = "value", re_formula =  ~1|year + 1|species ) #simplify the REs
hist(post_pred)
hist(flowdat$value)

post_predq<-posterior_epred(flowmodq, nd, resp = "value", re_formula =  ~1|year + 1|species ) #simplify the REs
hist(post_predq)
hist(flowdat$value)

# Predictions
#https://vincentarelbundock.github.io/marginaleffects/articles/brms.html#continuous-predictors
pred<-predictions(flowmod, nd, resp = "value", re_formula =  NA) |> #setting RE=NA here because otherwise error ribbon is weird
  posterior_draws()

predq<-predictions(flowmodq, nd, resp = "value", re_formula =  NA) |>
  posterior_draws()
  
#linear
ggplot(pred, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  geom_ribbon(aes(ymin = (conf.low*1.11)+2,
                  ymax = (conf.high*1.11)+2), alpha=0.2, )+
  scale_fill_brewer(palette = "Reds") +
  labs(x = "Flowering doy",
       y = "Flower # (predicted)",
       fill = "")
#quadratic
flowpredplot<-ggplot(predq, aes(x = (doy*14)+172, y = (draw*1.11)+2)) + #backcalculate 
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  geom_ribbon(aes(ymin = (conf.low*1.11)+2,
                  ymax = (conf.high*1.11)+2), alpha=0.2, outline.type = "both")+
  geom_point(data=flowdat, aes(x = (doy*14)+172, y=(value*1.11)+2), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Flower # (predicted)")+ scale_color_manual(values = specColor)+ theme_bw()
#between beginning of flowering to doy 190 it is better to flower earlier
#after about DOY 190 it's not any worse to flower later just the same (curve flattens)

flowpredplot

#Fruit number----
fruitdat<-fruitmodq$data
fruitmodq$formula

nd <- with(fruitdat, expand.grid(Summer=unique(Summer), 
                                 Summer_lag=unique(Summer_lag), 
                                 doy=unique(doy), 
                                # species:plantid=unique(species:plantid), 
                                species=unique(species), 
                                year=unique(year), 
                                value=NA))%>%
  slice(which(row_number() %% 10000 == 1))#make much smaller n~100

predq<-predictions(fruitmodq, nd, resp = "value", re_formula =  NA) |>
  posterior_draws()

#quadratic
fruitpredplot<-ggplot(predq, aes(x = (doy*9.25)+182, y = (draw*0.97)+1.9)) + #backcalculate 
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2), se = T)+
  geom_ribbon(aes(ymin = ((0.5*conf.low)*0.97)+1.9, #50% CIs 
                  ymax = ((0.5*conf.high)*0.97)+1.9), alpha=0.2, outline.type = "both")+
  geom_point(data=fruitdat, aes(x = (doy*9.25)+182, y=(value*0.97)+1.9), alpha=0.2)+# plot raw data
  labs(x = "Flowering doy",
       y = "log Fruit # (predicted)")+ scale_color_manual(values = specColor)+ theme_bw()

#between beginning of flowering to doy 190 it is better to flower earlier
#after about DOY 190 it's not any worse to flower later just the same (curve flattens)

fruitpredplot


ggpubr::ggarrange(flowpredplot, fruitpredplot)



mod2q
expand


newdata <- with(fruitdat, expand.grid(Summer=unique(Summer), Summer_lag=unique(Summer_lag), 
                                      doy=unique(doy)))# , species= species, plantid=plantid, year=year))
test<-lmer(value~  Summer_lag + doy + I(doy^2) +  I(Summer_lag^2)+(1|species:plantid) + (1|year) + (1|species), fruitdat)

test_preds<-predict(test, nd, re.form=NA)

hist(test_preds)
hist(fruitdat$value)

#x values to make predictions for ###*** edit X here
nd <- tibble(value=NA, 
             Summer =seq(min(dat$Summer), max(dat$Summer), 0.1) ,
             Summer_lag =seq(min(dat$Summer_lag), max(dat$Summer_lag), 0.096),
             doy =seq(min(dat$doy), max(dat$doy), 0.14))


value_pred<-fitted(fruitmod, newdata=nd,
                   resp = "value", nsamples = 1000, 
                   re_formula = NA)

plot(density(as.vector(value_pred)))

ndq <- tibble(
  Summer =seq(min(dat$Summer), max(dat$Summer), 0.1) ,
  Summer_lag =seq(min(dat$Summer_lag), max(dat$Summer_lag), 0.096),
  doy =seq(min(dat$doy), max(dat$doy), 0.14),
  IdoyE2= seq(min(dat$`I(doy^2)`), max(dat$`I(doy^2)`), 0.17), 
  ISummerE2= seq(min(dat$`I(Summer^2)`), max(dat$`I(Summer^2)`), 0.096), 
  ISummer_lagE2= seq(min(dat$`I(Summer_lag^2)`), max(dat$`I(Summer_lag^2)`), 0.088))

#treatment= c("OTC", "OTC", "OTC", "OTC", "OTC", "CTL", "CTL", "CTL", "CTL", "CTL"))

#use brms fitted function 
f <-
  fitted(fruitmod,
         newdata = nd,
         re_formula = NA,
         probs = c(.05, .95)) %>% 
  as_tibble() %>% 
  bind_cols(nd)

fit <- fitted(fruitmod, newdata=nd, 
              resp = "value", nsamples = 1000, 
              summary = FALSE)

median(fit)
mean(fit)
posterior_interval(fit)



pred <- predict(fruitmod, newdata=nd, re_formula = ,
                resp = "value", nsamples = 1000, 
                summary = FALSE)

plot(density(as.vector(pred)))
mean(pred)
posterior_interval(pred)

#calculate OTC-CTL fitted
f<-group_by(f, siteT)%>%pivot_wider(names_from = treatment, 
                                    values_from = c(Estimate, Est.Error, Q5, Q95))%>%
  mutate(diff=Estimate_OTC-Estimate_CTL,
         diff.err=Est.Error_OTC-Est.Error_CTL, 
         diff.Q5=Q5_OTC-Q5_CTL, 
         diff.Q95=Q95_OTC-Q95_CTL)

#plot predline 
names(f)

ggplot(data=f, aes(x = Estimate.doy, y =Estimate.value )) +
  geom_ribbon(aes(ymin = Q5.value, ymax = Q95.value),
              fill = "grey", alpha=0.2) +
  geom_line()+
  theme(panel.background = element_blank())
#+ transparent_theme 


# Create the external graphical elements called a 'grop' in Grid terminology
p2_grob = ggplotGrob(f_plot)

# Insert p2_grob inside the ggplot
a<- A+ annotation_custom(grob = p2_grob)
a
