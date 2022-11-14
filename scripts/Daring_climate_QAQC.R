# -- SETUP ----
# load libraries
library(dplyr)
library (tidyr)
library(tidyverse)
library (magrittr)
library (readxl)

load(file='data/DLphen_w_demog_all.Rdata')

specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

#average sizes across reps
daring_phen<-mutate(phen_demw, growth_inc_mm_x=(growth_inc_mm_1+growth_inc_mm_2+growth_inc_mm_3)/3,
                    diameter_mm_x=(length_mm_1+ width_mm_1)/2, diameter_mm_y= (diameter_mm_1+diameter_mm_2+diameter_mm_3)/3)

#pull out cols 
#repro phen only 
phen_colsx<-c("first_flower_bud",  "first_flower_open",               
               "first_petal_shed",                 
               "last_petal_shed", "first_flower_shed" , "first_fruit_visible" ,              
               "last_flower_shed","first_catkin_female",                                  
              "first_catkin_male", "first_stigma",             
              "pollen_shed","first_anther")                      
                      
trait_colsx<-c("num_flowering_stalks_1","num_flowers_per_stalk_1", "num_flowers_1",
               "num_fruit_per_stalk_1", "num_fruit_1",  "leaf_length_mm_1", 
               "growth_inc_mm_x", "num_male_catkins_1","num_female_catkins_1","num_catkins_1", "num_mature_female_catkins_1",  
               "flowering_stalk_length_mm_early_1", "flowering_stalk_length_mm_late_1",  "flowering_stalk_length_mm_1",
               "length_mature_female_catkins_mm_1",  "diameter_mm_x", "diameter_mm_y" ,  "diameter_mm_1") 
#diameter_mm_x=oxytropis
#diameter_mm_y=saxifraga
#diameter_mm_1=eriophorum
  

#pivot long 
daring_phen_long<-pivot_longer(daring_phen, cols = all_of(phen_colsx), 
                                names_to = "phen", values_to = "doy")%>%
  pivot_longer(.,  cols = all_of(trait_colsx), 
               names_to = "trait", values_to = "value")%>%
  filter(!is.na(doy))%>%                
  filter(!is.na(value))%>%
  select(species:treatment, phen:value)

names(daring_phen_long)

#plot 
ggplot(daring_phen_long,
       aes(x=doy, y=log(value+1), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait+phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("days past snowmelt")

hist(daring_phen_long$doy)
min(daring_phen_long$doy)#MAY 
max(daring_phen_long$doy)#AUG


#pull in climate data----
#filter for Daring and May-Aug
clim<-read.csv("data/Daring_raw_data/Daring_climate.csv")%>%filter(month<9&month>4)
#tally(clim, infill==1) #29 infilled values total (mostly Aug 2012)
clim$av_air_temp_C<-round(clim$av_air_temp_C, digits = 3)#all same df
clim$year<-as.character(as.numeric(clim$year))

#merge with phen
daring_phen_long<-left_join(daring_phen_long, select(clim, year, doy, av_air_temp_C))

#now merge snowfree data 
daring_phen_long<-left_join(daring_phen_long, sf)%>%mutate(DOY_stdsf=doy-sfDOY)

#how to account for OTC warming?? 
#pull in hobo data 
load(file="data/hobo_daily_allyears.Rdata")
  
hobo_dailyw$year<-as.character(as.numeric(hobo_dailyw$year))

#add site info to phen df
daring_phen_long<-mutate(daring_phen_long, site=case_when(species=="eriophorum" ~"F", 
                                                            species=="vaccinium"|species=="ledum"~"B"))
#join
daring_phen_long<-left_join(daring_phen_long, hobo_dailyw)

#checK if av daily air temp and hobo temps in CTL are similar
plot(daring_phen_long$av_air_temp_C~daring_phen_long$CTL)
cor.test(daring_phen_long$av_air_temp_C,daring_phen_long$CTL) #98.6% correlation- V good!

#now add tdiff if treatment=OTC
#only if Tdiff>0??
daring_phen_long<-mutate(daring_phen_long, av_air_temp_C2=if_else(treatment=="OTC", 
                                               (av_air_temp_C+Tdiff), av_air_temp_C))

save(daring_phen_long, file='data/DLphen_dem_climate.Rdata')
