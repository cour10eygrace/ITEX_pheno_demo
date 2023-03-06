# -- SETUP ----
# load libraries
library(dplyr)
library (tidyr)
library(tidyverse)
library (magrittr)
library (readxl)
library(lubridate)
library(ggplot2)

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
#add counts??

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
#2001-2019 from Colins et al 2021 OTC synthesis 
clim<-read.csv("data/Daring_raw_data/Daring_climate.csv")%>%subset(year!="2019") #2019 values off compared to hobos 
#tally(clim, infill==1) #infilled values total (mostly Aug 2012)

#2019-2022 from raw hourly data-from Shawne Kokelj 11/15/22
clim2019<-read.csv("data/Daring_raw_data/Data - Weather - Daring FTS - 2019.csv")%>%slice(-1)%>%
  select(X, Temp)
clim2020<-read.csv("data/Daring_raw_data/Data - Weather - Daring FTS - 2020.csv")%>%slice(-1)%>%
  select(X, Temp)
clim2021<-read.csv("data/Daring_raw_data/Data - Weather - Daring FTS - 2021.csv")%>%slice(-1)%>%
  select(X, Temp)
clim2022<-read.csv("data/Daring_raw_data/Data - Weather - Daring FTS - 2022.csv")%>%slice(-1)%>%
  select(X, Temp)
clim2<-rbind(clim2019, clim2020,clim2021, clim2022)%>%  mutate(Date=mdy_hms(X))%>%
  mutate(doy=yday(Date), month=month(Date))%>%separate(X, into=c("x", "y","year"), sep="/")%>%
  separate(year, into = c("year", sep=" "))%>%
   select(-x, -y)%>%
  mutate(Temp=as.numeric(Temp))
str(clim2)
#take daily averages
clim2<-group_by(clim2,doy, year)%>%
  mutate(av_air_temp_C=mean(as.numeric(as.character(Temp)), na.rm=T))%>%
  select( year, doy, av_air_temp_C, month)

clim<-select(clim, year, doy, av_air_temp_C, month)
clim<-rbind(clim, clim2)%>%distinct(.)

clim$av_air_temp_C<-round(clim$av_air_temp_C, digits = 3)#all same deg freedom
clim$year<-as.character(as.numeric(clim$year))

#merge with phen
daring_phen_long<-left_join(daring_phen_long, select(clim, year, doy, av_air_temp_C))%>%distinct(.)

#deal with missing snowfree values- 
#so take avg snowfree from that year in all other plots
#except for 2011 where no snowmelt data observed so use doy first positive consecutive 
#positive temps= 143 (very similar to average across all years= 142)
sf<-group_by(sf, year)%>%mutate(sf_mean=round(mean(sfDOY, na.rm=T)))%>%
  mutate(sfDOY=if_else(is.na(sfDOY), sf_mean, sfDOY))%>%
  mutate(sfDOY=if_else(year==2011, 143, sfDOY))
           
#now merge snowfree data 
daring_phen_long<-left_join(daring_phen_long, sf)%>%mutate(DOY_stdsf=doy-sfDOY)%>%distinct(.)

#how to account for OTC warming?? 
#pull in hobo data 
#load(file="data/hobo_daily_allyears.Rdata")
  
#hobo_dailyw$year<-as.character(as.numeric(hobo_dailyw$year))


#add site info to phen df
daring_phen_long<-mutate(daring_phen_long, site=case_when(species=="eriophorum" ~"F", 
                                                            species=="vaccinium"|species=="ledum"~"B"))
#join
#daring_phen_long<-left_join(daring_phen_long, select(hobo_dailyw, -Date))

#see how many warming individuals are missing hobo temp 
#can be due to phenology earlier/later than hobo deployed or missing years (2001, 2010) 
#missing_hobo<-filter(daring_phen_long, is.na(OTC)&treatment=="OTC")
#lots->need to infill these 

#checK if av daily air temp and hobo temps in CTL are similar
#plot(daring_phen_long$av_air_temp_C~daring_phen_long$CTL)
#cor.test(daring_phen_long$av_air_temp_C,daring_phen_long$CTL) #97% correlation- good

#model relationship between ambient air temp and OTC/CTL temps (hobos)  
#use equations to infill missing temps 
#summary(lm(OTC~av_air_temp_C, daring_phen_long)) #R2=0.92
#summary(lm(CTL~av_air_temp_C, daring_phen_long)) #R2=0.95
#summary(lm(OTC~CTL, daring_phen_long)) #R2=0.98 #warms about 0.5 degrees


#now fill in missing hobo temps from above eqns 
#if we had hobo value (CTL or OTC) we used that value, 
#if not we infilled from airtemp for control or regression coeff for warming

#daring_phen_longx<-mutate(daring_phen_long, 
#                  CTL2=if_else(is.na(CTL),  0.938*av_air_temp_C + 1.086, CTL),
#                  OTC2= if_else(is.na(OTC), 0.929*av_air_temp_C + 1.563, OTC))
#pull out correct value by treatment 
#daring_phen_longx<-mutate(daring_phen_longx, 
#        hobo_temp_C=if_else(treatment=="OTC", OTC2, CTL2))

#manually fill in July 21st temp (DOY 202) for 2022 (received from Shawne on 11/22/22)
#daring_phen_longx<-mutate(daring_phen_longx,
#  hobo_temp_C=if_else(doy==202 & year==2022,14.095, hobo_temp_C))


#now calculate GDDs----
#temp sums up to DOY from snowfree  
#use same year and prev year as predictors
sfx<-select(sf, year, sfDOY)%>%distinct(.)

climsf<-
  left_join(clim, sfx)%>%
  filter(av_air_temp_C>0)%>%
  arrange(year, sfDOY, doy)%>%
  group_by(year, sfDOY)%>%#mutate(GDD=cumsum(0.938*av_air_temp_C+1.086), 
                           #      GDD_OTC=cumsum(0.929*av_air_temp_C + 1.563))%>%
  filter(doy>sfDOY)

  #join back to phen data 
daring_phen_longx<-left_join(daring_phen_long, select(climsf, -av_air_temp_C))#%>%
  #mutate(GDD=if_else(treatment=="OTC", GDD_OTC, GDD))

#calculate average growing season temps 
clim<-mutate(clim, season=case_when(month==4|month==5~"Spring",
                          month==7|month==8|month==6~"Summer",
                          month==9|month==10~"Fall", TRUE~NA_character_))%>%
  group_by(year, season)%>%mutate(seas_avg=mean(av_air_temp_C, na.rm=T), n_obs=n())%>%
  filter(!is.na(season))

seas_clim<-select(clim, year, season, seas_avg)%>%distinct(.)%>%
  pivot_wider(names_from = season, values_from = seas_avg)%>%ungroup(.)%>%
  arrange(year)%>%#arrange(year)%>%
  mutate(Spring_lag=lag(Spring), Summer_lag=lag(Summer), Fall_lag=lag(Fall))%>%
  mutate(Summer_diff=Summer-Summer_lag)%>%
  mutate_if(is.numeric, round, digits=3) 


#daring_phen_longx$hobo_temp_C<-round(daring_phen_longx$hobo_temp_C, digits = 3)#all same deg freedom
#daring_phen_longx$GDD<-round(daring_phen_longx$GDD, digits = 3)#all same deg freedom

save(daring_phen_long, seas_clim, file='data/DLphen_dem_climate.Rdata')
