library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(optimx)

#load alex data----
load(file='data/alex_cleaned_phen.Rdata')

#select columns of interest 
#just looking at flower phenology
names(alex_phen)
alex_phen2<-select(alex_phen, site, plot, year, species, plant_id, otc_treatment, 
                   snow_treatment, fert_treatment, pheno_flower_bud_first,
                   pheno_flower_mature_first, pheno_flower_senescence_first,
                   trait_catkin_length_avg, trait_catkin_length_max,
                   trait_flower_height, trait_flower_height_avg, trait_flower_height_max, 
                   trait_flower_fruit_immature_no, trait_flower_fruit_mature_no,
                   trait_agi_meas, trait_flower_bud_no, 
                   trait_flower_total_per_plant_no, trait_leaf_mature_no, 
                   trait_leaf_total_no)

phen_cols<-dplyr::select(alex_phen2, contains("pheno_" )) 
phen_colsx<-names(phen_cols)
trait_cols<-dplyr::select(alex_phen2, contains("trait_" )) 
trait_colsx<-names(trait_cols)

#pivot long for renaming
alex_phen2_long<-pivot_longer(alex_phen2, cols = all_of(phen_colsx), 
                              names_to = "phen", values_to = "doy")%>%
  pivot_longer(.,  cols = all_of(trait_colsx), 
               names_to = "trait", values_to = "value")%>%
  filter(!is.na(doy))%>%
  filter(!is.na(value))

#renaming sheet
#used this to decide which traits to use
#took trait(s) with best coverage for each spp-traitsimple2
#and/or the measurement with the best/most data for a given spp 
#alex_cts<-group_by(alex_phen2_long, species, trait)%>%summarise(ct=n())
#write.csv(alex_cts, 'data/traits_AF.csv')

traitsAF<-read.csv("data/traits_AF.csv")
alex_phen2_long<-left_join(alex_phen2_long, traitsAF)%>%select(-ct)       

#load climate data
# -- SETUP ----
# load libraries
library(dplyr)
library (tidyr)
library(tidyverse)
library (magrittr)
library (readxl)
library(lubridate)
library(ggplot2)

#read in data 
clim<-read.csv("data/Alex_raw_data/claude_climate_data_1980-2018_daily.csv")

#munge 
clim2<-mutate(clim, Date=mdy(date))%>%
  mutate(doy=yday(Date), month=month(Date))%>%
  mutate(season=case_when(month==4|month==5~"Spring",
                          month==7|month==8|month==6~"Summer",
                          month==9|month==10~"Fall", TRUE~NA_character_))%>%
  group_by(year, season)%>%mutate(seas_avg=mean(temp_air_2m_mean, na.rm=T), n_obs=n())%>%
  select(site, plot_id, plot, date, day, year, temp_air_2m_mean, Date, doy, month, season, seas_avg, n_obs)  

seas_clim<-select(clim2, year, season, seas_avg, n_obs)%>%distinct(.)%>%
  pivot_wider(names_from = season, values_from = seas_avg)%>%ungroup(.)%>%
  arrange(year)%>%#arrange(year)%>%
  mutate(Spring_lag=lag(Spring), Summer_lag=lag(Summer), Fall_lag=lag(Fall))%>%
  mutate(Summer_diff=Summer-Summer_lag)%>%
  mutate_if(is.numeric, round, digits=3) 

seas_clim<-filter(seas_clim, year<2019)#2019 Claude values are off 



alex_phen2_long<-left_join(alex_phen2_long, seas_clim)


#subset for reproductive output 
flower_open<-subset(alex_phen2_long, trait_simple2=="flower_no"|trait_simple2=="fruit_no")%>%
  subset(year!=2019)%>%subset(phen=="pheno_flower_mature_first"& snow_treatment=="control"& site!="Fert") #only warming and control plots


#put in zeroes for fruit when missing... 
flower_openx<-group_by(flower_open, site, plot, species, plant_id,otc_treatment, trait_simple2)%>%select(-trait, -trait_simple)%>%
  pivot_wider(names_from = trait_simple2, values_from = value, values_fn = mean) %>%#7 dups take mean 
  mutate(fruit_no= if_else(is.na(fruit_no), 0, fruit_no))%>%  
  mutate(fruit_no= case_when(!is.na(fruit_no)&species=="Luzula"~NA_real_, #never measured 
                             !is.na(fruit_no)&species=="Oxyria"~NA_real_, TRUE~fruit_no))%>% 
                               mutate(infill_fruit=if_else(fruit_no==0, "Y", "N"))

#pivot back long 
flower_open<-pivot_longer(flower_openx, cols = c(flower_no, fruit_no), names_to = "trait_simple2", values_to = "value")


save(alex_phen2_long, flower_open, file='data/AFphen_dem_climate.Rdata')


#plot clim data over time 
#Fig S1b
ggplot(filter(seas_clim, year>1991 & year<2004), 
              aes(x=as.numeric(year), y=Summer))+
         geom_point(alpha=0.5)+
         geom_smooth(method="lm") + theme_bw()+
         xlab("Year")+ ylab("Growing Season temp (C)")

df<-subset(seas_clim, year>1991 & year<2004)
summary(lm(Summer~as.numeric(year), df))    


#calculate avgs 
seas_clim<-filter(seas_clim, year>2000)#to compare with starting 2001 at Daring
seas_clim<-filter(seas_clim, n_obs>60)#remove season-years with too few obs 
seas_clim<-seas_clim[-28, ]#remove -21 value for summer 2013 (weird outlier)

mean(seas_clim$Spring, na.rm=T)
mean(seas_clim$Summer, na.rm=T)
mean(seas_clim$Fall, na.rm=T)


