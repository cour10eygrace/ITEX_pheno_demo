library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)


#Daring data 
load('data/DLphen_dem_climate.Rdata')
load(file="data/Mixed_mods_df.Rdata")

#add in season lags 
flower_open<-left_join(flower_open, seas_clim)

#deal with summer_lag for year 0 (2001) replace with current year Summer
flower_open<-mutate(flower_open, Summer_lag=if_else(is.na(Summer_lag), Summer, Summer_lag))

semdat<-select(flower_open, species, year, plantid, treatment, phen, trait, value, doy, sfDOY,  phen2, trait2, Spring, Fall, Summer, 
               Spring_lag, Summer_lag, Fall_lag, Summer_diff) 
semdat<-subset(semdat, treatment=="CTL") #only keep control data - look at OTCs separately 

#Fig3a
plota<-ggplot(semdat,
       aes(x=as.numeric(year), y=Summer))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", se = T) + theme_bw()+
  xlab("Year")+ ylab("Growing Season temp (C)")+
   geom_text(aes(x=2001, y=15, label = "a)"))


# Alex data
#import the data air temp
#only sites with pre 2003 data 
Dryas_air_temp <- read.csv( "data/Alex_raw_data/compiled_hobo/Dryas/air_temp_data/compiled_hobo_dry_air_temp_NA-NA.csv", header=TRUE)
Dryas_air_temp <- Dryas_air_temp[!is.na(Dryas_air_temp$date),]
Dryas_air_temp$form_date_time <- as.POSIXct(Dryas_air_temp$date, format="%Y-%m-%d %H:%M:%S")

Mead_air_temp <- read.csv( "data/Alex_raw_data//compiled_hobo/Meadow/air_temp_data/compiled_hobo_mead_air_temp_NA-NA.csv", header=TRUE)
Mead_air_temp$form_date_time <- as.POSIXct(Mead_air_temp$date, format="%Y-%m-%d %H:%M:%S")

all_air_temp<-rbind(Dryas_air_temp, Mead_air_temp)
all_air_temp$year<-lubridate::year(all_air_temp$date)
all_air_temp$month<-lubridate::month(all_air_temp$date)
all_air_temp$doy<-lubridate::yday(all_air_temp$date)

all_air_temp<-filter(all_air_temp, month=="6"|month=="7"|month=="8")%>%
  filter(year<2004) #only have 2000, 2001, 2002

names(all_air_temp)

all_air_tempx<-select(all_air_temp,
  site, plot, otc_treatment, air_temp, year, month, doy)%>%group_by(site, plot, year, month, doy, otc_treatment)%>%
  summarise(avg_air_temp=mean(air_temp))%>%
  pivot_wider(names_from = otc_treatment, values_from = avg_air_temp)%>%
  mutate(diff=OTC-control)


all_air_tempx<-subset(all_air_tempx, !is.na(diff))

plot(all_air_tempx$diff~all_air_tempx$doy)
#filter outlier
all_air_tempx<-subset(all_air_tempx, diff>-2)

mean(all_air_tempx$diff) #1.13 deg C

#plot Fig 3b
plotb<-ggplot(all_air_tempx,
       aes(x=doy, y=diff))+
  geom_point(alpha=0.5)+
 # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth()+
     geom_hline(yintercept=0, lty=2)+
    geom_hline(yintercept=1.13, lty=2, color="red")+ #average diff 
     # scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw() +  #facet_wrap(~site)+ 
  ylab("Air temp(C) OTC-CTL")+ xlab("DOY") + geom_text(aes(x=175, y=5, label = "b)"))

#Fig 3
ggpubr::ggarrange(plota,plotb)
