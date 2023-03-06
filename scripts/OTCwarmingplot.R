library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

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

all_air_temp<-filter(all_air_temp, month=="6"|month=="7"|month=="8")%>%filter(year<2004) #only have 2000, 2001, 2002

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

#plot 
ggplot(all_air_tempx,
       aes(x=doy, y=diff))+
  geom_point(alpha=0.5)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
   geom_hline(yintercept=0, lty=2)+
 # scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw() +  #facet_wrap(~site)+ 
  ylab("Air temp(C) OTC-CTL")+ xlab("doy")
