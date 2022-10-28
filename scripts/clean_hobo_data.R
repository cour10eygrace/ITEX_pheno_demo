
library(lubridate)
library(tidyr)
library(dplyr)

#B1 OTC
hobo<-read.csv("Daring_raw_data/HOBOs/PlotB/2002/New logger S_N419752 TEMP_TEMP 0007.csv")

hobo<-separate(hobo, Date.Time, into=c("Date", "Time"), sep = " ")
hobo$Date<-mdy(hobo$Date)
hobo$doy<-yday(hobo$Date)
hobo<-separate(hobo, Date, into = "year", remove = F)
hobo$year<-as.numeric(hobo$year)
hobo<-select(hobo, -contains("...F"))
hobo$logger<-as.numeric(hobo$X.1[13])
hobo<-select(hobo,-X, -X.1)                         
str(hobo)

map<-read.csv("Daring_raw_data/HOBOs/hobomaps.csv")
plantids<-read.csv("Daring_raw_data/HOBOs/plantidmaps.csv")
plantids$plant_id<-as.character(plantids$plant_id)

hobo<-left_join(hobo, map)%>%left_join(., plantids)

hobo<-group_by(hobo, doy)%>% mutate(avgT_day=mean(Temperature...C..c.1))


hobo_daily<-select(hobo, year, doy:avgT_day)%>%distinct(.)

#B1 CTL
hobo<-read.csv("Daring_raw_data/HOBOs/PlotB/2002/New logger S_N419755 TEMP_TEMP 0007.csv")

hobo<-separate(hobo, Date.Time, into=c("Date", "Time"), sep = " ")
hobo$Date<-mdy(hobo$Date)
hobo$doy<-yday(hobo$Date)
hobo<-separate(hobo, Date, into = "year", remove = F)
hobo$year<-as.numeric(hobo$year)
hobo<-select(hobo, -contains("...F"))
hobo$logger<-as.numeric(hobo$X.1[13])
hobo<-select(hobo,-X, -X.1)                         
str(hobo)

map<-read.csv("Daring_raw_data/HOBOs/hobomaps.csv")
plantids<-read.csv("Daring_raw_data/HOBOs/plantidmaps.csv")
plantids$plant_id<-as.character(plantids$plant_id)

hobo<-left_join(hobo, map)%>%left_join(., plantids)

hobo<-group_by(hobo, doy)%>% mutate(avgT_day=mean(Temperature...C..c.1))


hobo_daily2<-select(hobo, year, doy:avgT_day)%>%distinct(.)

hobo_daily2002<-rbind(hobo_daily, hobo_daily2)

DL<-subset(all_phen, site=="Daring Lake")%>%select(-plot)
DL<-left_join(DL, plantids)

DL<-left_join(DL, hobo_daily2002)

#match doy hobo to doy flower 
DL<-mutate(DL, keep=if_else(first_flower_open==doy, 1,0))%>%
  filter(keep>0)
ledum<-subset(DL,species=="ledum")

ggplot(ledum, aes(x=avgT_day, y=flower_no, fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment),)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth(method='lm', lty=2) +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~species,scales = "free")+ 
  ylab("Flower number")+ xlab("Flowering temp (C)")

ggplot(ledum, aes(x=avgT_day, y=fruit_no, fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment),)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth(method='lm', lty=2) +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~species,scales = "free")+ 
  ylab("Fruit number")+ xlab("Flowering temp (C)")

ggplot(ledum, aes(x=avgT_day, y=veg_growth, fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment),)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth(method='lm', lty=2) +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~species,scales = "free")+ 
  ylab("Veg growth")+ xlab("Flowering temp (C)")


