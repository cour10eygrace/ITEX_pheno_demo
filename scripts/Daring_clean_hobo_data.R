#setup
library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

#pull in all hobo data and collapse into one df 
dir_path <- 'C:/Users/court/Documents/Git/ITEX_pheno_demo/data/Daring_raw_data/HOBOs/'
file_pattern <- '.csv' # regex pattern to match the file name format
#file_name<-'New logger S_N#460331 TEMP_TEMP 0007.csv'

clean_hobo <- function(dir_path, file_name){
    read.csv2(paste0(dir_path, file_name), col.names = "all") %>% 
    mutate(logger=all[1])%>%
    slice(-1)%>%
    type_convert()%>%
    separate(all,into=c("x", "Date.Time", "T1", "T2HR", "T3", "T4HR"), sep = ",")%>%
    separate(Date.Time, into=c("Date", "Time"), sep = " ")%>%
    mutate(Date=mdy(Date), doy=yday(Date))%>%
    separate(Date, into = "year", remove = F)%>%
    mutate(year=as.numeric(year))%>%
    separate(logger, into = c("y", "logger"), sep = "/", remove = F)%>%
    mutate(logger=parse_number(logger, trim_ws = T))%>%
    select(-x,-y) 
}

hobo_cleaned_all <- 
  list.files(dir_path, pattern = file_pattern, recursive = T) %>% 
  map_df(~ clean_hobo(dir_path, .))


#deal with 2019 hobo data in excel files 
library(readxl)
dir_path <- 'C:/Users/court/Documents/Git/ITEX_pheno_demo/data/Daring_raw_data/HOBOs/2019/'
file_pattern <- '.xlsx' 

clean_hobo_2019 <- function(dir_path, file_name){
  readxl::read_xlsx(paste0(dir_path, file_name))%>% 
    mutate(logger=...10[1])%>%
    mutate(logger=parse_number(logger, trim_ws = T))%>%
    select(!contains("*F"))%>%
    rename(datetime="Date/Time")%>%
    mutate(Date=mdy_hms(datetime))%>%
    rename(T1="Temperature (*C) c:1", T2HR="High-Res Temp (*C) c:1 2", T3= "Temperature (*C) c:3", T4HR="High-Res Temp (*C) c:3 4")%>%
    mutate(doy=yday(Date))%>%
    separate(Date,into=c("Date","Time"), sep = " ")%>%
    separate(Date, into = "year", remove = F)%>%
    mutate(year=as.numeric(year))%>%
    select(Date, year, Time, T1, T2HR, T3, T4HR, doy, logger) 
    
}


hobo_cleaned_2019 <- 
  list.files(dir_path, pattern = file_pattern) %>% 
  map_df(~ clean_hobo_2019(dir_path, .))

hobo_cleaned_all<-rbind(hobo_cleaned_all, hobo_cleaned_2019)

save(hobo_cleaned_all, file='data/Hobo_cleaned_allyears.Rdata')
#load(file='data/Hobo_cleaned_allyears.Rdata')

#calculate daily averages across 4 temp readings 
#hobo_cleaned_all<-rowwise(hobo_cleaned_all)%>%
 #                 mutate(avg_temp_C=mean(T1:T4HR))%>%ungroup()
                           
#read in logger info   
map<-read.csv("data/Daring_raw_data/hobomaps.csv") #need to update with Eriophorum plots info  
#plantids<-read.csv("data/Daring_raw_data/plantidmaps.csv")
#plantids$plant_id<-as.character(plantids$plant_id)
#plantids$plot<-as.character(as.integer(plantids$plot))

#merge and calculate daily vals
#for now remove missing eriophorum loggers 
hobo_daily<-left_join(hobo_cleaned_all, map)%>%filter(!is.na(otc_treatment))%>%
  group_by(year, doy,otc_treatment)%>% mutate(avgT_day=mean(as.numeric(T1)), na.rm=T)%>%
   select(year, doy, avgT_day, otc_treatment, site)%>%distinct(.)

#plot
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

ggplot(hobo_daily, aes(x=doy, y=avgT_day, fill=otc_treatment))+
  geom_point(aes(colour=as.factor(otc_treatment)),)+ 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  facet_wrap(~as.factor(year), scales = 'free')+
  scale_fill_manual(values=specColor)+  scale_color_manual(values=specColor)

#look at diff b/w warming and ctl 
hobo_dailyw<-pivot_wider(hobo_daily, names_from = "otc_treatment", values_from = "avgT_day")%>%rowwise()%>%
  group_by(year, doy, site)%>%
  mutate(Tdiff=OTC-CTL)

#plot
ggplot(hobo_dailyw, aes(x=doy, y=Tdiff, fill=as.factor(year)))+
  geom_point()+ 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth(method = lm) +
    facet_wrap(~as.factor(year), scales = 'free_y')+ 
  scale_fill_manual(values=specColor)+  scale_color_manual(values=specColor)+
  ylab(expression(Delta~"temp OTC-CTL")) 
#there are a lot of negative values in this plot, realistic?

hist(hobo_dailyw$Tdiff)
mean(hobo_dailyw$Tdiff, na.rm=T) #0.3 deg C warming 
median(hobo_dailyw$Tdiff, na.rm=T) #0.2 deg C warming 
sd(hobo_dailyw$Tdiff, na.rm=T)#0.39
outlier<-0.3+(3*0.4)


save(hobo_dailyw, file="data/hobo_daily_allyears.Rdata")

#filter outliers
#hobo_dailyw<-filter(hobo_dailyw, Tdiff<outlier)

#different time frames of hobo measurements within each year- standardize across 
#take average of warming Tdiff from 180-230 each year?
