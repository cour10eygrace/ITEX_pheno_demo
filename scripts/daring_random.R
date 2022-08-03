load(file='data/DLphen_w_demog_all.Rdata')

unique(phen_demw$species)
names(phen_demw)
library(ggplot2)
library(dplyr)
library(tidyr)

#manual color 
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

#eriophorum, ledum and vaccinium have OTCs
OTCspp<-subset(phen_demw, species=="ledum"|species=="eriophorum"|species=="vaccinium")

#num Flowers by flowering time 
#eriophorum
a<-ggplot(subset(OTCspp,species=="eriophorum"),
       aes(x=first_flower_bud, y=log(num_flowering_stalks_1+1), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='lm') + 
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+   #facet_wrap(~species,scales = "free_y")+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")+
  ggtitle("Eriophorum")

#ledum
b<-ggplot(subset(OTCspp,species=="ledum"),
       aes(x=first_flower_bud, y=log(num_flowers_per_stalk_1+1), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='lm') + 
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  # facet_wrap(~species,scales = "free_y")+ 
  xlab("DOY mature flower")+  ylab(" ")+
  ggtitle("Ledum")

#vaccinium 
c<-ggplot(subset(OTCspp,species=="vaccinium"),
       aes(x=first_flower_bud, y=log(num_flowers_1+1), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') +
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  scale_color_manual(values=specColor)+ scale_fill_manual(values=specColor)+
  theme_bw()+  # facet_wrap(~species,scales = "free_y")+ 
  xlab("DOY mature flower")+   ylab(" ")+
  ggtitle("Vaccinium")

ggpubr::ggarrange(a,b,c, ncol=3, nrow=1, common.legend = TRUE,legend="right")

#num FRUIT by flowering time 

#ledum
b<-ggplot(subset(OTCspp,species=="ledum"),
          aes(x=first_flower_bud, y=log(num_fruit_per_stalk_1+1), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='lm') + 
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  # facet_wrap(~species,scales = "free_y")+ 
  xlab("DOY mature flower")+  ylab("Num fruit (log) ")+
  ggtitle("Ledum")

#vaccinium 
c<-ggplot(subset(OTCspp,species=="vaccinium"),
          aes(x=first_flower_bud, y=log(num_fruit_1+1), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  geom_smooth(method='lm') +
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  scale_color_manual(values=specColor)+ scale_fill_manual(values=specColor)+
  theme_bw()+  # facet_wrap(~species,scales = "free_y")+ 
  xlab("DOY mature flower")+   ylab(" ")+
  ggtitle("Vaccinium")+ ylim(0,2.5)

ggpubr::ggarrange(b,c, ncol=2, nrow=1, common.legend = TRUE,legend="right")

#separate by spp 
#Eriophorum
eri<-subset(phen_demw, species=="eriophorum")
eri<-eri %>% select_if(~sum(!is.na(.)) > 0)  #remove cols with no data

#fix issues with inconsistent naming 
unique(eri$plantid)

#see how individuals are tracked through time 
eri<-arrange(eri, plantid, treatment, year)
cts<-group_by(eri, plantid, treatment)%>%count()

#check all cols
str(eri)
names(eri)

eri$diameter_mm<-eri$diameter_mm_1*eri$diameter_mm_2

#calculate growth
eri<-group_by(eri, plantid, treatment)%>%arrange(year)%>%
  mutate(diameter_mm_next=lead(diameter_mm, 1))%>%mutate(growth_mm=lag(diameter_mm_next-diameter_mm))

eri<-left_join(eri, grow)%>%relocate(Notes, .after=last_col())

hist(eri$growth_mm)

#Ledum
led<-subset(phen_demw, species=="ledum")
led<-led %>% select_if(~sum(!is.na(.)) > 0)  #remove cols with no data

#check all cols
str(led)
names(led)
names(led)<-str_replace_all(names(led)," ", "_")
names(led)<-str_replace_all(names(led),"/", "")

#rename demog cols
eriOTC<-filter(eri, treatment=="OTC")
eri<-anti_join(eri, eriOTC)%>%select(-mean_leaf_length)
eriOTC<-select(eriOTC, -Q3_mean)%>%rename(Q3_mean=mean_leaf_length)
eri<-rbind(eri, eriOTC)
rm(eriOTC)
names(eri)


#pull out numbers only 
unique(led$plant_id).
led<-mutate(led, plant_id=as.character(extract_numeric(plant_id)))%>%
  arrange(led, plant_id, treatment, year)
cts<-group_by(led, plant_id, treatment)%>%count()


#Oxytropis
oxy<-subset(phen_demw, species=="oxytropis")
oxy<-oxy %>% select_if(~sum(!is.na(.)) > 0)  #remove cols with no data

#fix issues with inconsistent naming 
unique(oxy$plant_id)


#now just pull out numbers from all 
oxy<-mutate(oxy, plant_id=as.character(extract_numeric(plant_id))%>%mutate(plant_id=as.character(plant_id))
            
            #see how individuals are tracked through time 
            oxy<-arrange(oxy, plant_id, year)
            indiv<-group_by(oxy, plant_id)%>%count()
            
            
            