load(file='data/alex_cleaned_phen.Rdata')
#look at phen x traits relationships in and out of OTCs 
#ideally will want to have delta days OTC vs CTL as the predictor and the trait values as response (aggregated how?)
#also look into whether warming alters the max vs averages of the traits 

# standardize trait names across species 

#this was a quick fix on 4/19/22 to compile the traits
#want to go back and rewrite everything with Ross's compiled dataset in Alex phen QAQC
#also probably want to make sure the traits were measured in multiple years for future analyses****
traits<-read.csv("data/traits_AF.csv")
traits<-select(traits,-Notes, -ct)%>%mutate_all(na_if,"")%>%filter(!is.na(trait_simple))
colnames<-unique(traits$counts)
colnames2<-names(alex_phen_all[, c(1:8)])
colnames3<-append(colnames2, colnames)

alex_phen_long<-alex_phen_all%>%
  pivot_longer( cols = all_of(colnames), 
                names_to = "counts", values_to = "value")%>%
  left_join(., traits)%>%filter(!is.na(value))%>%filter(!is.na(trait_simple))%>%distinct(.)
#plot
ggplot(alex_phen_long, aes(value, fill=Species))+
  geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  facet_wrap(~trait_simple, scales="free")+ theme_bw()

alex_phen_trait<-pivot_wider(alex_phen_long, names_from = c("trait_simple","early_late", "mean_max"), 
                            values_from = "value")%>% 
  mutate_all(na_if,"NULL")

#Time of flowering ~ flower height/size
#all-#OTC taller (direct effect of warming)
#max
ggplot(alex_phen_trait,
       aes(x=Flower_mat_first, y=repro_length_NA_max, fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + 
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ # facet_wrap(~Species,scales = "free_y")+ 
  ylab("Max repro height (cm)")+ xlab("DOY mature flower")

#mean
ggplot(alex_phen_trait,
       aes(x=Flower_mat_first, y=repro_length_NA_mean, fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ # facet_wrap(~Species,scales = "free_y")+ 
  ylab("Mean repro height (cm)")+ xlab("DOY mature flower")

#by species-diff patterns 
ggplot(subset(alex_phen_trait,Species!="Arctagrostis"&Species!="Cassiope"), #no data
       aes(x=Flower_mat_first, y=repro_length_NA_max, fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='lm') + theme_bw()+ facet_wrap(~Species,scales = "free_y")+ 
  ylab("Max repro height (cm)")+ xlab("DOY mature flower")

ggplot(subset(alex_phen_trait,Species!="Arctagrostis"&Species!="Cassiope"&Species!="Oxyria"), #no data
       aes(x=Flower_mat_first, y=repro_length_NA_mean, fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + theme_bw()+ facet_wrap(~Species,scales = "free_y")+ 
  ylab("Mean repro height (cm)")+ xlab("DOY mature flower")

#Time of flowering ~ flower number
#flowers
ggplot(alex_phen_trait,
       aes(x=Flower_mat_first, y=log(num_flowers_NA_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+#  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

ggplot(alex_phen_trait,
       aes(x=Flower_bud_first, y=log(num_flowers_NA_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+#  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

ggplot(subset(alex_phen_trait,Species!="Arctagrostis"&Species!="Salix"& Species!="SaxOpp"),
       aes(x=Flower_mat_first, y=log(num_flowers_NA_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")+ theme(legend.position = 'none')

#flower buds
ggplot(alex_phen_trait,
       aes(x=Flower_mat_first, y=log(num_flowers_early_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  #geom_smooth(method='lm') +
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+#  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num flowers")+ xlab("DOY first flower")

ggplot(alex_phen_trait,
       aes(x=Leaf_new, y=log(num_flowers_early_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  #geom_smooth(method='lm') +
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num flowers")+ xlab("DOY first flower")

#Time of flowering ~ fruit number 
#fruit
ggplot(alex_phen_trait,
       aes(x=Flower_mat_first, y=log(num_fruit_NA_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+ xlim(165,212)+
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
 # geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+#  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num fruit (log)")+ xlab("DOY mature flower")

ggplot(subset(alex_phen_trait,Species=="Cassiope"| Species=="Dryas"|Species=="Papaver"),
       aes(x=Flower_mat_first, y=log(num_fruit_NA_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+ xlim(165,212)+
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  # geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num fruit (log)")+ xlab("DOY mature flower")


#Time of leaf out num leaves
ggplot(alex_phen_trait,
       aes(x=Leaf_new, y=log(num_leaves_NA_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + xlim(155,190)+scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  # geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+#  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num leaves (log)")+ xlab("DOY leaf out")

ggplot(subset(alex_phen_trait,Species=="Arctagrostis"|Species=="Luzula"|Species=="Oxyria"|Species=="Papaver"),
       aes(x=Leaf_new, y=log(num_leaves_NA_NA), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') + xlim(155,190)+scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  # geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num leaves (log)")+ xlab("DOY leaf out")

#time of green up and new growth 
ggplot(alex_phen_trait,
       aes(x=Leaf_new, y=log(growth_NA_mean), fill=Otctreatment))+
  geom_point(aes(colour=factor(Otctreatment)), alpha=0.5)+
  geom_smooth(method='lm') +scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
 # geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()#+  facet_wrap(~Species,scales = "free_y")+ 
  ylab("Num leaves (log)")+ xlab("DOY leaf out") + xlim(155,190)

#time of green up and new growth 
#Cassiope
ggplot(subset(alex_phen_all,Species=="Cassiope"), aes(x=Bud_exp, y=log(Newgrowth) ,fill=Otctreatment))+
  geom_point()+  geom_smooth(method='lm') + theme_bw() 

ggplot(subset(alex_phen_all,Species=="Cassiope"), aes(x=Bud_exp, y=log(Newgrowth_max) ,fill=Otctreatment))+
  geom_point()+  geom_smooth(method='lm') + theme_bw() +xlim(170,210) #warming shifts from pos to neg 

ggplot(subset(alex_phen_all,Species=="Cassiope"), aes(x=Shoot_bbk, y=log(Newgrowth) ,fill=Otctreatment))+
  geom_point()+  geom_smooth() + theme_bw()

ggplot(subset(alex_phen_all,Species=="Cassiope"), aes(x=Shoot_bbk, y=log(Newgrowth_max) ,fill=Otctreatment))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()

ggplot(alex_phen_all, aes(x=Shoot_elong, y=log(Newgrowth) ,fill=Otctreatment))+
  geom_point()+  geom_smooth(method='lm') + theme_bw() 

ggplot(alex_phen_all, aes(x=First_day, y=log(Newgrowth) ,fill=Otctreatment))+
  geom_point()+  geom_smooth(method="lm") + theme_bw()

##time of green up and leaf size/length 
#Salix
ggplot(alex_phen_all, aes(x=Leaf_exp, y=Leaf_length ,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("Otctreatment") 

#Arctagrostis 
ggplot(alex_phen_all, aes(x=Leaf_exp, y=Green_leaf_length ,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("Otctreatment") 

##time of green up and leaf number 
#Oxyria
ggplot(alex_phen_all, aes(x=Leaf_mat, y=Leaf_mat_no ,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("ent") 
#Papaver Otctreatm
ggplot(alex_phen_all, aes(x=Leaf_mat, y=Leaf_no ,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("Otctreatment") 
#Luzula
ggplot(alex_phen_all, aes(x=Green_leaf, y=Green_leaf_no ,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("Otctreatment") 

#time of flowering/fruiting and fruit number 
#Cassiope
ggplot(alex_phen_all, aes(x=Frt_imm_first, y=Frt_imm_no,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("Otctreatment") 

ggplot(alex_phen_all, aes(x=Flower_mat_first, y=Seed_no,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("Otctreatment") 

ggplot(alex_phen_all, aes(x=Flower_mat_first, y=Seed_no,fill=Species))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap("Otctreatment") 

#how many years for each species, site, treatment etc? 
check<-alex_phen_all%>%filter(!is.na(Otctreatment))%>%group_by(Site, Species, Year, Otctreatment)%>%dplyr::summarise(n=n())
check2<-alex_phen%>%filter(!is.na(Otctreatment))%>%group_by(Species, Year, Otctreatment)%>%dplyr::summarise(n=n())
check3<-alex_phen_all%>%filter(!is.na(Otctreatment))%>%group_by(Species, Year, Otctreatment)%>%dplyr::summarise(n=n())


#look at species over time 
ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_mat_first, 
            x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_bud_first, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_bbk_first, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_elong_first, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_open_first, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_elong_first, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_sen_first, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Flower_sen_first, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Leaf_sen, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all, Species!="Carmem"&Species!="Carsta" &Species!="Eriang"), aes(y=Leaf_sen, 
                                                                                          x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot()+facet_wrap(~Species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#relationship between growth and reproduction? 

ggplot(subset(alex_phen_all,Species=="Cassiope"), aes(x=Flower_mat_no, y=Newgrowth, color=Otctreatment))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap(~Year) 
ggplot(subset(alex_phen_all,Species=="Cassiope"), aes(x=Frt_mat_no, y=Newgrowth, color=Otctreatment))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap(~Year) 
ggplot(subset(alex_phen_all,Species=="Cassiope"), aes(x=Frt_imm_no, y=Newgrowth, color=Otctreatment))+
  geom_point()+  geom_smooth(method='lm') + theme_bw()+ facet_wrap(~Year) 



ggplot(subset(alex_phen_all), aes(y=Green_leaf_no, 
                                  x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot() +theme_bw()+#+facet_wrap(~Species)
  theme(axis.text.x = element_text(angle = 90))

ggplot(subset(alex_phen_all), aes(y=Shoot_no, 
                                  x=as.factor(Year) ,fill=Otctreatment))+geom_boxplot() +theme_bw()+facet_wrap(~Species)+
  theme(axis.text.x = element_text(angle = 90))

library(ggplot2)
library(dplyr)
library(mgcv)
library(tidymv)
library(lme4)
library(lmerTest)
#with interactions
#http://r.qcbs.ca/workshop08/book-en/gam-with-interaction-terms.html
str(alex_phen_trait)
alex_phen_trait$Otctreatment<-as.factor(alex_phen_trait$Otctreatment)
alex_phen_trait$Species<-as.factor(alex_phen_trait$Species)

test<-lmer(repro_length_NA_max ~ scale(Flower_sen_first)*Otctreatment + (1|Species), alex_phen_trait) 
summary(test)

gammtest<-mgcv::gam(repro_length_NA_max ~ scale(Flower_sen_first)*Otctreatment +
                      s(scale(Flower_sen_first), by = Otctreatment, k=5) +
                      s(Species,bs='re'), 
                    data = alex_phen_trait, method="REML")
summary(gammtest)
