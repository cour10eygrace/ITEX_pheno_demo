#Setup----
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(optimx)


#color pallette
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

#load alex data----
load(file='data/alex_cleaned_phen.Rdata')

#OR with prior visit censored (averaged) DOYs 
#load(file='data/alex_cleaned_phen_censored.Rdata')

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
#alex_cts<-group_by(alex_phen2_long, species, trait)%>%summarise(ct=n())
#write.csv(alex_cts, 'data/traits_AF.csv')

traitsAF<-read.csv("data/traits_AF.csv")
alex_phen2_long<-left_join(alex_phen2_long, traitsAF)%>%select(-ct)       

#std normalize doys within each year
#can do this with snowmelt date also...
alex_phen2_long<-group_by(alex_phen2_long, year)%>%mutate(avg=mean(doy), sd=sd(doy))%>%
                                                            mutate(DOY=(doy-avg)/sd)
hist(alex_phen2_long$DOY)



#plot_all----
ggplot(alex_phen2_long,
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait_simple2+phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#flower time ~ flower #
#No overall relationship
ggplot(subset(alex_phen2_long,trait_simple2=="flower_no"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ # facet_wrap(~species,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

#by spp
#Cassiope and Dryas- flower bud #
#Papaver, Oxyria, Luzula flower #
ggplot(subset(alex_phen2_long,trait_simple2=="flower_no"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~species,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

#flower time ~ fruit num
#negative relationship
ggplot(subset(alex_phen2_long,trait_simple2=="fruit_num"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  # geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+#  facet_wrap(~species,scales = "free")+ 
  ylab("Num fruit (log)")+ xlab("DOY mature flower")

#by spp
#Cassiope, papaver immature fruit, dryas mature fruit
ggplot(subset(alex_phen2_long,trait_simple2=="fruit_num"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
 geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
# geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~species,scales = "free")+ 
  ylab("Num fruit (log)")+ xlab("DOY mature flower")

#flower time ~ repro size 
#pos relationship, OTC higher 
ggplot(subset(alex_phen2_long,trait_simple2=="repro_size"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+# facet_wrap(~species,scales = "free")+ 
  ylab("Mean repro size")+ xlab("DOY mature flower")

#by spp
#Oxyria max, all other spp mean 
#strong direct effect of warming on size in arctagrostis
ggplot(subset(alex_phen2_long,trait_simple2=="repro_size"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ facet_wrap(~species,scales = "free")+ 
  ylab("Mean repro size")+ xlab("DOY mature flower")

#flower time ~ leaf no  
#neg relationship 
ggplot(subset(alex_phen2_long,trait_simple2=="leaf_no"&phen=="pheno_flower_mature_first"),
         aes(x=doy, y=log(value+1), fill=otc_treatment))+
    geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
    #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
    geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
    #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
    theme_bw() + # facet_wrap(~species,scales = "free")+ 
    ylab("Leaf no")+ xlab("DOY mature flower")

#by spp
ggplot(subset(alex_phen2_long,trait_simple2=="leaf_no"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw() + facet_wrap(~species,scales = "free")+ 
  ylab("Leaf no")+ xlab("DOY mature flower")

#flower time ~ veg growth
#neg relationship 
ggplot(subset(alex_phen2_long,trait_simple2=="veg_growth"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw() +#  facet_wrap(~species,scales = "free")+ 
  ylab("Veg growth")+ xlab("DOY mature flower")

#by spp
ggplot(subset(alex_phen2_long,trait_simple2=="veg_growth"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw() +  facet_wrap(~species,scales = "free")+ 
  ylab("Veg growth")+ xlab("DOY mature flower")


#load daring data----
load(file='data/DLphen_w_demog_all.Rdata')
daring_phen<-subset(phen_demw, species=="ledum"|species=="eriophorum"|species=="vaccinium")

#select columns of interest- 
#match to Alex
names(daring_phen)
daring_phen2<-select(daring_phen, species, year, plantid, treatment, first_flower_bud, 
                     first_anther, first_flower_open,first_flower_shed, num_flowering_stalks_1,
                     num_flowers_per_stalk_1, num_flowers_1, num_fruit_per_stalk_1, 
                     num_fruit_1, growth_inc_mm_1, growth_inc_mm_2, growth_inc_mm_3,
                     flowering_stalk_length_mm_early_1, flowering_stalk_length_mm_late_1)
                     
phen_colsx<-c("first_flower_bud","first_anther", "first_flower_open")
trait_colsx<-c("num_flowering_stalks_1","num_flowers_per_stalk_1", "num_flowers_1",
                "num_fruit_per_stalk_1", "num_fruit_1",
                 "growth_inc_mm_1", "growth_inc_mm_2", "growth_inc_mm_3",
                    "flowering_stalk_length_mm_early_1", "flowering_stalk_length_mm_late_1") 

#pivot long for renaming
daring_phen2_long<-pivot_longer(daring_phen2, cols = all_of(phen_colsx), 
                              names_to = "phen", values_to = "doy")%>%
  pivot_longer(.,  cols = all_of(trait_colsx), 
               names_to = "trait", values_to = "value")%>%
  filter(!is.na(doy))%>%                
  filter(!is.na(value))


#std normalize doys within each year
daring_phen2_long<-group_by(daring_phen2_long, year)%>%mutate(avg=mean(doy), sd=sd(doy))%>%
  mutate(DOY=(doy-avg)/sd)
hist(daring_phen2_long$DOY)

#renaming sheet
#daring_cts<-group_by(daring_phen2_long, species, trait)%>%summarise(ct=n())
#write.csv(daring_cts, 'data/traits_DL2.csv')

#plot 
#ggplot(daring_phen2_long,
 #        aes(x=doy, y=log(value+1), fill=treatment))+
#  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
#  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
 # theme_bw()+  facet_wrap(~trait+phen,scales = "free")+ 
#  ylab("trait value (log)")+ xlab("phen DOY")

#rename to match alex
traitsDL<-read.csv("data/traits_DL2.csv")
daring_phen2_long<-left_join(daring_phen2_long, traitsDL)%>%select(-ct)   
names(alex_phen2_long)
names(daring_phen2_long)


#combine dfs??
daring_phen2_long$site<-"DL"
daring_phen2_long$plot<-NA
daring_phen2_long$year<-as.numeric(as.character(daring_phen2_long$year))
daring_phen2_long<-rename(daring_phen2_long, plant_id=plantid, otc_treatment=treatment)
alex_phen2_long<-select(alex_phen2_long, -snow_treatment, -fert_treatment, -trait_simple)

daring_phen2_long<-select(daring_phen2_long, 
  site, plot, year, species, plant_id, otc_treatment, phen, doy, DOY, trait, value, trait_simple2)
#rename first anther to first flower open to match other daring spp 
daring_phen2_long<- mutate(daring_phen2_long,
                           phen=if_else(phen=="first_anther", "first_flower_open", phen))

all_phen_long<-rbind(daring_phen2_long, alex_phen2_long)
all_phen_long<- mutate(all_phen_long, otc_treatment=if_else(otc_treatment=="control", "CTL", otc_treatment))


#combine phen names into bud, open, shed  
all_phen_long<- mutate(all_phen_long, phen=case_when(
                      phen=="pheno_flower_bud_first"~"first_flower_bud", 
                      phen=="pheno_flower_mature_first"~"first_flower_open", 
                      phen=="pheno_flower_senescence_first"~"first_flower_shed", 
                      TRUE~phen))
                      

all_phen_long<-subset(all_phen_long,trait_simple2!="")

#plot all----
ggplot(subset(all_phen_long,phen!="first_flower_shed"& trait_simple2!="leaf_no"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait_simple2 + phen ,scales = "free", nrow = 2)+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#plot by year
ggplot(subset(all_phen_long,phen!="first_flower_shed"& trait_simple2!="leaf_no"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=year), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ #scale_color_manual(values=specColor)+
  scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free", nrow=2)+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#plot by years warm
ggplot(subset(all_phen_long,phen!="first_flower_shed"& trait_simple2!="leaf_no"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=yearswarm), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ #scale_color_manual(values=specColor)+
  scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free", nrow=2)+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#plot separately by site? 
ggplot(subset(all_phen_long, site=="Alexandra Fiord"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=yearswarm), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ #scale_color_manual(values=specColor)+
  scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

ggplot(subset(all_phen_long, site=="Daring Lake"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=yearswarm), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ #scale_color_manual(values=specColor)+
  scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#subset for those with full time span observed 
ggplot(subset(all_phen_long,trait_simple2!="leaf_no"&phen!="first_flower_shed"), 
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=year), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ #scale_color_manual(values=specColor)+
  scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ # facet_wrap(~phen+ trait_simple2,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#flower time ~ flower #
ggplot(subset(all_phen_long,trait_simple2=="flower_no"&phen=="first_flower_open"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + #scale_colour_viridis_b()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+   facet_wrap(~species,scales = "free", nrow=2)+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")+
  labs(colour="Treatment")+ guides(fill="none")
  
#flower time ~ fruit #
ggplot(subset(all_phen_long,trait_simple2=="fruit_no"&phen=="first_flower_open"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + #scale_colour_viridis_b()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ #facet_wrap(~species,scales = "free")+ 
  ylab("Num fruits (log)")+ xlab("DOY mature flower") + ylim(0,3.5)+
  labs(colour="Treatment")+ guides(fill="none")

#flower time ~ repro size
ggplot(subset(all_phen_long,trait_simple2=="repro_size"&phen=="first_flower_open"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + #scale_colour_viridis_b()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ facet_wrap(~species,scales = "free", nrow=2)+ 
  ylab("Reproductive size (mm) (log)")+ xlab("DOY mature flower")+ ylim(2,6.5)+
  labs(colour="Treatment")+ guides(fill="none")

#flower time ~ veg growth
ggplot(subset(all_phen_long,trait_simple2=="veg_growth"&phen=="first_flower_open"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + #scale_colour_viridis_b()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ facet_wrap(~species,scales = "free")+ 
  ylab("Stem growth (mm) (log)")+ xlab("DOY mature flower")+
  labs(colour="Treatment")+ guides(fill="none")


ggplot(subset(all_phen_long,trait_simple2=="veg_growth"&phen=="first_flower_open"),
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  #geom_smooth(method='lm') + #scale_colour_viridis_b()+
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+ #facet_wrap(~species,scales = "free")+ 
  ylab("Stem growth (mm) (log)")+ xlab("DOY mature flower")+
  labs(colour="Treatment")+ guides(fill="none")


#run mixed effects models---- 
#rename into site, subsite 
all_phen_long<- rename(all_phen_long,subsite=site)%>%
  mutate(site=if_else(subsite=="DL", "Daring Lake", "Alexandra Fiord"))

all_phen_long<-mutate(all_phen_long, expstart=case_when(site=="Daring Lake"~2001,
                                              site=="Alexandra Fiord"~1992))%>%
  mutate(yearswarm=year-expstart)

all_phen<-pivot_wider(all_phen_long, names_from = phen, values_from = doy, values_fn = mean)%>% #almost all duplicated so values_fn=mean
  pivot_wider(., names_from = trait_simple2, values_from = value)
all_phen$flower_no<-as.numeric(as.character(all_phen$flower_no))
all_phen$fruit_no<-as.numeric(as.character(all_phen$fruit_no))
all_phen$veg_growth<-as.numeric(as.character(all_phen$veg_growth))
all_phen$repro_size<-as.numeric(as.character(all_phen$repro_size))
all_phen$leaf_no<-as.numeric(as.character(all_phen$leaf_no))
all_phen$first_flower_open<-as.numeric(as.character(all_phen$first_flower_open))
all_phen$first_flower_bud<-as.numeric(as.character(all_phen$first_flower_bud))
all_phen$first_flower_shed<-as.numeric(as.character(all_phen$first_flower_shed))

hist(log(all_phen$year))
hist(all_phen$first_flower_open)

#flower #
summary(lmer(log(flower_no+1)~first_flower_open*otc_treatment + (1|species) + (1|site), all_phen))
#flowering time negative, OTC negative, interaction positive

#fruit # *doesn't converge with site level effect*
summary(lmer(log(fruit_no+1)~first_flower_open*otc_treatment + (1|species) , all_phen))
#flowering time negative 

#repro size
summary(lmer(log(repro_size+1)~first_flower_open*otc_treatment + (1|species) + (1|site), all_phen))
#flowering time negative, interaction positive (weak)

#veg growth
summary(lmer(log(veg_growth+1)~first_flower_open*otc_treatment + (1|species) + (1|site), all_phen))
#flowering time negative 


###Post ESA analyses----
#Sep 1 2022

ggplot(all_phen_long, 
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=year), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ #scale_color_manual(values=specColor)+
  scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#calculate years warming
#need to adjust for alex missing years***
all_phen<-mutate(all_phen, expstart=case_when(site=="Daring Lake"~2001,
                                              site=="Alexandra Fiord"~1992))%>%
  mutate(yearswarm=year-expstart)

#flower #
summary(lmer(log(flower_no+1)~first_flower_open*otc_treatment*yearswarm + (1|species) + (1|site) + (1|subsite)+ (1|year), all_phen))
#flowering time negative, OTC negative, interaction positive

#fruit # 
summary(lmer(log(fruit_no+1)~first_flower_open*otc_treatment + (1|species) + (1|species) + (1|site:year) + (1|year) , all_phen))
#flowering time negative 

#repro size
summary(lmer(log(repro_size+1)~first_flower_open*otc_treatment + (1|species) + (1|site) + (1|year), all_phen))
#flowering time negative, interaction positive

#veg growth (stem growth shrubs)
summary(lmer(log(veg_growth+1)~first_flower_bud*otc_treatment + (1|species) + (1|site)  +
               (1|year),control = lmerControl(optimizer= "optimx",
                                              optCtrl  = list(method="nlminb")), all_phen))
#flowering time negative, OTC positive, interaction negative  


#Look at non-linearity----
ggplot(subset(all_phen_long,trait_simple2!="leaf_no"&phen!="first_flower_shed"), 
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 2)) + #splines
  #geom_smooth(method='lm') 
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #scale_color_viridis_c()+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")
#some are non-linear
ggplot(subset(all_phen_long,trait_simple2=="veg_growth"&phen=="first_flower_bud"), 
       aes(x=DOY, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 2)) + #splines
  geom_smooth(method='lm', lty=2) +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #scale_color_viridis_c()+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

ggplot(subset(all_phen_long,trait_simple2=="flower_no"&phen=="first_flower_open"), 
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=otc_treatment), alpha=0.5)+
#  geom_smooth(method = lm, formula = y ~ splines::bs(x, 2)) + #splines
  geom_smooth(method='lm', lty=2) +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #scale_color_viridis_c()+
  geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait_simple2+ phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

