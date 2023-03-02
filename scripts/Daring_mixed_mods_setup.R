library(dplyr)
library (tidyr)
library(tidyverse)
load('data/DLphen_dem_climate.Rdata')

#Lag effects----
#determine prev years' demog for lag effects 
#check for gaps in observations 
#if > 1 year since last observed this individual, put NA for lag value
daring_phen_lag<-select(daring_phen_long, -phen, -doy, -av_air_temp_C, -sfDOY)%>%distinct(.)%>%
  group_by(species, plantid, treatment, trait)%>%arrange(year)%>%
  mutate(year=as.numeric(year), value_lag=lag(value), year_lag=lag(year))%>%
  mutate(lagtime=year-year_lag)%>%
  mutate(value_lag=if_else(lagtime>1, NA_real_, value_lag))

#merge back with phen data 
daring_phen_lag$year<-as.character(as.integer(daring_phen_lag$year))
daring_phen_long<-left_join(daring_phen_long, select(daring_phen_lag,  -year_lag, -lagtime))

#raw values
plot(daring_phen_long$value~daring_phen_long$value_lag)
cor.test(daring_phen_long$value, daring_phen_long$value_lag)

#logged 
plot(log(daring_phen_long$value+1)~(daring_phen_long$value_lag+1))
cor.test(log(daring_phen_long$value+1),log(daring_phen_long$value_lag+1))
#highly correlated...but we know this... 

#final cleaning----
#load plot numbers
#only for spp with OTCs!!
#plots<-read.csv("data/Daring_raw_data/plantidmaps.csv")%>%rename(plantid=plant_id, treatment=otc_treatment)
#daring_phen_long<-left_join(daring_phen_long, plots)
#daring_phen_long$plot<-as.character(as.numeric(daring_phen_long$plot))
#names(daring_phen_long)

#take off numbers after traits
daring_phen_long$trait<-substring(daring_phen_long$trait,1,nchar(daring_phen_long$trait)-2)

#add back in info about phenology and trait order 
#traitsDL<-read.csv("data/traits_DL.csv")%>%select(species, treatment,trait_new,trait_order)%>%rename(trait=trait_new)%>%
#  distinct(.)
#daring_phen2_long<-left_join(daring_phen_long, traitsDL)

#remove duplicate traits
#ledum has num stalks and num flowers per stalk- keep only second
#saxifraga has 2 diameter measurements, delete second
daring_phen_long<-mutate(daring_phen_long, remove=case_when(trait=="num_flowering_stalks"& species=="ledum"~"Y",
                                                            trait=="diameter_y"& species=="saxifraga"~"Y",
                                                            TRUE~"N"))
daring_phen_long<-subset(daring_phen_long, remove!="Y")


#Look at phen stages and traits by species 
phen_check<-select(daring_phen_long, species, phen, treatment)%>%distinct(.)
phen_check$plantid<-NULL
phen_check$year<-NULL
phen_check<-distinct(phen_check)

trait_check<-select(daring_phen_long, species, trait, treatment)%>%distinct(.)
trait_check$plantid<-NULL
trait_check$year<-NULL
trait_check<-distinct(trait_check)


#reclassifying to all phen stage equivalents
#first flower open and first flower bud equivalents

#not sure if these are all correct?
#salix does not seem to have a flower 'bud' equivalaent
daring_phen_long<-mutate(daring_phen_long, phen2=case_when(
  species=="salix"& phen=="first_stigma"~"first_flower_open", 
  species=="betula"& phen=="first_stigma"~"first_flower_open", 
  species=="carex"& phen=="first_anther"~"first_flower_open", 
  species=="eriophorum"& phen=="first_anther"~"first_flower_open", 
  species=="betula"& phen=="first_catkin_male"~"first_flower_bud", 
  species=="carex" & phen=="first_stigma"~"first_flower_bud"))
daring_phen_long<-mutate(daring_phen_long, 
                         phen2=if_else(is.na(phen2), phen, phen2))


#reclassifying to num flowers, num fruits, repro_size equivalents
daring_phen_long<-mutate(daring_phen_long, trait2=case_when(
  trait=="num_flowers"|trait=="num_flowers_per_stalk"|trait=="num_catkins"|trait=="num_flowering_stalks"|
    trait=="num_female_catkins"~"num_flowers", 
  trait=="num_fruit"|trait=="num_fruit_per_stalk"~"num_fruit", 
  trait=="flowering_stalk_length_mm"|trait=="flowering_stalk_length_mm_early"|
    trait=="length_mature_female_catkins_mm"~"repro_size", TRUE~NA_character_))

daring_phen_long<-mutate(daring_phen_long, 
                         trait2=if_else(is.na(trait2), trait, trait2))

#now select only standardized traits
flower_open<-subset(daring_phen_long, phen2=="first_flower_open")
flower_open<-subset(flower_open, trait2=="num_flowers"|trait2=="num_fruit"|trait2=="repro_size"|
                      trait2=="diameter_mm"|trait=="growth_inc_mm"|trait=="leaf_length_mm")
#flower_bud<-subset(daring_phen_long, phen2=="first_flower_bud")
#flower_bud<-subset(flower_bud, trait2=="num_flowers"|trait2=="num_fruit"|trait2=="repro_size"|
#                     trait2=="diameter_mm"|trait=="growth_inc_mm"|trait=="leaf_length_mm")

#plot
source('scripts/colorscale.R')
ggplot(flower_open,
       aes(x=scale(doy), y=log(value+1), fill=species))+
  geom_point(aes(colour=species), alpha=0.5)+
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') +
  scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 2)) + 
  theme_bw()+  facet_wrap(~trait2+phen2,scales = "free")+ 
  ylab("trait value (log)")+ xlab("DOY flower open")


#make sure only 1 measurement type per species
check<-group_by(flower_open, species, trait2)%>%summarise(count=n_distinct(trait2))

save(daring_phen_long, flower_open, file="data/Mixed_mods_df.Rdata")
