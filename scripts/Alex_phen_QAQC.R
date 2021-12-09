#Courtney G. Collins QA/QC on Cassandra K's phenology file 
#November 2021
library(tidyr)
library(ggplot2)
library(dplyr)

setwd("C:/Users/court/Google Drive/UBC Postdoc/Alex_phenology")
alex_phen<-read.csv("data/AlexandraFiord_Phenology.csv")#32592 obs
str(alex_phen)

#check unique entries on metadata
unique(alex_phen$Site)#"Site" for some ??
unique(alex_phen$Species)#looks good
unique(alex_phen$Year)#looks good
unique(alex_phen$Plot)#looks good
unique(alex_phen$Otctreatment)#210?? blanks??
unique(alex_phen$Snowtreatment)#blanks-should be control??
unique(alex_phen$Ferttreatment)#blanks-should be control??

#filter out obs missing treatment info from above ~1000 obs 
alex_phen<-subset(alex_phen, Site!="Site"& Otctreatment!=210 & Otctreatment!="")%>% 
  distinct(.)#31536 obs 

#PHENOLOGY----
#remove columns that are not phenology data
phen_cols<-alex_phen%>% select_if(funs(is.numeric(.)))
names(phen_cols)                             
phen_cols<-dplyr::select(phen_cols, -Plot, -Replicate, -Flowers_per_plot, -Totals_day, -Year)%>%
  dplyr::select(-contains(c("_no", "length" ,"height", "growth", "max", "Axis", "Agi", "Fpc", "measurement", "Retag", 
    "last" )))#remove "last' phases because only in early years-re Greg 
phen_colsx<-names(phen_cols)

#pivot long for plotting 
alex_phen_long<-pivot_longer(alex_phen, cols = all_of(phen_colsx), 
                             names_to = "phen", values_to = "doy")%>%
  select(Site, Species, Year, Plot, Plant, Otctreatment, Snowtreatment, Ferttreatment, Replicate,phen, doy)%>%
  filter(!is.na(doy))

#plot all 
ggplot(alex_phen_long, aes(doy))+
  geom_histogram()+
  facet_wrap(~phen, scales="free")

#Remove outliers
sort(unique(phen_cols$Cap_ripe_first))# 46 -remove 
sort(unique(phen_cols$First_day))#80?-- should be 180
filter(alex_phen, First_day=="80")
sort(unique(phen_cols$Flower_mat_first))#129? -remove 
filter(alex_phen, Flower_mat_first=="129")#remove value--this plant only measured fruiting 

#remove outliers (n=3)
alex_phen<-filter(alex_phen, Cap_ripe_first!=46|is.na(Cap_ripe_first))%>%filter(First_day!=80|is.na(First_day))%>%
  filter(Flower_mat_first!=129|is.na(Flower_mat_first)) 

#pivot long for plotting 
alex_phen_long<-pivot_longer(alex_phen, cols = all_of(phen_colsx), 
                             names_to = "phen", values_to = "doy")%>%
  select(Site, Species, Year, Plot, Plant, Otctreatment, Snowtreatment, Ferttreatment, Replicate,phen, doy)%>%
  filter(!is.na(doy))

#plot again
ggplot(alex_phen_long, aes(doy))+
  geom_histogram()+
  facet_wrap(~phen, scales="free")

#how many observations for each measurement x spp x year? 
#alex_phen_long_cts<-filter(alex_phen_long, !is.na(doy))%>%group_by(phen)%>%dplyr::summarise(ct=n())
alex_phen_long_cts<-filter(alex_phen_long)%>%group_by(phen, Year, Species)%>%
  dplyr::summarise(ct=n()) 

#Now plot by species 
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

#plot 
ggplot(alex_phen_long, aes(doy, fill=Species))+
  geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  facet_wrap(~phen, scales="free")+ theme_bw()

#update inconsistent naming
#naming could be inconsistent in same species across years &/or same species across sites  
alex_phen_long2<-mutate(alex_phen_long, phen2=case_when(phen=="Aborted_seed_disp"~"Abort", 
                                                        phen=="Cap_deh"~"Cap_open_first", 
                                                        phen=="Cap_mat_first"& Species=="SaxOpp"~"Cap_ripe_first",
                                                        phen=="Cap_twist"~"Cap_first",
                                                        phen=="Frt_deh"~"Frt_mat_first",
                                                        phen=="Leaf_eaten"& Species=="Salix"~"Herb",
                                                        phen=="Leaf_mat_sen"~"Leaf_mat",
                                                        phen=="Pd_elong"& Species=="SaxOpp"~"Petal_fall_first",
                                                        phen=="Seed_disp_cap_mat"~"Seed_set_first",
                                                        phen=="Seed_fall"& Species=="Luzula"~"Seed_set_first",
                                                        phen=="Seed_imm_cap_cap_twist_fem_swell"~"Seed_set_first",
                                                        phen=="Sen_first"~"Leaf_sen", 
                                                        phen=="Slv"~"Leaf_sen", 
                                                        phen=="Flower_exp"~"Flower_bbk_first",
                                                        phen=="Leaf_bud"~"Leaf_bbk",
                                                        phen=="Leaf_bbk"&Year==2001&Species=="Arctagrostis"~"Leaf_sen",
                                                        TRUE~ phen))


#Remove rarely measured (and non-phen)
alex_phen_long2<-subset(alex_phen_long2, 
                        phen!="Elong_old"&phen!="Flower_bbk_and_elong"& phen!="Flower_herb" &phen!="Dendro_distance_from_centre_m"&phen!="Finaldate" 
                        &phen!="Leaf_mat_border_line"&phen!="Leaf_mat_old"&phen!="Pd_brkn"&phen!="Rosets_alive"&phen!="Unfertilized"
                        &phen!="Unisex_senescence"&phen!="Percent_green" &phen!="Leaf_eaten")%>%distinct(.)

##keep track of renamed 
alex_phen_long2<- mutate(alex_phen_long2,renamed=if_else(phen2!=phen, 'Y', 'N'))%>%mutate(phen=phen2)%>%select(-phen2)

#plot cleaned 
ggplot(alex_phen_long2, aes(doy, fill=Species))+
  geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  facet_wrap(~phen, scales="free")+ theme_bw()

#put NAs in blank plant #s 
alex_phen_long2 <-alex_phen_long2 %>% mutate_at(.vars = 'Plant', na_if,"")

#make full id column to pull out dups 
alex_phen_long3<-unite(alex_phen_long2, "all", 
                       Site:phen, remove=F)%>%distinct(.) #  %>%group_by(all)%>% mutate(n=n())

#deal with duplicates bc of renaming or due to no plant ID#
#TAKE MEAN OF VALUES
#look at difference between each value and mean, if >3 days~fill doy with NA 
alex_phen_long3<-group_by(alex_phen_long3,all)%>%mutate(doy2=round(mean(doy, na.rm=T)))%>%
  mutate(diff=abs(doy2-doy), n=n())

#use NA real for double vector   
alex_phen_long4<- mutate(alex_phen_long3, doy=doy2)%>%distinct(.)%>%mutate(doy=if_else(diff<4, doy, NA_real_))%>%
  select(-doy2, -diff, -renamed, -n)%>%
  distinct(.) 

#spread back wide
alex_phen_long4$all<-NULL
#if still dups take mean
alex_phen2<-pivot_wider(alex_phen_long4, names_from = "phen", values_from = "doy",values_fn = mean)



#TRAITS/FITNESS----
#now look at range of non phenology measurements 
#select numeric non-phenology data 
num_cols<-alex_phen%>% select_if(funs(is.numeric(.)))
names(num_cols)                             
num_cols<-dplyr::select(num_cols, -Year, -Plot, -Replicate)%>%
  dplyr::select(contains(c("_no", "length" ,"height", "growth", "max", "Axis", "Agi", "Fpc", "plot", "Rosets",
                    "Totals")))%>%dplyr::select(!contains(c("date", "day")))%>%dplyr::select(-Plant_no, Growth_ceased)
num_colsx<-names(num_cols)

#pivot long for plotting
alex_num_long<-pivot_longer(alex_phen, cols = all_of(num_colsx), 
                             names_to = "counts", values_to = "value")%>%
  select(Site, Species, Year, Plot, Plant, Otctreatment, Snowtreatment, Ferttreatment, Replicate, counts, value)%>%
  filter(!is.na(value))

#plot all 
ggplot(alex_num_long, aes(value))+
  geom_histogram()+
  facet_wrap(~counts, scales="free")

#outliers
sort(unique(num_cols$Newgrowth))# 575?? possible? next highest is 37
sort(unique(num_cols$Flower_height_max))# 1144?? next highest is 310 

#remove outliers (n=2)
alex_phen<-filter(alex_phen, Newgrowth!=575|is.na(Newgrowth))%>%
  filter(Flower_height_max!=1144|is.na(Flower_height_max))

#pivot long for plotting
alex_num_long<-pivot_longer(alex_phen, cols = all_of(num_colsx), 
                            names_to = "counts", values_to = "value")%>%
  select(Site, Species, Year, Plot, Plant, Otctreatment, Snowtreatment, Ferttreatment, Replicate, counts, value)%>%
  filter(!is.na(value))

#plot again 
ggplot(alex_num_long, aes(value))+
  geom_histogram()+
  facet_wrap(~counts, scales="free")

#how many observations for each measurement x spp x year? 
alex_num_long_cts<-filter(alex_num_long, !is.na(value))%>%group_by(counts, Year, Species)%>%
  dplyr::summarise(ct=n())

#plot by species
ggplot(alex_num_long, aes(value,fill=Species))+
  geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  facet_wrap(~counts, scales="free")+ theme_bw()

#update inconsistent naming
#naming could be inconsistent in same species across years &/or same species across sites  
alex_num_long2<-mutate(alex_num_long, counts2=case_when(counts=="Aborted_total_no"~"Aborted_no", 
                                                        counts=="Cap_imm_total_no"~"Cap_imm_no", 
                                                        counts=="Cap_mat_total_no"~"Cap_mat_no",
                                                        counts=="Cap_total_no"~"Cap_max", 
                                                        counts=="Flower_bbk_total_no"~"Flower_bbk_no",
                                                        counts=="Flower_bbk_max"~"Flower_bbk_no",
                                                        counts=="Flower_bud_total_no"~"Flower_bud_no",
                                                        counts=="Flower_bud_max"~"Flower_bud_no",
                                                        counts=="Flower_mat_total_no"~"Flower_mat_no",
                                                        counts=="Flower_mat_max"~"Flower_mat_no",
                                                        counts=="Flower_no_max"~"Flower_no", #these have dups 
                                                        counts=="Flower_no_old"~"Flower_no", #these have dups
                                                        counts=="Flowers_plot_max"~"Flowers_per_plot", 
                                                        counts=="Fpc"~"Flowers_no_per_clone", 
                                                        counts=="Slv_no"~"Leaf_sen_no", 
                                                        counts=="Flower_sen_max"~"Flower_sen_no",
                                                        counts=="Flower_sen_total_no"~"Flower_sen_no",
                                                        counts=="Leaf_no_max"~"Leaf_no",  
                                                        counts=="Petal_wilt_total_no"~"Petal_sen_no", 
                                                        counts=="Pollen_total_no"~"Poll_no", 
                                                        counts=="Seed_fall_no"~"Seed_no", #these have dups
                                                        counts=="Seed_disp_max"~"Seed_no", 
                                                        counts=="Seed_disp_no"~"Seed_no",
                                                         TRUE~ counts))

#Remove rarely measured (and non-phen)
alex_num_long2<-subset(alex_num_long2, 
        counts!="Flower_elong_total_no" & counts!="Frt_deh_no"&counts!="Flower_open_no" & counts!="Cap_ripe_total_no"&
          counts!="First_leaf_mat_no" &counts!= "Flower_herb_no"& counts!= "Flower_unfert_total_no" &counts!="Petal_fall_no")%>%
           distinct(.)
##keep track of renamed 
alex_num_long2<- mutate(alex_num_long2,renamed=if_else(counts2!=counts, 'Y', 'N'))%>%mutate(counts=counts2)%>%select(-counts2)

#plot cleaned 
ggplot(alex_num_long2, aes(value, fill=Species))+
  geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  facet_wrap(~counts, scales="free")+ theme_bw()

#put NAs in blank plant #s 
alex_num_long2 <-alex_num_long2 %>% mutate_at(.vars = 'Plant', na_if,"")

#make full id column to pull out dups 
alex_num_long3<-unite(alex_num_long2, "all", 
                       Site:counts, remove=F)%>%distinct(.) #  %>%group_by(all)%>% mutate(n=n())

#deal with duplicates bc of renaming or due to no plant ID#
#TAKE MEAN OF VALUES
alex_num_long3<-group_by(alex_num_long3,all)%>%mutate(value2=round(mean(value, na.rm=T)))%>%
  mutate(diff=abs(value2-value), n=n())

alex_num_long4<- mutate(alex_num_long3, value=value2)%>%distinct(.)%>%#mutate(doy=if_else(diff<4, doy, NA_real_))%>%
  select(-value2, -diff, -renamed, -n)%>%
  distinct(.)

#spread back wide
alex_num_long4$all<-NULL
alex_num2<-pivot_wider(alex_num_long4, names_from = "counts", values_from = "value")


#FULL JOIN 
alex_phen_all<-left_join(alex_phen2, alex_num2)
names(alex_phen_all)
save(alex_phen_all, file='data/alex_cleaned_phen.Rdata')
