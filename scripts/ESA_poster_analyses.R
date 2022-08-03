#load alex data
load(file='data/alex_cleaned_phen.Rdata')
library(dplyr)
#color pallette
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

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

#plot_all
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


#load daring data 
load(file='data/DLphen_w_demog_all.Rdata')
daring_phen<-subset(phen_demw, species=="ledum"|species=="eriophorum"|species=="vaccinium")

#select columns of interest- 
#match to Alex
names(daring_phen)
daring_phen2<-select(daring_phen, species, year, plantid, treatment, first_flower_bud, 
                     first_anther, first_flower_open,num_flowering_stalks_1,
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

#renaming sheet
#daring_cts<-group_by(daring_phen2_long, species, trait)%>%summarise(ct=n())
#write.csv(daring_cts, 'data/traits_DL2.csv')

#plot all 
ggplot(daring_phen2_long,
       aes(x=doy, y=log(value+1), fill=treatment))+
  geom_point(aes(colour=factor(treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+  facet_wrap(~trait+phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#rename to match alex
traitsDL<-read.csv("data/traits_DL2.csv")
daring_phen2_long<-left_join(daring_phen2_long, traitsDL)%>%select(-ct)   
names(alex_phen2_long)
names(daring_phen2_long)

#combine dfs
daring_phen2_long$site<-"DL"
daring_phen2_long$plot<-NA
daring_phen2_long$year<-as.numeric(as.character(daring_phen2_long$year))
daring_phen2_long<-rename(daring_phen2_long, plant_id=plantid, otc_treatment=treatment)
alex_phen2_long<-select(alex_phen2_long, -snow_treatment, -fert_treatment, -trait_simple)

daring_phen2_long<-select(daring_phen2_long, 
  site, plot, year, species, plant_id, otc_treatment, phen, doy, trait, value, trait_simple2)

all_phen_long<-rbind(daring_phen2_long, alex_phen2_long)
                          