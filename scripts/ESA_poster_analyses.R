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
names(alex_phen)
alex_phen2<-select(alex_phen, site, plot, year, species, plant_id, otc_treatment, 
                         snow_treatment, fert_treatment, pheno_flower_bud_first, 
                         pheno_flower_fruit_immature_first, pheno_flower_fruit_mature_first, 
                         pheno_flower_mature_first, pheno_flower_senescence_first,
                         pheno_leaf_mature, pheno_leaf_new, pheno_leaf_senescence,
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
  theme_bw()+  facet_wrap(~trait_simple+phen,scales = "free")+ 
  ylab("trait value (log)")+ xlab("phen DOY")

#load daring data 
load(file='data/DLphen_w_demog_all.Rdata')
daring_phen<-subset(phen_demw, species=="ledum"|species=="eriophorum"|species=="vaccinium")

#select columns of interest- 
#match to Alex? 

names(daring_phen)
daring_phen2<-select(daring_phen, species, year, plantid, treatment, first_flower_bud, 
                     first_flower_open, first_leaf, first_petal_shed, seed_shed, 
                     first_flower_shed, first_fruit_visible

#flower number
ggplot(subset(alex_phen2_long,trait_simple=="flower_no"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ facet_wrap(~trait_simple+phen,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("phen DOY")
#buds
ggplot(subset(alex_phen2_long,trait_simple=="flower_no_bud"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ facet_wrap(~trait_simple+phen,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("phen DOY")

ggplot(subset(alex_phen2_long,trait_simple=="flower_no"&phen=="pheno_flower_mature_first"),
       aes(x=doy, y=log(value+1), fill=otc_treatment))+
  geom_point(aes(colour=factor(otc_treatment)), alpha=0.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + #cubic spline
  geom_smooth(method='lm') + scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
  #geom_smooth(method='gam', formula= y ~ s(x, bs = "cs", fx = TRUE, k = 3)) + 
  theme_bw()+ #+ facet_wrap(~trait_simple+phen,scales = "free")+ 
  ylab("Num flowers (log)")+ xlab("DOY mature flower")

