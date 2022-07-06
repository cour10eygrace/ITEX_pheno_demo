#Courtney G. Collins QA/QC on Cassandra K's phenology file 
#November 2021
library(tidyr)
library(ggplot2)
library(dplyr)

alex_phen<-read.csv("data/Alex_raw_data/compiled_phenology.csv")#Ross compiled 
names(alex_phen)

#check unique entries on metadata
unique(alex_phen$site)#Site? 
unique(alex_phen$species)#looks good
unique(alex_phen$year)#looks good
unique(alex_phen$plot)#need to remove D and G 
unique(alex_phen$otc_treatment)#210?? blanks??
unique(alex_phen$snow_treatment)#blanks-should be control??
unique(alex_phen$fert_treatment)#blanks-should be control??

#filter out obs missing treatment info from above ~1000 obs 
alex_phen<-subset(alex_phen,site!="Site"& otc_treatment!=210 & otc_treatment!="")%>% 
  distinct(.)#31534 obs 

#PHENOLOGY----
#remove columns that are not phenology data
phen_cols<-alex_phen%>% select_if(funs(is.numeric(.)))
phen_cols<-dplyr::select(phen_cols, contains("pheno_" )) 
phen_colsx<-names(phen_cols)

#pivot long for plotting 
alex_phen_long<-pivot_longer(alex_phen, cols = all_of(phen_colsx), 
                             names_to = "phen", values_to = "doy")%>%
  select(site, species, year, plot, plant_id, otc_treatment, snow_treatment,
         fert_treatment, na_replicate,phen, doy)%>%
  filter(!is.na(doy))

#plot all 
ggplot(alex_phen_long, aes(doy))+
  geom_histogram()+
  facet_wrap(~phen, scales="free")

#Remove outliers
sort(unique(phen_cols$pheno_flower_fruit_mature_first))# 46 -remove 
sort(unique(phen_cols$pheno_flower_mature_first))#129? -remove 

#remove outliers (n=2)
alex_phen<-filter(alex_phen, pheno_flower_fruit_mature_first!=46|is.na(pheno_flower_fruit_mature_first))%>%
  filter(pheno_flower_mature_first!=129|is.na(pheno_flower_mature_first)) 

#pivot long for plotting 
alex_phen_long<-pivot_longer(alex_phen, cols = all_of(phen_colsx), 
                             names_to = "phen", values_to = "doy")%>%
  select(site, species, year, plot, plant_id, otc_treatment, snow_treatment,
         fert_treatment, na_replicate,phen, doy)%>%
  filter(!is.na(doy))

#how many observations for each measurement x spp x year? 
#alex_phen_long_cts<-filter(alex_phen_long, !is.na(doy))%>%group_by(phen)%>%dplyr::summarise(ct=n())
alex_phen_long_cts<-filter(alex_phen_long)%>%group_by(phen, species)%>%
  dplyr::summarise(ct=n())%>%group_by(phen)%>%
  mutate(nspp=(n_distinct(species)))
#low_cts<-filter(alex_phen_long_cts, nspp<3)
#low_cts<-unique(low_cts$phen)

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
ggplot(alex_phen_long, aes(doy, fill=species))+
  geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  facet_wrap(~phen, scales="free")+ theme_bw()

##Remove rarely measured 
#alex_phen<-select(alex_phen,-any_of(low_cts))

#remove low cts cols
#phen_colsx<-phen_colsx[!(phen_colsx %in% low_cts)]

#put NAs in blank plant #s 
sort(unique(alex_phen_long$plant_id))
alex_phen_long <-alex_phen_long %>% mutate_at(.vars = 'plant_id', na_if,"")


#TRAITS/FITNESS----
#now look at range of non phenology measurements 
#select numeric non-phenology data 
trait_cols<-alex_phen%>% select_if(funs(is.numeric(.)))
trait_cols<-dplyr::select(trait_cols, contains("trait_" )) 
trait_colsx<-names(trait_cols)


#pivot long for plotting
alex_trait_long<-pivot_longer(alex_phen, cols = all_of(trait_colsx), 
                            names_to = "trait", values_to = "value")%>%
  select(site, species, year, plot, plant_id, otc_treatment, snow_treatment, 
         fert_treatment, na_replicate, trait, value)%>%
  filter(!is.na(value))

#plot all 
ggplot(alex_trait_long, aes(value))+
  geom_histogram()+
  facet_wrap(~trait, scales="free")

#outliers
sort(unique(trait_cols$trait_agi_meas))# 575?? possible? next highest is 37
sort(unique(trait_cols$trait_flower_height_max))# 1144?? next highest is 310 

#remove outliers (n=2)
alex_phen<-filter(alex_phen, trait_agi_meas!=575|is.na(trait_agi_meas))%>%
  filter(trait_flower_height_max!=1144|is.na(trait_flower_height_max))

#pivot long for plotting
alex_trait_long<-pivot_longer(alex_phen, cols = all_of(trait_colsx), 
                              names_to = "trait", values_to = "value")%>%
  select(site, species, year, plot, plant_id, otc_treatment, snow_treatment, 
         fert_treatment, na_replicate, trait, value)%>%
  filter(!is.na(value))%>%distinct(.)

#how many observations for each measurement x spp x year? 
alex_trait_long_cts<-filter(alex_trait_long, !is.na(value))%>%
  group_by(trait, species)%>%
  dplyr::summarise(ct=n())%>%group_by(trait)%>%mutate(nspp=n_distinct(species))
#low_tcts<-filter(alex_trait_long_cts, nspp<3)
#low_tcts<-unique(low_tcts$trait)

#Remove rarely measured (
#alex_phen<-select(alex_phen,-any_of(low_tcts))

#remove low cts cols
#trait_colsx<-trait_colsx[!(trait_colsx %in% low_tcts)]

#put NAs in blank plant #s 
alex_trait_long <-alex_trait_long %>% mutate_at(.vars = 'plant_id', na_if,"")

#now plot by spp 
ggplot(alex_trait_long, aes(value, fill=species))+
  geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  facet_wrap(~trait, scales="free")+ theme_bw()
#weird remove 
alex_phen<-select(alex_phen, -trait_flower_open_petals_no)

save(alex_phen, file='data/alex_cleaned_phen.Rdata')


