library(dplyr)
library (tidyr)
library(tidyverse)
load('data/DLphen_w_priorvisit.Rdata')

#ISSUES 
#need to make sure zeroes are true zeroes not just unmeasured individuals**ISSUE #1 
#ensure consistent measurement units (cm vs mm across years) **ISSUE #2 
#deal with different names for OTC vs CTL within a species **ISSUE #3
#deal with plant ids different across years and renaming of new individuals with 'a' **ISSUE #4 
#traits-calculate means when multiple measurements within an individual ? or keep separate for hierarchical modeling?

#trait/demographic values-> make numeric 
#deal with weird entries 
phen_dem$value[phen_dem$value=='twelve']=12
phen_dem$value[phen_dem$value=='no new growth']=0
phen_dem$value[phen_dem$value=='none'|phen_dem$value=="n/a"|phen_dem$value=="-"|phen_dem$value=="?"|phen_dem$value=="\\"|
                 grepl('day', phen_dem$value)]=NA
phen_dem$value[phen_dem$year==2017&phen_dem$plant_id=="B3"&phen_dem$species=='ledum'&phen_dem$treatment=="CTL"&
      phen_dem$trait=="Q2 number of flowers/ stalk...16"]=8
phen_dem$value[phen_dem$year==2014&phen_dem$plant_id=="BE14"&phen_dem$species=='ledum'&phen_dem$treatment=="OTC"&
                 phen_dem$trait=="Q2 no. flowers/ stalk...14"]=6

#separate out values 
phen_dem<- separate(phen_dem, value, into = c("valx","valy", "valz"), sep = " ", remove=F) 
check<-subset(phen_dem, !is.na(valy))#all doy info-can remove 

#make numeric
phen_dem$value<-as.numeric(phen_dem$valx)
phen_dem<-select(phen_dem, -valx, -valy,-valz)
hist(phen_dem$value)

#outliers? entered wrong?
sort(unique(phen_dem$value), decreasing = T)
#1556? yes ~156
#935? yes ~ 94
#750? yes ~75
phen_dem$value[phen_dem$value==1556]=156
phen_dem$value[phen_dem$value==935]=94
phen_dem$value[phen_dem$value==750]=75
hist(phen_dem$value) #looks good 

#deal with plant_ids
#mutate(phen_dem,plantid=as.character(extract_numeric(plant_id)))%>%
  
ids<-mutate(phen_dem, plantid= gsub(".*[^0-9]","",plant_id))%>% #take letters off front 
   mutate(plantid=if_else(plantid=="", plant_id, plantid))%>% #keep letters at end 
   mutate(plantid = str_remove(plantid, "^0+"))%>% #remove zeroes from front 
  mutate(plantid = str_remove(plantid, "s"))%>%     #remove S from front 
  mutate(plantid = str_remove(plantid, "S"))%>%     #remove S from front 
    mutate(plantid = str_remove(plantid, "E"))%>%     #remove E from front 
         group_by(species, treatment, plantid)%>%
  mutate(count=n_distinct(year))%>%select(species, treatment,plant_id, plantid, count)%>%distinct(.)

ids<-mutate(ids, plantid=case_when(plantid=="124a"~"134a", #betula #typo checked 
                                   plantid=="136 or a?"~"136a",
                                   plantid=="147 or a?"~"147a",
                                   plantid=="141a"~"141", #renamed in the middle of the time-series and then went back 
                                   plantid=="148a"~"148",#renamed in the middle of the time-series and then went back
                                   plantid=="O10a"~"10a", #oxytropis #remove Os
                                   plantid=="O9a"~"9a", #remove Os
                                   plantid=="101"~"10", #typo
                                   plantid=="100a"~'100',
                                   plantid=="311a"~'311',
                                   TRUE~plantid))%>%select(-count)
phen_dem<-left_join(phen_dem, ids)

#update in specific years, species 
phen_dem$plantid[phen_dem$plantid=='209'&phen_dem$species=="oxytropis"]=272 #typo 
phen_dem$plantid[phen_dem$plantid=='260'&phen_dem$species=="carex"]=206 #typo 
phen_dem$plantid[phen_dem$plantid=='160'&phen_dem$species=="saxifraga"]=16 #typo 
phen_dem$plantid[phen_dem$plantid=='14a'&phen_dem$species=="saxifraga"]=14 #inconsistent naming-update 
phen_dem$plantid[phen_dem$plantid=='3'&phen_dem$species=="eriophorum"&phen_dem$year>2000&phen_dem$treatment=="CTL"]='3a' 
phen_dem$plantid[phen_dem$plantid=='261a']=NA #only measured once 
phen_dem$plantid[phen_dem$plantid=='260a']=NA #unclear why added in 2019
phen_dem$plantid[phen_dem$plantid=='132a']=NA #inconsistent naming-remove 
phen_dem$plantid[phen_dem$plantid=='???']=NA
#lots of issues with betula & salix naming consistency across years for new tagged branches
#went with the most consistent name used across later years when no clear notes indicating whats going on 
#anything in the years after the 'a' added also keeps an 'a' can't go back to number only 
phen_dem$plantid[phen_dem$plantid=='136'&phen_dem$species=="betula"&phen_dem$year>1998]='136a'   
phen_dem$plantid[phen_dem$plantid=='147'&phen_dem$species=="betula"&phen_dem$year>1998]='147a'   
phen_dem$plantid[phen_dem$plantid=='145'&phen_dem$species=="betula"&phen_dem$year==2006]=145   
phen_dem$plantid[phen_dem$plantid=='112'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='112a'   
phen_dem$plantid[phen_dem$plantid=='112b'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='112a'   
phen_dem$plantid[phen_dem$plantid=='114'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='114a'   
phen_dem$plantid[phen_dem$plantid=='114b'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='114a'   
phen_dem$plantid[phen_dem$plantid=='107'&phen_dem$species=="salix"&phen_dem$year==2011]='107a'   
phen_dem$plantid[phen_dem$plantid=='110'&phen_dem$species=="salix"&phen_dem$year==2007]='110a'   
phen_dem$plantid[phen_dem$plantid=='115'&phen_dem$species=="salix"&phen_dem$year>2005]='115a'   
phen_dem$plantid[phen_dem$plantid=='117'&phen_dem$species=="salix"&phen_dem$year>2006]='117a'   
phen_dem$plantid[phen_dem$plantid=='111'&phen_dem$species=="salix"&phen_dem$year>2005]='111a'
phen_dem$plantid[phen_dem$plantid=='106'&phen_dem$species=="salix"&phen_dem$year>2004]='106a'
phen_dem$plantid[phen_dem$plantid=='121'&phen_dem$species=="salix"&phen_dem$year>2001]='121a'
phen_dem$plantid[phen_dem$plantid=='105'&phen_dem$species=="salix"&phen_dem$year>2008]='105a'
phen_dem$plantid[phen_dem$plantid=='109'&phen_dem$species=="salix"&phen_dem$year>2006]='109a'

#OTCs put in 2001-data much more consistent after this time frame as well 
phen_dem<-filter(phen_dem, year>2000)

#left off here 1/20/22
check<- group_by(phen_dem,species, treatment, plantid)%>%
  mutate(count=n_distinct(year))%>%select(species, treatment, plantid, count)%>%distinct(.)

#deal with zeroes 
unique(phen_dem$trait)

#measurement ids within individuals 




%>%mutate(value=extract_numeric(valx))

%>%select(-valx)

hist(as.numeric(phen_dem$value))

hist(as.numeric(phen_dem$valx))

#fix more naming
unique(phen_dem$phen_stage)
phen_dem<-mutate(phen_dem, phen_stage=str_replace_all(phen_stage, " ", "_"))%>% #fill blanks with underscores 
  mutate(phen_stage=str_replace(phen_stage, "\\.", ""))
unique(phen_dem$phen_stage)


str(phen_dem)


#pull the data back wide 
phen_demw<-select(phen_dem, species, year, plant_id, treatment, phen_stage, DOY, trait, value)%>%
  group_by(species, year, plant_id, treatment)%>%
  pivot_wider(names_from = "phen_stage", values_from="DOY")%>%
  pivot_wider(names_from = "trait", values_from="value")


#add back in notes
notes<-select(phen_dem, species, year, plant_id, treatment, Notes)%>%distinct(.)
phen_demw<-left_join(phen_demw, notes)

#separate by spp 
#Eriophorum----
eri<-subset(phen_demw, species=="eriophorum")
eri<-eri %>% select_if(~sum(!is.na(.)) > 0)  #remove cols with no data

#fix issues with inconsistent naming 
unique(eri$plant_id)
#E/EE and numbers 1-15 are the same 
#E is CTL, EE is OTC
eri<-mutate(eri, plant_id=as.character(extract_numeric(plant_id)))%>%
  mutate(plant_id=if_else(plant_id=="3"& year>2000& treatment=="CTL","3a", plant_id))#3a gets renamed in 2001 

unique(eri$plant_id)

#see how individuals are tracked through time 
eri<-arrange(eri, plant_id, treatment, year)
cts<-group_by(eri, plant_id, treatment)%>%count()

#check all cols
str(eri)
names(eri)
names(eri)<-str_replace_all(names(eri)," ", "_")

#rename demog cols
eriOTC<-filter(eri, treatment=="OTC")
eri<-anti_join(eri, eriOTC)%>%select(-mean_leaf_length)
eriOTC<-select(eriOTC, -Q3_mean)%>%rename(Q3_mean=mean_leaf_length)
eri<-rbind(eri, eriOTC)
rm(eriOTC)
names(eri)

eri<-rename(eri, no_flower_stalks=Q1_no._stalks, mean_diam_mm=Mean_diametre, mean_leaf_length_mm=Q3_mean, 
            early_shaft_length_mm=mean_shaft_length,late_shaft_length_mm=Mean_shaft_length)
names(eri)

#calculate growth
grow<-select(eri, year, plant_id, treatment,mean_diam_mm)%>%group_by(plant_id, treatment)%>%arrange(year)%>%
  mutate(mean_diam_mm1=lead(mean_diam_mm, 1))%>%mutate(growth_mm=lag(mean_diam_mm1-mean_diam_mm))
eri<-left_join(eri, grow)%>%relocate(Notes, .after=last_col())


#Ledum----
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


#Oxytropis----
oxy<-subset(phen_demw, species=="oxytropis")
oxy<-oxy %>% select_if(~sum(!is.na(.)) > 0)  #remove cols with no data

#fix issues with inconsistent naming 
unique(oxy$plant_id)


#now just pull out numbers from all 
oxy<-mutate(oxy, plant_id=as.character(extract_numeric(plant_id))%>%mutate(plant_id=as.character(plant_id))

#see how individuals are tracked through time 
oxy<-arrange(oxy, plant_id, year)
indiv<-group_by(oxy, plant_id)%>%count()



  #standardize names
  #(trait=="mean diametre"|trait=="Mean_diametre"|trait=="mean diameter (mm)")~"mean_diam_mm", 
  #(trait=="Mean growth increment (mm)"|trait=="mean growth increment(mm)"|trait=="mean_growth increment") ~"mean_growth_inc_mm", 
  #(trait=="Mean leaf  length (mm)"|trait=="mean_leaf length"|trait=="Q3_mean")~"mean_leaf_length_mm", 
  #trait=="Q2_longest leaf (mm)"~"longest_leaf_mm",
  #trait=="Sum green_leaf length"~"sum_green_leaf_length_mm",   
  #(trait=="Q1 no. of flowering stalks"|trait=="Q1_no. stalks")~"no_flower_stalks", 
  #trait=="Q1 no. of flowers"~"no_flowers", 
  #trait=="Q2 no. of fruit"~ "no_fruit", 
  #trait=="Q1_no. male catkins"~"no_male_catkins", 
  #trait=="Q2_no. female"~"no_female_catkins", 
  #trait=="Q1_total no. catkins"~"no_catkins", 
  #trait=="Q1_total no. catkins"~"no_catkins", 
  #trait=="Q1a no. of buds"~"no_flower_buds", 
  #trait=="Q1b no. of pods"~"no_seed_pods", 
  #trait== "Q2 number of flowers/ stalk" ~"no_flowers_per_stalk", 
  #trait== "Q3 number of fruit / stalk"~"no_fruit_per_stalk"
