library(dplyr)
library (tidyr)
library(tidyverse)
load('data/DLphen_w_priorvisit.Rdata')

#ISSUES 
#need to make sure zeroes are true zeroes not just unmeasured individuals**ISSUE #1 
#ensure consistent measurement units (cm vs mm across years) **ISSUE #2 
#traits-calculate means when multiple measurements within an individual ? or keep separate for hierarchical modeling? 
#deal with different measurement names for OTC vs CTL within a species **ISSUE #3
#deal with plant ids different across years and renaming of new individuals with 'a' **ISSUE #4 
#check when individuals are marked 'dead' in the Notes and see if this matches with id numbering changes  
#differentiate between spp where 'a', 'b' is a new individual vs a new branch within same individual 

#rename traits and add measurement replicate info 
trait_names<-read.csv("data/traits_DL.csv")

phen_dem<-left_join(phen_dem, trait_names)%>%select(-trait)%>%rename(trait=trait_new)%>%relocate(value, .after=trait)


#trait/demographic values-> make numeric 
#deal with weird entries 
phen_dem$value[phen_dem$value=='twelve']=12
phen_dem$value[phen_dem$value=='no new growth']=0
phen_dem$value[phen_dem$value=='none'|phen_dem$value=="n/a"|phen_dem$value=="-"|phen_dem$value=="?"|phen_dem$value=="\\"|
                 phen_dem$value=="-" |phen_dem$value=="---"|phen_dem$value=="\ "|	phen_dem$value=="n/a" |
                 grepl('day', phen_dem$value)]=NA
phen_dem$value[phen_dem$value=='41721']='8' #columns where numbers got saved as dates and weird conversions-ledum B3 CTL 2017 
phen_dem$value[phen_dem$value=='42925']='5' #columns where numbers got saved as dates and weird conversions-ledum BE14 OTC 2014 

#separate out values when multiple taken and there is a comma, backslash, etc. between and take the first one 
#separate out values where doy of measurement is recorded in same cell  
#for saxifraga the date of measurement is first and value  second -for all else date second\
#very helpful guide on regex https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285

phen_dem<- separate(phen_dem, value, into = c("valx","valy", "valz"), sep = "(\\s|\\+|\\?|-|,|/)" , remove=F) %>%
mutate(value=if_else(!is.na(valy)&species=='saxifraga',valy,valx)) 

#make numeric
phen_dem<-select(phen_dem, -valx, -valy,-valz)
phen_dem$value2<-as.numeric(phen_dem$value) 
nas<-subset(phen_dem, is.na(value2)&!is.na(value)) #check this is 0
hist(phen_dem$value2)
phen_dem$value<-as.numeric(phen_dem$value)
phen_dem$value2<-NULL

#outliers? entered wrong?
#sort(unique(phen_dem$value), decreasing = T)
#1556? yes ~156
#935? yes ~ 94
#750? yes ~75
phen_dem$value[phen_dem$value==1556]=156
phen_dem$value[phen_dem$value==935]=94
phen_dem$value[phen_dem$value==750]=75
hist(phen_dem$value) #looks good 

#deal with measurement units Issue #2----
#Some years measured in cm some in mm. for leaf/stalk lengths, diameters, growth etc. 

unique(phen_dem$trait)
#plot 
ggplot(filter(phen_dem,grepl("growth|mm|length|diam|width", trait)),aes(y=value, x=year))+
  #geom_histogram()+
  geom_point()+
  facet_wrap(~species + trait + treatment, scales="free")+ theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#years that are measured in the wrong unit (should be mm but in cm)
#multiply x 10 
#betula growth inc 2009, 2010, 2012, 2017, 2020, 2021
#betula leaf length 2009, 2010, 2012, 2017, 2020, 2021
#carex stalk length  2011, 2012, 2014, 2015. (id 316 & 319 in 2017) 
#carex leaf length  2011, 2012, 2014, 2015-yes
#eriophorum stalk length early 2009, 2010 (CTL only), 2011, 2012 
#eriophorum stalk length late 2007, 2010 (CTL only), 2011, 2012, 2015
#eriophorum leaf_length 2011, 2012, 2015
#ledum growth inc 2008, 2009, 2010, 2012
#oxytropis width 2008, 2009, 2012
#oxytropis length 2008, 2009, 2012, 2017
#salix growth inc 2010
#salix catkin length 2010, 2017
#saxifraga diameter 2010, 2011, 2012, 2014, 2017
#vaccinium growth 2001,2002, 2008, 2010 (OTC only), 2012

fixunits<-group_by(phen_dem,trait, species, treatment, year)%>%summarise(max_val=max(value, na.rm=T))%>%
#filter(grepl("growth|mm|length|diam|width", trait))%>%group_by(trait, species, treatment)%>%
mutate(need_fix=case_when(species=="betula"& trait=="growth_inc_mm" & year==2009~"Y",
                          species=="betula"& trait=="growth_inc_mm" & year==2010~"Y",
                          species=="betula"& trait=="growth_inc_mm" & year==2012~"Y",
                          species=="betula"& trait=="growth_inc_mm" & year==2017~"Y",
                          species=="betula"& trait=="growth_inc_mm" & year==2020~"Y",
                          species=="betula"& trait=="growth_inc_mm" & year==2021~"Y",
                          species=="betula"& trait=="leaf_length_mm" & year==2009~"Y",
                          species=="betula"& trait=="leaf_length_mm" & year==2010~"Y",
                          species=="betula"& trait=="leaf_length_mm" & year==2012~"Y",
                          species=="betula"& trait=="leaf_length_mm" & year==2017~"Y",
                          species=="betula"& trait=="leaf_length_mm" & year==2020~"Y",
                          species=="betula"& trait=="leaf_length_mm" & year==2021~"Y", 
                          species=="carex"& trait=="flowering_stalk_length_mm" & year==2011~"Y",
                          species=="carex"& trait=="flowering_stalk_length_mm" & year==2012~"Y",
                          species=="carex"& trait=="flowering_stalk_length_mm" & year==2014~"Y",
                          species=="carex"& trait=="flowering_stalk_length_mm" & year==2015~"Y",
                          species=="carex"& trait=="leaf_length_mm" & year==2011~"Y",
                          species=="carex"& trait=="leaf_length_mm" & year==2012~"Y",
                          species=="carex"& trait=="leaf_length_mm" & year==2014~"Y",
                          species=="carex"& trait=="leaf_length_mm" & year==2015~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_early" & year==2009~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_early" & year==2010 & treatment=="CTL"~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_early" & year==2011~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_early" & year==2012~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_late" & year==2007~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_late" & year==2010 & treatment=="CTL"~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_late" & year==2011~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_late" & year==2012~"Y",
                          species=="eriophorum"& trait=="flowering_stalk_length_mm_late" & year==2015~"Y",
                          species=="eriophorum"& trait=="leaf_length_mm" & year==2011~"Y",
                          species=="eriophorum"& trait=="leaf_length_mm" & year==2012~"Y",
                          species=="eriophorum"& trait=="leaf_length_mm" & year==2015~"Y",
                          species=="ledum"& trait=="growth_inc_mm" & year==2008~"Y",
                          species=="ledum"& trait=="growth_inc_mm" & year==2009~"Y",
                          species=="ledum"& trait=="growth_inc_mm" & year==2010~"Y",
                          species=="ledum"& trait=="growth_inc_mm" & year==2012~"Y",
                          species=="ledum"& trait=="growth_inc_mm" & year==2008~"Y",
                          species=="oxytropis"& trait=="width_mm" & year==2008~"Y",
                          species=="oxytropis"& trait=="width_mm" & year==2009~"Y",
                          species=="oxytropis"& trait=="width_mm" & year==2012~"Y",
                          species=="oxytropis"& trait=="length_mm" & year==2008~"Y",
                          species=="oxytropis"& trait=="length_mm" & year==2009~"Y",
                          species=="oxytropis"& trait=="length_mm" & year==2012~"Y",
                          species=="oxytropis"& trait=="length_mm" & year==2017~"Y",
                          species=="salix"& trait=="growth_inc_mm" & year==2010~"Y",
                          species=="salix"& trait=="length_mature_female_catkins_mm" & year==2010~"Y",
                          species=="salix"& trait=="length_mature_female_catkins_mm" & year==2017~"Y",
                          species=="saxifraga"& trait=="diameter_mm" & year==2010~"Y",
                          species=="saxifraga"& trait=="diameter_mm" & year==2011~"Y",
                          species=="saxifraga"& trait=="diameter_mm" & year==2012~"Y",
                          species=="saxifraga"& trait=="diameter_mm" & year==2014~"Y",
                          species=="saxifraga"& trait=="diameter_mm" & year==2017~"Y",
                          species=="vaccinium"& trait=="growth_inc_mm" & year==2001~"Y",
                          species=="vaccinium"& trait=="growth_inc_mm" & year==2002~"Y",
                          species=="vaccinium"& trait=="growth_inc_mm" & year==2008~"Y",
                          species=="vaccinium"& trait=="growth_inc_mm" & year==2010 & treatment=="OTC"~"Y",
                          species=="vaccinium"& trait=="growth_inc_mm" & year==2012~"Y", TRUE~"N"))%>% select(-max_val)

                          
                          #also issues with these specific individuals...can't get the case_when to work with these... 
                          species=="salix"& trait=="longest_leaf_mm" & year==2011& plant_id=='100'~"Y",
                          species=="salix"& trait=="longest_leaf_mm" & year==2011& plant_id=='101'~"Y",
                          species=="salix"& trait=="longest_leaf_mm" & year==2011& plant_id=='102'~"Y",              
 
                          species=="carex"&year==2017& trait=="flowering_stalk_length_mm"& plant_id=='316'~"Y", 
                         plant_id=='319'&species=="carex"&year==2017& trait=="flowering_stalk_length_mm"~"Y", 
                        plant_id=='1'&species=="eriophorum"&year==2009&trait=="leaf_length_mm"& treatment=="CTL"~"Y",  
              
  
phen_dem<-left_join(phen_dem, fixunits)

phen_dem<-mutate(phen_dem, value=if_else(need_fix=="Y", value*10, value))


#plot again -log to look for out
ggplot(filter(phen_dem,grepl("growth|mm|length|diam|width", trait)),aes(y=log(value), x=year))+
  #geom_histogram()+
  geom_point()+
  facet_wrap(~species + trait + treatment, scales="free")+ theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





#deal with plant_ids- Issue #4----
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
phen_dem$plantid[phen_dem$plantid=='260'&phen_dem$species=="carex"]=206 #typo 
phen_dem$plantid[phen_dem$plantid=='160'&phen_dem$species=="saxifraga"]=16 #typo 
phen_dem$plantid[phen_dem$plantid=='209'&phen_dem$species=="oxytropis"]=272 #typo 
phen_dem$plantid[phen_dem$plantid=='14a'&phen_dem$species=="saxifraga"]=14 #inconsistent naming-update 
phen_dem$plantid[phen_dem$plantid=='3'&phen_dem$species=="eriophorum"&phen_dem$year>2000&phen_dem$treatment=="CTL"]='3a' 
phen_dem$plantid[phen_dem$plantid=='261'&phen_dem$species=="oxytropis"&phen_dem$year>2019]='261a' #inconsistent naming-update
phen_dem$plantid[phen_dem$plantid=='132a']=NA #inconsistent naming-remove #132 measured all years so use this
phen_dem$plantid[phen_dem$plantid=='???']=NA
phen_dem$plantid[phen_dem$plantid=='213'&phen_dem$species=="saxifraga"]=NA #extra id not measured in other years-remove  
phen_dem$plantid[phen_dem$plantid=='118'&phen_dem$species=="salix"&phen_dem$year==2001]=NA #inconsistent naming-remove #118a measured 2001 so use this
phen_dem$plantid[phen_dem$plantid=='260a'&phen_dem$species=="oxytropis"]=NA #only measured in 1 year and 260 measured every yr

#lots of issues with betula & salix naming consistency across years for new tagged branches
#went with the most consistent name used across later years when no clear notes indicating whats going on 
#anything in the years after the 'a' added also keeps an 'a' can't go back to number only for example
phen_dem$plantid[phen_dem$plantid=='136'&phen_dem$species=="betula"&phen_dem$year>1998]='136a'   
phen_dem$plantid[phen_dem$plantid=='147'&phen_dem$species=="betula"&phen_dem$year>1998]='147a'   
phen_dem$plantid[phen_dem$plantid=='145a'&phen_dem$species=="betula"&phen_dem$year==2006]=145   
phen_dem$plantid[phen_dem$plantid=='112'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='112a'   
phen_dem$plantid[phen_dem$plantid=='112b'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='112a'   
phen_dem$plantid[phen_dem$plantid=='114'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='114a'   
phen_dem$plantid[phen_dem$plantid=='114b'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='114a'   
phen_dem$plantid[phen_dem$plantid=='134'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2013]='134a'
phen_dem$plantid[phen_dem$plantid=='138'&phen_dem$species=="betula"&phen_dem$year>2005& phen_dem$year<2016]='138a'
phen_dem$plantid[phen_dem$plantid=='145a'&phen_dem$species=="betula"&phen_dem$year>2007]='145'
phen_dem$plantid[phen_dem$plantid=='118a'&phen_dem$species=="salix"&phen_dem$year>2007& phen_dem$year<2017]='118b'
phen_dem$plantid[phen_dem$plantid=='107'&phen_dem$species=="salix"&phen_dem$year==2011]='107a'   
phen_dem$plantid[phen_dem$plantid=='110'&phen_dem$species=="salix"&phen_dem$year==2007]='110a'   
phen_dem$plantid[phen_dem$plantid=='115'&phen_dem$species=="salix"&phen_dem$year>2005]='115a'   
phen_dem$plantid[phen_dem$plantid=='117'&phen_dem$species=="salix"&phen_dem$year>2006]='117a'   
phen_dem$plantid[phen_dem$plantid=='111'&phen_dem$species=="salix"&phen_dem$year>2005]='111a'
phen_dem$plantid[phen_dem$plantid=='106'&phen_dem$species=="salix"&phen_dem$year>2004]='106a'
phen_dem$plantid[phen_dem$plantid=='121'&phen_dem$species=="salix"&phen_dem$year>2001]='121a'
phen_dem$plantid[phen_dem$plantid=='105'&phen_dem$species=="salix"&phen_dem$year>2008]='105a'
phen_dem$plantid[phen_dem$plantid=='109'&phen_dem$species=="salix"&phen_dem$year>2006]='109a'
phen_dem$plantid[phen_dem$plantid=='114'&phen_dem$species=="salix"&phen_dem$year>2005& phen_dem$year<2013]='114a' 
phen_dem$plantid[phen_dem$plantid=='114b'&phen_dem$species=="salix"&phen_dem$year>2005& phen_dem$year<2013]='114a' 
phen_dem$plantid[phen_dem$plantid=='112'&phen_dem$species=="salix"&phen_dem$year==2007]='112a'
phen_dem$plantid[phen_dem$plantid=='112'&phen_dem$species=="salix"&phen_dem$year==2010]='112b'
phen_dem$plantid[phen_dem$plantid=='112'&phen_dem$species=="salix"&phen_dem$year==2011]='112b'
phen_dem$plantid[phen_dem$plantid=='112a'&phen_dem$species=="salix"&phen_dem$year==2012]='112b'
phen_dem$plantid[phen_dem$plantid=='207'&phen_dem$species=="carex"&phen_dem$year>2003]='207a' 


#OTCs put in 2001-data much more consistent after this time frame as well 
phen_dem<-filter(phen_dem, year>2000)

#check plant IDs with low counts-
#double check that years are either at beginning or end of series sequentially
checkIDs<- group_by(phen_dem,species, treatment, plantid)%>%
  mutate(count=n_distinct(year))%>%select(species, treatment, plantid, count, year)%>%distinct(.)%>%
  arrange(count, plantid, year)%>%
  group_by(species, treatment, plantid, count) %>%
  summarise(year = paste(year, collapse = ","))%>%
  separate(year, into = c('y1', 'y2', 'y3', 'y4', 'y5', 'y6', 'y7', 'y8', 'y9', 'y10', 
                          'y11', 'y12', 'y13', 'y14', 'y15', 'y16', 'y17', 'y18', 'y19', 'y20', 'y21'))

check2<-read.csv('data/plantids_DL.csv') #notes from manually checking all ids 2/22/22
checkIDs<-left_join(checkIDs, check2)%>%relocate(Notes, .after=count)

#look at dead stuff 
dead_check<-select(phen_dem, year, plantid,Notes, species, treatment)%>%filter(grepl("dead|died|new plant|New Plant|Dead|Died",Notes))%>%
                                distinct(.)%>%filter(!grepl("bud|Bud|pod|flower|Flower|stalk|Partially|almost|looks",Notes))

#unresolved 
#vaccinium CTL #5 marked dead 2002- No new tag in 2003
#vaccinium CTL #12,15,16,18 marked dead 2013- No new tag in 2014
#vaccinium 14-says new plant tagged in 2013 but not re-named 
#saxifraga #5 marked dead 2014, No new tag in 2015
#oxytropis #269 marked dead 2014, No new tag in 2015
#oxytropis #3, 9 marked dead 2020, No new tag in 2021
#ledum 9 marked dead 2002, no new tag 2003
#oxytropis 261 says retagged 2013 but no new tag 2014
#carex #304, 309, 316 marked dead 2011, No new tag in 2012
#salix #116a marked dead 2009, No new tag in 2010
#betula #142 marked dead 2011, No new tag in 2012
#betula #138a marked dead 2011 & 2012, No new tag in 2013
#betula #132 marked dead 2008, no new tag 2009


#correct
#oxytropis #1, 2 marked dead 2020, changed to 1a, 2a in 2021
#salix #106 marked dead 2004-changed to 106a 2004
#betula #134a branch marked dead 2004-changed to 134a 2004-assuming measurements in 2004 are from newly tagged branch
#betula 130a-branch marked dead in 2012 and renamed 130a in 2012-assuming measurements in 2012 are from newly tagged branch
#oxytropis 261 died 2014, retagged 261a, not remeasured until 2019 

#for now, let's just remove the unresolved dead ids

dead_check<-select(dead_check, plantid, species)%>%
    filter(plantid!="1"& plantid!="2"& plantid!="1a"& plantid!="2a"& plantid!="106a"& plantid!="134a"& plantid!="130a"&
             plantid!="261"& plantid!="261a")

phen_dem<-anti_join(phen_dem, dead_check)
#remove other years 
phen_dem$plantid[phen_dem$plantid=='138'|phen_dem$plantid=='138b'|phen_dem$plantid=='142a'&phen_dem$species=="betula"]=NA

#remove plant ids set to NA
phen_dem<-filter(phen_dem, !is.na(plantid))

#deal with zeroes 
unique(phen_dem$trait)

#measurement ids within individuals-EG branches





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
#Eriophorum
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


