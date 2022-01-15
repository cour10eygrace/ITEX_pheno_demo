#load('data/DLphen_w_priorvisit.Rdata')

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

#Oxytropis----
oxy<-subset(phen_demw, species=="oxytropis")
oxy<-oxy %>% select_if(~sum(!is.na(.)) > 0)  #remove cols with no data

#fix issues with inconsistent naming 
unique(oxy$plant_id)

#first name anything with an 'a' a new number bc it is a distinct individual 
oxy<-mutate(oxy, plant_id=case_when(grepl('10a', plant_id) ~"11",
                                    grepl('01a', plant_id) ~"12",
                                    grepl('02a', plant_id) ~"13",
                                    grepl('09a', plant_id) ~"14",
                                    grepl('260a', plant_id) ~"280",
                                    grepl('261a', plant_id) ~"281",TRUE~plant_id))
#now just pull out numbers from all 
oxy<-mutate(oxy, plant_id=as.character(extract_numeric(plant_id))%>%mutate(plant_id=as.character(plant_id))

#see how individuals are tracked through time 
oxy<-arrange(oxy, plant_id, year)
indiv<-group_by(oxy, plant_id)%>%count()



  #standardize
  (trait=="mean diametre"|trait=="Mean_diametre"|trait=="mean diameter (mm)")~"mean_diam_mm", 
  (trait=="Mean growth increment (mm)"|trait=="mean growth increment(mm)"|trait=="mean_growth increment") ~"mean_growth_inc_mm", 
  (trait=="Mean leaf  length (mm)"|trait=="mean_leaf length"|trait=="Q3_mean")~"mean_leaf_length_mm", 
  trait=="Q2_longest leaf (mm)"~"longest_leaf_mm",
  trait=="Sum green_leaf length"~"sum_green_leaf_length_mm",   
  (trait=="Q1 no. of flowering stalks"|trait=="Q1_no. stalks")~"no_flower_stalks", 
  trait=="Q1 no. of flowers"~"no_flowers", 
  trait=="Q2 no. of fruit"~ "no_fruit", 
  trait=="Q1_no. male catkins"~"no_male_catkins", 
  trait=="Q2_no. female"~"no_female_catkins", 
  trait=="Q1_total no. catkins"~"no_catkins", 
  trait=="Q1_total no. catkins"~"no_catkins", 
  trait=="Q1a no. of buds"~"no_flower_buds", 
  trait=="Q1b no. of pods"~"no_seed_pods", 
  trait== "Q2 number of flowers/ stalk" ~"no_flowers_per_stalk", 
  trait== "Q3 number of fruit / stalk"~"no_fruit_per_stalk"
