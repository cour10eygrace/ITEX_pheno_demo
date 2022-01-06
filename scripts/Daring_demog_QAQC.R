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
#Oxytropis
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
oxy<-mutate(oxy, plant_id=extract_numeric(plant_id))

#see how individuals are tracked through time 
oxy<-arrange(oxy, plant_id, year)
indiv<-group_by(oxy, plant_id)%>%count()

#Eriophorum 
eri<-subset(phen_demw, species=="eriophorum")
eri<-eri %>% select_if(~sum(!is.na(.)) > 0)  #remove cols with no data
#fix issues with inconsistent naming 
unique(eri$plant_id)

