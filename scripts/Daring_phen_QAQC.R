# -- SETUP ----
# load libraries
library(dplyr)
library (tidyr)
library(tidyverse)
library (magrittr)
library (readxl)

#from Sarah's code 
# source useful functions, findNonnumeric here
source('scripts/utility_functions_all.R')


# -- READ PHENOLOGY DATA ----
excelfile='data/Daring_raw_data/2021 ITEX multi-year phenology.xlsx' #RAW data from Karin Clark with Pivot table removed (version 8/26/21)
sheets=excel_sheets(excelfile)

new_data=list()
for (j in sheets){
  if (j %in% c('betula', 'carex', 'salix', 'eriophorum OTC')){
    #these have two row headers
    headers=read_excel(excelfile, sheet=j, 
                       n_max=2, col_names=F)%>%
      apply (., 2, function(x) paste(x, collapse='_'))
    new_data[[j]]=read_excel(excelfile, sheet=j, skip=2, col_names=F)%>%
      mutate_all(., as.character)
    names (new_data[[j]])=headers
    #new_data$sheet=j
    #new_data[[j]]%<>%
    #mutate(sheet=j)
  }else if (j%in%c('eriophorum')){
    headers=read_excel(excelfile, sheet=j,
                       n_max=4, col_names=F)%>%
      apply (., 2, function(x) paste(x, collapse='_'))
    new_data[[j]]=read_excel(excelfile, sheet=j, skip=4, col_names=F)%>%
      mutate_all(., as.character)
    names (new_data[[j]])=headers
  }else{
    new_data[[j]]=read_excel(excelfile, sheet=j)%>%
      mutate_all(., as.character)#%>%
    #mutate(sheet=j)
    if ('Year...2'%in%names(new_data[[j]])){
      new_data[[j]]=new_data[[j]]%>%rename(Year=Year...2)
    }
    #new_data$sheet=j
  }
}

new_data=lapply(new_data, function(x) setNames(x, gsub('NA_|_NA', '', names(x))))
new_data=lapply(new_data, function(x) setNames(x, gsub('notes', 'Notes', names(x))))


#this includes phenology data and growth/size, repro counts 
#only keep raw data-re-compute means later
cols_to_keep=c(#all
               "Location", "Year", "ID no.", "sheet", "Notes",
               #oxtytropis
              #"P1( MET) snow free", #leave these out for now and just use on the ground obs
               "P1 (obs)a snow free",
               "P2a 1st green leaf","P2b 1st flower bud","P3 1st flower open","P4 1st petal drop",
               "P5 last petal drop","P6 1st seed shed","Q1a no. of buds", "Q1b no. of pods", "Q2 length (mm)", "Q2 width (mm)",  
               #ledum
               #"P1 (MET) snow free",
               "P1 (obs) snow free" , "P2 flower buds visible"  ,  "P3 first flower open",  "P4 first flower shed" ,      
               "P5 last flower shed"  ,  "P6 first fruit visible" , "Q1 no. flowering stalks", 
               "Q2 number of flowers/ stalk","...15",   "...16" ,  "Q3 number of fruit / stalk",  "...19"  ,"...20" ,                     
                 "Q 4 Growth Increment(mm)" , "...24",  "...25" , 
              #OTC
              "Q2 no. flowers/ stalk...14", "Q2 no. flowers/ stalk...15", "Q2 no. flowers/ stalk...16",
              "Q2 no. flowers/ stalk...17","Q3 no. fruit/ stalk...19" ,  "Q3 no. fruit/ stalk...20",  
              "Q3 no. fruit/ stalk...21"  , "Q3 no. fruit/ stalk...22"  ,      
              "Q4 growth increment...25"  , "Q4 growth increment...26"  , "Q4 growth increment...27",  
              #vaccinium 
               "P1(obs) snow free",
               "P2 flower bud visible", "P6 first friut visible",
               "Q1 no. of flowers", "Q2 no. of fruit", "Q3 growth increment (mm)" , "...18", "...19" , 
              #OTC
               "Q3 growth increment...17",   "Q3 growth increment...18",   "Q3 growth increment...19",
             #betula
               #"P1(MET)",
               "P1(obs)", "P2_1st leaf","P3a_1st male catkin","P3b_1st female catkin",     
               "P4a_1st stigma", "P4b_pollen shed", "P5a_Ist leaf turn","P5b_last leaf turn", "P6_1st leaf shed" ,"P7_all shed",
               "Q1_no. male catkins", "Q2_no. female", "Q3_leaf length","Q4_growth increment"  ,      
             #salix 
               #"P1(MET)_snow", 
               "P1(obs)_snow free",
               "P2_1st leaf bud", "P3a_1st stigma","P3b_pollen shed", "P4_1st leaf turn", "P5_last leaf turn", 
               "P6_1st abs. of leaf", "P8_seed disp.", "Q1_total no. catkins", "Q2_longest leaf (mm)",
                "m a t u r e  f e m a l e  c a t k I n  l e n g t h (m m)", "Q5",
               "Q4_no. mature female catkins", "growth  increment (mm)", "Q6", "NA", 

               #Saxifraga
               "P2 first new leaves","P3 first flower buds visible",
               "P4 first flower open",  "P5 first petal shed","P6 last petal shed",
               "Q1 no. of flowering stalks", "Q2 diam1", "Q2 diam2", "Q2 diam3",           
               #eriophorum & OTC
               "P3_1st bud","P4_1st anther","P5_seed shed", "Q1_no. stalks", "e a r l y  s e a s o n  s h a f t   l e n g t h (m m)",
                "Q2", "l a t e  s e a s o n  s h a f t   l e n g t  h (m m)", "l e a f  l e n g t h (m m)", "Q3",
                  "t u s s o c k  d i a m e t e r (c m )", "Q4", "shaft length @ seed shed  (mm)", "shaft  length @ exposed anthers (m m)",
                         
               #carex
               "P3_1st stigma", "P5_yellow leaf", "P6_seed shed","Q1_age class", "Q2_stalk length (mm)", "Q3_leaf length (mm)")

#select only the cols you want
phen=lapply(new_data, function(x) x=x[,names(x)%in%cols_to_keep])

#tidy up naming
phen=lapply(phen, function(x) setNames(x, sub('^P1(obs)$', 'P1 (obs) snow free', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^P[0-9]a|^P[0-9]b', '', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^P[0-9] ', '', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^P[0-9]_', '', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^P[0-9]\\(obs\\)', 'obs', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^P[0-9]\\(MET\\)', 'MET', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^P[0-9]\\( MET\\)', 'MET', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^_', '', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('Ist|1st', 'first', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('\\(obs\\)a', 'obs', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('^ ', '', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('\\(obs\\) snow free', 'obs snow free', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('obs_snow free', 'obs snow free', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('\\(MET\\) snow free', 'MET snow free', names(x))))
phen=lapply(phen, function(x) setNames(x, sub('first friut visible', 'first fruit visible', names(x))))
phen=lapply(phen, function(x)
  setNames(x, sub('first flower bud|first flower buds visible|flower bud visible|flower buds visible',
                  'first flower bud', names(x))))

#deal with duplicated column names
phen$salix<-as.tibble(phen$salix, .name_repair="unique")
phen$betula<-as.tibble(phen$betula, .name_repair="unique")
phen$eriophorum<-as.tibble(phen$eriophorum, .name_repair="unique")
phen$`eriophorum OTC`<-as.tibble(phen$`eriophorum OTC`, .name_repair="unique")
phen$carex<-as.tibble(phen$carex, .name_repair="unique")

phen$oxytropis<-select(phen$oxytropis, -...20) #remove stray column 
phen$saxifraga<-select(phen$saxifraga, -...20) 
phen$saxifraga<-select(phen$saxifraga, -...18) 

#replace periods with NA for consistency
phen%<>%data.table::rbindlist(., fill=TRUE, use.names=TRUE,
                              idcol = 'sheet') %>%
  mutate_all(list(~na_if(., ".")))%>%
  mutate(species=sub(' OTC', '', sheet),
         treatment=ifelse(grepl('OTC', sheet), 'OTC', 'CTL'))%>%
  select(-sheet)%>%
  #some hidden cells in oxytropis with no useful data have no year value - remove
  filter(!is.na(Year))

#hand remove two, duplicate entries but one has more info than the other
dup1=which(phen$`ID no.`=='BE14'&phen$Year==2012&!is.na(phen$Year)&phen$species=='vaccinium'&is.na(phen$`first flower open`))

phen=phen[-dup1,]
unique(phen$notes[phen$`ID no.`=='BE14'])


dup2=which(phen$`ID no.`=='319'&phen$Year==2012&!is.na(phen$Year)&phen$species=='carex'&is.na(phen$`first flower open`))
phen=phen[-dup2,]

#make long
#separate phen and non-phen 
phen%<>%filter(!is.na(`ID no.`))%>% #remove NAs in ID number 
 gather(key='phen_stage', value='doy', c("all shed","first abs. of leaf",               
                                         "first anther", "first bud","first female catkin" ,             
                                         "first flower bud", "first flower open", "first flower shed",                
                                         "first fruit visible","first green leaf", "first leaf" ,                      
                                         "first leaf bud", "first leaf shed", "first leaf turn",                  
                                         "first male catkin","first new leaves", "first petal drop" ,                
                                         "first petal shed","first seed shed", "first stigma",
                                         "last flower shed" ,"last leaf turn", "last petal drop", 
                                         "last petal shed", "obs", "obs snow free",                    
                                         "pollen shed"  , "seed disp." ,"seed shed", "yellow leaf"))

phen<-filter(phen, !is.na(`ID no.`))%>%
  gather(key='trait', value='value', -c('species','treatment', 'Location','Year', 
                                           'ID no.', 'Notes','phen_stage', 'doy'))

#check naming
sort(unique(phen$phen_stage))

#remove phenophases for which there is NEVER data-mostly due to diff naming across spp 
# these not relevant in keeping NAs
allmissing=phen%>%group_by(species, phen_stage)%>%
  summarize(has_data=sum(!is.na(doy)))%>%
  mutate(has_any_data=ifelse(has_data>0, TRUE, FALSE))%>%
  filter(has_any_data>0)
#keeps only values with data 
phen%<>%
  inner_join(., allmissing%>%select(species, phen_stage))

#clean up one typo, <142 in 2000 should be >2000
#sf doy 151 misentered as 15, etc, can tell due to synchrony and days visited
# reading the notes
phen$doy[phen$doy=='>142'&!is.na(phen$doy)&phen$Year=='2000']='<142'
phen$doy[phen$doy=='15'&!is.na(phen$doy)&phen$Year=='2010']='151'
phen$doy[phen$doy=='22'&!is.na(phen$doy)&phen$Year=='2011']='186-201'
phen$doy[phen$doy=='21'&!is.na(phen$doy)&phen$Year=='2009']='212'
phen$doy[phen$doy=='94'&!is.na(phen$doy)&phen$Year=='2011']='194'
phen$doy[phen$doy=='1955'&!is.na(phen$doy)&phen$Year=='2010']='195'
phen$doy[phen$doy=='<1911'&!is.na(phen$doy)&phen$Year=='2003']='<191'
phen$doy[phen$doy=='<153,158'&!is.na(phen$doy)&phen$Year=='2009']='153<158'
phen$doy[phen$doy=='2120'&!is.na(phen$doy)&phen$Year=='2001']='212'
phen$doy[phen$doy=='153158'&!is.na(phen$doy)&phen$Year=='2009']='153<158'
phen$doy[phen$doy=='22'&!is.na(phen$doy)&phen$Year=='2004']='212'
phen$doy[phen$doy=='291-221'&!is.na(phen$doy)&phen$Year=='2018']='201-221'
phen$doy[phen$doy=='169<167'&!is.na(phen$doy)&phen$Year=='2014']='167<169'
phen$doy[phen$doy=='169<168'&!is.na(phen$doy)&phen$Year=='2014']='168<169'
phen$doy[phen$doy=='169<169'&!is.na(phen$doy)&phen$Year=='2014']='168<169'
phen$doy[phen$doy=='169>169'&!is.na(phen$doy)&phen$Year=='2014']='168<169'
phen$doy[phen$doy=='169>168'&!is.na(phen$doy)&phen$Year=='2014']='168<169'
phen$doy[phen$doy=='144<144'&!is.na(phen$doy)&phen$Year=='2013']='144<146'
phen$doy[phen$doy=='192-192'&!is.na(phen$doy)&phen$Year=='2010']='190-192'
phen$doy[phen$doy=='260'&!is.na(phen$doy)&phen$Year=='2007']='206'
phen$doy[phen$doy=="-"]=NA

#put these in comments, then set these to NA
phen$Notes[phen$doy%in%c('0', 's', "206\\s", "x",
                         "Female", "grazed", "eaten", "209s")&
             !is.na(phen$doy)]=
  paste(phen$Notes[phen$doy%in%c('0', 's', "206\\s", "x",
                                 "Female", "grazed", "eaten", "209s")&
                     !is.na(phen$doy)],phen$doy[phen$doy%in%c('0', 's', "206\\s", "x",
                                                              "Female", "grazed", "eaten", "209s")&
                                                  !is.na(phen$doy)], sep=';')

phen$doy[phen$doy%in%c('0', 's', "206\\s", "209s", "x",
                       "Female", "grazed", "eaten")&
           !is.na(phen$doy)]=NA


#remove question marks, retain the 191
phen$doy[phen$doy=="or=191"&
                  !is.na(phen$doy)]='191'

phen$doy[grepl('\\?', phen$doy)&
                  !is.na(phen$doy)]=NA


#update naming on sf carex
phen<-mutate(phen,phen_stage= case_when(phen_stage=='obs'~'obs snow free', 
                            # phen_stage=='MET'~'MET snow free', 
                             TRUE~phen_stage))


#remove traits for which there is NEVER data-mostly due to diff naming across spp 
# these not relevant in keeping NAs
allmissing2=phen%>%group_by(species, treatment, trait)%>%
  summarize(has_data=sum(!is.na(value)))%>%
  mutate(has_any_data=ifelse(has_data>0, TRUE, FALSE))%>%
  filter(has_any_data>0)
#keeps only values with data 
phen%<>%
  inner_join(., allmissing2%>%select(species, trait))

#deal with ranges in doys 
#separate ranges into prior visit and phen visit 
phen2<-filter(phen, grepl("<|-|>", doy))%>%
  separate(doy, 
           into = c("prior_visit", "phen_visit"), 
           sep = "<|-|>", remove=F)

phen<-filter(phen, !grepl("<|-|>", doy)) #take out the ones with ranges 
phen$phen_visit<-phen$doy
phen$prior_visit<-NA

phen<-select(phen, "Location", "Year", "ID no.","Notes", "species","treatment", "phen_stage","doy",
             "prior_visit", "phen_visit", "trait", "value")

phen<-rbind(phen, phen2)#put back in separated 

#check 
check<-select(phen, doy, prior_visit, phen_visit)%>%distinct(.)

#update where >doy should be prior visit 
phen<-mutate(phen, prior_visit2=case_when(doy==">168"~"168", 
                                           doy==">185"~"185", 
                                           doy==">211"~"211",
                                           doy==">212"~"212", 
                                           doy==">214"~"214",  
                                           doy==">222"~"222"))%>%
  mutate(prior_visit=if_else(!is.na(prior_visit2), prior_visit2, prior_visit))%>%
  mutate(phen_visit=ifelse(!is.na(prior_visit2), NA,phen_visit))%>%select(-prior_visit2)%>%
  mutate(phen_visit=case_when(phen_visit=="or=191"~"191", 
                               TRUE~phen_visit)) #get rid of less than or equal to 
#check again
check2<-select(phen, doy, prior_visit, phen_visit)%>%distinct(.)

#keep one original copy of the prior_visit and doy not imputed
phen%<>%mutate(prior_visit_orig=prior_visit)%>%
  rename(doy_orig=doy)%>%
  rename(doy=phen_visit)%>%
  rename(year=Year, plant_id="ID no.")
#convert all to numeric
phen<-phen%<>%mutate(doy=as.numeric(doy),
                     prior_visit=as.numeric(prior_visit))

#remove rows with neither doy nor prior visit 
#phen_nas<-subset(phen, is.na(prior_visit)&is.na(doy))
#phen<-anti_join(phen, phen_nas)

# -- ASSIGN PRIOR VISITS ----
#pull out all observation dates for each year x spp
obsdates <- phen %>% select(year, species, doy) %>% distinct()
# reassign names
names(obsdates) <- c('year', 'species', "Obs.date")

#find observation date with the minimum difference to the doy of the phenstage for the same spp x year
#often the obs snow free
phenxx<-left_join(phen, obsdates)%>%group_by(year, species, treatment)%>%filter(!is.na(doy))%>%
  mutate(diff=doy-Obs.date)%>%group_by(year, species, treatment,doy, phen_stage)%>%filter(diff>0)%>%
    slice(which.min(diff))%>%
  select(year, species, treatment, phen_stage, Obs.date)

#join with full dataset
#if prior visit is NA fill in with min obs date 
phen<-left_join(phen, phenxx)%>%
  mutate(prior_visit=if_else(is.na(prior_visit), Obs.date, prior_visit))%>%
  select(-Obs.date)
#check all phenology where pv not assigned
#pvmiss<-subset(phen, is.na(prior_visit))%>%
#  select( species, year, phen_stage)%>%distinct(.)
#either the case that snow free was not observed in that year/spp 
#or that snow free entered as coarse date range which is same as first phenophases so diff=0
#either way not informative for prior visits of early season phenology so remove these observations 


#look at prior visits 
#find earliest/latest observed dates across all years for each spp x phenophase
pv_mins<-group_by(phen, species, phen_stage)%>%summarise(pv_min=min(prior_visit, na.rm = T), 
                                                              doy_max=max(doy, na.rm = T))
#merge full dataset with averages to infill 
phen_dem<-mutate(phen, diff=doy-prior_visit)%>%
  left_join(., pv_mins)

#first remove anything where NEITHER prior visit nor DOY has info- don't want to infill both values 
nophen<-subset(phen_dem, is.na(doy)&is.na(prior_visit))
phen_dem<-anti_join(phen_dem, nophen)

#next step
# anything where the prior visit is not assigned or the window is >21 days (i.e. not-informative)
#replace with min prior visit for that spp x phenophase from across all years
#do the same for doy 

phen_dem<-mutate(phen_dem, prior_visit=if_else(is.na(prior_visit), pv_min, prior_visit))%>%
mutate(doy=if_else(is.na(doy), doy_max, doy))

#check that all prior visits are earlier than doys & vice-versa
check3<-phen_dem%>% mutate(check=doy-pv_min, check2=doy_max-prior_visit)

#one situation in snow free <120 where check is negative, put 117 (last day still neg air temp in 2014 climate data)
phen_dem<-mutate(phen_dem, prior_visit=if_else(doy_orig=="<120", 117, prior_visit))

#put back in rows with missing phen data for demographic tracking of individuals 
phen_dem<-rbind(phen_dem, nophen)

#check again
check4<-filter(phen_dem, grepl("<|-|>", doy_orig))%>%select(-Notes,-trait,-value, -plant_id)%>%distinct(.)%>%
  mutate(check=doy-prior_visit)
  
#then take average bw prior visit and doy as 'censored' phen observation
#only do this for observations with a range listed##
phen_dem<-mutate(phen_dem, DOY=if_else(grepl("<|-|>", doy_orig), round((doy+prior_visit)/2), doy))

#plot(phen_dem$DOY~phen_dem$doy)#visualize - error gets worse for later phenophases 


#plot all phen
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861")

#ggplot(phen_dem, aes(DOY, fill=species))+
 # geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
  #facet_wrap(~phen_stage+treatment, scales="free")+ theme_bw()


#combine phases that are same across spp 
phen_dem<-mutate(phen_dem, phen_stage=case_when(
phen_stage=="first green leaf"|phen_stage=="first leaf bud"|phen_stage=="first new leaves"~"first leaf", 
phen_stage=="first bud" ~"first flower bud", 
phen_stage=="first petal drop" ~"first petal shed",
phen_stage=="last petal drop" ~"last petal shed",
phen_stage=="seed disp."|phen_stage=="first seed shed" ~"seed shed",
phen_stage=="first abs. of leaf"~"first leaf shed",
phen_stage=="yellow leaf"~"first leaf turn", 
phen_stage=="all shed"~"last leaf shed",
phen_stage=="first female catkin"~"first catkin female",
phen_stage=="first male catkin"~"first catkin male" ,
TRUE~phen_stage))

#ggplot(phen_dem, aes(DOY, fill=species))+
 # geom_histogram(alpha=0.7)+ scale_fill_manual(values=specColor[c(1:5,34:36,38:40)]) +
#  facet_wrap(~phen_stage+treatment, scales="free")+ theme_bw()


#put snow free dates in separate column ----
sf<-filter(phen_dem, phen_stage=="obs snow free")%>%select(year, species,plant_id, treatment, DOY)%>%distinct(.)%>%rename(sfDOY=DOY)

phen_dem<-filter(phen_dem, phen_stage!="obs snow free")
phen_dem<-left_join(phen_dem, sf)

save(phen_dem, file='data/DLphen_w_priorvisit.Rdata')
#load('data/DLphen_w_priorvisit.Rdata')



