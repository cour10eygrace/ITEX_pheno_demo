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
excelfile='data/2021 ITEX multi-year phenology_clean.xlsx'
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


#this includes phenology data and growth/size, repro counts 
cols_to_keep=c(#all
               "Location", "Year", "ID no.", "sheet", "Notes", 
               #oxtytropis
              #"P1( MET) snow free", #leave these out for now and just use on the ground obs
               "P1 (obs)a snow free",
               "P2a 1st green leaf","P2b 1st flower bud","P3 1st flower open","P4 1st petal drop",
               "P5 last petal drop","P6 1st seed shed","Q1a no. of buds", "Q1b no. of pods", "mean diametre", 
               #ledum
               #"P1 (MET) snow free",
               "P1 (obs) snow free", 
               "P2 flower buds visible","P3 first flower open", 
               "P4 first flower shed",  "P5 last flower shed","P6 first fruit visible",  
               "Q1 no. of flowering stalks", "Q2 number of flowers/ stalk", "Q3 number of fruit / stalk", 
               "mean growth increment(mm)",
               #vaccinium
               "P1(obs) snow free",
               "P2 flower bud visible", "Q1 no. of flowers", "Q2 no. of fruit", 
               #betula
               #"P1(MET)",
               "P1(obs)",
               "P2_1st leaf",  "P3a_1st male catkin","P3b_1st female catkin",
               "P4a_1st stigma","P4b_pollen shed",   "P5a_1st leaf turn",
               "P5b_last leaf turn","P6_1st leaf shed",  "P7_all shed",  
               "Q1_no. male catkins", "Q2_no. female", "Mean leaf  length (mm)", 
               "Mean growth increment (mm)",
               #salix 
               #"P1(MET)_snow", 
               "P1(obs)_snow free",
               "P2_1st leaf bud",  
               "P3a_1st stigma","P3b_pollen shed",   "P4_1st leaf turn", 
               "P5_last leaf turn", "P6_1st abs. of leaf","P7_all leaf shed", 
               "P8_seed disp.", "Q1_total no. catkins", "Q2_longest leaf (mm)", "mean_growth increment",
               #Saxifraga
               "P2 first new leaves","P3 first flower buds visible",
               "P4 first flower open",  "P5 first petal shed","P6 last petal shed",
               "Q1 no. of flowering stalks", "mean diameter (mm)", 
               #eriophorum
               "P3_1st bud","P4_1st anther","P5_seed shed", "Q1_no. stalks", "mean_leaf length","Q3_mean",
               "Mean_diametre", "mean shaft_length" , "Mean_shaft length",                     
               #carex
               "P3_1st stigma", "P4_1st anther", "P5_yellow leaf",   
               "P6_seed shed","Sum green_leaf length")

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
phen%<>%filter(!is.na(`ID no.`))%>%
 gather(key='phen_stage', value='doy', -c('species','treatment', 'Location','Year', 
                                           'ID no.', 'Notes',"Q1a no. of buds",	"Q1b no. of pods",	"mean diametre", 
                                            "mean growth increment(mm)", "Q1_no. stalks", "Q1 no. of flowering stalks",
                                            "Q1 no. of flowers",	"Q2 no. of fruit",	"mean growth increment(mm)" ,
                                           "Q1_no. male catkins", "Q2_no. female", "Mean leaf  length (mm)", 
                                           "Mean growth increment (mm)",
                                           "Q1_total no. catkins", "Q2_longest leaf (mm)", "mean_growth increment",
                                           "mean diameter (mm)",  "Q2 number of flowers/ stalk", 
                                           "Q3 number of fruit / stalk", "mean shaft_length" , "Mean_shaft length",      
                                          "Q3_mean",  "mean_leaf length", "Mean_diametre",
                                           "Q3_mean", "Sum green_leaf length"))

phen<-filter(phen, !is.na(`ID no.`))%>%
  gather(key='trait', value='value', -c('species','treatment', 'Location','Year', 
                                           'ID no.', 'Notes','phen_stage', 'doy'))

#check both
sort(unique(phen$phen_stage))
sort(unique(phen$trait))

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
allmissing2=phen%>%group_by(species, trait)%>%
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
#pvmiss<-subset(phen, is.na(prior_visit)& phen_stage!="obs snow free")%>%
#  select( species, year, phen_stage)%>%distinct(.)
#either the case that snow free was not observed in that year/spp 
#or that snow free entered as coarse date range which is same as first phenophases so diff=0
#either way not informative for prior visits of early season phenology so remove these observations 

# filter for phenology only (remove snow free) 
phen_dem<-filter(phen, phen_stage!="obs snow free")

#look at prior visits 
#find earliest/latest observed dates across all years for each spp x phenophase
pv_mins<-group_by(phen_dem, species, phen_stage)%>%summarise(pv_min=min(prior_visit, na.rm = T), 
                                                              doy_max=max(doy, na.rm = T))
#merge full dataset with averages to infill 
phen_dem<-mutate(phen_dem, diff=doy-prior_visit)%>%
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
check<-phen_dem%>% mutate(check=doy-pv_min, check2=doy_max-prior_visit)


#put back in rows with missing phen data for demographic tracking of individuals 
phen_dem<-rbind(phen_dem, nophen)

#then take average bw prior visit and doy as 'censored' phen observation
phen_dem$DOY<-round((phen_dem$doy+phen_dem$prior_visit)/2)
plot(phen_dem$DOY~phen_dem$doy)#visualize - error gets worse for later phenophases 


#work on trait values->clean and make numeric 
#need to make sure zeroes are true zeroes not just unmeasured individuals**ISSUE #1 

#traits<-filter(phen_dem, !is.na(value))
#unique(traits$value)#too many
#traits<-separate(traits, value, into = "valx", remove=F, sep = " ")%>%select(value, valx)%>%
#  mutate(match=if_else(value==valx, T, F))%>%mutate(valy=extract_numeric(valx))

phen_dem<- separate(phen_dem, value, into = "valx", sep = " ") %>%mutate(value=extract_numeric(valx))%>%select(-valx)
hist(phen_dem$value)

#fix more naming
unique(phen_dem$phen_stage)
phen_dem<-mutate(phen_dem, phen_stage=str_replace_all(phen_stage, " ", "_"))%>% #fill blanks with underscores 
  mutate(phen_stage=str_replace(phen_stage, "\\.", ""))
unique(phen_dem$phen_stage)


str(phen_dem)
save(phen_dem, file='data/DLphen_w_priorvisit.Rdata')
#load('data/DLphen_w_priorvisit.Rdata')

