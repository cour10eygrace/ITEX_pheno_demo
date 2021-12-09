# -- SETUP ----
# load libraries
library(dplyr)
library (tidyr)
library (magrittr)
library (readxl)


setwd("C:/Users/court/Google Drive/UBC Postdoc/Daring_phenology")

#from Sarah's code 
# source useful functions, findNonnumeric here
source('utility_functions_all.R')

prior_visit=function(x){
  if(grepl('^<', x)){
    NA
  }else if (grepl('^>', x)){
    gsub('^>', '', x)
  }else if (grepl('<|>|-', x)){
    gsub('>.*|-.*|<.*', '', x)
  }else{NA}
}

phen_visit=function(x){
  if(grepl('^<', x)){
    gsub('^<', '', x)
  }else if (grepl('^>', x)){
    NA
  }else if (grepl('<|>|-', x)){
    gsub('.*>|.*-|.*<', '', x)
  }else{x}
}


# -- READ PHENOLOGY DATA ----
excelfile='2021 ITEX multi-year phenology.xlsx'
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
               "P1( MET) snow free","P1 (obs)a snow free",
               "P2a 1st green leaf","P2b 1st flower bud","P3 1st flower open","P4 1st petal drop",
               "P5 last petal drop","P6 1st seed shed","Q1a no. of buds", "Q1b no. of pods", "mean diametre", 
               #ledum
               "P1 (MET) snow free", "P1 (obs) snow free", 
               "P2 flower buds visible","P3 first flower open", 
               "P4 first flower shed",  "P5 last flower shed","P6 first fruit visible",  
               "Q1 no. of flowering stalks", "Q2 number of flowers/ stalk", "Q3 number of fruit / stalk", 
               "mean growth increment(mm)",
               #vaccinium
               "P2 flower bud visible", "Q1 no. of flowers", "Q2 no. of fruit", 
               #betula
               "P1(MET)", "P1(obs)",
               "P2_1st leaf",  "P3a_1st male catkin","P3b_1st female catkin",
               "P4a_1st stigma","P4b_pollen shed",   "P5a_Ist leaf turn",
               "P5b_last leaf turn","P6_1st leaf shed",  "P7_all shed",  
               "Q1_no. male catkins", "Q2_no. female", "total_catkin", "Mean leaf  length (mm)", 
               "Mean growth increment (mm)",
               #salix 
               "P1(MET)_snow",  "P1(obs)_snow free",
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
               "Mean_diametre",                     
               #carex
               "P3_1st stigma", "P4_1st anther", "P5_yellow leaf",   
               "P6_seed shed","Sum green_leaf length")

#names=lapply(new_data, function(x) names(x))%>%unlist()%>%unique()
#grep('^P', names, value=TRUE)
#lapply(new_data, function(x) names(x))

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
#filter out non-phen 
phen%<>%filter(!is.na(`ID no.`))%>%
  gather(key='phen_stage', value='doy', -c('species','treatment', 'Location','Year', 
                                           'ID no.', 'Sex', 'Notes',"Q1a no. of buds",	"Q1b no. of pods",	"mean diametre", 
                                           "mean no. flowers/ stalk", "mean no. fruit/ stalk", "mean growth increment(mm)", 
                                           "Q1 no. of flowers",	"Q2 no. of fruit",	"mean growth increment(mm)" ,
                                           "Q1_no. male catkins", "Q2_no. female", "total_catkin", "Mean leaf  length (mm)", 
                                           "Mean growth increment (mm)",
                                           "Q1_total no. catkins", "Q2_longest leaf (mm)", "mean_growth increment",
                                           "mean diameter (mm)",  
                                           "mean shaft_length", "Mean_shaft length", "Q3_mean", "Q4", "mean_leaf length", "Mean_diametre",
                                           "Q3_mean", "Sum green_leaf length"))

phen_trait<-filter(phen, !is.na(`ID no.`))%>%
  gather(key='trait', value='value', -c('species','treatment', 'Location','Year', 
                                           'ID no.', 'Sex', 'Notes','phen_stage', 'doy'))

#remove phenophases for which there is NEVER data
# these not relevant in keeping NAs
allmissing=phen%>%group_by(species, phen_stage)%>%
  summarize(has_data=sum(!is.na(doy)))%>%
  mutate(has_any_data=ifelse(has_data>0, TRUE, FALSE))%>%
  filter(has_any_data>0)

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



for (k in 1:nrow(phen)){
  phen$prior_visit[k]=prior_visit(phen$doy[k])
  phen$phen_visit[k]=phen_visit(phen$doy[k])
}
#put these in comments, then set these to NA
phen$Notes[phen$phen_visit%in%c('0', 's', "206\\s", "x",
                                "Female", "grazed", "eaten", "209s")&
             !is.na(phen$phen_visit)]=
  paste(phen$Notes[phen$phen_visit%in%c('0', 's', "206\\s", "x",
                                        "Female", "grazed", "eaten", "209s")&
                     !is.na(phen$phen_visit)],phen$phen_visit[phen$phen_visit%in%c('0', 's', "206\\s", "x",
                                                                                   "Female", "grazed", "eaten", "209s")&
                                                                !is.na(phen$phen_visit)], sep=';')

phen$phen_visit[phen$phen_visit%in%c('0', 's', "206\\s", "209s", "x",
                                     "Female", "grazed", "eaten")&
                  !is.na(phen$phen_visit)]=NA


#remove question makrs, retain the 191
phen$phen_visit[phen$phen_visit=="or=191"&
                  !is.na(phen$phen_visit)]='191'

phen$phen_visit[grepl('\\?', phen$phen_visit)&
                  !is.na(phen$phen_visit)]=NA

#convert all to numeric
phen%<>%mutate(phen_visit=as.numeric(phen_visit),
               prior_visit=as.numeric(prior_visit))

#keep one original copy of the prior_visit and doy not imputed
phen%<>%mutate(prior_visit_orig=prior_visit)%>%
  rename(doy_orig=doy)%>%
  rename(doy=phen_visit)%>%
  rename(year=Year, plant_id="ID no.")
