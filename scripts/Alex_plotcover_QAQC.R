#2000,2007, 2008, 2010----
#Spp_abundance_XYs.csvis direct copy from PlantAbundance tab in 
#'Point_Frame Data_AllYears_AllSites_(1995-2010).xls'
plot_cover<-read.csv("data/Spp_abundance_XYs.csv")
plot_cover<-separate(plot_cover, PLOT, c('x', 'y','PLOT', 'plot2'), remove = T)%>%
  select(-x, -y)%>%
  mutate(PLOT=if_else(!is.na(plot2), plot2, PLOT))%>%select(-plot2)

#PLOT HITS= total number of hits across all spp,status categories within a subsite x treatment x plot x year
#PLOT ABUND= number of hits of each spp x status category in the same subsite x treatment x plot x year
#REL ABUND= PLOT ABUND/PLOT HITS 
#only using 'LIVE' REL ABUNDs

plot_coverx<-
  group_by(plot_cover, SUBSITE, PLOT, TRTMT,YEAR, X,Y)%>% distinct(.)%>%
  slice_sample(n=1)%>% #if duplicate hits within an XY, randomly remove one, this is only for 34 observations 
  ungroup(.)%>%group_by(SUBSITE, PLOT, TRTMT,YEAR)%>%
  mutate(PLOT_HITS=n())%>%ungroup(.)%>%
  group_by(SUBSITE, PLOT, TRTMT,YEAR, SPP, STATUS)%>%
  mutate(PLOT_ABUND=n(), REL_ABUND=(PLOT_ABUND/PLOT_HITS)*100)

plot_abund<-select(plot_coverx, SUBSITE, PLOT, TRTMT,YEAR, SPP, 
                   PLOT_ABUND, PLOT_HITS, REL_ABUND)%>%distinct(.)%>%
  filter(PLOT_HITS>90) #throw out plot-years with less than 90 hits total (only 1)

#check that they all add up to 100% 
check<-plot_abund%>%
  group_by(SUBSITE, PLOT, TRTMT,YEAR)%>%summarise(tot=sum(REL_ABUND))
unique(check$tot)

#subset for main spp and LIVE status
plot_abund<-subset(plot_abund, SPP=="CASTET"|SPP=="DRYINT"|SPP=="OXYDIG"|SPP=="SALARC"|SPP=="SAXOPP"|
  SPP=="PAPRAD"|SPP=="ERIANG"|SPP=="ARCLAT"|SPP=="LUZARC" & STATUS=="LIVE")
#subset for main sites (remove 'new' sites from Anne)
#remove d or g designations in dolomite 
unique(sort(plot_abund$PLOT))
plot_abund<-filter(plot_abund, !grepl('new', PLOT))%>%
mutate(PLOT=stringr::str_extract(PLOT, "[0-9]+"))
unique(sort(plot_abund$PLOT))

  ggplot(subset(plot_abund), aes(y=REL_ABUND, x=as.factor(YEAR), fill=TRTMT ))+
   geom_boxplot() +facet_wrap(~SPP, scales="free")+theme_bw()+
   theme(axis.text.x = element_text(angle = 90))+theme(legend.position = 'none')
  
    ggplot(subset(plot_abund), aes(y=REL_ABUND, x=YEAR, color=TRTMT))+
    geom_point() + geom_smooth()+facet_wrap(~SPP, scales="free")+theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+theme(legend.position = 'none')
  
  

  
  
  
  
  
  
  #look at changes over time 
plot_abund<-group_by(plot_abund, SUBSITE, PLOT, TRTMT, SPP, STATUS)%>%arrange(YEAR)%>%
  mutate(DELT_ABUND=REL_ABUND-lag(REL_ABUND))

ggplot(subset(plot_abund),aes(y=log(DELT_ABUND+100), x=as.factor(YEAR) ,fill=TRTMT))+
  geom_boxplot()+facet_wrap(~SPP)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
  
