#1995, 2000,2007, 2008, 2010----
#Spp_abundance_XYs.csvis direct copy from PlantAbundance tab in 
#'Point_Frame Data_AllYears_AllSites_(1995-2010).xls'
library(dplyr)
library(tidyr)
plot_cover<-read.csv("data/Alex_raw_data/Spp_abundance_XYs.csv")
plot_cover<-separate(plot_cover, PLOT, c('x', 'y','PLOT', 'plot2'), remove = T)%>%
  select(-x, -y, -TISSUE)%>%
  mutate(PLOT=if_else(!is.na(plot2), plot2, PLOT))%>%select(-plot2)


#count number of hits by status type, spp and total for each plot x year
#PLOT HITS= total number of hits across all spp,status categories within a subsite x treatment x plot x year
#STATUS HITS= = total number of hits within each status category within a subsite x treatment x plot x year
#SPP HITS= = total number of hits within each spp x status category within a subsite x treatment x plot x year

plot_abund<-group_by(plot_cover, SUBSITE, PLOT, TRTMT,YEAR, STATUS)%>%mutate(STATUS_HITS=n())%>%ungroup(.)%>%
  group_by(SUBSITE, PLOT, TRTMT,YEAR, STATUS, SPP)%>%mutate(SPP_HITS=n())%>%ungroup(.)%>%
  group_by(SUBSITE, PLOT, TRTMT,YEAR)%>%mutate(PLOT_HITS=n())

##REL ABUND= number of spp hits / number of total hits within a subsite x treatment x plot x year
##check that they all add up to 1
plot_abund<-plot_abund%>%group_by(SUBSITE, PLOT, TRTMT,YEAR)%>%
  mutate(REL_ABUND=SPP_HITS/PLOT_HITS)%>%
  dplyr::select(-X, -Y, -Abundance, -Hit.Order, -Height..cm., -Cover.Understory)%>%distinct(.)%>%
  mutate(tot=sum(REL_ABUND))
  
unique(plot_abund$tot)

#subset for main sites (remove 'new' sites from Anne)
#remove d or g designations in dolomite 
unique(sort(plot_abund$PLOT))
plot_abund<-filter(plot_abund, !grepl('new', PLOT))%>%
  mutate(PLOT=stringr::str_extract(PLOT, "[0-9]+"))
unique(sort(plot_abund$PLOT))

#subset for main spp and LIVE status
xx<-subset(plot_abund, SPP=="CASTET"|SPP=="DRYINT"|SPP=="OXYDIG"|SPP=="SALARC"|SPP=="SAXOPP"|
  SPP=="PAPRAD"|SPP=="ERIANG"|SPP=="ARCLAT"|SPP=="LUZARC" & STATUS=="LIVE")

#plot data 
library(ggplot2)
  ggplot(xx, aes(y=REL_ABUND, x=as.factor(YEAR), fill=TRTMT ))+
   geom_boxplot() +facet_wrap(~SPP, scales="free")+theme_bw()+
   theme(axis.text.x = element_text(angle = 90))+theme(legend.position = 'none')
  
    ggplot(subset(xx), aes(y=REL_ABUND, x=YEAR, color=TRTMT))+
    geom_point() + geom_smooth()+facet_wrap(~SPP, scales="free")+theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+theme(legend.position = 'none')
  
  