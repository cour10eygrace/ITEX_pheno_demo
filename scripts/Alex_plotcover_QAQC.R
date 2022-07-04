#1995, 2000,2007, 2008, 2010----
#Spp_abundance_XYs.csvis direct copy from PlantAbundance tab in 
#'Point_Frame Data_AllYears_AllSites_(1995-2010).xls'
library(dplyr)
library(tidyr)
#plot_cover<-read.csv("data/Alex_raw_data/Spp_abundance_XYs.csv")
plot_cover<-read.csv('data/Alex_raw_data/compiled_point_frame_1995-2019.csv') #Ross full point frame 4/2022
#rename otc treatments 
plot_cover<-mutate(plot_cover, otc_treatment=if_else(otc_treatment=="T", "OTC", otc_treatment))
#count number of hits by status type, spp and total for each plot x year
#PLOT HITS= total number of hits across all spp,status categories within a subsite x treatment x plot x year
#STATUS HITS= = total number of hits within each status category within a subsite x treatment x plot x year
#SPP HITS= = total number of hits within each spp x status category within a subsite x treatment x plot x year
plot_abund<-filter(plot_cover, snow_treatment=="control"|is.na(snow_treatment)& #remove snow addition/removal 
  fert_treatment=="control"|is.na(fert_treatment))%>%#remove fert/water addition
  group_by( site, plot, otc_treatment,year, status,  species)%>%mutate(spp_hits=n())%>%ungroup(.)%>%
  group_by( site, plot, otc_treatment,year, status)%>%mutate(status_hits=n())%>%ungroup(.)%>%
  group_by(site, plot, otc_treatment,year)%>%mutate(plot_hits=n())

##REL ABUND= number of spp hits / number of total hits within a subsite x treatment x plot x year
##check that they all add up to 1
plot_abund<-plot_abund%>%group_by(site, plot, otc_treatment,year,)%>%
  mutate(rel_abund=spp_hits/plot_hits)%>%
  dplyr::select(-file, -canopy_height_mm, -height_a, -height_b, -height_c,-tissue, -leaf_state, -flower_state,
     -x,-y,-hit_order,   -abundance, -height_d,-additional_species, -observer,  -co2_plot, -cover_understory)%>%distinct(.)%>%
  mutate(tot=sum(rel_abund))
unique(plot_abund$tot)

#subset for main sites 
#remove d or g designations in dolomite 
unique(sort(plot_abund$plot))
plot_abund<-mutate(plot_abund, plot=stringr::str_extract(plot, "[0-9]+"))
unique(sort(plot_abund$plot))

#subset for main spp and LIVE status
xx<-subset(plot_abund, species=="CASTET"|species=="DRYINT"|species=="OXYDIG"|species=="SALARC"|species=="SAXOPP"|
  species=="PAPRAD"|species=="ERIANG"|species=="ARCLAT"|species=="LUZARC" & status=="live")

#plot data 
library(ggplot2)
#just otc/ctl 
  ggplot(filter(xx,otc_treatment!="cover"), aes(y=rel_abund, x=year, fill=otc_treatment ))+
  geom_smooth(method="gam" ,formula = y ~ s(x, bs = "cs", fx = F, k = 3))+
  #  geom_smooth(method="lm") +
  # geom_point()+ 
    scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor)+
   facet_wrap(~species, scales="free")+theme_classic()+
   theme(axis.text.x = element_text(angle = 90))

  #'cover' trt only done at cassiope site 
order<- c("control", "OTC", "cover")
xx$otc_treatment <- factor(xx$otc_treatment, levels=order)
  ggplot(filter(xx,site=="Cassiope"), aes(y=rel_abund, x=year, fill=otc_treatment ))+
    geom_smooth(method="gam" ,formula = y ~ s(x, bs = "cs", fx = F, k = 3))+
    scale_fill_manual(values=specColor)+ scale_color_manual(values=specColor))+
      #  geom_smooth(method="lm") +
    #geom_point()+
    facet_wrap(~species, scales="free")+theme_classic()+
    theme(axis.text.x = element_text(angle = 90))
  

  