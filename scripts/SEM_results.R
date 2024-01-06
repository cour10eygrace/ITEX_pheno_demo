
#RESULTS TABLES----
rm(list=ls()) 
#load SEM outputs
load("data/brms_SEM_output/flownumber_quad.Rdata")
load("data/brms_SEM_output/flownumberOTC_quad.Rdata")
load("data/brms_SEM_output/fruitnumber_quad.Rdata")
load("data/brms_SEM_output/fruitnumberOTC_quad.Rdata")

#create Table 2- mod results 
Table2a<-as.data.frame(summary(flowmodq)$fixed)%>%mutate(Model="Flower number")
ci2a<-bayestestR::ci(flowmodq, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter"))%>%select(-x, -Effects, -Component)%>%mutate(Model="Flower number")

Table2c<-as.data.frame(summary(fruitmodq)$fixed)%>%mutate(Model="Fruit number")
ci2c<-bayestestR::ci(fruitmodq, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter", "Param2"))%>%unite(Parameter, Parameter, Param2)%>%
  separate(Parameter, into= "Parameter", sep="_NA")%>%select(-x, -Effects, -Component)%>%mutate(Model="Fruit number")

Table2d<-as.data.frame(summary(flowmodOTCq)$fixed)%>%mutate(Model="Flower number")
ci2d<-bayestestR::ci(flowmodOTCq, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter"))%>%select(-x, -Effects, -Component)%>%mutate(Model="Flower number")

Table2e<-as.data.frame(summary(fruitmodOTCq)$fixed)%>%mutate(Model="Fruit number")
ci2e<-bayestestR::ci(fruitmodOTCq, method="ETI", ci=c(0.85,0.9,0.95))%>%
  pivot_wider(names_from = CI, values_from = c(CI_low, CI_high))%>%
  separate(Parameter, into= c("x","Response","Parameter"))%>%select(-x, -Effects, -Component)%>%mutate(Model="Fruit number")

#Daring
#only 95% CIs for main text 
Table2<-rbind(Table2a, Table2c)%>%mutate(Site="Daring Lake")%>%mutate(Response=rownames(.))%>%
  separate(Response, into= c("Response","Parameter", "Param2"))%>%unite(Parameter, Parameter, Param2)%>%
  separate(Parameter, into= "Parameter", sep="_NA")%>%
  select(Site, Model, Response, Parameter, Estimate, Est.Error, "l-95% CI" , "u-95% CI",Rhat , Bulk_ESS)%>%
  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))

#Full mod results for supplement with 3 levels of CIs
#ci2<-rbind(ci2a, ci2c)
#Table2supp<-rbind(Table2a, Table2b, Table2c)%>%mutate(Site="Daring Lake")%>%mutate(Response=rownames(.))%>%
 # separate(Response, into= c("Response","Parameter", "Param2"))%>%unite(Parameter, Parameter, Param2)%>%
#  separate(Parameter, into= "Parameter", sep="_NA")%>%
#  select(Site, Model, Response, Parameter, Estimate, Est.Error, Rhat , Bulk_ESS)%>%
#  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))%>%
#  left_join(., ci2)


#Alex 
#only 95% CIs for main text 
Table2x<-rbind(Table2d, Table2e)%>%mutate(Site="Alexandra Fiord")%>%mutate(Response=rownames(.))%>%
  separate(Response, into= c("Response","Parameter"))%>%
  select(Site, Model, Response, Parameter, Estimate, Est.Error, "l-95% CI" , "u-95% CI",Rhat , Bulk_ESS)%>%
  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))

#Full mod results for supplement with 3 levels of CIs
#ci2x<-rbind(ci2d, ci2e)
#Table2xsupp<-rbind(Table2d, Table2e)%>%mutate(Site="Alexandra Fiord")%>%mutate(Response=rownames(.))%>%
#  separate(Response, into= c("Response","Parameter"))%>%
#  select(Site, Model, Response, Parameter, Estimate, Est.Error, Rhat , Bulk_ESS)%>%
#  mutate(Parameter=str_remove_all(string = Parameter, pattern =  "[:digit:]"))%>%
#  left_join(., ci2x)


#combine
Table2_all<-rbind(Table2, Table2x)
#Table2_all_supp<-rbind(Table2supp, Table2xsupp)

write.csv(Table2_all, "MS_docs/Table2.csv")
#write.csv(Table2_all_supp, "MS_docs/TableS2.csv")


#create Table 3- group level hyperparameters
Table3a<-as.data.frame(summary(flowmodq)$random)%>%mutate(Model="Flower number")
Table3c<-as.data.frame(summary(fruitmodq)$random)%>%mutate(Model="Fruit number")
Table3d<-as.data.frame(summary(flowmodOTCq)$random)%>%mutate(Model="Flower number")
Table3e<-as.data.frame(summary(fruitmodOTCq)$random)%>%mutate(Model="Fruit number")

Table3<-rbind(Table3a, Table3c)%>%mutate(Site="Daring Lake")%>%mutate(Response=rownames(.))%>%
  select(!contains(c("Bulk","95", "Tail", "Rhat")))%>%select(Site, Model, Response, species.Estimate, species.plantid.Estimate, 
                                                             year.Estimate, species.Est.Error,  species.plantid.Est.Error, year.Est.Error)%>%
  rename(species_plantid.Est.Error= species.plantid.Est.Error, species_plantid.Estimate= species.plantid.Estimate)%>%
  pivot_longer(cols = species.Estimate:year.Estimate, names_to = "Group", values_to = "SD", names_repair = "minimal")%>%
  pivot_longer(cols = species.Est.Error:year.Est.Error, names_to = "Group2", values_to = "Error", names_repair = "minimal")%>%
  separate(Group, into = "Group", sep = ".Estimate", fill="left")%>%separate(Group2, into = "Group2", sep = ".Est.Error", fill="left")%>%
  mutate(keep=if_else(Group==Group2, 1, 0))%>%filter(keep>0)%>%
  separate(Response, into= c("Response1","Response"), fill="left")%>%select(-Response1, -keep, -Group2)


Table3x<-rbind(Table3d, Table3e)%>%mutate(Site= "Alexandra Fiord")%>%mutate(Response=rownames(.))%>%
  select(!contains(c("Bulk","95", "Tail", "Rhat")))%>%select(Site, Model, Response, species.Estimate, site.plot.Estimate, 
                                                             year.Estimate, species.Est.Error,  site.plot.Est.Error, year.Est.Error)%>%
  rename(site_plot.Est.Error= site.plot.Est.Error, site_plot.Estimate= site.plot.Estimate)%>%
  pivot_longer(cols = species.Estimate:year.Estimate, names_to = "Group", values_to = "SD", names_repair = "minimal")%>%
  pivot_longer(cols = species.Est.Error:year.Est.Error, names_to = "Group2", values_to = "Error", names_repair = "minimal")%>%
  separate(Group, into = "Group", sep = ".Estimate", fill="left")%>%separate(Group2, into = "Group2", sep = ".Est.Error", fill="left")%>%
  mutate(keep=if_else(Group==Group2, 1, 0))%>%filter(keep>0)%>%
  separate(Response, into= c("Response1","Response"), fill="left")%>%select(-Response1,  -keep, -Group2)

Table3all<-rbind(Table3, Table3x)

write.csv(Table3all, "MS_docs/Table3.csv")

#create tables for random intercepts 
#Species 
#flowering
summary(flowmodq)
ranef<-ranef(flowmodq)
speciestab<-as.data.frame(ranef$species)
speciestab$Site<-"Daring Lake"

ranef2<-ranef(flowmodOTCq)
speciestab2<-as.data.frame(ranef2$species)
speciestab2$Site<-"Alexandra Fiord"

speciestabflow<-rbind(speciestab, speciestab2)
speciestabflow$Species<-row.names(speciestabflow)
speciestabflow<-relocate(speciestabflow, c(Site, Species), .before = Estimate.doy_Intercept)
speciestabflow$model<-"Flower number"

#fruiting 
ranef<-ranef(fruitmodq)
speciestab<-as.data.frame(ranef$species)
speciestab$Site<-"Daring Lake"

ranef2<-ranef(fruitmodOTCq)
speciestab2<-as.data.frame(ranef2$species)
speciestab2$Site<-"Alexandra Fiord"

speciestabfruit<-rbind(speciestab, speciestab2)
speciestabfruit$Species<-row.names(speciestabfruit)
speciestabfruit<-relocate(speciestabfruit, c(Site, Species), .before = Estimate.doy_Intercept)
speciestabfruit$model<-"Fruit number"

speciestab<-rbind(speciestabflow, speciestabfruit)
write.csv(speciestab, "MS_docs/TableS2.csv") 


#year
#flowering
summary(flowmodq)
ranef<-ranef(flowmodq)
yeartab<-as.data.frame(ranef$year)
yeartab$Site<-"Daring Lake"

ranef2<-ranef(flowmodOTCq)
yeartab2<-as.data.frame(ranef2$year)
yeartab2$Site<-"Alexandra Fiord"

yeartabflow<-rbind(yeartab, yeartab2)
yeartabflow$year<-row.names(yeartabflow)
yeartabflow<-relocate(yeartabflow, c(Site, year), .before = Estimate.doy_Intercept)
yeartabflow$model<-"Flower number"

#fruiting 
ranef<-ranef(fruitmodq)
yeartab<-as.data.frame(ranef$year)
yeartab$Site<-"Daring Lake"

ranef2<-ranef(fruitmodOTCq)
yeartab2<-as.data.frame(ranef2$year)
yeartab2$Site<-"Alexandra Fiord"

yeartabfruit<-rbind(yeartab, yeartab2)
yeartabfruit$year<-row.names(yeartabfruit)
yeartabfruit<-relocate(yeartabfruit, c(Site, year), .before = Estimate.doy_Intercept)
yeartabfruit$model<-"Fruit number"

yeartab<-rbind(yeartabflow, yeartabfruit)

write.csv(yeartab, "MS_docs/TableS3.csv")

#look at the intercepts 
ggplot(yeartab,
       aes(x=as.factor(year), y=Estimate.doy_Intercept))+
  geom_point( alpha=0.5)+ facet_wrap(~Site, scales = "free")+
  geom_smooth(method='lm') +  theme_bw()


