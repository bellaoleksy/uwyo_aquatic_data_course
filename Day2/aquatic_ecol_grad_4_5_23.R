install.packages('neonUtilities')
library(neonUtilities)
library(tidyverse)
###dpID=data product ID, gray oval below link###

###end date defaults to all dates with available data##

invert<-
  loadByProduct(dpID="DP1.20120.001",
                site=c("MART"), 
                startdate ="2016-01",
                check.size = F)

#pull out the files you want
invert.data<- 
  invert[[4]] %>%            
  as_tibble()

invert.field<-
  invert[[2]]%>%
  as_tibble()

#combine everything into one tibble
invert.all<-
  inner_join(invert.data,invert.field,by="sampleID")

#Calculate the invertebrate density from the count data
NEONFILE.CLEAN<-invert.all %>%
  select(sampleID,scientificName,estimatedTotalCount,benthicArea)%>%
  ##mutate(Sample=substr(sampleID,1,11))%>%##
  group_by(sampleID,scientificName)%>%
  summarize(inv.density=sum(estimatedTotalCount)/sum(benthicArea))
###Sum by to get total sample density##
total_den<-NEONFILE.CLEAN %>% 
  group_by(sampleID)%>%
  summarize(total_density=sum(inv.density))
###write it to csv if that is your thing##
write.csv(total_den,'MART_invdensity.csv')

###Invert Richness###
#count num. of observations by group, group being sample ID##
head(NEONFILE.CLEAN)
total_richness<-NEONFILE.CLEAN %>%
  count(sampleID)
total_richness
write.csv(total_richness,'MART_invRichness.csv')

###Perip. stoichiomentry####

peri.chem<-
  loadByProduct(dpID="DP1.20163.001",
                site=c("BLDE"), startdate ="2016-01",check.size = F)

peri.chem<-
  loadByProduct(dpID="DP1.20163.001",
                site=c("BLDE"), startdate ="2016-01", enddate="2021-12",
                check.size = F)

peri.chem.data<-
  peri.chem$alg_algaeExternalLabDataPerSample


peri.chem.data<- peri.chem[[1]] %>%            
  as_tibble()

BLDE.sum<-
  peri.chem.data%>%
  mutate(habitat=substr(sampleID,15,17))%>%
  filter(analyte %in% c("carbon","nitrogen","phosphorus"),habitat %in% "EPI")%>%
  group_by(collectDate,habitat,analyte,siteID)

write.csv(BLDE.sum,'blde_peri2.csv')

###Periphyton chla and AFDM###

###Extracting chla data### use "DP1.20163.001"###
peri.chla<-
  loadByProduct(dpID="DP1.20163.001",
                site=c("ARIK"), startdate ="2016-01",check.size = F)

peri.chla.data<-
  peri.chla$alg_algaeExternalLabDataPerSample


peri.chla.data<- peri.chla[[1]] %>%            
  as_tibble()

write.csv(peri.chla.data,'arik_chla9.csv')
####For chla###test run 2##
chla<-
  loadByProduct(dpID="DP1.20163.001",
                site=c("BLDE"), startdate ="2016-01",check.size = F)

chla.data<-
  chla$alg_algaeExternalLabDataPerSample


chla.data<- chla[[1]] %>%            
  as_tibble()

write.csv(chla.data,'chla13.csv')


###periphyton biomass afdm### this works to grab periphyton ashfree dry mass###

peri.afdm<-
  loadByProduct(dpID="DP1.20166.001",
                site=c("ARIK"), startdate ="2016-01",check.size = F)

peri.afdm.data<-
  peri.afdm$alg_biomass


peri.afdm.data<- peri.afdm[[1]] %>%            
  as_tibble()

write.csv(peri.afdm.data,'ariktest6.csv')

###Lets try to break the above code to make it work for CHLA###

peri.chla<-
  loadByProduct(dpID="DP1.20166.001",
                site=c("ARIK"), startdate ="2016-01",check.size = F)

peri.chla.data<-
  peri.chla$alg_algaeExternalLabDataPerSample


peri.chla.data<- peri.chla[[1]] %>%            
  as_tibble()

write.csv(peri.chla.data,'pleasework.csv')

###

###fish##################
fish<-
  loadByProduct(dpID="DP1.20107.001",
                site=c("MCDI"), 
                startdate ="2016-01",
                check.size = F)
####
#pull out the files you want
###table2 is fish bulk count###
fish.data<- 
  fish[[2]] %>%            
  as_tibble()
head(fish.data)

fish_field<- 
  fish[[3]] %>%            
  as_tibble()
head(fish.field)

fish_pass<-
  fish[[5]] %>%            
  as_tibble()

fish_fish<-
  fish[[4]] %>%            
  as_tibble()


fish_bulk<-
  fish[[]] %>%            
  as_tibble()
###

fish_bulk_pass<-
  inner_join(fish.data,fish_pass,by="eventID")
###joining fish bulk pass to field.field through reach ID###
bulk_field_fish<-
  inner_join(fish_bulk_pass,fish_field,by="reachID")


f_bulkcount<-fish_bulk_pass %>%
  group_by(reachID) %>%
  summarize(bulk_density=sum(bulkFishCount))

#summarize(bulkcount=sum(bulkFishCount))


fish_field_pass<-
  inner_join(fish_field,fish_pass,by="reachID")

fish_fish_pass<-
  inner_join(fish_fish,fish_pass,by="eventID")

FFP<-
  inner_join(fish_field_pass,fish_fish_pass,by="eventID")

write.csv(fish_bulk_pass,'MCDI_fish.csv')###provides bulk counts of fish species per pass##




fish_bulkcount<-fish_bulk_pass %>%
  group_by(reachID) %>%
  
  summarize(bulkcount=sum(bulkFishCount))


fish_bulk<-fish.data %>%
  select(eventID,bulkFishCount) %>%
  group_by(eventID) %>%
  summarize(bulkcount=sum(bulkFishCount))
write.csv(Pred_ept_R2,'predator_density.csv')


###4 is variables csv####
invert.field<-
  invert[[2]]%>%
  as_tibble()
perip_richness<-microbe.data %>%
  group_by(dnaSampleID) %>%
  count(scientificName)
###Data vis. example###
library(car)
library(MASS)
library(ggplot2)
library(lme4)
library(lmerTest)
library(scales)
library(lsmeans)
library(scales)

NEON_aquatic_ecol_grad_db <- read_csv("Data/NEON/NEON_aquatic_ecol_grad_db.csv")

P<-ggplot(data = NEON_aquatic_ecol_grad_db, aes(x = log(chlorophylla_g_m2), y= log(INV_DEN)   , color =Site)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size=1))+
  #geom_point(aes(color=Site), position = position_jitterdodge(), shape = 19,size = 1, alpha = .3)+
  geom_point(size=1.52, shape = 21,aes(fill=Site),colour="black")+
  #geom_point(data = den.tab, size = 4)+
  geom_smooth(method = 'lm',formula = y ~ x,size = 1,alpha = 0.3,se=TRUE)+
  P+facet_wrap(~Domain)###play around with facet wrap to explore data patterns
