###### summary statistics 

#load packages
library(dplyr)
library(openxlsx)
library(stringr)
library(purrr)  
library(readxl)
library(sjPlot)
library(vegan)
library(tidyr)
library(tidyverse)
library(reshape2)
library(goeveg) 
library(lubridate)
library(gplots)
library(ggplot2)
library(vegan)
library(cowplot)
library(gridExtra)
library(gridExtra)


#first load in data from GBIF_organization_Life_MICA_biodiversity_surveys_2021
gbif2021 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/LifeMICA_GBIF_2021_complete.csv", header=T, stringsAsFactors = FALSE)
gbif2022 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/LifeMICA_GBIF_2022_complete.csv", header=T, stringsAsFactors = FALSE)
all <- rbind(gbif2021, gbif2022)

birdsbe2021 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Belgium/vogels1.csv", header=T, stringsAsFactors = FALSE)
birdsde2021 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/monitoring_birds_de2.csv", header=T, stringsAsFactors = FALSE)
birdsnl2021 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Netherlands/monitoring_birds_nl.csv", header=T, stringsAsFactors = FALSE)
birdsbe2022 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/Belgium/birds.csv", header=T, stringsAsFactors = FALSE)
birdsde2022 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/Germany/bird_surveys.csv", header=T, stringsAsFactors = FALSE)
birdsnl2022 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/Netherlands/Life MICA Biodiversity Surveys Data Collection Sheet - Birds.csv", header=T, stringsAsFactors = FALSE)

IUCN_status2022 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/List of endangered species_WvL_2022.csv", header=T, stringsAsFactors = FALSE, sep = ";")
iucn_de_birds <-read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Germany/02_Datentabelle_RL_Brutvoegel_2016_Deutschland_20200930-1405.xlsx")
iucn_de_birds_legend <-read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Germany/03_Legende_RL_Brutvoegel_2016_20200930-1405.xlsx")
iucn_de_lib <-read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Germany/02_Datentabelle_RL_Libellen__Deutschland_20220307-1123.xlsx")
iucn_de_lib_legend <-read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Germany/03_Legende_RL_Libellen__20220307-1123.xlsx")
iucn_de_plants <-read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Germany/02_Datentabelle_RL_Farn-_und_Bluetenpflanzen_2018_Deutschland_20210317-1607.xlsx")
iucn_de_plants_legend <-read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Germany/03_Legende_RL_Farn-_und_Bluetenpflanzen_2018_20210317-1607.xlsx")

iucn_nl_birds <- read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Netherlands/LNV soorten 2023-01-31T17-34-05.xlsx")
iucn_nl_lib <- read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Netherlands/LNV soorten 2023-01-31T17-33-13.xlsx")
iucn_nl_plants <- read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Netherlands/LNV soorten 2023-01-31T17-33-58.xlsx")

iucn_be_status <- read.delim("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Belgium/distribution.txt")
iucn_be_taxa <- read.delim("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Belgium/taxon.txt")

#check how many libellen species in areas 4 and 5 each year, looks good!
  all$eventDate <- as.Date(all$eventDate)
  gbif2022$eventDate <- as.Date(gbif2022$eventDate)
  
  length(unique(subset(all$scientificName, all$project_area==4 & all$Group=="Libellen" & year(all$eventDate)==2021))) #25
  length(unique(subset(all$scientificName, all$project_area==4 & all$Group=="Libellen" & year(all$eventDate)==2022))) #22
  
  length(unique(subset(all$scientificName, all$project_area==5 & all$Group=="Libellen" & year(all$eventDate)==2021))) #8
  length(unique(subset(all$scientificName, all$project_area==5 & all$Group=="Libellen" & year(all$eventDate)==2022))) #10

#remove any duplicates where 2021 data was included twice (in both 2021 and 2022 data sheets)
  nrow(all) #5363
  all <- all[!duplicated(all),]
    nrow(all)
    
#1. Manipulate data
    #####create category (birds, dragonflies, damselflies, plants) for each species group
      #merge with IUCN_status2022 (Wolf van Lier's data)
      #NOTE: ultimately, there are gaps in Wolf's data, so I went with the original datasets (see below), I have kept the code here in case
        #IUCN_status2022$scientificName <-IUCN_status2022$Species
        #all <- merge(all, IUCN_status2022,by="scientificName", all.x=T)
        
        #these still need IUCN status
        #need_status <- subset(all, !complete.cases(all$In.factsheet))
        #unique(need_status$scientificName) #77 species
        
        #remove those that only have genus level
        #all$Status.Germany[!all$taxonRank=="species"] <- NA
        #all$Status.Belgium[!all$taxonRank=="species"] <- NA
        #all$Status.Netherlands[!all$taxonRank=="species"] <- NA
        #all$In.factsheet[!all$taxonRank=="species"] <- NA
        #need_status <- unique(need_status$scientificName[need_status$taxonRank=="species"]) #70 species
        #write.csv(need_status, "/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/needs_IUCN_status_updated.xlsx")
      #####
        
      
    #since some are missing, let's instead use these:
      #https://www.rote-liste-zentrum.de/en/Red-Lists-1707.html
      #https://www.gbif.org/dataset/fc18b0b1-8777-4c8a-8cb8-f9f15870d6a9
      #https://minlnv.nederlandsesoorten.nl/content/rode-lijsten

      #germany
        #birds
        iucn_de_birds$scientificName <- word(iucn_de_birds$Name, 1,2, sep=" ") #take just first two words, don't need subspecies (we don't know it), warning beware of duplicates
        iucn_de_birds <- iucn_de_birds[,c("scientificName","RL.Kat.")]
        colnames(iucn_de_birds)[2] <-"Wert"
        
        iucn_de_birds_legend <- subset(iucn_de_birds_legend, iucn_de_birds_legend$Bezeichnung=="RL Kat.")
        iucn_de_birds_legend <- iucn_de_birds_legend[, c("Wert","Legende")]
        
        iucn_de_birds <-merge(iucn_de_birds, iucn_de_birds_legend, by="Wert", all.x=T)
        iucn_de_birds <- iucn_de_birds[,c(2,3)]
        colnames(iucn_de_birds)[2] <- "IUCN.Status.Germany"
        iucn_de_birds$Group <- "Birds"
        
        all <- merge(all, iucn_de_birds, by="scientificName", all.x=T)
        all$IUCN.Status.Germany
        
        #libellen
        iucn_de_lib$scientificName <- word(iucn_de_lib$Name, 1,2, sep=" ") #take just first two words, don't need subspecies (we don't know it), warning beware of duplicates
        iucn_de_lib <- iucn_de_lib[,c("scientificName","RL.Kat.")]
        colnames(iucn_de_lib)[2] <-"Wert"
        
        iucn_de_lib_legend <- subset(iucn_de_lib_legend, iucn_de_lib_legend$Bezeichnung=="RL Kat.")
        iucn_de_lib_legend <- iucn_de_lib_legend[, c("Wert","Legende")]
        
        iucn_de_lib <-merge(iucn_de_lib, iucn_de_lib_legend, by="Wert", all.x=T)
        iucn_de_lib <- iucn_de_lib[,c(2,3)]
        colnames(iucn_de_lib)[2] <- "IUCN.Status.Germany"
        iucn_de_lib$Group <- "Libellen"
        
        all <- merge(all, iucn_de_lib, by="scientificName", all.x=T) #note there are now multiple group columns that need to be combined
        
        #plants
        iucn_de_plants$scientificName <- word(iucn_de_plants$Name, 1,2, sep=" ") #take just first two words, don't need subspecies (we don't know it), warning beware of duplicates
        iucn_de_plants <- iucn_de_plants[,c("scientificName","RL.Kat.")]
        colnames(iucn_de_plants)[2] <-"Wert"
        
        iucn_de_plants_legend <-read.xlsx("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/IUCN lists/Germany/03_Legende_RL_Farn-_und_Bluetenpflanzen_2018_20210317-1607.xlsx")
        iucn_de_plants_legend <- subset(iucn_de_plants_legend, iucn_de_plants_legend$Bezeichnung=="RL Kat.")
        iucn_de_plants_legend <- iucn_de_plants_legend[, c("Wert","Legende")]
        
        iucn_de_plants <-merge(iucn_de_plants, iucn_de_plants_legend, by="Wert", all.x=T)
        iucn_de_plants <- iucn_de_plants[,c(2,3)]
        colnames(iucn_de_plants)[2] <- "IUCN.Status.Germany"
        iucn_de_plants$Group <- "Plants"
        
        all <- merge(all, iucn_de_plants, by="scientificName", all.x=T)

        #combine redundant columns, ignoring NAs
        all$IUCN.Status.Germany <- coalesce(all$IUCN.Status.Germany, all$IUCN.Status.Germany.x, all$IUCN.Status.Germany.y, "")
        all$Group <- coalesce(all$Group, all$Group.x, all$Group.y, "")
        all <- select(all, -c(X,IUCN.Status.Germany.x,Group.x,
                       IUCN.Status.Germany.y,Group.y))

      #Netherlands
        #birds
        iucn_nl_birds$scientificName <- word(iucn_nl_birds$Wetenschappelijke.naam, 1,2, sep=" ") #take just first two words, don't need subspecies (we don't know it), warning beware of duplicates
        iucn_nl_birds <- iucn_nl_birds[,c("scientificName","Publicatie")]
        colnames(iucn_nl_birds)[2] <-"IUCN.Status.Netherlands"
        iucn_nl_birds$Group <- "Birds"
        
        #libellen
        iucn_nl_lib$scientificName <- word(iucn_nl_lib$Wetenschappelijke.naam, 1,2, sep=" ") #take just first two words, don't need subspecies (we don't know it), warning beware of duplicates
        iucn_nl_lib <- iucn_nl_lib[,c("scientificName","Publicatie")]
        colnames(iucn_nl_lib)[2] <-"IUCN.Status.Netherlands"
        iucn_nl_lib$Group <- "Libellen"
        
        #libellen
        iucn_nl_plants$scientificName <- word(iucn_nl_plants$Wetenschappelijke.naam, 1,2, sep=" ") #take just first two words, don't need subspecies (we don't know it), warning beware of duplicates
        iucn_nl_plants <- iucn_nl_plants[,c("scientificName","Publicatie")]
        colnames(iucn_nl_plants)[2] <-"IUCN.Status.Netherlands"
        iucn_nl_plants$Group <- "Plants"
        
        #combine NL redlists
        iucn_nl_all <- rbind(iucn_nl_birds, iucn_nl_lib, iucn_nl_plants)
        
        #merge with larger dataset
        all <- merge(all, iucn_nl_all, by="scientificName", all.x=T)
        all$Group <- coalesce(all$Group.x, all$Group.y, "")
        all <- select(all, -c(Group.x, Group.y))
        
        #fix typos
        unique(subset(all$scientificName, all$Group==""))
        all$scientificName[all$scientificName=="Bruine glazenmaker"] <- "Aeshna grandis"
        all$scientificName[all$scientificName=="Bruine kiekendief"] <- "Circus aeruginosus"
        all$scientificName[all$scientificName=="Chilidonias niger"] <- "Chlidonias niger"
        all$scientificName[all$scientificName=="Eplilobium angustifolium"] <- "Epilobium angustifolium"
        all$scientificName[all$scientificName=="Lestes viridis"] <- "Chalcolestes viridis"
        all$scientificName[all$scientificName=="Lysmachia vulgaris"] <- "Lysimachia vulgaris"
        all$scientificName[all$scientificName=="Myosotis palustris"] <- "Myosotis scorpioides"
        all$scientificName[all$scientificName=="Pyrrhosomma nymphula"] <- "Pyrrhosoma nymphula"
        all$scientificName[all$scientificName=="Rubus fructicosus agg."] <- "Rubus fruticosus"
        all$scientificName[all$scientificName=="Rubus fruticosus"] <- "Rubus plicatus"
        all$scientificName[all$scientificName=="Schoeniclus schoeniclus"] <- "Emberiza schoeniclus"
        all$scientificName[all$scientificName=="Spatula clypeataI"] <- "Spatula clypeata"
        all$scientificName[all$scientificName=="Aeshna cyana"] <- "Aeshna cyanea"
        all$scientificName[all$scientificName=="Alopochen aegytiaca"] <- "Alopochen aegyptiaca"
        all$scientificName[all$scientificName=="Epilobium cf. roseum"] <- "Epilobium roseum"
        all$scientificName[all$scientificName=="Eryhtromma najas"] <- "Erythromma najas"
        all$scientificName[all$scientificName=="Galeopsis tetrahit agg."] <- "Galeopsis tetrahit"
        all$scientificName[all$scientificName=="Hercaleum sphondyleum"] <- "Heracleum sphondylium"
        all$scientificName[all$scientificName=="Rubus fruticosus agg."] <- "Rubus fruticosus"
        all$scientificName[all$scientificName=="Sempetrum vulgatum"] <- "Sympetrum vulgatum"
        all$scientificName[all$scientificName==" Aeshna cyana"] <- "Aeshna cyanea"
        
        
       
        #fix the IUCN.Status.Netherlands column so that it says status instead of Publication where it can be found
          #documents where this data is found:
            #"Staatscourant nr. 68427, 30 november 2017", https://zoek.officielebekendmakingen.nl/stcrt-2017-68427.html
          sp <- as.data.frame(unique(subset(all$scientificName, complete.cases(all$IUCN.Status.Netherlands))))
          colnames(sp)[1] <- "scientificName"
          subset_all <- unique(all[,c("scientificName","IUCN.Status.Netherlands")])
          subset_all <- subset_all[complete.cases(subset_all$IUCN.Status.Netherlands),]
          all$IUCN.Status.Netherlands[all$scientificName=="Acrocephalus arundinaceus"] <- "Endangered"
          all$IUCN.Status.Netherlands[all$scientificName=="Actitis hypoleucos"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Alauda arvensis"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Anas crecca"] <- "Vulnerable"
          all$IUCN.Status.Netherlands[all$scientificName=="Anas querquedula"] <- NA #missing from document
          all$IUCN.Status.Netherlands[all$scientificName=="Anthus pratensis"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Botaurus stellaris"] <- "Vulnerable"
          all$IUCN.Status.Netherlands[all$scientificName=="Cuculus canorus"] <- "Vulnerable"
          all$IUCN.Status.Netherlands[all$scientificName=="Falco tinnunculus"] <- "Vulnerable"
          all$IUCN.Status.Netherlands[all$scientificName=="Gallinago gallinago"] <- "Endangered"
          all$IUCN.Status.Netherlands[all$scientificName=="Grus grus"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Ixobrychus minutus"] <- "Critically endangered"
          all$IUCN.Status.Netherlands[all$scientificName=="Limosa limosa"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Locustella luscinioides"] <- "Vulnerable"
          all$IUCN.Status.Netherlands[all$scientificName=="Motacilla flava"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Numenius arquata"] <- "Vulnerable"
          all$IUCN.Status.Netherlands[all$scientificName=="Porzana porzana"] <- "Vulnerable"
          all$IUCN.Status.Netherlands[all$scientificName=="Sterna hirundo"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Tringa totanus"] <- "Sensitive"
          all$IUCN.Status.Netherlands[all$scientificName=="Remiz pendulinus"] <- "Sensitive"
          
          all$IUCN.Status.Netherlands[all$scientificName=="Brachytron pratense"] <- "Vulnerable" #https://minlnv.nederlandsesoorten.nl/content/glassnijder-brachytron-pratense
          all$IUCN.Status.Netherlands[all$scientificName=="Lestes virens"] <- "Vulnerable" #https://minlnv.nederlandsesoorten.nl/content/tengere-pantserjuffer-lestes-virens-ssp-vestalis
          all$IUCN.Status.Netherlands[all$scientificName=="Libellula fulva"] <- "Vulnerable" #https://minlnv.nederlandsesoorten.nl/content/bruine-korenbout-libellula-fulva
          all$IUCN.Status.Netherlands[all$scientificName=="Sympecma fusca"] <- "Threatened" #https://minlnv.nederlandsesoorten.nl/content/bruine-winterjuffer-sympecma-fusca

          all$IUCN.Status.Netherlands[all$IUCN.Status.Netherlands=="Sensitive"] <- "Near Threatened"

      #belgium
        iucn_be_status <- iucn_be_status[,c("id", "threatStatus")]
        iucn_be_all <- merge(iucn_be_taxa, iucn_be_status, by="id", all.x=T)
        iucn_be_all <- iucn_be_all[c("scientificName", "threatStatus","class")]
        iucn_be_all <- iucn_be_all[!duplicated(iucn_be_all$scientificName),] #remove duplicates in original data
        colnames(iucn_be_all)[3] <- "Group"
        colnames(iucn_be_all)[2] <- "IUCN.Status.Belgium"
        
        
        all <- merge(all, iucn_be_all, by="scientificName", all.x=T)
        all$Group <- all$Group.x
        all <- select(all, -c(Group.y))
        
  #fix names of the different status categories 
        unique(all$IUCN.Status.Belgium)
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium=="CR"] <- "Critically Endangered"
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium=="LC"] <- "Least Concern"
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium=="EN"] <- "Endangered"
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium=="NE"] <- "Not Evaluated"
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium=="VU"] <- "Vulnerable"
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium=="NT"] <- "Near Threatened"
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium=="DD"] <- "Data Deficient"
        all$IUCN.Status.Belgium[all$IUCN.Status.Belgium==""] <- NA
        
        unique(all$IUCN.Status.Germany) #https://www.rote-liste-zentrum.de/en/Categories-1711.html
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Ungefährdet"] <- "Least Concern"
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Stark gefährdet"] <- "Endangered" 
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Vorwarnliste"] <- "Near Threatened" 
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Gefährdet"] <- "Vulnerable"
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Nicht bewertet"] <- "Not Evaluated" 
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Daten unzureichend"] <- "Data Deficient"
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Vom Aussterben bedroht"] <- "Critically Endangered"
        all$IUCN.Status.Germany[all$IUCN.Status.Germany=="Extrem selten"] <- "Least Concern" #"Extremely Rare"?, only species is Lycopus europaeus
        all$IUCN.Status.Germany[all$IUCN.Status.Germany==""] <- NA
        
  #add column for project area where it is missing
    subset(all, !complete.cases(all$project_area))    
    all$project_area[all$decimalLongitude>8.8] <- 2 #some missing for area 2
    
  #fill in gaps where group is missing
    groups_missing <- subset(all$scientificName, all$Group=="")
    all$Group[all$scientificName=="Acrocephalus"] <- "Birds"
    all$Group[all$scientificName=="Aeshna grandis"] <- "Libellen"
    all$Group[all$scientificName=="Circus aeruginosus"] <- "Birds"
    all$Group[all$scientificName=="Cettia cetti"] <- "Birds"
    all$Group[all$scientificName=="Chlidonias niger"] <- "Birds"
    all$Group[all$scientificName=="Coenagrionidae"] <- "Libellen"
    all$Group[all$scientificName=="Epilobium angustifolium"] <- "Plants"
    all$Group[all$scientificName=="Larinae"] <- "Birds"
    all$Group[all$scientificName=="Chalcolestes viridis"] <- "Libellen"
    all$Group[all$scientificName=="Lycopus"] <- "Plants"
    all$Group[all$scientificName=="Lysmachia vulgaris"] <- "Libellen"
    all$Group[all$scientificName=="Mareca strepera"] <- "Birds"
    all$Group[all$scientificName=="Motacilla citreola"] <- "Birds"
    all$Group[all$scientificName=="Myosotis scorpioides"] <- "Plants"
    all$Group[all$scientificName=="Platycnemididae"] <- "Libellen"
    all$Group[all$scientificName=="Populus ×canadensis"] <- "Plants"
    all$Group[all$scientificName=="Rubus fruticosus"] <- "Plants"
    all$Group[all$scientificName=="Rubus plicatus"] <- "Plants"
    all$Group[all$scientificName=="Emberiza schoeniclus"] <- "Birds"
    all$Group[all$scientificName=="Spatula clypeata"] <- "Birds"
    all$Group[all$scientificName=="Sympetrum"] <- "Libellen"
    all$Group[all$scientificName=="Urtica"] <- "Plants"
    all$Group[all$scientificName=="Utricularia"] <- "Plants"
    all$Group[all$scientificName=="Waterlelie SP"] <- "Plants"
    all$Group[all$scientificName=="Ardea alba"] <- "Birds"
    all$Group[all$scientificName=="Athya ferina"] <- "Birds"
    all$Group[all$scientificName=="Carex"] <- "Plants"
    all$Group[all$scientificName=="Carex cuprina"] <- "Plants"
    all$Group[all$scientificName=="Carex Pseudocyperus"] <- "Plants"
    all$Group[all$scientificName=="Coenagrion"] <- "Libellen"
    all$Group[all$scientificName=="Conyza canadensis"] <- "Plants"
    all$Group[all$scientificName=="Elytrigia repens"] <- "Plants"
    all$Group[all$scientificName=="Hydrocotyle tripartita"] <- "Plants"
    all$Group[all$scientificName=="Ludwigia grandiflora"] <- "Plants"
    all$Group[all$scientificName=="Lysimachia vulgaris"] <- "Plants"
    all$Group[all$scientificName=="Pyrrhosoma nymphula"] <- "Libellen"
    all$Group[all$scientificName=="Aeshna cyanea"] <- "Libellen"
    all$Group[all$scientificName=="Alopochen aegyptiaca"] <- "Birds"
    all$Group[all$scientificName=="Epilobium roseum"] <- "Plants"
    all$Group[all$scientificName=="Erythromma najas"] <- "Libellen"
    all$Group[all$scientificName=="Galeopsis tetrahit"] <- "Plants"
    all$Group[all$scientificName=="Heracleum sphondylium"] <- "Plants"
    all$Group[all$scientificName=="Sympetrum vulgatum"] <- "Libellen"
    
    
    #clean up plantCategory
    unique(all$plantCategory)
    all$plantCategory[all$plantCategory==" + "] <- "+"
    all$plantCategory[all$plantCategory==""] <- NA
    
    
    
#2. Summary statistics
    #remove occurenceID column (needed for GBIF only)
    all <- select(all, -c("occurenceID"))
    #remove observatiosn where scientificName missing
    all <- subset(all, complete.cases(all$scientificName))
   
    
#total # of records
  nrow(all) #5780 observations
  length(unique(all$scientificName)) #202 species  
  length(unique(subset(all$scientificName, all$Group=="Birds"))) #70 bird species
  length(unique(subset(all$scientificName, all$Group=="Libellen"))) #36 libellen species
  length(unique(subset(all$scientificName, all$Group=="Plants"))) #96 plant species
  subset(all$scientificName, all$Group=="")
  all$Group[all$scientificName=="Calidris pugnax"] <- "Birds"
  all$Group[all$scientificName=="Mareca penelope"] <- "Birds"
  all$Group[all$scientificName=="Phasianus colchius"] <- "Birds"
  all$Group[all$scientificName=="Tringa nebularia"] <- "Birds"
  
  
  
#fix strings
  all$project_area <- as.character(all$project_area)
  
#check why there is plant data missing from areas 3 and 10 in 2021
length(unique(subset(all$scientificName, all$project_area==3 & all$kingdom=="Plantae" & year(all$eventDate)==2021))) #0
length(unique(subset(all$scientificName, all$project_area==10 & all$kingdom=="Plantae" & year(all$eventDate)==2021))) #0
#check why there is plant data missing from areas 4 and 5 in 2022, clearly that isn't an issue here
length(unique(subset(all$scientificName, all$project_area==4 & all$kingdom=="Plantae" & year(all$eventDate)==2022))) #0
length(unique(subset(all$scientificName, all$project_area==5 & all$kingdom=="Plantae" & year(all$eventDate)==2022))) #0

  
  #create dataframes with summary stats per project area per survey date
  #species richness
  sum_PA_date <-all %>% 
    group_by(Group, project_area, eventDate) %>% 
    summarize(SpeciesRichness=n_distinct(scientificName), .groups = 'keep') %>%
    as.data.frame()
    
 
   sum_PA_date <- 
      sum_PA_date %>% 
      arrange(project_area, Group, eventDate)
    colnames(sum_PA_date) <- c("Species Group", "Project Area", "Survey Date", "Species Richness")
    sum_PA_date <- sum_PA_date[,c("Project Area","Species Group", "Survey Date","Species Richness")]
    
    #create table A
    tab_df(sum_PA_date,
           file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table A.doc") 

    
    #create dataframe with summary stats per project area per survey date
    #sum_PA_date2 <- all %>% 
      #group_by(Group, project_area) %>% 
      #summarize(SpeciesRichness=length(unique(scientificName))) %>%
      #as.data.frame()
    #colnames(sum_PA_date2) <- c("Project Area", "Species Group", "Species Richness")
    #sum_PA_date2 <- sum_PA_date2[,c("Project Area","Species Group", "Species Richness")]
    #create table B
    #tab_df(sum_PA_date2,
           #file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table B.doc") 
    
  #create figures
    #Figure A: Figure A1-A3, B1-B3, and C1-C3
    plots <- list()
    
    sum_PA_date$`Project Area` <- as.factor(sum_PA_date$`Project Area`)
    #birds
      sum_PA_date_birds <- subset(sum_PA_date, sum_PA_date$`Species Group`=="Birds")
      sum_PA_date_birds$Year <- year(sum_PA_date_birds$`Survey Date`)
      
      cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      sum_PA_date_birds$`Project Area` <- factor(sum_PA_date_birds$`Project Area`,levels = c("1", "2", "3", "4", "5", "10"))
      
      #both years, figure a1
      
      pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure A1.pdf") 
      plots[[1]] <- ggplot(aes(x=`Project Area`, y= `Species Richness`), data=sum_PA_date_birds) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
        geom_boxplot() +
        xlab("")  +
        ylab("Species Richness") +
        scale_y_continuous(breaks=c(0,10,20,30,40,50), limits=c(0,50)) +
        ylim(c(0, 50))     
        dev.off() 
      
      
        #2021 figure a2
        sum_PA_date_birds2021 <- subset(sum_PA_date_birds, sum_PA_date_birds$Year==2021)
        pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure A2.pdf") 
        plots[[2]] <- ggplot(aes(x=`Project Area`, y= `Species Richness`), data=sum_PA_date_birds2021) +
            theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_boxplot() +
          xlab("")  +
          ylab("") +
          scale_y_continuous(breaks=c(0,10,20,30,40,50), limits=c(0,50)) +
          ylim(c(0, 50))       
        dev.off() 
        
        #2022 figure a3
        sum_PA_date_birds2022 <- subset(sum_PA_date_birds, sum_PA_date_birds$Year==2022)
        pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure A3.pdf") 
        plots[[3]] <- ggplot(aes(x=`Project Area`, y= `Species Richness`), data=sum_PA_date_birds2022) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_boxplot() +
          xlab("")  +
          ylab("") +
          scale_y_continuous(breaks=c(0,10,20,30,40,50), limits=c(0,50)) +
          ylim(c(0, 50))       
        dev.off() 
        
        
      #libellen
        sum_PA_date_lib <- subset(sum_PA_date, sum_PA_date$`Species Group`=="Libellen")
        sum_PA_date_lib$Year <- year(sum_PA_date_lib$`Survey Date`)
        
        sum_PA_date_lib$`Project Area` <- factor(sum_PA_date_lib$`Project Area`,levels = c("1", "2", "3", "4", "5", "10"))
        
        #both years, figure b1
        library(ggplot2)
        
        plots[[4]] <- ggplot(aes(x=`Project Area`, y= `Species Richness`), data=sum_PA_date_lib) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_boxplot() +
          ylab("Species Richness")  +
          xlab("") +
          scale_y_continuous(breaks=c(0,10,20,30,40,50), limits=c(0,50)) +
          ylim(c(0, 50))
        
        # Save the plot as a PNG file
        ggsave("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure B1.png", 
               plots[[4]], dpi = 300, width = 6, height = 4)
        
        
        #2021 figure b2
        sum_PA_date_lib2021 <- subset(sum_PA_date_lib, sum_PA_date_lib$Year==2021)
        
        # Create the plot
        plots[[5]] <- ggplot(aes(x=`Project Area`, y= `Species Richness`), data=sum_PA_date_lib2021) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_boxplot() +
          xlab("")  +
          ylab("") +
          scale_y_continuous(breaks=c(0,10,20,30,40,50), limits=c(0,50)) +
          ylim(c(0, 50))
        
        # Save the plot as a PNG file
        ggsave("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure B2.png", 
               plots[[5]], dpi = 300, width = 6, height = 4)
        
        
        #2022 figure b3
        # Subset data for 2022
        sum_PA_date_lib2022 <- subset(sum_PA_date_lib, Year == 2022)
        
        # Create PNG file
        png(file = "/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure B3.png", width = 800, height = 600, units = "px")
        
        # Create boxplot
        ggplot(sum_PA_date_lib2022, aes(x = `Project Area`, y = `Species Richness`)) +
          geom_boxplot() +
          theme_bw() + 
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.position = "none") +
          xlab("")  +
          ylab("") +
          scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0, 50)) +
          ylim(c(0, 50)) 
        
        # Close PNG file
        dev.off()
        
        
  #Plants
        sum_PA_date_plants <- subset(sum_PA_date, sum_PA_date$`Species Group`=="Plants")
        sum_PA_date_plants$Year <- year(sum_PA_date_plants$`Survey Date`)
        
        sum_PA_date_plants$`Project Area` <- factor(sum_PA_date_plants$`Project Area`,levels = c("1", "2", "3", "4", "5", "10"))
        
      #both years, figure c1
        # Create PNG file
        png(file = "/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure C1.png", width = 800, height = 600, units = "px")
        
        # Create boxplot
        ggplot(sum_PA_date_plants, aes(x = `Project Area`, y = `Species Richness`)) +
          geom_boxplot() +
          theme_bw() + 
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.position = "none") +
          xlab("Area")  +
          ylab("Species Richness") +
          scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0, 50)) +
          ylim(c(0, 50)) 
        
        # Close PNG file
        dev.off()
        
        
      #2021 figure c2
        sum_PA_date_plants2021 <- subset(sum_PA_date_plants, sum_PA_date_plants$Year==2021)
        pa3 <- c(3, NA, NA, NA, NA)
        pa10 <- c(10, NA, NA, NA, NA)
        sum_PA_date_plants2021 <- rbind(sum_PA_date_plants2021, pa3, pa10)
        sum_PA_date_plants2022 <- rbind(sum_PA_date_plants2022, pa3, pa10)
        
        # Create PNG file
        png(file = "/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure C2.png", width = 800, height = 600, units = "px")
        
        # Create boxplot
        ggplot(sum_PA_date_plants2021, aes(x = `Project Area`, y = `Species Richness`)) +
          geom_boxplot() +
          theme_bw() + 
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.position = "none") +
          ylab("")  +
          xlab("Area") +
          scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 10)) +
          scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0, 50)) +
          ylim(c(0, 50)) 
        
        # Close PNG file
        dev.off()
        
        
      #2022 figure c3
        sum_PA_date_plants2022 <- subset(sum_PA_date_plants, sum_PA_date_plants$Year==2022)
        pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure C3.pdf") 
        plots[[9]] <- ggplot(aes(x=`Project Area`, y= `Species Richness`), data=sum_PA_date_plants2022) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_boxplot() +
          ylab("")  +
          xlab("Area") +
          scale_x_discrete(limits=factor(c(1,2,3,4,5,10))) +
          scale_y_continuous(breaks=c(0,10,20,30,40,50), limits=c(0,50)) +
          ylim(c(0, 50))    
        
        dev.off() 
        grid.arrange(grobs = plots, ncol = 3, nrow = 3)
        
        
        #figure for 2020 veg fact sheets
        sum_PA_date_plants2020 <- subset(sum_PA_date_plants, sum_PA_date_plants$Year==2020)
        pa3 <- c(3, NA, NA, NA, NA)
        pa10 <- c(10, NA, NA, NA, NA)
        sum_PA_date_plants2020 <- rbind(sum_PA_date_plants2020, pa3, pa10)
        sum_PA_date_plants2020 <- rbind(sum_PA_date_plants2020, pa3, pa10)
        
        # Create PNG file
        png(file = "/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/SR_2020_plant_factsheet.png", width = 800, height = 600, units = "px")
        
        # Create boxplot
        ggplot(sum_PA_date_plants2020, aes(x = `Project Area`, y = `Species Richness`)) +
          geom_boxplot() +
          theme_bw() + 
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.position = "none") +
          ylab("")  +
          xlab("Area") +
          scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 10)) +
          scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0, 50)) +
          ylim(c(0, 50)) 
        
        # Close PNG file
        dev.off()
        
        
#evenness
        
    #all years, groups, areas
      evenness_sum <- all[,c("project_area", "eventDate", "individualCount")]
        species_abundance_matrix <- acast(evenness_sum, project_area ~ eventDate, value.var = "individualCount", fun.aggregate = length)
        diversity_index_shannon <- diversity(species_abundance_matrix, index="shannon")
        diversity_index_simpson <- diversity(species_abundance_matrix, index="simpson")
        evenness_sum <- data.frame(Project.area=c(1,10,2,3,4,5), Shannon.Index = diversity_index_shannon, Simpson.Index = diversity_index_simpson)
        evenness_sum <- subset(evenness_sum, complete.cases(evenness_sum))
        
    #separate into years, groups, areas and get shannon and simpson indices for each then combine into one table
      evenness_all_categories <-mutate(all, Groupings=paste(all$Group, year(all$eventDate)))

        evenness_sum_birds2021 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Birds 2021")
        evenness_sum_birds2021 <- evenness_sum_birds2021[,c("project_area", "eventDate", "individualCount")]
        species_abundance_matrix <- acast(evenness_sum_birds2021, project_area ~ eventDate, value.var = "individualCount")
        diversity_index_shannon <- diversity(species_abundance_matrix, index="shannon")
        diversity_index_simpson <- diversity(species_abundance_matrix, index="simpson")
        evenness_sum_birds2021 <- data.frame(Project.area=c(1,10,2,3,4,5), Shannon.Index = diversity_index_shannon, Simpson.Index = diversity_index_simpson)
        evenness_sum_birds2021 <- subset(evenness_sum_birds2021, complete.cases(evenness_sum))
        evenness_sum_birds2021$Year <- 2021
        evenness_sum_birds2021$Group <- "Birds"
        evenness_sum_birds2021 <- subset(evenness_sum_birds2021, complete.cases(evenness_sum_birds2021$Simpson.Index))
        
        evenness_sum_libellen2021 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Libellen 2021")
        evenness_sum_libellen2021 <- evenness_sum_libellen2021[,c("project_area", "eventDate", "individualCount")]
        species_abundance_matrix <- acast(evenness_sum_libellen2021, project_area ~ eventDate, value.var = "individualCount")
        diversity_index_shannon <- diversity(species_abundance_matrix, index="shannon")
        diversity_index_simpson <- diversity(species_abundance_matrix, index="simpson")
        evenness_sum_libellen2021 <- data.frame(Project.area=c(1,10,2,3,4,5), Shannon.Index = diversity_index_shannon, Simpson.Index = diversity_index_simpson)
        evenness_sum_libellen2021 <- subset(evenness_sum_libellen2021, complete.cases(evenness_sum))
        evenness_sum_libellen2021$Year <- 2021
        evenness_sum_libellen2021$Group <- "Libellen"
        evenness_sum_libellen2021 <- subset(evenness_sum_libellen2021, complete.cases(evenness_sum_libellen2021$Simpson.Index))
        
        evenness_sum_birds2022 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Birds 2022")
        evenness_sum_birds2022 <- evenness_sum_birds2022[,c("project_area", "eventDate", "individualCount")]
        species_abundance_matrix <- acast(evenness_sum_birds2022, project_area ~ eventDate, value.var = "individualCount")
        diversity_index_shannon <- diversity(species_abundance_matrix, index="shannon")
        diversity_index_simpson <- diversity(species_abundance_matrix, index="simpson")
        evenness_sum_birds2022 <- data.frame(Project.area=c(1,10,2,3,4,5), Shannon.Index = diversity_index_shannon, Simpson.Index = diversity_index_simpson)
        evenness_sum_birds2022 <- subset(evenness_sum_birds2022, complete.cases(evenness_sum))
        evenness_sum_birds2022$Year <- 2022
        evenness_sum_birds2022$Group <- "Birds"
        evenness_sum_birds2022 <- subset(evenness_sum_birds2022, complete.cases(evenness_sum_birds2022$Simpson.Index))
        
        evenness_sum_libellen2022 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Libellen 2022")
        evenness_sum_libellen2022 <- evenness_sum_libellen2022[,c("project_area", "eventDate", "individualCount")]
        species_abundance_matrix <- acast(evenness_sum_libellen2022, project_area ~ eventDate, value.var = "individualCount")
        diversity_index_shannon <- diversity(species_abundance_matrix, index="shannon")
        diversity_index_simpson <- diversity(species_abundance_matrix, index="simpson")
        evenness_sum_libellen2022 <- data.frame(Project.area=c(1,10,2,3,4,5), Shannon.Index = diversity_index_shannon, Simpson.Index = diversity_index_simpson)
        evenness_sum_libellen2022 <- subset(evenness_sum_libellen2022, complete.cases(evenness_sum))
        evenness_sum_libellen2022$Year <- 2022
        evenness_sum_libellen2022$Group <- "Libellen"
        evenness_sum_libellen2022 <- subset(evenness_sum_libellen2022, complete.cases(evenness_sum_libellen2022$Simpson.Index))
        
        evenness_sum <- rbind(evenness_sum_libellen2021, evenness_sum_libellen2022, evenness_sum_birds2021, evenness_sum_birds2022)
        evenness_sum <- arrange(evenness_sum, Project.area, Group, Year)
        evenness_sum <- evenness_sum[,c("Project.area","Group","Year","Shannon.Index","Simpson.Index")]
        evenness_sum <- subset(evenness_sum, complete.cases(evenness_sum$Project.area))
        
        colnames(evenness_sum)[2] <- "Species.Group"
        
        tab_df(evenness_sum,
           file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table C.doc") 
    
        
 #create dataframe with summary stats per project area per survey date
  all$Year <- year(all$eventDate)
        
  sum_PA_date2 <- all %>% 
    group_by(Group, project_area, Year) %>% 
    summarize(SpeciesRichness=length(unique(scientificName))) %>%
    arrange(project_area, Group, Year) %>%
    as.data.frame()
  colnames(sum_PA_date2) <- c("Species Group", "Project Area", "Survey Year", "Species Richness")
  sum_PA_date2 <- sum_PA_date2[,c("Project Area","Species Group", "Survey Year", "Species Richness")]
    #remove NAs
    sum_PA_date2 <- subset(sum_PA_date2, complete.cases(sum_PA_date2))
    sum_PA_date2 <- subset(sum_PA_date2, !sum_PA_date2$"Species Group"=="")
    
#create table B
    tab_df(sum_PA_date2,
           file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table B.doc") 
    
  #rank abundance plots
    evenness_sum_birds2021 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Birds 2021")
    evenness_sum_birds2022 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Birds 2022")
    evenness_sum_libellen2021 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Libellen 2021")
    evenness_sum_libellen2022 <- subset(evenness_all_categories, evenness_all_categories$Groupings=="Libellen 2022")
    evenness_sum_birds2021 <-evenness_sum_birds2021[,c("scientificName","individualCount","project_area")]
    
    # Plot the rank abundance 
      par(mfrow=c(2,2))
      
      species_abundance_matrix_birds2021 <- acast(evenness_sum_birds2021, project_area ~ scientificName, value.var = "individualCount")
      pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure D1.pdf") 
      ra_plot_birds2021 <- racurves(species_abundance_matrix_birds2021, bw=F, main="")
        dev.off() 
        
      species_abundance_matrix_birds2022 <- acast(evenness_sum_birds2022, project_area ~ scientificName, value.var = "individualCount")
      species_abundance_matrix_birds2022 <- species_abundance_matrix_birds2022[c(1:6),]
      pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure D2.pdf") 
      ra_plot_birds2022 <- racurves(species_abundance_matrix_birds2022, bw=F, main="")
        dev.off() 
  
      species_abundance_matrix_libellen2021 <- acast(evenness_sum_libellen2021, project_area ~ scientificName, value.var = "individualCount")
      species_abundance_matrix_libellen2021 <- species_abundance_matrix_libellen2021[c(1:6),]
      pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure D3.pdf") 
      ra_plot_libellen2021 <- racurves(species_abundance_matrix_libellen2021, bw=F, main="")
        dev.off() 
        
      species_abundance_matrix_libellen2022 <- acast(evenness_sum_libellen2022, project_area ~ scientificName, value.var = "individualCount")
      species_abundance_matrix_libellen2022 <- species_abundance_matrix_libellen2022[c(1:6),]
      pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure D4.pdf") 
      ra_plot_libellen2022 <- racurves(species_abundance_matrix_libellen2022, bw=F, main="")
        dev.off() 
      
  #Beta diversity
      # Create a Bray-Curtis dissimilarity matrix tables and figures
      
        #birds 2021
          #rows samples, columns taxa, values as counts
          bc_birds2021 <- acast(evenness_sum_birds2021, project_area ~ scientificName, value.var = "individualCount")
          bc_birds2021 <- vegdist(bc_birds2021, method = "bray")
          nmds_birds2021 <-metaMDS(bc_birds2021)
          pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure E1.pdf") 
          scores(nmds_birds2021) %>%
            as_tibble(rownames= "Group") %>%
            ggplot(aes(x=NMDS1, y=NMDS2)) +
            geom_point()
          dev.off() 
          #sample size is  small, cannot make concrete conclusions
          
        #birds 2022
          #rows samples, columns taxa, values as counts
          bc_birds2022 <- acast(evenness_sum_birds2022, project_area ~ scientificName, value.var = "individualCount")
          bc_birds2022 <- vegdist(bc_birds2022, method = "bray")
          nmds_birds2022 <-metaMDS(bc_birds2022)
          pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure E2.pdf") 
          scores(nmds_birds2022) %>%
            as_tibble(rownames= "Group") %>%
            ggplot(aes(x=NMDS1, y=NMDS2)) +
            geom_point()
          dev.off() 
          #sample size is  small, cannot make concrete conclusions
          
        #libellen 2021
          #rows samples, columns taxa, values as counts
          bc_libellen2021 <- acast(evenness_sum_libellen2021, project_area ~ scientificName, value.var = "individualCount")
          bc_libellen2021 <- vegdist(bc_libellen2021, method = "bray")
          nmds_libellen2021 <-metaMDS(bc_libellen2021)
          pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure E3.pdf") 
            scores(nmds_libellen2021) %>%
              as_tibble(rownames= "Group") %>%
              ggplot(aes(x=NMDS1, y=NMDS2)) +
              geom_point()
            dev.off() 
          #sample size is  small, cannot make concrete conclusion
      
            
      
        #libellen 2022
          #rows samples, columns taxa, values as counts
          bc_libellen2022 <- acast(evenness_sum_libellen2022, project_area ~ scientificName, value.var = "individualCount")
          bc_libellen2022 <- vegdist(bc_libellen2022, method = "bray")
          
          #
         nmds_libellen2022 <-metaMDS(bc_libellen2022)
          #pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure E4.pdf") 
          scores(nmds_libellen2022) %>%
            as_tibble(rownames= "Group") %>%
            ggplot(aes(x=NMDS1, y=NMDS2)) +
            geom_point()
          #dev.off() 
            #sample size is  small, cannot make concrete conclusions
        
  
#Red List  
  #create data frame summarizing number of threatened species in each project area by date by country
  all$Year <- year(all$eventDate)
  redlist_be <- subset(all, all$countryCode=="BE" & complete.cases(all$IUCN.Status.Belgium))
  redlist_be_sum <- redlist_be %>% 
    group_by(IUCN.Status.Belgium, Group, project_area, Year) %>% 
    summarize(No.species=length(unique(scientificName))) %>%
    as.data.frame()
  colnames(redlist_be_sum)[1] <-"Red.List.status"
  colnames(redlist_be_sum)[2] <-"Species.group"
  colnames(redlist_be_sum)[3] <-"Project.area"
  colnames(redlist_be_sum)[5] <-"Number.of.species"
  redlist_be_sum$Country <- "Belgium"
  redlist_be_sum <- arrange(redlist_be_sum, Project.area, Year, Country, Red.List.status, Species.group, Number.of.species)
  redlist_be_sum <- redlist_be_sum[,c("Project.area","Year","Country", "Project.area", "Red.List.status", "Species.group", "Number.of.species")]

  
    #which species are in each of the red list categories
    redlist_be_CR <- unique(subset(redlist_be[,c("scientificName","Group", "Year", "project_area")], redlist_be$IUCN.Status.Belgium=="Critically Endangered"))
    redlist_be_DD <- unique(subset(redlist_be[,c("scientificName","Group", "Year", "project_area")], redlist_be$IUCN.Status.Belgium=="Data Deficient"))
    redlist_be_EN <- unique(subset(redlist_be[,c("scientificName","Group", "Year", "project_area")], redlist_be$IUCN.Status.Belgium=="Endangered"))
    redlist_be_LC <- unique(subset(redlist_be[,c("scientificName","Group", "Year", "project_area")], redlist_be$IUCN.Status.Belgium=="Least Concern"))
    redlist_be_NT <- unique(subset(redlist_be[,c("scientificName","Group", "Year", "project_area")], redlist_be$IUCN.Status.Belgium=="Near Threatened"))
    redlist_be_NE <- unique(subset(redlist_be[,c("scientificName","Group", "Year", "project_area")], redlist_be$IUCN.Status.Belgium=="Not Evaluated"))
    redlist_be_VU <- unique(subset(redlist_be[,c("scientificName","Group", "Year", "project_area")], redlist_be$IUCN.Status.Belgium=="Vulnerable"))
    
    redlist_be_CR$Red.List.status <- "Critically Endangered"
    redlist_be_DD$Red.List.status <- "Data Deficient"
    redlist_be_EN$Red.List.status <- "Endangered"
    redlist_be_LC$Red.List.status <- "Least Concern"
    redlist_be_NT$Red.List.status <- "Near Threatened"
    redlist_be_NE$Red.List.status <- "Not Evaluated"
    redlist_be_VU$Red.List.status <- "Vulnerable"

    redlist_be_all <- rbind(redlist_be_CR, redlist_be_DD, redlist_be_EN, 
                            redlist_be_LC, redlist_be_NT, redlist_be_NE, redlist_be_VU)
    redlist_be_all$Country <- "Belgium"
    redlist_be_all <- arrange(redlist_be_all, Year, Country, Red.List.status, Group, scientificName, project_area)
    colnames(redlist_be_all)[2] <- "Species.group"
    colnames(redlist_be_all)[1] <-"Species"
    redlist_be_all <- redlist_be_all[,c("project_area", "Year","Country","Red.List.status", "Species.group", "Species")]
    
    
  redlist_de <- subset(all, all$countryCode=="DE" & complete.cases(all$IUCN.Status.Germany))
  redlist_de_sum <- redlist_de %>% 
    group_by(IUCN.Status.Germany, Group, project_area, Year) %>% 
    summarize(No.species=length(unique(scientificName))) %>%
    as.data.frame()
  colnames(redlist_de_sum)[1] <-"Red.List.status"
  colnames(redlist_de_sum)[2] <-"Species.group"
  colnames(redlist_de_sum)[3] <-"Project.area"
  colnames(redlist_de_sum)[5] <-"Number.of.species"
  redlist_de_sum$Country <- "Germany"
  redlist_de_sum <- arrange(redlist_de_sum, Year, Country, Project.area, Red.List.status, Species.group, Number.of.species, Project.area)
  redlist_de_sum <- redlist_de_sum[,c("Project.area","Year","Country", "Project.area", "Red.List.status", "Species.group", "Number.of.species")]
  
    #which species are in each of the red list categories
    redlist_de_CR <- unique(subset(redlist_de[,c("scientificName","Group", "Year", "project_area")], redlist_de$IUCN.Status.Germany=="Critically Endangered"))
    redlist_de_DD <- unique(subset(redlist_de[,c("scientificName","Group", "Year", "project_area")], redlist_de$IUCN.Status.Germany=="Data Deficient"))
    redlist_de_EN <- unique(subset(redlist_de[,c("scientificName","Group", "Year", "project_area")], redlist_de$IUCN.Status.Germany=="Endangered"))
    redlist_de_LC <- unique(subset(redlist_de[,c("scientificName","Group", "Year", "project_area")], redlist_de$IUCN.Status.Germany=="Least Concern"))
    redlist_de_NT <- unique(subset(redlist_de[,c("scientificName","Group", "Year", "project_area")], redlist_de$IUCN.Status.Germany=="Near Threatened"))
    redlist_de_NE <- unique(subset(redlist_de[,c("scientificName","Group", "Year", "project_area")], redlist_de$IUCN.Status.Germany=="Not Evaluated"))
    redlist_de_VU <- unique(subset(redlist_de[,c("scientificName","Group", "Year", "project_area")], redlist_de$IUCN.Status.Germany=="Vulnerable"))
    
    redlist_de_CR$Red.List.status <- "Critically Endangered"
    redlist_de_DD$Red.List.status <- "Data Deficient"
    redlist_de_EN$Red.List.status <- "Endangered"
    redlist_de_LC$Red.List.status <- "Least Concern"
    redlist_de_NT$Red.List.status <- "Near Threatened"
    redlist_de_NE$Red.List.status <- "Not Evaluated"
    redlist_de_VU$Red.List.status <- "Vulnerable"
    
    redlist_de_all <- rbind(redlist_de_CR, redlist_de_DD, redlist_de_EN, 
                            redlist_de_LC, redlist_de_NT, redlist_de_NE, redlist_de_VU)
    redlist_de_all$Country <- "Germany"
    redlist_de_all <- arrange(redlist_de_all, Country, Red.List.status, Group, scientificName, Year, project_area)
    colnames(redlist_de_all)[2] <- "Species.group"
    colnames(redlist_de_all)[1] <-"Species"
    redlist_de_all <- redlist_de_all[,c("project_area","Year","Country","Red.List.status", "Species.group", "Species")]
    
    
  redlist_nl <- subset(all, all$countryCode=="NL" & complete.cases(all$IUCN.Status.Netherlands))
  redlist_nl_sum <- redlist_nl %>% 
    group_by(IUCN.Status.Netherlands, Group, project_area, Year) %>% 
    summarize(No.species=length(unique(scientificName))) %>%
    as.data.frame()
  colnames(redlist_nl_sum)[1] <-"Red.List.status"
  colnames(redlist_nl_sum)[2] <-"Species.group"
  colnames(redlist_nl_sum)[3] <-"Project.area"
  colnames(redlist_nl_sum)[5] <-"Number.of.species"
  redlist_nl_sum$Country <- "Netherlands"
  redlist_nl_sum <- arrange(redlist_nl_sum, Year, Country, Project.area, Red.List.status, Species.group, Number.of.species, Project.area)
  redlist_nl_sum <- redlist_nl_sum[,c("Project.area","Year","Country", "Project.area", "Red.List.status", "Species.group", "Number.of.species")]
  
    #which species are in each of the red list categories
    redlist_nl_CR <- unique(subset(redlist_nl[,c("scientificName","Group", "Year","project_area")], redlist_nl$IUCN.Status.Netherlands=="Critically Endangered"))
    redlist_nl_DD <- unique(subset(redlist_nl[,c("scientificName","Group", "Year","project_area")], redlist_nl$IUCN.Status.Netherlands=="Data nlficient"))
    redlist_nl_EN <- unique(subset(redlist_nl[,c("scientificName","Group", "Year","project_area")], redlist_nl$IUCN.Status.Netherlands=="Endangered"))
    redlist_nl_LC <- unique(subset(redlist_nl[,c("scientificName","Group", "Year","project_area")], redlist_nl$IUCN.Status.Netherlands=="Least Concern"))
    redlist_nl_NT <- unique(subset(redlist_nl[,c("scientificName","Group", "Year","project_area")], redlist_nl$IUCN.Status.Netherlands=="Near Threatened"))
    redlist_nl_NE <- unique(subset(redlist_nl[,c("scientificName","Group", "Year","project_area")], redlist_nl$IUCN.Status.Netherlands=="Not Evaluated"))
    redlist_nl_VU <- unique(subset(redlist_nl[,c("scientificName","Group", "Year","project_area")], redlist_nl$IUCN.Status.Netherlands=="Vulnerable"))
    
    #redlist_nl_CR$Red.List.status <- "Critically Endangered"
    #redlist_nl_DD$Red.List.status <- "Data deficient"
    #redlist_nl_EN$Red.List.status <- "Endangered"
    #redlist_nl_LC$Red.List.status <- "Least Concern"
    #redlist_nl_NT$Red.List.status <- "Near Threatened"
    #redlist_nl_NE$Red.List.status <- "Not Evaluated"
    redlist_nl_VU$Red.List.status <- "Vulnerable"
    
    redlist_nl_all <- rbind(redlist_nl_CR, redlist_nl_DD, redlist_nl_EN, 
                            redlist_nl_LC, redlist_nl_NT, redlist_nl_NE, redlist_nl_VU)
    redlist_nl_all$Country <- "Netherlands"
    redlist_nl_all <- arrange(redlist_nl_all, Year, Country, Red.List.status, Group, scientificName)
    colnames(redlist_nl_all)[2] <- "Species.group"
    colnames(redlist_nl_all)[1] <-"Species"
    redlist_nl_all <- redlist_nl_all[,c("Year","Country","Red.List.status", "Species.group", "Species","project_area")]
    
    
  #combine summary red list data into one table d  
    redlist_all_sum <-rbind(redlist_de_sum, redlist_be_sum, redlist_nl_sum)
    redlist_all_sum <- subset(redlist_all_sum, complete.cases(redlist_all_sum))
    tab_df(redlist_all_sum,
           file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table D.doc") 
    
    #combine red list species data into one table e  
    redlist_all_species <-rbind(redlist_de_all, redlist_be_all, redlist_nl_all)
    redlist_all_species <- subset(redlist_all_species, complete.cases(redlist_all_sum))
    tab_df(redlist_all_species,
           file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table E.doc") 
    
      #subset further to highlight abundance for those species of interest
      redlist_de_subset <- subset(redlist_de_all, redlist_de_all$Red.List.status=="Critically Endangered" |
                                    redlist_de_all$Red.List.status=="Endangered" |
                                    redlist_de_all$Red.List.status=="Near Threatened" |
                                    redlist_de_all$Red.List.status=="Vulnerable")
      
      redlist_nl_subset <- subset(redlist_nl_all, redlist_nl_all$Red.List.status=="Critically Endangered" |
                                    redlist_nl_all$Red.List.status=="Endangered" |
                                    redlist_nl_all$Red.List.status=="Near Threatened" |
                                    redlist_nl_all$Red.List.status=="Vulnerable")
      
      redlist_be_subset <- subset(redlist_be_all, redlist_be_all$Red.List.status=="Critically Endangered" |
                                    redlist_be_all$Red.List.status=="Endangered" |
                                    redlist_be_all$Red.List.status=="Near Threatened" |
                                    redlist_be_all$Red.List.status=="Vulnerable")
    
      redlist_all_subset <- rbind(redlist_de_subset, redlist_nl_subset, redlist_be_subset)
      redlist_species <- unique(redlist_all_subset$Species)
      length(redlist_species) #32 species that are either endangered, critically endangered, near threatened, or vulnerable
      
    #when were these species found in the data set and how many times were they counted each project area and year?
      redlist_all_subset2 <- subset(all, all$scientificName %in% redlist_all_subset$Species)
      redlist_all_subset2 <- subset(redlist_all_subset2, complete.cases(redlist_all_subset2$individualCount))
      redlist_all_subset2 <- redlist_all_subset2 %>% 
        group_by(countryCode, project_area, Year, scientificName) %>%
        summarise(Count=sum(individualCount)) %>%
        ungroup() %>%
        as.data.frame()
      
      colnames(redlist_all_subset2)[1] <- "Country"
      colnames(redlist_all_subset2)[4] <- "Species"
      redlist_all_subset2$Country[redlist_all_subset2$Country=="BE"] <- "Belgium"
      redlist_all_subset2$Country[redlist_all_subset2$Country=="DE"] <- "Germany"
      redlist_all_subset2$Country[redlist_all_subset2$Country=="NL"] <- "Netherlands"
      redlist_all_subset2$Country_Year_Species_Project.area <-paste(redlist_all_subset2$Country, redlist_all_subset2$Year, redlist_all_subset2$Species, redlist_all_subset2$project_area)
      redlist_all_subset$Country_Year_Species_Project.area <-paste(redlist_all_subset$Country, redlist_all_subset$Year, redlist_all_subset$Species, redlist_all_subset$project_area)
      
      redlist_all_subset2 <- redlist_all_subset2[, c("Country_Year_Species_Project.area", "Count")]
      redlist_summary <- merge(redlist_all_subset, redlist_all_subset2, by="Country_Year_Species_Project.area", all.x=T)
    
    #calculate species richness for each red list status, per project area, per year     
      SR_red <- redlist_summary %>%
        group_by(Red.List.status, project_area, Year) %>%
        summarize(Species.richness= n()) %>%
        as.data.frame()
      
      tab_df(SR_red,
             file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table F.doc") 
      
      #calculate species richness per project area, per year     
      SR_red_all <- redlist_summary %>%
        group_by(project_area, Year) %>%
        summarize(Species.richness= n()) %>%
        as.data.frame()
      
      tab_df(SR_red_all,
             file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table G.doc") 
      
      
  #species of interest from GA
    #Aquatic warbler, black tern, little bittern, reed bunting, water vole, freshwater bivalve, lesser bulrush
    subset(all, all$scientificName=="Acrocephalus paludicola") #Aquatic warbler, none
    subset(all, all$scientificName=="Chlidonias niger") #black tern, 1 found project_area 10, 2022-05-26
    subset(all, all$scientificName=="Ixobrychus minutus") #little bittern, 2 found project area 5 2022-6-01
    reedbunting <- subset(all, all$scientificName=="Emberiza schoeniclus") #reed bunting
      nrow(subset(all, all$scientificName=="Emberiza schoeniclus")) #101 obvervations
      reedbunting2 <- subset(reedbunting, complete.cases(reedbunting$individualCount))
      sum(as.numeric(reedbunting2$individualCount))
      reedbunting2 %>% group_by(project_area) %>% summarize(sum(individualCount)) #138 total count, 2021 and 2022, project areas 1 5 and 4, BE and DE
   subset(all, all$scientificName=="Typha latifolia") #lesser bulrush, 24 observations, all 3 countries, 1 2 4 5 10 project areas
    nrow(subset(all, all$scientificName=="Typha latifolia"))
   subset(all, all$scientificName=="Botaurus stellaris ") #Eurasian bittern
    
    subset(all, all$scientificName=="Arvicola amphibius") #water vole, mammals not surveyed
    #freshwater bivalve, bivalves not surveyed
    
    
    #summary stats per year, red list
    sum_2021 <- subset(redlist_all_sum, redlist_all_sum$Year==2021)
    sum_2021 <- sum_2021 %>% 
      group_by(Country, Red.List.status) %>%
      summarize(sum(Number.of.species))
    colnames(sum_2021)[3] <- "Number.of.species"
    
    #table Fact_sheet_red_list_summary2021
    tab_df(sum_2021,
           file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Fact_sheet_red_list_summary2021.doc") 
    
    sum_2022 <- subset(redlist_all_sum, redlist_all_sum$Year==2022)
    sum_2022 <- sum_2022 %>% 
      group_by(Country, Red.List.status) %>%
      summarize(sum(Number.of.species))
    colnames(sum_2022)[3] <- "Number.of.species"
    
    #tableFact_sheet_red_list_summary2022
    tab_df(sum_2022,
           file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Fact_sheet_red_list_summary2022.doc") 
    
    
#Read in data from Emiel van Loon's reports (20230201)
      #Life MICA deliverable D.2.1: Evaluation of the numbers of muskrat and coypu in the project areas 
      #Life MICA deliverable D.2.2.1: Report on vegetation change by decreasing numbers of muskrat and Coypu 
    
    reedbed <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/Emiel van Loon reports/Table 2.1 – Size of project areas and reed surfaces.csv", header=T, stringsAsFactors = FALSE, sep=";")
    catchperkm <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/Emiel van Loon reports/Table 3.1. Catch of Coypu and Muskrat per km of waterway.csv", header=T, stringsAsFactors = FALSE, sep=";")
    cleanareas <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/Emiel van Loon reports/Table 4.1 – Size of the clean areas in km2.csv", header=T, stringsAsFactors = FALSE, sep=";")
    popsize <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/Emiel van Loon reports/Table 5.1 – The population sizes of muskrat and Coypu in absolute numbers.csv", header=T, stringsAsFactors = FALSE, sep=";")
   
    #merge all data and with species richness per project area, per year     
    SR_red_all <- redlist_summary %>%
      group_by(project_area, Year) %>%
      summarize(Species.richness= n()) %>%
      as.data.frame()
    
    colnames(SR_red_all)[1] <-"Area"
    SR_red_all$Area_Year <- paste(SR_red_all$Area, SR_red_all$Year)
    SR_red_all <- rbind(SR_red_all, SR_red_all)
    SR_red_all$Species <- c(rep("Muskrat", times = nrow(SR_red_all)/2),
                            rep("Coypu", times = nrow(SR_red_all)/2))
    SR_red_all$Area_Species <- paste(SR_red_all$Area, SR_red_all$Species)
    
    SR_red_all$Area_Year_Species <- paste(SR_red_all$Area, SR_red_all$Year, SR_red_all$Species)
    reedbed$Area_Year <- paste(reedbed$Area, reedbed$Year)
    catchperkm$Area_Year_Species <- paste(catchperkm$Area, catchperkm$Year, catchperkm$Species)
    cleanareas$Area_Year_Species <- paste(cleanareas$Area, cleanareas$Year, cleanareas$Species)
    popsize$Area_Species <- paste(popsize$Area, popsize$Species)
    
    reedbed <- reedbed[,c("Area_Year","Project.area","Reed.surface.area")]
    catchperkm <- catchperkm[,c("Area_Year_Species","Catch.per.km.waterway","Active.control")]
    cleanareas <- cleanareas[,c("Area_Year_Species", "Size.of.clean.areas")]
    popsize <- popsize[,c("Area_Species", "Resident", "Immigr")]
    
    combined_df <- merge(SR_red_all, reedbed, by="Area_Year", all.x=T)
    combined_df <- merge(combined_df, catchperkm, by="Area_Year_Species", all.x=T)
    combined_df <- merge(combined_df, cleanareas, by="Area_Year_Species", all.x=T)
    combined_df <- merge(combined_df, popsize, by="Area_Species", all.x=T)
    
    combined_df <- combined_df[!duplicated(combined_df),]
    combined_df <- combined_df[complete.cases(combined_df$Species.richness),]
    combined_df <- combined_df[!duplicated(combined_df$Area_Year_Species),]

    combined_df <- combined_df[, !colnames(combined_df) %in% c("Area_Species", "Area_Year_Species", "Area_Year")]
    


    #plot Species richness of all red list species observed in each project area and year
      combined_df$Year <- as.factor(combined_df$Year)
      combined_df$Area <- as.factor(combined_df$Area)
      combined_df$Area <- factor(combined_df$Area, levels = c("1", "2", "3", "4", "5", "10"))
      
      
      pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure F1.pdf") 
      ggplot(aes(x =Area, y= Species.richness, color=Year), data=combined_df) +
        scale_fill_manual(values=cbPalette) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        geom_point(size=3) +
        ylab("Species Richness of Red List Species")
      dev.off() 
      
    
#Evaluate if changes to each column in combined_df (estimates from Emmiel's report) can have led to changes in the Endangered Species KPLIs
      combined_df$Active.control[combined_df$Active.control==0] <- "No"
      combined_df$Active.control[combined_df$Active.control==1] <- "Yes"
      combined_df$Active.control <- as.factor(combined_df$Active.control)
      
    #combine all plots into one 2 x 6 plot
        plots <- list()
        
        # Add the first plot to the list
        plots[[1]] <- ggplot(aes(x =Catch.per.km.waterway, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Muskrat")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("Species Richness of Red List Species") +
          xlab("") +
          scale_x_continuous(limits=c(min(combined_df$Catch.per.km.waterway), max(combined_df$Catch.per.km.waterway)), expand=c(0,0.1)) +
          scale_y_continuous(limits=c(min(combined_df$Species.richness), max(combined_df$Species.richness)), expand=c(0,0.2), breaks=c(0,2,4,6,8,10,12,14))
        
        # Add the second plot to the list
        plots[[5]] <- ggplot(aes(x =Catch.per.km.waterway, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Coypu")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("Species Richness of Red List Species") +
          xlab("Catch per km of waterway") +
          scale_y_continuous(limits=c(min(combined_df$Species.richness), max(combined_df$Species.richness)), expand=c(0,0.2), breaks=c(0,2,4,6,8,10,12,14))
      
          
        plots[[2]] <- ggplot(aes(x =Resident, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Muskrat")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("") +
          xlab("") +
          scale_x_continuous(breaks=c(0,300,600,900,1200), limits=c(0,1200)) 
            
        
        plots[[6]] <- ggplot(aes(x =Resident, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Coypu")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("") +
          xlab("Number of residents")
        
        plots[[3]] <- ggplot(aes(x =Immigr, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Muskrat")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("") +
          xlab("") +
          scale_x_continuous(breaks=c(0,300,600,900,1200,1500,1800)) 
        
        plots[[7]] <- ggplot(aes(x =Immigr, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Coypu")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("") +
          xlab("Number of immigrants") +
          scale_x_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) 
        
        
        plots[[4]] <- ggplot(aes(x =Size.of.clean.areas, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Muskrat")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("") +
          xlab("") +
          scale_x_continuous(breaks=c(0,1,2,3,4), limits=c(0,)) 
        
        plots[[8]] <- ggplot(aes(x =Size.of.clean.areas, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Coypu")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("") +
          xlab("Size of clean areas (km2)") +
          scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70), limits=c(0,70)) 
        
        
        #note, these margins, etc could be improved
        #pdf("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Figure G.pdf") 
          grid.arrange(grobs = plots, ncol = 4, nrow = 2)
          #dev.off() 
             
             
            
  #reed surface area and active control same for both species so doesn't fit here (bc it's the same for both species, just subsettd for one)
        plots <- list()
          
        plots[[1]] <- ggplot(aes(x =Reed.surface.area, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Coypu")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
          geom_point(aes(size=Area)) +
          ylab("Species Richness of Red List Species") +
          xlab("Reed surface area (km2)") +
          scale_x_continuous(breaks=c(0,1,2,3,4), limits=c(0,4)) +
          scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,14)) 
        
      
        plots[[2]] <- ggplot(aes(x =Active.control, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Coypu")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none", axis.text.y = element_blank()) +
          geom_boxplot() +
          ylab("") +
          xlab("Active control measures")    
      
        
        plots[[3]] <- ggplot(aes(x = Project.area, y= Species.richness, color=Year), data=subset(combined_df, combined_df$Species=="Coypu")) +
          scale_fill_manual(values=cbPalette) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none", axis.text.y = element_blank()) +
          geom_boxplot() +
          ylab("") +
          xlab("Project area (km2)") +    
          scale_x_continuous(breaks=c(0,20,40,60,80), limits=c(0,80)) 
        
          
        grid.arrange(grobs = plots, ncol = 3, nrow = 1)
        


    
    
#make combined_df into a table that includes all the data from Emiel van Loon's reports 
  #combined_df <- combined_df[,c("Species", "Area", "Year", "Project.area", "Reed.surface.area", "Species.richness")]
    combined_df <- combined_df %>% arrange(Species, Area, Year)
    combined_df <- combined_df[,c("Species", "Area", "Year",
                                  "Species.richness", "Catch.per.km.waterway",
                                  "Resident", "Immigr", "Active.control",
                                  "Size.of.clean.areas", "Project.area", "Reed.surface.area")]
  tab_df(combined_df,
         file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/tables_and_figures/Table H.doc") 

    
  
#check comments from project partners
  #odonata SR should not be the same for project area 4 and 5
  sum_PA_date2 
  
  odo_pa4 <- subset(all, all$Group=="Libellen" & all$project_area==4)
  unique(odo_pa4$scientificName)
  
  odo_pa5 <- subset(all, all$Group=="Libellen" & all$project_area==5)
  unique(odo_pa5$scientificName)
  
  #there are issues with the data. is it translation? 
    #need to go back to original data both years

    
  
  
  
  
    