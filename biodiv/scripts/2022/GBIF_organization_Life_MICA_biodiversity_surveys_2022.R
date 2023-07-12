#Organizing 2022 data for Life MICA biodiversity surveys for GBIF and data analysis

#columns needed
  #occurrenceID= sharepoint folder+ unique code number, https://hetwaterschapshuis.sharepoint.com/sites/1084/Dashboard/Forms/AllItems.aspx?csf=1&web=1&e=Y5CqNG%2F&cid=672837e7%2D83d8%2D42ed%2D8bba%2D244d58943c2b&RootFolder=%2Fsites%2F1084%2FDashboard%2FNetherlands&FolderCTID=0x0120003F39983DD750B64A9A79D97E7303C9A7
  #basisOfRecord= AcceptedTaxon
  #eventDate= e.g. 2021-04-26
  #eventTime: 18:00+01:00
  #scientificName- e.g. Emberiza schoeniclus
  #kingdom- e.g. Animalia
  #taxonRank- e.g. species
  #individualCount- e.g. 4
  #decimalLatitude
  #decimalLongitude
  #countryCode- e.g. NL, BE, DE

#load packages
  library(stringr)
  library(sp)
  library(leaflet)
  library(lubridate)
  library(tidyr)

#1. Belgium, areas 4 (Sint-Laureins) and 5 (Sint-Maaartensheide- De Luysen)
#read in Belgian data
  setwd("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/Belgium")
  survey <- read.csv("surveys.csv", header=T, stringsAsFactors = FALSE)
  pcoordinates <- read.csv("pcoordinates.csv", header=T, stringsAsFactors = FALSE)
  ltransects <- read.csv("ltransects.csv", header=T, stringsAsFactors = FALSE)
  vtransects <- read.csv("vtransects.csv", header=T, stringsAsFactors = FALSE)
  plants <- read.csv("plants.csv", header=T, stringsAsFactors = FALSE, sep=";")
  birds <- read.csv("birds.csv", header=T, stringsAsFactors = FALSE)
  libellen <- read.csv("libelllen.csv", header=T, stringsAsFactors = FALSE)

#clean up Belgian dataset, selecting only necessary columns and renaming in order to merge
  #plants

    #date
    plants <- plants[,1:6]
    plants$Date <- as.Date("2021-09-21")
    plants$Date[plants$Survey_ID==17] <- "2021-09-24"
    plants$Date[plants$Survey_ID==16] <- "2021-09-21"
    plants$Date[plants$Survey_ID==34] <- "2022-09-09"
    plants$Date[plants$Survey_ID==35] <- "2022-09-15"
    
    #latitude/longitude
    pcoordinates <- subset(pcoordinates, complete.cases(pcoordinates$Long)) #remove repetitive rows
    pcoordinates$Stop <- as.numeric(substr(pcoordinates$Stop, 3, 3)) #remove PQ so it is just a number for stop
    pcoordinates$TransectStop <- paste(pcoordinates$Transect, pcoordinates$Stop)
    plantspcoordinates <-pcoordinates[,c(4,5,7)]
    plants$TransectStop <- paste(plants$Transect, plants$Plot)
    plants <- merge(plants, plantspcoordinates, by="TransectStop")
    
  #birds
    
    #date
      survey$Datum <- as.Date(survey$Datum)
      birds <- birds[,1:6]
      survey <- survey[,1:9]
      vtransects <- vtransects[,1:6]
     
      birds$Date <- as.Date("2021-09-21")
      birds$Date[birds$Survey_ID==1] <- "2021-04-26"
      birds$Date[birds$Survey_ID==2] <- "2021-04-30"
      birds$Date[birds$Survey_ID==3] <- "2021-05-05"
      birds$Date[birds$Survey_ID==4] <- "2021-05-03"
      birds$Date[birds$Survey_ID==5] <- "2021-05-03"
      birds$Date[birds$Survey_ID==6] <- "2021-05-10"
      birds$Date[birds$Survey_ID==7] <- "2021-05-12"
      birds$Date[birds$Survey_ID==8] <- "2021-05-14"
      birds$Date[birds$Survey_ID==9] <- "2021-05-17"
      birds$Date[birds$Survey_ID==10] <- "2021-05-06"
      birds$Date[birds$Survey_ID==18] <- "2022-04-12"
      birds$Date[birds$Survey_ID==19] <- "2022-04-28"
      birds$Date[birds$Survey_ID==20] <- "2022-05-03"
      birds$Date[birds$Survey_ID==21] <- "2022-04-12"
      birds$Date[birds$Survey_ID==22] <- "2022-06-03"
      birds$Date[birds$Survey_ID==24] <- "2022-06-10"
      birds$Date[birds$Survey_ID==26] <- "2022-06-01"
      birds$Date[birds$Survey_ID==27] <- "2022-06-13"
      birds$Date[birds$Survey_ID==29] <- "2022-07-01"
      birds$Date[birds$Survey_ID==31] <- "2022-07-04"
      
    #lat/long
      birds$Location <- birds$Survey_ID
      birds$Location[birds$Location==c(3,6,7,8,9,11,15,17,18,19,22,23,24,25,29,30)] 
      birds$Location[birds$Location==3 | 
                       birds$Location==6 |
                       birds$Location==7 |
                       birds$Location==8 |
                       birds$Location==9 |
                       birds$Location==11 |
                       birds$Location==15 |
                       birds$Location==17 |
                       birds$Location==18 |
                       birds$Location==19 |
                       birds$Location==22 |
                       birds$Location==23 |
                       birds$Location==24 |
                       birds$Location==25 |
                       birds$Location==29 |
                       birds$Location==30]<- "Sint-Laureins"
      
      birds$Location[birds$Location==1 | 
                    birds$Location==2 |
                    birds$Location==4 |
                    birds$Location==5 |
                    birds$Location==10 |
                    birds$Location==12 |
                    birds$Location==13 |
                    birds$Location==14 |
                    birds$Location==16 |
                    birds$Location==20 |
                    birds$Location==21 |
                    birds$Location==26 |
                    birds$Location==27 |
                    birds$Location==28 |
                    birds$Location==31 |
                    birds$Location==32 |
                    birds$Location==33] <- "De Luysen"
      
      vtransects$LocationTransectStop <- paste(vtransects$Locatie, vtransects$Transect, vtransects$Stop)
      birds$LocationTransectStop <- paste(birds$Location, birds$Transect, birds$Stop)
      
      birds <- merge(vtransects, birds, by="LocationTransectStop", all.y=T)
      birds <- birds[,c(5,6,8:ncol(birds))]
      colnames(birds)[4] <- "Transect"
      colnames(birds)[5] <- "Stop"
      
    #libellen
      #date
        libellen <- libellen[,c(1:6)]
        libellen$Date <- as.Date("2021-09-21")
        libellen$Date[libellen$Survey_ID==11] <- "2021-06-04"
        libellen$Date[libellen$Survey_ID==12] <- "2021-06-02"
        libellen$Date[libellen$Survey_ID==13] <- "2021-06-16"
        libellen$Date[libellen$Survey_ID==14] <- "2021-06-24"
        libellen$Date[libellen$Survey_ID==15] <- "2021-07-02"
        libellen$Date[libellen$Survey_ID==23] <- "2022-06-03"
        libellen$Date[libellen$Survey_ID==25] <- "2022-06-10"
        libellen$Date[libellen$Survey_ID==28] <- "2022-06-13"
        libellen$Date[libellen$Survey_ID==30] <- "2022-07-01"
        libellen$Date[libellen$Survey_ID==32] <- "2022-07-04"
        libellen$Date[libellen$Survey_ID==33] <- "2022-06-01"
        
      #lat/long
        #ltransects$Part is the same as libellen$Deeltraject, emailed Emma to clarify 2022-09-29. Her response: " The only difference with the birds is that we have a start and a stop since these were line transects and not single points, so maybe use the midway point as the lat/long?" Choose starting point to be consistent with other areas. 
        libellen$TransectPart <- paste(libellen$Transect, libellen$Deeltraject)
        ltransects$TransectPart <- paste(ltransects$Transect, ltransects$Part)
        libellenltransects <- ltransects[,c(7,8,30)]
        libellen <- merge(libellen, libellenltransects, by="TransectPart")
        
#create each GBIF column for Belgian data
    #eventDate
      birds$eventDate <- as.Date(birds$Date)
      libellen$eventDate <- as.Date(libellen$Date)
      plants$eventDate <- as.Date(plants$Date)
        
    #eventTime
      birds$Tijd <- str_pad(birds$Tijd, 5, pad = "0")
      birds$eventTime <-paste(birds$Tijd,"+01:00",sep="")
      libellen$eventTime <-paste(libellen$Tijd,"+01:00",sep="")
      plants$eventTime <- "12:00+01:00" #time is missing from plant data, emailed Emma Cartuyvels 2022-09-29 and was told they did not record date so use 1200 (noon) instead

    #basisOfRecord
      birds$basisOfRecord <-"AcceptedTaxon"
      libellen$basisOfRecord <- "AcceptedTaxon"
      plants$basisOfRecord <- "AcceptedTaxon"
      
    #scientificName, change language to Latin
      #note: these names were taken from 2021 data with any additional 2022 species added
      
      #birds
        birds$scientificName <- birds$Vogel 
        birds$scientificName <-as.character(birds$scientificName)
        birds$scientificName[birds$scientificName=="Tafeleend"] <- "Aythya ferina"
        birds$scientificName[birds$scientificName=="Kleine karekiet"] <- "Acrocephalus scirpaceus"
        birds$scientificName[birds$scientificName=="Roerdomp"] <- "Botaurus stellaris"
        birds$scientificName[birds$scientificName=="Dodaars"] <- "Tachybaptus ruficollis"
        birds$scientificName[birds$scientificName=="Kuifeend"] <- "Aythya fuligula"
        birds$scientificName[birds$scientificName=="Cetti's zanger"] <- "Cettia cetti"
        birds$scientificName[birds$scientificName==""] <- "NA" #77 observation missing
        birds$scientificName[birds$scientificName=="Rietgors"] <- "Emberiza schoeniclus"
        birds$scientificName[birds$scientificName=="Blauwborst"] <- "Luscinia svecica"
        birds$scientificName[birds$scientificName=="Witgatje"] <- "Tringa ochropus"
        birds$scientificName[birds$scientificName=="Slobeend"] <- "Spatula clypeata"
        birds$scientificName[birds$scientificName=="Waterral"] <- "Rallus aquaticus"
        birds$scientificName[birds$scientificName=="Rietzanger"] <- "Acrocephalus schoenobaenus"
        birds$scientificName[birds$scientificName=="Watersnip"] <- "Gallinago gallinago"
        birds$scientificName[birds$scientificName=="Snor"] <- "Locustella luscinioides"
        birds$scientificName[birds$scientificName=="Sprinkhaanzanger"] <- "Locustella naevia"
        birds$scientificName[birds$scientificName=="Wintertaling"] <- "Anas crecca"
        birds$scientificName[birds$scientificName=="Grote karekiet"] <- "Acrocephalus arundinaceus"
        birds$scientificName[birds$scientificName=="rietzanger"] <- "Acrocephalus schoenobaenus"
        birds$scientificName[birds$scientificName=="kleine karekiet"] <- "Acrocephalus scirpaceus"
        birds$scientificName[birds$scientificName=="blauwborst"] <- "Luscinia svecica"
        birds$scientificName[birds$scientificName=="Ijsvogel"] <- "Alcedo atthis"
        birds$scientificName[birds$scientificName=="Bosrietzanger"] <- "Acrocephalus palustris"
        birds$scientificName[birds$scientificName=="kuifeend"] <- "Aythya fuligula"
        birds$scientificName[birds$scientificName=="Oeverloper"] <- "Actitis hypoleucos"
        birds$scientificName[birds$scientificName=="Buine kiekendief"] <- "Circus aeruginosus"
        
        #remove NAs for birds$scientificName
          birds <- subset(birds, !birds$Vogel=="")
        
        #are any scientific names still missing?
        subset(birds, birds$scientificName==NA | birds$scientificName=="")
        unique(birds$scientificName) 
        
        #additional species missing from 2021
        birds$scientificName[birds$scientificName=="Roodborsttapuit"] <- "Saxicola rubicola"
        birds$scientificName[birds$scientificName=="Gele kwikstaart"] <- "Motacilla flava"
        birds$scientificName[birds$scientificName=="Braamsluiper"] <- "Sylvia curruca"
        birds$scientificName[birds$scientificName=="Bruine kiekendief"] <- "Circus aeruginosus"
        birds$scientificName[birds$scientificName=="Krakeend"] <- "Mareca strepera"
        birds$scientificName[birds$scientificName=="Woudaap"] <- "Ixobrychus minutus"
        birds$scientificName[birds$scientificName=="Visdief"] <- "Sterna hirundo"
        birds$scientificName[birds$scientificName=="Witgat"] <- "Tringa ochropus"
        birds$scientificName[birds$scientificName=="Grasmus"] <- "Sylvia communis"
        birds$scientificName[birds$scientificName=="Kwartel"] <- "Coturnix coturnix"
        birds$scientificName[birds$scientificName=="Scholekster"] <- "Haematopus ostralegus"
        
    
    #libellen
    
      libellen$scientificName <- libellen$Soort
      libellen$scientificName <-as.character(libellen$scientificName)
      libellen$scientificName[libellen$scientificName=="Azuurwaterjuffer"] <- "Coenagrion puella"
      libellen$scientificName[libellen$scientificName=="Glassnijder"] <- "Brachytron pratense"
      libellen$scientificName[libellen$scientificName=="Zwervende pantserjuffer"] <- "Lestes barbarus"
      libellen$scientificName[libellen$scientificName=="Bloedrode heidelibel"] <- "Sympetrum sanguineum"
      libellen$scientificName[libellen$scientificName=="Bruine korenbout"] <- "Libellula fulva"
      libellen$scientificName[libellen$scientificName=="Gewone oeverlibel"] <- "Orthetrum cancellatum"
      libellen$scientificName[libellen$scientificName=="Lantaarntje"] <- "Ischnura elegans"
      libellen$scientificName[libellen$scientificName=="Grote roodoogjuffer"] <- "Erythromma najas"
      libellen$scientificName[libellen$scientificName=="Viervlek"] <- "Libellula quadrimaculata"
      libellen$scientificName[libellen$scientificName=="Gewone pantserjuffer"] <- "Lestes sponsa"
      libellen$scientificName[libellen$scientificName=="Bruine winterjuffer"] <- "Sympecma fusca"
      libellen$scientificName[libellen$scientificName=="Watersnuffel"] <- "Enallagma cyathigerum"
      libellen$scientificName[libellen$scientificName=="Variabele waterjuffer"] <- "Coenagrion pulchellum"
      libellen$scientificName[libellen$scientificName=="Smaragdlibel"] <- "Cordulia aenea"
      libellen$scientificName[libellen$scientificName=="Grote keizerlibel"] <- "Anax imperator"
      libellen$scientificName[libellen$scientificName=="Bruinrode heidelibel"] <- "Sympetrum striolatum"
      libellen$scientificName[libellen$scientificName=="Platbuik"] <- "Libellula depressa"
      libellen$scientificName[libellen$scientificName=="Vuurlibel"] <- "Crocothemis erythraea"
      libellen$scientificName[libellen$scientificName=="Vuurjuffer"] <- "Pyrrhosoma nymphula"
      libellen$scientificName[libellen$scientificName=="Breedscheenjuffer"] <- "Platycnemididae"
      libellen$scientificName[libellen$scientificName=="Vroege glazenmaker"] <- "Aeshna isoceles"
      libellen$scientificName[libellen$scientificName=="Zuidelijke keizerlibel"] <- "Anax parthenope"
      libellen$scientificName[libellen$scientificName==""] <- NA #33 observations missing 
      libellen$scientificName[libellen$scientificName=="Weidebeekjuffer"] <- "Calopteryx splendens"
      libellen$scientificName[libellen$scientificName=="Tengere pantserjuffer"] <- "Lestes virens"
      
      #remove NAs for libellen$scientificName
      libellen <- subset(libellen, !libellen$Soort=="")
      
      #are any scientific names still missing?
      subset(libellen, libellen$scientificName==NA | libellen$scientificName=="")
      unique(libellen$scientificName) 
      
      #latin names missing from 2021
      libellen$scientificName[libellen$scientificName=="Platbuiklibel"] <- "Libellula depressa"
      libellen$scientificName[libellen$scientificName=="Viervleklibel"] <- "Libellula quadrimaculata"
      libellen$scientificName[libellen$scientificName=="Variable waterjuffer"] <- "Coenagrionidae" #family level
      libellen$scientificName[libellen$scientificName=="lantaarntje"] <- "Ischnura elegans"
      libellen$scientificName[libellen$scientificName=="Platycnemididae"] <- "Platycnemis pennipes"
      libellen$scientificName[libellen$scientificName=="Blauwe breedscheenjuffer"] <- "Platycnemis pennipes"
      
    #plants
      #alredy in Latin
      plants$scientificName <- plants$Soort
      unique(plants$scientificName)
      #remove NAs for plants$scientificName
      plants <- subset(plants, !plants$Soort=="")

      #a few latin names still need to be changed
      plants$scientificName[plants$scientificName=="Galeopsis bifida/tetrahit"] <- "Galeopsis bifida"
      plants$scientificName[plants$scientificName=="Utricularia sp."] <- "Utricularia" #genus level
      plants$scientificName[plants$scientificName=="Hydrocotyle sp."] <- "Hydrocotyle tripartita"
      plants$scientificName[plants$scientificName=="Myosotis laxa subsp. cespitosa"] <- "Myosotis laxa" #removed subspecies since we are IDing to species level
      
  #kingdom
  birds$kingdom <- "Animalia"
  libellen$kingdom <- "Animalia"
  plants$kingdom <- "Plantae"
  
  #taxonRank- e.g. species, genus, family
  unique(birds$scientificName)
  birds$taxonRank <- "species"
  libellen$taxonRank <- "species"
  plants$taxonRank <- "species"
  libellen$taxonRank[libellen$scientificName=="Coenagrionidae"] <- "family"
  plants$taxonRank[plants$scientificName=="Utricularia"] <- "genus"
  libellen$taxonRank[libellen$scientificName=="Platycnemididae"] <- "family"
  
  #individualCount
  birds$individualCount <- birds$Aantal
  birds$individualCount[birds$individualCount==""] <- NA #1 observation missing
  subset(birds, !complete.cases(birds$individualCount)) #1 observation missing
  
  libellen$individualCount <- libellen$Aantal
  libellen$individualCount[libellen$individualCount==""] <- NA #0 observations missing
  subset(libellen, !complete.cases(libellen$individualCount)) #0 observations missing
  
  plants$individualCount <- "NA" #there is no count, just category with corresponds to % coverage (see Monitoring protocol to determine the ecological effects of Coypu in the research areas of LIFE MICA, page 5)
  
  #decimalLatitude
  birds$decimalLatitude <- birds$Lat
  libellen$decimalLatitude <- libellen$Lat
  plants$decimalLatitude <- plants$Lat
  
  #decimalLongitude
  birds$decimalLongitude <- birds$Long
  libellen$decimalLongitude <- libellen$Long
  plants$decimalLongitude <- plants$Long
  
  #countryCode- e.g. NL, BE, DE
  birds$countryCode <- "BE"
  libellen$countryCode <- "BE"
  plants$countryCode <- "BE"
  
  #plant_Category (not DarwinCore but needed for other data)
  plants$plantCategory <- plants$Categorie
  birds$plantCategory <- NA
  libellen$plantCategory <-NA
  
  #add project area AND species group columns to make it easier to check number of species in each project area later
  libellen$project_area <- NA
  libellen$project_area[libellen$Survey_ID %in% c(12,13,14,28,32,33)] <- 4 #sint laureins
  libellen$project_area[libellen$Survey_ID %in% c(11,15,23,25,30)] <- 5 #de lusysen
  
  birds$project_area <- NA
  birds$project_area[birds$Survey_ID %in% c(3,6,7,8,9,18,19,22,24,29)] <- 4
  birds$project_area[birds$Survey_ID %in% c(1,2,4,5,10,20,21,26,27,31)] <- 5
  
  plants$project_area <- NA
  plants$project_area[plants$Survey_ID %in% c(17)] <- 4
  plants$project_area[plants$Survey_ID %in% c(16)] <- 5
  plants$project_area[plants$Survey_ID %in% c(35)] <- 4
  plants$project_area[plants$Survey_ID %in% c(34)] <- 5
  
  libellen$Group <- "Libellen"
  birds$Group <- "Birds"
  plants$Group <- "Plants"
  
  #change columns order and combine into one Belgian dataset
  birds <- birds[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode", "plantCategory", "project_area", "Group")]
  libellen <- libellen[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  plants <- plants[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  
  be <-rbind(birds, libellen, plants)
  
  #check missing values in Belgian data
  #all missing values make sense based on original dataset, see below for summary
  subset(be, !complete.cases(be$basisOfRecord)) #complete  
  subset(be, !complete.cases(be$eventDate)) #complete 
  subset(be, !complete.cases(be$eventTime)) #complete  
  subset(be, !complete.cases(be$scientificName)) #complete    
  subset(be, !complete.cases(be$kingdom)) #complete  
  subset(be, !complete.cases(be$taxonRank)) #complete  
  subset(be, !complete.cases(be$individualCount)) #1 missing, same as from original dataset  
  subset(be, !complete.cases(be$decimalLatitude)) #complete 
  subset(be, !complete.cases(be$decimalLongitude)) #complete 
  subset(be, !complete.cases(be$countryCode)) #complete  

#2. Germany, areas 1 (Lake Dümmer), 2 (Aschau Teiche), and 3 (Vechtegebiet)
  
  #read in data
    setwd("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/Germany")
    transects <- read.csv("coordinates_transects.csv", header=T, stringsAsFactors = FALSE)
    
    dragonflycoords <- read.csv("general_data_dragonfly.csv", header=T, stringsAsFactors = FALSE)
    damselflycoords <- read.csv("general_data_damselfly.csv", header=T, stringsAsFactors = FALSE)
    birdcoords <- read.csv("general_data_birds.csv", header=T, stringsAsFactors = FALSE)
    plantcoords <- read.csv("general_data_vegetation.csv", header=T, stringsAsFactors = FALSE)
    
    plants <- read.csv("vegetation_surveys.csv", header=T, stringsAsFactors = FALSE)
    birds <- read.csv("bird_surveys.csv", header=T, stringsAsFactors = FALSE)
    libellen <- read.csv("libellen_surveys.csv", header=T, stringsAsFactors = FALSE)
    vechte <- read.csv("Vechte_data_2021_2022.csv", header=T, stringsAsFactors = FALSE, sep = ";")
    
    
    head(transects)
    head(dragonflycoords)  
    head(damselflycoords)
    head(birdcoords)
    head(plantcoords)
    head(plants)
    head(birds)
    head(libellen)
    head(vechte)
    
  #check organization of data, including missing values
    
    #birds
      unique(birds$Area)
      unique(birds$Stop)
      unique(birds$Round)
      unique(birds$Year)
      unique(birds$Month)
      unique(birds$Day)
      unique(birds$Time)
      unique(birds$Amount)
      
      #lat/long
        #birds$Stop is the same as birdcoords$Meters
        #change degrees to decimals
          unique(birdcoords$X.coordinates)
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.414' "] <- 52.86500000
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.393' "] <- 52.85916667
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.442' "] <- 52.87277778
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.495' "] <- 52.88750000
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.535' '"] <- 52.89861111
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.586' "] <- 52.91277778
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.548'"] <- 52.90222222
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.490' "] <- 52.88611111
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.447' "] <- 52.87416667
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.396' "] <- 52.86000000
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.386'"] <- 52.85722222
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°32'37,078\"N "] <- 52.54363278
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°32'39,112\"N "] <- 52.54419778
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°32'43,976\"N "] <- 52.54554889
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°32'44,282\"N "] <- 52.54563389
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°32'45,804\"N "] <- 52.54605667
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°32'46,497\"N "] <- 52.54624917
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°33'36,819\"N "] <- 52.56022750
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°33'42,569\"N"] <- 52.56182472
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°33'48,223\"N "] <- 52.56339528
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°33'54,571\"N "] <- 52.56515861
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°34'0,9\"N "] <- 52.56691667
          birdcoords$X.coordinates[birdcoords$X.coordinates=="52°34'6,968\"N "] <- 52.56860222
          birdcoords$X.coordinates[birdcoords$X.coordinates=="N52° 45.535' "] <- 52.89861111
          
          
          unique(birdcoords$Y.coordinates)
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.281'"] <- 10.34472222
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.199'"] <- 10.32194444
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.180'"] <- 10.31666667
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.184'"] <- 10.31777778
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.195'"] <- 10.32083333
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.218'"] <- 10.32722222
          birdcoords$Y.coordinates[birdcoords$Y.coordinates==" E10° 16.245'"] <- 10.33472222
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.262'"] <- 10.33944444
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.275'"] <- 10.34305556
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="E10° 16.293'"] <- 10.34805556
          birdcoords$Y.coordinates[birdcoords$Y.coordinates==" E10° 16.243'"] <- 10.33416667
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°19'32,869\"E "] <- 8.32579694
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°19'42,483\"E"] <- 8.32846750
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°19'49,81\"E "] <- 8.33050278
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°19'49,78\"E"] <- 8.33049444
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'10,765\"E"] <- 8.33632361
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'21,515\"E"] <- 8.33930972
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'33,573\"E"] <- 8.34265917
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'28,426\"E "] <- 8.34122944
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'23,601\"E"] <- 8.33988917
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'25,253\"E"] <- 8.34034806
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'26,403\"E"] <- 8.34066750
          birdcoords$Y.coordinates[birdcoords$Y.coordinates=="8°20'24,558\"E"] <- 8.34015500
        
          birds$area_meters <- paste(birds$Area, birds$Stop)
          
          birdcoords$Area[birdcoords$Area=="DS1 (am Randkanal)"] <- "DS1"
          birdcoords$Area[birdcoords$Area=="DS2 (an der Hunte)"] <- "DS2"
          birdcoords$area_meters <- paste(birdcoords$Area, birdcoords$Stop)
          birdcoords <- birdcoords[,c("area_meters","X.coordinates","Y.coordinates")]
          
          birds$area_meters[birds$area_meters=="DS1 1"] <- "DS1 0"
          birds$area_meters[birds$area_meters=="DS1 2"] <- "DS1 200"
          birds$area_meters[birds$area_meters=="DS1 3"] <- "DS1 400"
          birds$area_meters[birds$area_meters=="DS1 4"] <- "DS1 600"
          birds$area_meters[birds$area_meters=="DS1 5"] <- "DS1 800"
          birds$area_meters[birds$area_meters=="DS1 6"] <- "DS1 1000"
          
          birds$area_meters[birds$area_meters=="DS2 1"] <- "DS2 0"
          birds$area_meters[birds$area_meters=="DS2 2"] <- "DS2 200"
          birds$area_meters[birds$area_meters=="DS2 3"] <- "DS2 400"
          birds$area_meters[birds$area_meters=="DS2 4"] <- "DS2 600"
          birds$area_meters[birds$area_meters=="DS2 5"] <- "DS2 800"
          birds$area_meters[birds$area_meters=="DS2 6"] <- "DS2 1000"
          
          birds <- merge(birds, birdcoords, by="area_meters", all.x=T)
          subset(birds, birds$Year==2022 & birds$Area.x=="DS1")
          
          subset(birds$area_meters, !complete.cases(birds$X.coordinates))
          
      #date
        #add 0s to Month and Day (where needed)
        birds$Month <- paste(0, birds$Month, sep="")
        
        str(birds)
        unique(birds$Day[birds$Day<10])
        birds$Day <- as.character(birds$Day)
        birds$Day[birds$Day==8] <- "08"
        birds$Day[birds$Day==9] <- "09"
        birds$Day[birds$Day==7] <- "07"
        birds$Day[birds$Day==2] <- "02"
        birds$Day[birds$Day==5] <- "05"
        
        #create date column
        birds$date <-paste(birds$Year,birds$Month,birds$Day, sep="-")
        unique(birds$date)
        
      #time
        unique(birds$Time)
        subset(birds, birds$date=="2022-07-05")
        
        #remove words at end
        birds$Time <-substr(birds$Time, 1, 5)
        
        #add in 0s where needed
        birds$Time[substr(birds$Time,1,1)==c(1:9)]
        birds$Time[birds$Time=="3:40 "] <-"03:40"
        birds$Time[birds$Time=="4:28 "] <-"04:28"
        birds$Time[birds$Time=="3:10 "] <-"03:10"
        birds$Time[birds$Time=="3:09 "] <-"03:09"
        birds$Time[birds$Time=="3:55 "] <-"03:55"
        birds$Time[birds$Time=="3:42 "] <-"03:42"
        birds$Time[birds$Time=="3:24 "] <-"03:24"
        birds$Time[birds$Time=="3:28 "] <-"03:28"
        birds$Time[birds$Time=="4:09 "] <-"04:09"
        birds$Time[birds$Time=="9:30 "] <-"09:30"
        birds$Time[birds$Time=="8:05 "] <-"08:05"
        birds$Time[birds$Time=="7:20 "] <-"07:20"
        birds$Time[birds$Time=="5:09 "] <-"05:09"
        birds$Time[birds$Time=="7:50 "] <-"07:50"
        birds$Time[birds$Time=="7:05 "] <-"07:05"
        birds$Time[birds$Time=="6:00 "] <-"06:00"
        birds$Time[birds$Time=="5:30 "] <-"05:30"
        birds$Time[birds$Time=="8:40 "] <-"08:40"
        birds$Time[birds$Time=="6:40 "] <-"06:40"
        birds$Time[birds$Time=="4:16 "] <-"04:16"
        birds$Time[birds$Time=="4:23 "] <-"04:23"
        birds$Time[birds$Time=="3:54 "] <-"03:54"
        birds$Time[birds$Time=="7:59 "] <-"07:59"
        birds$Time[birds$Time=="7:51 "] <-"07:51"
        
      #species (latin)
        unique(birds$Species.lat)
        birds$Species.lat[birds$Species.lat=="#VALUE!"] <- NA
        birds$Species.lat[birds$Species.lat=="anas?"] <- NA
        
        #note for later, the following are not at species level
          #"Larinae"
          #"Acrocephalus"
          
      #observations ($Amount)
        unique(birds$Amount)
        birds$Amount[birds$Amount=="0" | birds$Amount=="" | birds$Amount=="x" | birds$Amount=="many"] <- NA
        birds$Amount[birds$Amount==">10"] <- "10"    
        birds$Amount[birds$Amount==">1" | birds$Amount=="1+"] <- "1"    
        birds$Amount[birds$Amount=="1+1" | birds$Amount=="1 + 1" | birds$Amount==">2"] <- "2"    
        birds$Amount[birds$Amount=="35+"] <- "35"  
        birds$Amount[birds$Amount=="2 + 2" | birds$Amount=="2+2" | birds$Amount=="3+1"] <- "4"  
        birds$Amount[birds$Amount=="3 + 10(juvenile)"] <- "13"  
        birds$Amount[birds$Amount=="2+1" | birds$Amount=="1+2"] <- "3"  
        birds$Amount[birds$Amount=="20+"] <- "20"  
        birds$Amount[birds$Amount=="2+3" | birds$Amount=="3+2"] <- "5"  
        birds$Amount[birds$Amount=="8 or 10"] <- "8"  
        birds$Amount[birds$Amount=="50+"] <- "50"  
        
      #remove any observations missing date, count, or species (37 removed)
        nrow(birds)
        birds <- subset(birds, complete.cases(birds$Amount) & complete.cases(birds$Species.lat) & complete.cases(birds$date))
      

  #damselflies and dragonflies
        head(libellen)
        unique(libellen$Area)
        unique(libellen$Section)
        unique(libellen$Round)
        unique(libellen$Year)
        unique(libellen$Month)
        unique(libellen$Day)
        unique(libellen$Time)
        unique(libellen$Amount)
        
      #lat/long
        head(dragonflycoords)  
        head(damselflycoords)
        
        
        #sections labelled either blank (damselfly survey) or *dragonfly (dragonfly survey), separate in order to merge with coords
        damselfly <- subset(libellen, libellen$Section=="A" | 
                              libellen$Section=="B" | 
                              libellen$Section=="C" |
                              libellen$Section=="D" |
                              libellen$Section=="E")
  
        dragonfly <- subset(libellen, !libellen$Section=="A" & 
                              !libellen$Section=="B" & 
                              !libellen$Section=="C" &
                              !libellen$Section=="D" &
                              !libellen$Section=="E")
        #remove words from end of $Section
        dragonfly$Section <-substr(dragonfly$Section, 1, 1)
        
        #convert degrees to decimals
        #see coords_de2 from 2021 for conversions of x-wert and y-wert
        coords_de2 <- read.csv("coordinates_de2.csv", header=T)
        
        unique(damselflycoords$X.coordinates)        
        unique(damselflycoords$Y.coordinates)
        
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="N52° 45.414' "] <- 52.86500000
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="N52° 45.403' "] <- 52.86194444
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="N52° 45.393' "] <- 52.85916667
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="N52° 45.411'"] <- 52.86416667
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="N52° 45.442' "] <- 52.87277778
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="X-Wert: 0454423"] <- 52.54363278
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0454460"] <- 52.54419778
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0454493"] <- 52.54554889
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0454536"] <- 52.54563389
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0454564 - 0454598"] <- 52.54605667
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="X-Wert: 0455259"] <- 52.56022750
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0455247"] <- 52.56182472
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0455250"] <- 52.56339528
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0455258"] <- 52.56515861
        damselflycoords$X.coordinates[damselflycoords$X.coordinates=="x-Wert: 0455272 - 0455284"] <- 52.56860222

        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="E10° 16.281'"] <- "10.34472222"
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="E10° 16.199'"] <- "10.32194444"
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="E10° 16.239'"] <- 10.33305556
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="E10° 16.181'"] <- 10.31694444
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="E10° 16.180'"] <- "10.31666667"
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="Y-Wert: 5821747"] <- 8.32579694
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5821780"] <- 8.32846750
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5821817"] <- 8.33050278
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5821849"] <- 8.33049444
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5821896 - 5821932"] <- 8.33632361
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="Y-Wert: 5823891"] <-8.34265917
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5823938"] <- 8.34122944
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5823988"] <- 8.33988917
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5824037"] <- 8.34034806
        damselflycoords$Y.coordinates[damselflycoords$Y.coordinates=="y-Wert: 5824081 - 5824130"] <- 8.34066750

        unique(dragonflycoords$X.coordinates)
        unique(dragonflycoords$Y.coordinates)
        
        #NOTE: filled in NAs after emailing Claudia 20221006, heard back from Lilja Fromme on 20221007, see "Monitoring_Daten_2020_21_22_correction.xlsx"
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="N52° 45.414' "] <- 52.86500000
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="N52° 45.393' "] <- 52.85916667
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="N52° 45.442' "] <- 52.87277778
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="N52° 45.495' "] <- 52.88750000
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="N52° 45.535' "] <- 52.89861111
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="X-Wert: 0454327"] <- 52.543639
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0454423"] <- 52.54363278
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0454493"] <- 52.54554889
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0454564"] <- 52.54605667
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0454643 - 0454743"] <- 52.54575
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0455317"] <- 52.562278
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0455270"] <- 52.563056
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0455246"] <- 52.563917
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0455268"] <- 52.56475
        dragonflycoords$X.coordinates[dragonflycoords$X.coordinates=="x-Wert: 0455290-0455306"] <- 52.565639
        
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="E10° 16.281'"] <- "10.34472222"
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="E10° 16.199'"] <- "10.32194444"
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="E10° 16.180'"] <- "10.31666667"
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="E10° 16.184'"] <- 10.31777778
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="E10° 16.195'"] <- 10.32083333
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="Y-Wert: 5821720"] <- 8.3265
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5821747"] <- 8.32579694
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5821817"] <- 8.33050278
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5821896"] <- 8.33632361
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5821950 - 5821953"] <- 8.331139
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="Y-Wert: 5823783"] <- 8.340833
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5823871"] <- 8.340111
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5823966"] <- 8.33975
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5824059"] <- 8.340083
        dragonflycoords$Y.coordinates[dragonflycoords$Y.coordinates=="y-Wert: 5824157-5824255"] <- 8.340389
          
        #merge with coords data sets
        damselfly <- merge(damselfly, damselflycoords, by="Section")
        dragonfly <- merge(dragonfly, dragonflycoords, by="Section")
        
        libellen <- rbind(damselfly, dragonfly)
        
        
      #date
        #add 0s to Month and Day (where needed)
        libellen$Month <- paste(0, libellen$Month, sep="")
        
        str(libellen)
        unique(libellen$Day[libellen$Day<10])
        libellen$Day <- as.character(libellen$Day)
        libellen$Day[libellen$Day==9] <- "09"
        libellen$Day[libellen$Day==7] <- "07"
        
        #create date column
        libellen$date <-paste(libellen$Year,libellen$Month,libellen$Day, sep="-")
        unique(libellen$date)
        
      #time
        unique(libellen$Time)

        #remove words at end
        libellen$Time <-substr(libellen$Time, 1, 5)
        
      #species (latin)
        unique(libellen$Species.lat)
        libellen$Species.lat[libellen$Species.lat==""] <- NA
        libellen$Species.lat[libellen$Species.lat=="x"] <- NA
        libellen$Species.lat[libellen$Species.lat=="Blue-tailed damselfly (Ischnura elegans)"] <- "Ischnura elegans"
        libellen$Species.lat[libellen$Species.lat=="Azure damselfly (Coenagrion puella)"] <- "Coenagrion puella"
        libellen$Species.lat[libellen$Species.lat=="Variable damselfly (Coenagrion pulchellum)"] <- "Coenagrion pulchellum"
        libellen$Species.lat[libellen$Species.lat=="Sympetrum (sanguineum)"] <- "Sympetrum sanguineum"
        libellen$Species.lat[libellen$Species.lat=="Lestes (sponsa)"] <- "Lestes sponsa"
        libellen$Species.lat[libellen$Species.lat=="Small red-eyed damselfly (Erythromma viridulum)"] <- "Erythromma viridulum"
        libellen$Species.lat[libellen$Species.lat=="Lestes (dryas)"] <- "Lestes dryas"
        libellen$Species.lat[libellen$Species.lat=="Lestes (sponsa/dryas)"] <- "Lestidae"
        libellen$Species.lat[libellen$Species.lat==" Ischnura elegans"] <- "Ischnura elegans"
        libellen$Species.lat[libellen$Species.lat=="Hairy Dragonfly (Brachytron pratense)"] <- "Brachytron pratense"
        libellen$Species.lat[libellen$Species.lat=="Sympetrum spec."] <- "Sympetrum"
        libellen$Species.lat[libellen$Species.lat=="Calopteryx splendens "] <- "Calopteryx splendens"
        libellen$Species.lat[libellen$Species.lat=="Lestens sponsa"] <- "Lestes sponsa"
        
        #note for later, the following are not at species level
        #"Sympetrum"
        #"Coenargium"
        #"Lestidae"
    
      #observations ($Amount)
        unique(libellen$Amount)
        libellen$Amount[libellen$Amount==">5"] <- "5"    
        libellen$Amount[libellen$Amount==">3"] <- "3"    
        libellen$Amount[libellen$Amount==">2"] <- "2"    
        libellen$Amount[libellen$Amount==">10"] <- "10"    
        libellen$Amount[libellen$Amount=="ca. 30-40"] <- "30"    
        libellen$Amount[libellen$Amount=="none"] <- NA   
        
        #remove any observations missing date, count, or species (4 removed)
        nrow(libellen)
        libellen <- subset(libellen, complete.cases(libellen$Amount) & complete.cases(libellen$Species.lat) & complete.cases(libellen$date))
        
        
  #vegetation
        head(plants)
        unique(plants$Sub.location..Area.)
        unique(plants$PQ.number)
        unique(plants$Round)
        unique(plants$Year)
        unique(plants$Month)
        unique(plants$Day)
        unique(plants$Time)
        unique(plants$Category)
        
        
      #lat/long 
        #https://www.latlong.net/degrees-minutes-seconds-to-decimal-degrees
        head(plantcoords)
        head(plants)
        
        unique(plants$area_PQ)
        unique(plantcoords$area_PQ)
        
        
        plants$area_PQ <- paste(plants$Sub.location..Area., plants$PQ.number, sep="")
        plantcoords$area_PQ <- paste(plantcoords$Sub.location..Area., plantcoords$PQ.number, sep="")
        plants <- merge(plants, plantcoords, by="area_PQ")
        unique(plants$X.coordinates)
        unique(plants$Y.coordinates)
        
        plants$X.coordinates[plants$X.coordinates=="N52° 45.419'"] <- 52.756983
        plants$X.coordinates[plants$X.coordinates=="N52° 45.575'"] <- 52.759583
        plants$X.coordinates[plants$X.coordinates=="N52° 45.444' "] <- 52.7574
        plants$X.coordinates[plants$X.coordinates=="N52° 45.659'"] <- 52.760983
        plants$X.coordinates[plants$X.coordinates=="N52° 32.668'"] <- 52.544467
        plants$X.coordinates[plants$X.coordinates=="N52° 32.683'"] <- 52.544717
        plants$X.coordinates[plants$X.coordinates=="N52° 33.848'"] <- 52.564133
        plants$X.coordinates[plants$X.coordinates=="N52° 33.870'"] <- 52.5645
        
        plants$Y.coordinates[plants$Y.coordinates=="E10° 16.271'"] <- 10.271183
        plants$Y.coordinates[plants$Y.coordinates=="E10° 16.207'"] <- 10.270117
        plants$Y.coordinates[plants$Y.coordinates=="E10° 16.356'"] <- 10.2726
        plants$Y.coordinates[plants$Y.coordinates=="E10° 16.527'"] <- 10.27545
        plants$Y.coordinates[plants$Y.coordinates==" E8° 19.753'"] <- 8.329217
        plants$Y.coordinates[plants$Y.coordinates=="E8° 19.796'"] <- 8.329933
        plants$Y.coordinates[plants$Y.coordinates=="E8° 20.420'"] <- 8.340333
        plants$Y.coordinates[plants$Y.coordinates=="E8° 20.395'"] <- 8.339917
          
        
      #date
        #add 0s to Month and Day (where needed)
        plants$Month <- paste(0, plants$Month, sep="")
        
        str(plants)
        unique(plants$Day[plants$Day<10])
        plants$Day <- as.character(plants$Day)
        plants$Day[plants$Day==6] <- "06"

        #create date column
        plants$date <-paste(plants$Year,plants$Month,plants$Day, sep="-")
        unique(plants$date)
  
      #time
        unique(plants$Time)
        
        #remove words at end
        plants$Time <-substr(plants$Time, 1, 5)
        
        #substitutes/ NAs
        plants$Time[plants$Time==""] <- NA
        plants$Time[plants$Time=="9:40"] <- "09:40"
        plants$Time[plants$Time=="9:45"] <- "09:45"
        
      #species (latin)
        unique(plants$Species..lat.)
        plants$Species..lat.[plants$Species..lat.=="#VALUE!"] <- NA

        #note for later, the following are not at species level
        #"Lycopus"
        #"Urtica"
  
  #create each GBIF column for German data
        
        #eventDate
        birds$eventDate <- as.Date(birds$date)
        libellen$eventDate <- as.Date(libellen$date)
        plants$eventDate <- as.Date(plants$date)

          #NOTE: datasheets include all data collected as of 2022-09-29, 2020-2021 included. Will only include 2022 data here since previous data already organized
          birds <-subset(birds, birds$Year==2022)
          plants <-subset(plants, plants$Year==2022)
          libellen <-subset(libellen, libellen$Year==2022)
          
          
        #eventTime

        birds$eventTime <-paste(birds$Time,"+01:00",sep="")
        libellen$eventTime <-paste(libellen$Time,"+01:00",sep="")
        plants$eventTime <-paste(plants$Time,"+01:00",sep="")
        vechte$eventTime <-paste(vechte$eventTime,"+01:00",sep="")
        vechte$eventTime[vechte$eventTime=="NA+01:00"] <- NA
        
        #basisOfRecord
        birds$basisOfRecord <-"AcceptedTaxon"
        libellen$basisOfRecord <- "AcceptedTaxon"
        plants$basisOfRecord <- "AcceptedTaxon"
        vechte$basisOfRecord <- "AcceptedTaxon"
        
        #scientificName, change language to Latin
        #note: these names were taken from 2021 data with any additional 2022 species added
        birds$scientificName <- birds$Species.lat
        libellen$scientificName <- libellen$Species.lat
        plants$scientificName <- plants$Species..lat.
          #remove NAs for species, removed 2 observations
          nrow(plants)
          plants <- subset(plants, complete.cases(plants$scientificName)==T)
          #are any scientific names still missing?
          subset(libellen, libellen$scientificName==NA | libellen$scientificName=="") #nothing missing

        #kingdom
        birds$kingdom <- "Animalia"
        libellen$kingdom <- "Animalia"
        plants$kingdom <- "Plantae"
        
        #taxonRank- e.g. species, genus, family
        birds$taxonRank <- "species"
        libellen$taxonRank <- "species"
        plants$taxonRank <- "species"
        vechte$taxonRank <- "species"
        
        birds$taxonRank[birds$scientificName=="Larinae"] <- "family"
        birds$taxonRank[birds$scientificName=="Acrocephalus"] <- "genus"
        
        libellen$taxonRank[libellen$scientificName=="Sympetrum"] <- "genus"
        libellen$scientificName[libellen$scientificName=="Coenargium"] <- "Coenagrion" #correct the spelling
        libellen$taxonRank[libellen$scientificName=="Coenagrion"] <- "genus"
        libellen$taxonRank[libellen$scientificName=="Lestidae"] <- "family"
        
        plants$taxonRank[plants$scientificName=="Lycopus"] <- "genus"
        plants$taxonRank[plants$scientificName=="Urtica"] <- "genus"
        
        #individualCount
        birds$individualCount <- as.numeric(birds$Amount)
        
        libellen$individualCount <-as.numeric(libellen$Amount)
        
        plants$individualCount <- "NA" #there is no count, just category with corresponds to % coverage (see Monitoring protocol to determine the ecological effects of Coypu in the research areas of LIFE MICA, page 5)
        
        #decimalLatitude
        birds$decimalLatitude <- birds$X.coordinates
        libellen$decimalLatitude <- libellen$X.coordinates
        plants$decimalLatitude <- plants$X.coordinates
        
        #decimalLongitude
        birds$decimalLongitude <- birds$Y.coordinates
        libellen$decimalLongitude <- libellen$Y.coordinates
        plants$decimalLongitude <- plants$Y.coordinates
        
        #countryCode- e.g. NL, BE, DE
        birds$countryCode <- "DE"
        libellen$countryCode <- "DE"
        plants$countryCode <- "DE"
        
        #plant_Category (not DarwinCore but needed for other data)
        plants$plantCategory <- plants$Category
        birds$plantCategory <- NA
        libellen$plantCategory <-NA
        vechte$plantCategory <-NA
        
        #add project area and group columns  (not DarwinCore but needed for other data)
        libellen$project_area <- NA
        libellen$decimalLatitude <- as.numeric(libellen$decimalLatitude)
        libellen$decimalLongitude <- as.numeric(libellen$decimalLongitude)
        libellen$project_area[libellen$decimalLongitude<8.8 & libellen$decimalLongitude>8.2] <- 1
        libellen$project_area[libellen$decimalLongitude>8.8] <- 2
        libellen$project_area[libellen$decimalLatitude>52.58 & libellen$decimalLatitude<52.65 & libellen$decimalLongitude<6.95 & libellen$decimalLongitude>6.74] <- 3
        
        birds$project_area <- NA
        birds$decimalLatitude <- as.numeric(birds$decimalLatitude)
        birds$decimalLongitude <- as.numeric(birds$decimalLongitude)
        birds$project_area[birds$decimalLongitude<8.8 & birds$decimalLongitude>8.2] <- 1
        birds$project_area[birds$decimalLongitude>8.8] <- 2
        birds$project_area[birds$decimalLatitude>52.58 & birds$decimalLatitude<52.65 & birds$decimalLongitude<6.95 & birds$decimalLongitude>6.74] <- 3
        
        plants$project_area <- NA
        plants$decimalLatitude <- as.numeric(plants$decimalLatitude)
        plants$decimalLongitude <- as.numeric(plants$decimalLongitude)
        plants$project_area[plants$decimalLongitude<8.8 & plants$decimalLongitude>8.2] <- 1
        plants$project_area[plants$decimalLongitude>8.8] <- 2
        plants$project_area[plants$decimalLatitude>52.58 & plants$decimalLatitude<52.65 & plants$decimalLongitude<6.95 & plants$decimalLongitude>6.74] <- 3
        
        vechte$project_area <- 3
        
        libellen$Group <- "Libellen"
        birds$Group <- "Birds"
        plants$Group <- "Plants"

        #change columns order and combine into one Belgian dataset
        birds <- birds[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
        libellen <- libellen[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
        plants <- plants[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
        vechte <- vechte[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
        
        de <-rbind(birds, libellen, plants, vechte)

#3. Netherlands (area 10, Gelderse Poort/ Kreis Kleve)
  #read in data
    setwd("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/Netherlands")
    plants <- read.csv("Life MICA Biodiversity Surveys Data Collection Sheet - Vegetation.csv", header=T, stringsAsFactors = FALSE)
    birds <- read.csv("Life MICA Biodiversity Surveys Data Collection Sheet - Birds.csv", header=T, stringsAsFactors = FALSE)
    dragonflies <- read.csv("Life MICA Biodiversity Surveys Data Collection Sheet - Dragonflies.csv", header=T, stringsAsFactors = FALSE)
    damselflies <- read.csv("Life MICA Biodiversity Surveys Data Collection Sheet - Damselflies.csv", header=T, stringsAsFactors = FALSE)
    
    head(plants)
    head(birds)
    head(dragonflies)
    head(damselflies)
  
  #data comes from appsheet (https://docs.google.com/spreadsheets/d/1ydnsMrVArbXe3w27mXHh3fXlDhFPvuV5oHc1kBLGZyU/edit#gid=15633191), only include bird data from Jeroen and Harmen
    birds <- subset(birds, birds$Observer=="Harmen verboom & Jeroen Gruijters" | birds$Observer=="Harmen verboom" | birds$Observer=="Jeroen Gruijters")
  
  #date
    plants$Date <-parse_date_time(plants$Date, "%d/%m/%Y")
    birds$Date <-parse_date_time(birds$Date, "%d/%m/%Y")
    dragonflies$Date <-parse_date_time(dragonflies$Date, "%d/%m/%Y")
    damselflies$Date <-parse_date_time(damselflies$Date, "%d/%m/%Y")
    
  #lat/long
    plants <- separate(plants, col=ID, into=c('lat', 'long'), sep=",")
    #The lat/long given do not make sense, Emailed Jeroen/Haarmen on 20220130 asking them to revise the lat/long in the ID column of the vegetation surveys: https://docs.google.com/spreadsheets/d/1ydnsMrVArbXe3w27mXHh3fXlDhFPvuV5oHc1kBLGZyU/edit#gid=206619484
    #Haarmen emailed me to use the same coordinates as 2021, used beginning point of surveys (PQ1), since PQ or other linking columns not included in the plants csv to match to 2021 data
    #see data/2021/Netherlands/coords_nl.csv for data (numbers used here)
    plants$lat[plants$Location=="Aerdt"] <- 51.8954965
    plants$long[plants$Location=="Aerdt"] <- 6.09157318
    plants$lat[plants$Location=="Gemaal"] <- 51.91405754
    plants$long[plants$Location=="Gemaal"] <- 6.00306136
    
    birds <- separate(birds, col=Location, into=c('lat', 'long'), sep=" ")
    birds$long <- str_replace(birds$long, "\n", "")
    birds$long <- str_replace(birds$long, ",", ".")
    birds$lat<- str_replace(birds$lat, ",", ".")
    birds$lat<- str_replace(birds$lat, ",", "")
    
    dragonflies <- separate(dragonflies, col=ID, into=c('lat', 'long'), sep=" ")
    dragonflies$long <- str_replace(dragonflies$long, "\n", "")
    dragonflies$long <- str_replace(dragonflies$long, ",", ".")
    dragonflies$lat<- str_replace(dragonflies$lat, ",", ".")
    dragonflies$lat<- str_replace(dragonflies$lat, ",", "")
    
    damselflies <- separate(damselflies, col=ID, into=c('lat', 'long'), sep=" ")
    damselflies$long <- str_replace(damselflies$long, "\n", "")
    damselflies$long <- str_replace(damselflies$long, ",", ".")
    damselflies$lat<- str_replace(damselflies$lat, ",", ".")
    damselflies$lat<- str_replace(damselflies$lat, ",", "")

  #create each GBIF column for Netherlands data
    #eventDate
      birds$eventDate <- as.Date(birds$Date)
      dragonflies$eventDate <- as.Date(dragonflies$Date)
      damselflies$eventDate <- as.Date(damselflies$Date)
      plants$eventDate <- as.Date(plants$Date)
    
    #eventTime, note: birds are the only ones padded with 0s here because all other data was recorded the afternoon
      birds$Time <- str_pad(birds$Time, 5, pad = "0")
      birds$eventTime <-paste(birds$Time, "+01:00",sep="")
      dragonflies$eventTime <-paste(dragonflies$Time,"+01:00",sep="")
      damselflies$eventTime <-paste(damselflies$Time,"+01:00",sep="")
      plants$eventTime <- paste(plants$Time,"+01:00",sep="")
      
    #basisOfRecord
      birds$basisOfRecord <-"AcceptedTaxon"
      dragonflies$basisOfRecord <- "AcceptedTaxon"
      damselflies$basisOfRecord <- "AcceptedTaxon"
      plants$basisOfRecord <- "AcceptedTaxon"
      
    #scientificName

      birds$scientificName <- birds$Species
      birds$scientificName <-as.character(birds$scientificName)
    
      dragonflies$scientificName <- dragonflies$Species
      dragonflies$scientificName <-as.character(dragonflies$scientificName)
      
      damselflies$scientificName <- damselflies$Species
      damselflies$scientificName <-as.character(damselflies$scientificName)
      damselflies$scientificName[damselflies$scientificName==""] <- NA
      damselflies$scientificName[damselflies$scientificName=="erythromma najas"] <- "Erythromma najas"
      
      plants$scientificName <- plants$Species 
      plants$scientificName <-as.character(plants$scientificName)
      plants$scientificName[plants$scientificName=="Reed"] <- "Phragmites australis"
      plants$scientificName[plants$scientificName=="Reed grass"] <- "Phalaris arundinacea"
      plants$scientificName[plants$scientificName=="Reed Grass"] <- "Phalaris arundinacea"
      plants$scientificName[plants$scientificName=="Bulrush"] <- "Scirpoides holoschoenus"
      plants$scientificName[plants$scientificName=="Lesser bulrush"] <- "Typha angustifolia"
      plants$scientificName[plants$scientificName=="Branched burreed"] <- "Sparganium erectum"
      
      #remove NAs for species (checked others and no other NAs)
      nrow(damselflies)
      damselflies <- subset(damselflies, complete.cases(damselflies$scientificName)==T) #removed 1 row
      
    #kingdom
      birds$kingdom <- "Animalia"
      dragonflies$kingdom <- "Animalia"
      damselflies$kingdom <- "Animalia"
      plants$kingdom <- "Plantae"
      
    #taxonRank- e.g. species, genus, family
      birds$taxonRank <- "species"
      dragonflies$taxonRank <- "species"
      damselflies$taxonRank <- "species"
      plants$taxonRank <- "species"
    
    #individualCount
      birds$individualCount <- as.numeric(birds$Count)
      dragonflies$individualCount <-as.numeric(dragonflies$Count)
      damselflies$individualCount <-as.numeric(damselflies$Count)
      plants$individualCount <- "NA" #there is no count, just category with corresponds to % coverage (see Monitoring protocol to determine the ecological effects of Coypu in the research areas of LIFE MICA, page 5)
      
    #decimalLatitude
      birds$decimalLatitude <- birds$lat
      dragonflies$decimalLatitude <- dragonflies$lat
      damselflies$decimalLatitude <- damselflies$lat
      plants$decimalLatitude <- plants$lat
    
    #decimalLongitude
      birds$decimalLongitude <- birds$long
      dragonflies$decimalLongitude <- dragonflies$long
      damselflies$decimalLongitude <- damselflies$long
      plants$decimalLongitude <- plants$long
    
    #countryCode- e.g. NL, BE, DE
      birds$countryCode <- "NL"
      dragonflies$countryCode <- "NL"
      damselflies$countryCode <- "NL"
      plants$countryCode <- "NL"
      
    #plant_Category (not DarwinCore but needed for other data)
      plants$plantCategory <- plants$Category
      birds$plantCategory <- NA
      dragonflies$plantCategory <-NA
      damselflies$plantCategory <-NA
      
    #add project area and group columns  (not DarwinCore but needed for other data)
      dragonflies$project_area <- 10
      damselflies$project_area <- 10
      birds$project_area <- 10
      plants$project_area <- 10
      
      
      dragonflies$Group <- "Libellen"
      damselflies$Group <- "Libellen"
      birds$Group <- "Birds"
      plants$Group <- "Plants"
      
    #change columns order and combine into one NL dataset
      birds <- birds[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
      dragonflies <- dragonflies[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
      damselflies <- damselflies[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
      plants <- plants[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
    
      nl <-rbind(birds, dragonflies, damselflies, plants)
    
      
#combine datasets from the three countries
  #change to date format to rbind
  be$eventDate <- as.Date(be$eventDate)
  nl$eventDate <- as.Date(nl$eventDate)
  de$eventDate <- as.Date(de$eventDate)
  all <- rbind(be, de, nl) #NOTE: don't forget to add in NL once I have data
  
  #add column occurenceID
  all$occurenceID <- paste("https://hetwaterschapshuis.sharepoint.com/sites/1084/Dashboard/Forms/AllItems.aspx?csf=1&web=1&e=Y5CqNG%2F&cid=672837e7%2D83d8%2D42ed%2D8bba%2D244d58943c2b&RootFolder=%2Fsites%2F1084%2FDashboard%2FNetherlands&FolderCTID=0x0120003F39983DD750B64A9A79D97E7303C9A7",formatC(1:nrow(all), width=4, flag = "0"), sep="/") 
  
  #double check data for inconsistencies
  all$individualCount[all$individualCount=="Meerdere"] <- NA
  
  #check NAs
  subset(all, !complete.cases(all$basisOfRecord)) #complete
  nrow(subset(all, !complete.cases(all$eventDate)))  #complete
  nrow(subset(all, !complete.cases(all$eventTime)))  #time missing from 97 observations at area 3 (DE)
  nrow(subset(all, !complete.cases(all$scientificName)))  #complete
  subset(all, !complete.cases(all$kingdom)) #complete
  subset(all, !complete.cases(all$taxonRank)) #complete
  nrow(subset(all, !complete.cases(all$individualCount))) #2 missing, both from BE, consistent
  nrow(subset(all, !complete.cases(all$decimalLatitude))) #complete
  nrow(subset(all, !complete.cases(all$decimalLongitude))) #complete
  subset(all, !complete.cases(all$countryCode)) #complete
  
  nrow(all) #3573 total observations
  nrow(subset(all, complete.cases(all))) #3443 with all columns complete
  
  #check all coordinates make sense
  # creating a sample data.frame with your lat/lon points
  longitude <- as.numeric(all$decimalLongitude)
  latitude <- as.numeric(all$decimalLatitude)
  df <- as.data.frame(cbind(longitude,latitude))
  df <- subset(df, complete.cases(df)==T)
  coordinates(df) <- ~longitude+latitude
  leaflet(df) %>% addMarkers() %>% addTiles() #map looks good!
  
  #another prettier version
    # Create data frame with latitude and longitude
    library(leaflet)
    library(leaflet.extras)
    library(mapview)
    
    # Create data frame with latitude and longitude
    longitude <- as.numeric(all$decimalLongitude)
    latitude <- as.numeric(all$decimalLatitude)
    df <- data.frame(longitude, latitude)
    df <- subset(df, complete.cases(df))
    
    # Convert data frame to spatial points data frame
    coordinates(df) <- ~longitude + latitude
    
    # Create leaflet map
    m <- leaflet(df) %>%
      
      # Add markers for each point
      addMarkers() %>% 
      
      # Add standard OpenStreetMap tiles
      addTiles() %>% 
      
      # Add scale bar
      addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%
      
      # Add compass
      addCompass(position = "bottomleft") %>%
      
      # Add lat/long lines
      addGraticule(step = 0.5, stroke = TRUE, weight = 0.5, color = "#666666", opacity = 0.5) %>%
      
      # Customize color scheme
      addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addProviderTiles(providers$Esri.WorldImagery)
    
    m  # Show map
    
    
  
  
  #write csv file, LifeMICA_GBIF_2022_complete.csv includes NAs
  write.csv(all, file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/LifeMICA_GBIF_2022_complete.csv")
  
  #LifeMICA_GBIF_2022_subset.csv excludes NAs
  sub_all <- all[complete.cases(all), ]
  write.csv(sub_all, file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2022/LifeMICA_GBIF_2022_subset_complete_cases.csv")
  

  #check why there is plant data missing from areas 4 and 5, no longer an issue 
  subset(all$eventDate, all$Group=="Plants" & all$project_area==4) 
  subset(all$eventDate, all$Group=="Plants" & all$project_area==5) 
  
  
 
  
  
  
  
  
  
