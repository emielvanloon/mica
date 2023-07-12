#Organizing 2021 data for Life MICA biodiversity surveys for GBIF
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

#Belgium, areas 4 (Sint-Laureins) and 5 (Sint-Maaartensheide- De Luysen)
#read in Belgian data
  #data update by Emma Cartuyvels on 20220120 (original data is 'transects1.csv')
  survey <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Belgium/surveys1.csv", header=T, stringsAsFactors = FALSE)
  transect <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Belgium/transects2.csv", header=T, stringsAsFactors = FALSE)
  lib <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Belgium/libellen1.csv", header=T, stringsAsFactors = FALSE)
  bird <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Belgium/vogels1.csv", header=T,stringsAsFactors = FALSE )
  
#clean up Belgian dataset, selecting only necessary columns and renaming in order to merge
  survey <- survey[,1:3]  
  colnames(survey)[1] <- "Survey_ID"
  colnames(survey)[2] <- "Date"

  transect$TransectStop <- paste(transect$Locatie, transect$Transect, transect$Part)
  transect <- subset(transect, transect$Punt=="start")
  transect <- transect[,c("Locatie","Long","Lat","TransectStop")]

  bird <- bird[,1:6]
  lib <- lib[,c(1:3,6:8)]
  
#merge Belgian datasets by transect stop in order to obtain latitude and longitude for each observation
  birds <-merge(bird, survey, by="Survey_ID", all.x=T)
  birds$TransectStop <- paste(birds$Locatie, birds$Transect, birds$Stop)
  birds <-merge(birds, transect, by="TransectStop", all=T)
  
  libs <-merge(lib, survey, by="Survey_ID", all.x=T)
  libs$TransectStop <- paste(libs$Locatie, libs$Transect, libs$Deeltraject)
  libs <-merge(libs, transect, by="TransectStop", all=T) #FOLLOWED UP WITH EMMA TO SEE WHY THERE IS A STOP #10 IN THE LIBELLEN SURVEYS BUT NOT IN THE TRANSECTS DATA

  be_od4 <- subset(libs, libs$Survey_ID %in% c(3,6,7,8,9,11,15))
  be_od5 <- subset(libs, libs$Survey_ID %in% c(1,2,4,5,10,12,13,14))
  length(unique(be_od4$Soort)) #9 species
  length(unique(be_od5$Soort)) #25 species
  
#create each GBIF columns for Belgian data
  #eventDate
  birds$eventDate <- birds$Date
  libs$eventDate <-libs$Date

  #eventTime
  library(stringr)
  birds$Tijd <- str_pad(birds$Tijd, 5, pad = "0")
  birds$eventTime <-paste(birds$Tijd,"+01:00",sep="")
  libs$eventTime <-paste(libs$Tijd,"+01:00",sep="")

  #basisOfRecord
  birds$basisOfRecord <-"AcceptedTaxon"
  libs$basisOfRecord <- "AcceptedTaxon"

  #scientificName, change language to Latin
  birds$scientificName <- birds$Vogel 
  birds$scientificName <-as.character(birds$scientificName)
  birds$scientificName[birds$scientificName=="Tafeleend"] <- "Aythya ferina"
  birds$scientificName[birds$scientificName=="Kleine karekiet"] <- "Acrocephalus scirpaceus"
  birds$scientificName[birds$scientificName=="Roerdomp"] <- "Botaurus stellaris"
  birds$scientificName[birds$scientificName=="Dodaars"] <- "Tachybaptus ruficollis"
  birds$scientificName[birds$scientificName=="Kuifeend"] <- "Aythya fuligula"
  birds$scientificName[birds$scientificName=="Cetti's zanger"] <- "Cettia cetti"
  birds$scientificName[birds$scientificName==""] <- "NA" #28 observation missing
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
  
  libs$scientificName <- libs$Soort
  libs$scientificName <-as.character(libs$scientificName)
  libs$scientificName[libs$scientificName=="Azuurwaterjuffer"] <- "Coenagrion puella"
  libs$scientificName[libs$scientificName=="Glassnijder"] <- "Brachytron pratense"
  libs$scientificName[libs$scientificName=="Zwervende pantserjuffer"] <- "Lestes barbarus"
  libs$scientificName[libs$scientificName=="Bloedrode heidelibel"] <- "Sympetrum sanguineum"
  libs$scientificName[libs$scientificName=="Bruine korenbout"] <- "Libellula fulva"
  libs$scientificName[libs$scientificName=="Gewone oeverlibel"] <- "Orthetrum cancellatum"
  libs$scientificName[libs$scientificName=="Lantaarntje"] <- "Ischnura elegans"
  libs$scientificName[libs$scientificName=="Grote roodoogjuffer"] <- "Erythromma najas"
  libs$scientificName[libs$scientificName=="Viervlek"] <- "Libellula quadrimaculata"
  libs$scientificName[libs$scientificName=="Gewone pantserjuffer"] <- "Lestes sponsa"
  libs$scientificName[libs$scientificName=="Bruine winterjuffer"] <- "Sympecma fusca"
  libs$scientificName[libs$scientificName=="Watersnuffel"] <- "Enallagma cyathigerum"
  libs$scientificName[libs$scientificName=="Variabele waterjuffer"] <- "Coenagrion pulchellum"
  libs$scientificName[libs$scientificName=="Smaragdlibel"] <- "Cordulia aenea"
  libs$scientificName[libs$scientificName=="Grote keizerlibel"] <- "Anax imperator"
  libs$scientificName[libs$scientificName=="Bruinrode heidelibel"] <- "Sympetrum striolatum"
  libs$scientificName[libs$scientificName=="Platbuik"] <- "Libellula depressa"
  libs$scientificName[libs$scientificName=="Vuurlibel"] <- "Crocothemis erythraea"
  libs$scientificName[libs$scientificName=="Vuurjuffer"] <- "Pyrrhosoma nymphula"
  libs$scientificName[libs$scientificName=="Breedscheenjuffer"] <- "Platycnemididae"
  libs$scientificName[libs$scientificName=="Vroege glazenmaker"] <- "Aeshna isoceles"
  libs$scientificName[libs$scientificName=="Zuidelijke keizerlibel"] <- "Anax parthenope"
  libs$scientificName[libs$scientificName==""] <- NA #33 observations missing 
  libs$scientificName[libs$scientificName=="Weidebeekjuffer"] <- "Calopteryx splendens"
  libs$scientificName[libs$scientificName=="Tengere pantserjuffer"] <- "Lestes virens"
  
  #kingdom
  birds$kingdom <- "Animalia"
  libs$kingdom <- "Animalia"
  
  #taxonRank- e.g. species, genus, family
  birds$taxonRank <- "species"
  libs$taxonRank <- "species"
  libs$taxonRank[libs$scientificName=="Platycnemididae"] <- "family"
  
  #individualCount
  birds$individualCount <- birds$Aantal
  birds$individualCount[birds$individualCount==""] <- NA #28 observations missing
  
  libs$individualCount <- libs$Aantal
  libs$individualCount[libs$individualCount==""] <- NA #33 observations missing
  
  #decimalLatitude
  birds$decimalLatitude <- birds$Lat
  libs$decimalLatitude <- libs$Lat
  
  #decimalLongitude
  birds$decimalLongitude <- birds$Long
  libs$decimalLongitude <- libs$Long
  
  #countryCode- e.g. NL, BE, DE
  birds$countryCode <- "BE"
  libs$countryCode <- "BE"

  #check if the number of species in each project area is still the same
  be_od4 <- subset(libs, libs$Survey_ID %in% c(3,6,7,8,9,11,15))
  be_od5 <- subset(libs, libs$Survey_ID %in% c(1,2,4,5,10,12,13,14))
  length(unique(be_od4$Soort)) #9 species, yes it is the same
  length(unique(be_od5$Soort)) #25 species, yes it is the same
  
  #add project area AND species group columns to make it easier to check number of species in each project area later
  libs$project_area <- NA
  libs$project_area[libs$Survey_ID %in% c(12,13,14,28,32,33)] <- 4 #sint laureins
  libs$project_area[libs$Survey_ID %in% c(11,15,23,25,30)] <- 5 #de lusysen
  
  birds$project_area <- NA
  birds$project_area[birds$Survey_ID %in% c(3,6,7,8,9,18,19,22,24,29)] <- 4
  birds$project_area[birds$Survey_ID %in% c(1,2,4,5,10,20,21,26,27,31)] <- 5
  
  libs$Group <- "Libellen"
  birds$Group <- "Birds"
  
  #change columns order and combine into one Belgian dataset
  birds <- birds[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode", "project_area", "Group")]
  libs <- libs[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode", "project_area", "Group")]
  be <-rbind(birds, libs)
  
  #check if areas 4 and 5 still have same number of species 
    length(unique(be$scientificName[be$Group=="Libellen" & be$project_area==4 & complete.cases(be$scientificName)])) #24, one missing but it was "" (blank)
    length(unique(be$scientificName[be$Group=="Libellen" & be$project_area==5 & complete.cases(be$scientificName)])) #8, one missing but it was "" (blank)
  
#plant_Category (not DarwinCore but needed for other data)
  be$plantCategory <- NA
  
#check missing values in Belgian data
  #all missing values make sense based on original dataset, see below for summary
  subset(be, !complete.cases(be$basisOfRecord)) #complete  
  length(subset(be, !complete.cases(be$eventDate))) #13 missing
  subset(be, !complete.cases(be$eventTime)) #complete  
  length(subset(be, !complete.cases(be$scientificName))) #13 missing
  subset(be, !complete.cases(be$kingdom)) #complete  
  subset(be, !complete.cases(be$taxonRank)) #complete  
  length(subset(be, !complete.cases(be$individualCount))) #13 missing 
  length(subset(be, !complete.cases(be$decimalLatitude))) #13 missing, all have NAs for scientificName  
  length(subset(be, !complete.cases(be$decimalLongitude))) #13 missing, all have NAs for scientificName  
  subset(be, !complete.cases(be$countryCode)) #complete  
  
  #remove extra NAs from merging
  be <-  subset(be, complete.cases(be$eventDate)) 
  be <-  subset(be, complete.cases(be$scientificName)) 
  be <-  subset(be, complete.cases(be$individualCount)) 
  be <-  subset(be, complete.cases(be$decimalLatitude)) 
  be <-  subset(be, complete.cases(be$decimalLongitude)) 
  
####
  
#Netherlands, area 10 (Gelderse Poort/Kreis Kleve)

#read in data
  #Updated data given to my by Harmen Verboom on 200220124 with updated latitude and longitudes
    #original data 'monitoring_vegetation_nl.csv'
  veg_nl1 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Netherlands/monitoring_vegetation_nl.csv", header=T)
  veg_nl1 <-veg_nl1[,-c(13)]
  veg_nl2 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Netherlands/monitoring_vegetation_nl2.csv", header=T)
  veg_nl <- rbind(veg_nl1, veg_nl2)
  dragon_nl <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Netherlands/monitoring_dragonflies_nl.csv", header=T)
  damsel_nl <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Netherlands/monitoring_damselflies_nl.csv", header=T)
  bird_nl <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Netherlands/monitoring_birds_nl.csv", header=T)
  coords_nl <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021//Netherlands/coords_nl.csv", header=T)

#check organization of data, including missing values
  veg_nl <- subset(veg_nl, complete.cases(veg_nl$Categorie) & !(veg_nl$Categorie=="")) #only include rows with observations since they sent raw data sheets (with all species regardless of whether they observed them that survvey), now includes 38 observations, instead of 324
  dragon_nl <- subset(dragon_nl, complete.cases(dragon_nl$Aantal)) #only include rows with observations since they sent raw data sheets (with all species regardless of whether they observed them that survvey), now includes 14 observations, instead of 48 
  damsel_nl <- subset(damsel_nl, complete.cases(damsel_nl$Aantal)) #only include rows with observations since they sent raw data sheets (with all species regardless of whether they observed them that survvey), now includes 37 observations, instead of 480 
  bird_nl <- subset(bird_nl, complete.cases(bird_nl$Aantal)) #only include rows with observations since they sent raw data sheets (with all species regardless of whether they observed them that survvey), now includes 15 observations, instead of 2220 
  
#create each GBIF column for Netherlands data
  #eventDate
    library(lubridate)
    veg_nl$Datum <- parse_date_time(veg_nl$Datum, "d/m/y")
    veg_nl$eventDate <- as.Date(veg_nl$Datum)
    
    dragon_nl$Datum  <- parse_date_time(dragon_nl$Datum, "m/d/y")
    dragon_nl$eventDate <- as.Date(dragon_nl$Datum)
    
    damsel_nl$Datum  <- parse_date_time(damsel_nl$Datum, "m/d/y")
    damsel_nl$eventDate <- as.Date(damsel_nl$Datum)
    
    bird_nl$Datum <- parse_date_time(bird_nl$Datum, "m/d/y")
    bird_nl$eventDate <- as.Date(bird_nl$Datum)
  
  #eventTime
    #they did not record time so as recommended by NLBIF data manager Dr. Jeroen Creuwels (nlbif@naturalis.nl), I added the time as midnight
    veg_nl$eventTime <-  "00:00+01:00"
    dragon_nl$eventTime <-  "00:00+01:00"
    damsel_nl$eventTime <-  "00:00+01:00"
    bird_nl$eventTime <-  "00:00+01:00"

  #basisOfRecord
    veg_nl$basisOfRecord <-"AcceptedTaxon"
    dragon_nl$basisOfRecord <- "AcceptedTaxon"
    damsel_nl$basisOfRecord <-"AcceptedTaxon"
    bird_nl$basisOfRecord <- "AcceptedTaxon"

  #scientificName
    #translate names to Latin
    veg_nl$scientificName <- veg_nl$Soorten 
    veg_nl$scientificName <-as.character(veg_nl$scientificName)
    veg_nl$scientificName[veg_nl$scientificName=="Riet"] <- "Phragmites australis"
    veg_nl$scientificName[veg_nl$scientificName=="Lisdodde"] <- "Typha latifolia"
    veg_nl$scientificName[veg_nl$scientificName=="Witte waterlelie"] <- "Nymphaea alba"
    veg_nl$scientificName[veg_nl$scientificName=="Rietgras"] <- "Phalaris arundinacea"
    veg_nl$scientificName[veg_nl$scientificName=="Liesgras"] <- "Glyceria maxima"
    veg_nl$scientificName[veg_nl$scientificName=="Grote egelskop"] <- "Sparganium erectum"
    veg_nl$scientificName[veg_nl$scientificName=="Mattebies"] <- "Schoenoplectus lacustris"
    veg_nl$scientificName[veg_nl$scientificName=="Ruwebies"] <- "Schoenoplectus tabernaemontani"
    veg_nl$scientificName[veg_nl$scientificName=="Kleine lisdodde"] <- "Typha angustifolia"
    veg_nl$scientificName[veg_nl$scientificName==""] <- NA
    
    dragon_nl$scientificName <- as.character("Brachytron pratense")
    
    damsel_nl$scientificName <- damsel_nl$Soorten
    damsel_nl$scientificName <-as.character(damsel_nl$scientificName)
    damsel_nl$scientificName[damsel_nl$scientificName==""] <- NA
    damsel_nl$scientificName[damsel_nl$scientificName=="Azuurwaterjuffer"] <- "Coenagrion puella"
    damsel_nl$scientificName[damsel_nl$scientificName=="Blauwe breedscheenjuffer"] <- "Platycnemis pennipes"
    damsel_nl$scientificName[damsel_nl$scientificName=="Watersnuffel"] <- "Enallagma cyathigerum"
    damsel_nl$scientificName[damsel_nl$scientificName=="Kleine Roodoofjuffer"] <- "Erythromma viridulum"
    damsel_nl$scientificName[damsel_nl$scientificName=="Grote Roodoogjuffer"] <- "Erythromma najas"
    damsel_nl$scientificName[damsel_nl$scientificName=="Lantaarntje"] <- "Ischnura elegans"
    damsel_nl$scientificName[damsel_nl$scientificName=="Variabele waterjuffer"] <- "Coenagrion pulchellum"
    damsel_nl$scientificName[damsel_nl$scientificName=="Maanwater juffer"] <- "Coenagrion lunulatum"
    damsel_nl$scientificName[damsel_nl$scientificName=="Speerwaterjuffer"] <- "Coenagrion hastulatum"
    damsel_nl$scientificName[damsel_nl$scientificName=="Donkere waterjuffer"] <- "Coenagrion armatum"
    
    bird_nl$scientificName <- bird_nl$Soorten
    bird_nl$scientificName <-as.character(bird_nl$scientificName)
    bird_nl$scientificName[bird_nl$scientificName=="Zwarte stern"] <- "Chlidonias niger"
    bird_nl$scientificName[bird_nl$scientificName=="Grote karekiet"] <- "Acrocephalus arundinaceus"
    bird_nl$scientificName[bird_nl$scientificName=="Tafeleend"] <- "Aythya ferina"
    bird_nl$scientificName[bird_nl$scientificName=="Kuifeend"] <- "Aythya fuligula"
    bird_nl$scientificName[bird_nl$scientificName=="Woudaap"] <- "Ixobrychus minutus"
    bird_nl$scientificName[bird_nl$scientificName=="Buidelmees"] <- "Remiz pendulinus"
    bird_nl$scientificName[bird_nl$scientificName=="Dodaars"] <- "Tachybaptus ruficollis"
    bird_nl$scientificName[bird_nl$scientificName=="Slobeend"] <- "Spatula clypeata"
    bird_nl$scientificName[bird_nl$scientificName=="Blauwborst"] <- "Luscinia svecica"
    bird_nl$scientificName[bird_nl$scientificName=="Roerdomp"] <- "Botaurus stellaris"

  #kingdom
    veg_nl$kingdom <- "Plantae"
    dragon_nl$kingdom <- "Animalia"
    damsel_nl$kingdom <- "Animalia"
    bird_nl$kingdom <- "Animalia"

  #taxonRank- e.g. species, genus, family
    veg_nl$taxonRank <- "species"
    dragon_nl$taxonRank <- "species"
    damsel_nl$taxonRank <- "species"
    bird_nl$taxonRank <- "species"

  #individualCount
    veg_nl$individualCount <- as.numeric(1)
    dragon_nl$individualCount <- dragon_nl$Aantal
    damsel_nl$individualCount <- damsel_nl$Aantal
    bird_nl$individualCount <- bird_nl$Aantal

  #decimalLatitude
    veg_nl$decimalLatitude <- veg_nl$X.Rechts 
    dragon_nl$decimalLatitude <- dragon_nl$X
    damsel_nl$decimalLatitude <- damsel_nl$X
    bird_nl$decimalLatitude <- bird_nl$X
  
  #decimalLongitude
    veg_nl$decimalLongitude <- veg_nl$Y.Rechts
    dragon_nl$decimalLongitude <- dragon_nl$Y
    damsel_nl$decimalLongitude <- damsel_nl$Y
    bird_nl$decimalLongitude <- bird_nl$Y
  
  #countryCode- e.g. NL, BE, DE
    veg_nl$countryCode <- "NL"
    dragon_nl$countryCode <- "NL"
    damsel_nl$countryCode <- "NL"
    bird_nl$countryCode <- "NL"

  #plant_Category (not DarwinCore but needed for other data)
    veg_nl$plantCategory <- veg_nl$Categorie
    dragon_nl$plantCategory <- NA
    damsel_nl$plantCategory <- NA
    bird_nl$plantCategory <- NA
    

  #add project area AND species group columns to make it easier to check number of species in each project area later
    dragon_nl$project_area <- 10
    damsel_nl$project_area <- 10
    bird_nl$project_area <- 10
    veg_nl$project_area <- 10

    
    dragon_nl$Group <- "Libellen"
    damsel_nl$Group <- "Libellen"
    bird_nl$Group <- "Birds"
    veg_nl$Group <- "Plants"
    
  #change columns order and combine datsets
  veg_nl <- veg_nl[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  dragon_nl <- dragon_nl[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  damsel_nl <- damsel_nl[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  bird_nl <- bird_nl[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  nl <-rbind(veg_nl, dragon_nl, damsel_nl, bird_nl)

#check missing values in Netherlands data
  #all missing values make sense based on original dataset, see below for summary
  subset(nl, !complete.cases(nl$basisOfRecord)) #complete  
  length(subset(nl, !complete.cases(nl$eventDate))) #13 missing, also missing from original damselfly dataset, date is likely either 2021-06-04, 2021-06-15, or 2021-07-09 but no way to know
  subset(nl, !complete.cases(nl$eventTime)) #complete  
  subset(nl, !complete.cases(nl$scientificName)) #complete 
  subset(nl, !complete.cases(nl$kingdom)) #complete  
  subset(nl, !complete.cases(nl$taxonRank)) #complete  
  subset(nl, !complete.cases(nl$individualCount)) #complete 
  subset(nl, !complete.cases(nl$decimalLatitude)) #complete  
  subset(nl, !complete.cases(nl$decimalLongitude)) #complete 
  subset(nl, !complete.cases(nl$countryCode)) #complete  
  
  
  ####
  
#Germany, areas 1 (Lake Dümmer), 2 (Aschau Teiche), and 3 (Vechtegebiet)
  
  #read in data
    coords_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/coordinates_de.csv", header=T)
    info_birds_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/general_info_birds_de.csv", header=T)
    info_damselflies_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/general_info_damselflies_de.csv", header=T)
    info_dragonflies_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/general_info_dragonflies_de.csv", header=T)
    info_veg_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/general_info_vegetation_de.csv", header=T)

    #Claudia Maistrelli sent missing dates for bird surveys on 20220117
    #original data 'monitoring_birds_de.csv'
    monitoring_birds_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/monitoring_birds_de2.csv", header=T)
    monitoring_lib_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/monitoring_libellen_de.csv", header=T)

    #Claudia Maistrelli sent missing dates for times for the vegetation surveys on 20220117
      #original data 'monitoring_vegetation_de.csv'
      #note from email: "There is still one missing value for the vegetation survey (time of the survey) at Vechtegebiet V3 of 6.09.2020. May you can ask Heiko, as the survey was done by his colleague and I do not have the original protocols."
        #Update: Caitlin Black did ask Heiko and it is not possible to fill in these missing values
    monitoring_veg_de <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/monitoring_veg_de2.csv", header=T)
    coords_de2 <- read.csv("/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/coordinates_de2.csv", header=T)
    
    #Heiko sent missing 2021 data on 20230425 via email
    pa3_2021_veg <- read.csv("~/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/Germany/Vegetation_data_20210921_reorganized.csv", header = TRUE, row.names = NULL, sep=";")
   
  
#check organization of data, including missing values
    #birds
      unique(monitoring_birds_de$Time) #complete
      unique(monitoring_birds_de$Day) #complete
      subset(monitoring_birds_de, !complete.cases(monitoring_birds_de$Amount)) #complete
      subset(monitoring_birds_de, monitoring_birds_de$Amount==0) #6 observations without count
      
      subset(monitoring_birds_de, !complete.cases(monitoring_birds_de$Species..ger.lat.)) #complete
      subset(monitoring_birds_de, monitoring_birds_de$Species..ger.lat.==0) #6 observations without species name, same as those without count
      #remove these observations without count or species
      monitoring_birds_de <- subset(monitoring_birds_de, !(monitoring_birds_de$Amount==0))
    
    #damselflies and dragonflies
      monitoring_lib_de
      unique(monitoring_lib_de$Time) #complete
      unique(monitoring_lib_de$Day) #complete
      subset(monitoring_lib_de, !complete.cases(monitoring_lib_de$Amount)) #complete
      subset(monitoring_lib_de, monitoring_lib_de$Amount==0) #complete
      subset(monitoring_lib_de, !complete.cases(monitoring_lib_de$Species..ger.lat.)) #complete
      subset(monitoring_lib_de, monitoring_lib_de$Species..ger.lat.==0) #complete
    
    #vegetation
      monitoring_veg_de
      unique(monitoring_veg_de$Time) #complete
      unique(monitoring_veg_de$Day) #complete
      subset(monitoring_veg_de, !complete.cases(monitoring_veg_de$Category)) #complete
      subset(monitoring_veg_de, monitoring_veg_de$Category==0) #complete
      subset(monitoring_veg_de, monitoring_veg_de$Category=="#VALUE!") #1 missing observation
        #remove this observation
          monitoring_veg_de <- subset(monitoring_veg_de, !monitoring_veg_de$Category=="#VALUE!")
      subset(monitoring_veg_de, !complete.cases(monitoring_veg_de$Species..ger.lat.)) #complete
      subset(monitoring_veg_de, monitoring_veg_de$Species..ger.lat.==0) #complete
      
    
#create each GBIF column for Germany data
    
  #eventDate
    monitoring_birds_de$eventDate <- paste(monitoring_birds_de$Day,monitoring_birds_de$Month,monitoring_birds_de$Year,sep="/")
    monitoring_birds_de$eventDate <- parse_date_time(monitoring_birds_de$eventDate, "d/m/Y")
    monitoring_birds_de$eventDate <- as.Date(monitoring_birds_de$eventDate)
    
    monitoring_lib_de$eventDate <- paste(monitoring_lib_de$Day,monitoring_lib_de$Month,monitoring_lib_de$Year,sep="/")
    monitoring_lib_de$eventDate <- parse_date_time(monitoring_lib_de$eventDate, "d/m/Y")
    monitoring_lib_de$eventDate <- as.Date(monitoring_lib_de$eventDate )
    
    monitoring_veg_de$eventDate <- paste(monitoring_veg_de$Day,monitoring_veg_de$Month,monitoring_veg_de$Year,sep="/")
    monitoring_veg_de$eventDate <- parse_date_time(monitoring_veg_de$eventDate, "d/m/Y")
    monitoring_veg_de$eventDate <- as.Date(monitoring_veg_de$eventDate )
    pa3_2021_veg$eventDate <- as.Date(pa3_2021_veg$eventDate)
  
  #eventTime
    monitoring_birds_de$eventTime <- gsub( " .*$", "", monitoring_birds_de$Time) #delete text that was in this column
    monitoring_birds_de$eventTime <- str_pad(monitoring_birds_de$eventTime, 5, pad = "0")
    monitoring_birds_de$eventTime[monitoring_birds_de$eventTime=="10:20:00"] <- "10:20"
    monitoring_birds_de$eventTime <- paste(monitoring_birds_de$eventTime,"+01:00",sep="")
    
    monitoring_lib_de$eventTime <- gsub( " .*$", "", monitoring_lib_de$Time) #delete text that was in this column
    monitoring_lib_de$eventTime <- str_pad(monitoring_lib_de$eventTime, 5, pad = "0")
    monitoring_lib_de$eventTime <- paste(monitoring_lib_de$eventTime,"+01:00",sep="")
    
    monitoring_veg_de$eventTime <- gsub( " .*$", "", monitoring_veg_de$Time) #delete text that was in this column
    monitoring_veg_de$eventTime[monitoring_veg_de$eventTime=="11:30:00"] <- "11:30"
    monitoring_veg_de$eventTime <- paste(monitoring_veg_de$eventTime,"+01:00",sep="")
    monitoring_veg_de$eventTime[monitoring_veg_de$eventTime=="+01:00"] <- NA
    
  #basisOfRecord
    monitoring_birds_de$basisOfRecord <-"AcceptedTaxon"
    monitoring_lib_de$basisOfRecord <- "AcceptedTaxon"
    monitoring_veg_de$basisOfRecord <-"AcceptedTaxon"
    pa3_2021_veg$basisOfRecord <- "AcceptedTaxon"
    
  #scientificName
    colnames(monitoring_birds_de)[8] <-"scientificName"
    monitoring_birds_de$scientificName[monitoring_birds_de$scientificName==0] <- "(NA)"
    monitoring_birds_de$scientificName <- regmatches(monitoring_birds_de$scientificName, gregexpr("(?<=\\().*?(?=\\))", monitoring_birds_de$scientificName, perl=T))
    monitoring_birds_de$scientificName <- unlist(lapply(monitoring_birds_de$scientificName,function(x) if(identical(x,character(0))) ' ' else x))
    monitoring_birds_de$scientificName[monitoring_birds_de$scientificName==" "] <- NA
    
    colnames(monitoring_lib_de)[13] <-"scientificName"
    monitoring_lib_de$scientificName <- as.character(monitoring_lib_de$scientificName)
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Blue-tailed damselfly (Ischnura elegans)"] <- "Ischnura elegans"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Azure damselfly (Coenagrion puella)"] <- "Coenagrion puella"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Variable damselfly (Coenagrion pulchellum)"] <-"Coenagrion pulchellum"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Sympetrum (sanguineum)"] <-"Sympetrum sanguineum"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Lestes (sponsa)"] <-"Lestes sponsa"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Small red-eyed damselfly (Erythromma viridulum)"] <-"Erythromma viridulum"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName==""] <- NA
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Lestes (dryas)"] <-"Lestes dryas"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Lestes (sponsa/dryas)"] <-"Lestes dryas"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Hairy Dragonfly (Brachytron pratense)"] <-"Brachytron pratense"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Common blue damselfly (Enallagma cyathigerum)"] <-"Enallagma cyathigerum"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="White-legged damselfly (Platycnemis pennipes)"] <-"Platycnemis pennipes"
    monitoring_lib_de$scientificName[monitoring_lib_de$scientificName=="Sympetrum spec."] <-"Sympetrum"
    
    colnames(monitoring_veg_de)[8] <-"scientificName"
    monitoring_veg_de$scientificName <- as.character(monitoring_veg_de$scientificName)
    pa3_2021_veg$scientificName <- as.character(pa3_2021_veg$scientificName)
    monitoring_veg_de$scientificName[monitoring_veg_de$scientificName=="Bsp1"] <- "(NA)"
    monitoring_veg_de$scientificName[monitoring_veg_de$scientificName=="X1"] <- "(NA)"
    monitoring_veg_de$scientificName[monitoring_veg_de$scientificName=="Segge"] <- "(Carex)"
    monitoring_veg_de$scientificName[monitoring_veg_de$scientificName=="Brennessel"] <- "(Urtica)"
    monitoring_veg_de$scientificName[monitoring_veg_de$scientificName=="Rubus fructicosus agg."] <- "(Rubus fructicosus)"
    monitoring_veg_de$scientificName <- regmatches(monitoring_veg_de$scientificName, gregexpr("(?<=\\().*?(?=\\))", monitoring_veg_de$scientificName, perl=T))
    monitoring_veg_de$scientificName <- unlist(lapply(monitoring_veg_de$scientificName,function(x) if(identical(x,character(0))) ' ' else x))
    monitoring_veg_de$scientificName[monitoring_veg_de$scientificName==" "] <- NA
  
  #kingdom
    monitoring_veg_de$kingdom <- "Plantae"
    monitoring_lib_de$kingdom <- "Animalia"
    monitoring_birds_de$kingdom <- "Animalia"
    
  #taxonRank- e.g. species, genus, family
    monitoring_veg_de$taxonRank <- "species"
    monitoring_veg_de$taxonRank[monitoring_veg_de$scientificName=="Carex"] <- "genus"
    monitoring_veg_de$taxonRank[monitoring_veg_de$scientificName=="Urtica"] <- "genus"
    monitoring_veg_de$taxonRank[monitoring_veg_de$scientificName=="Lycopus"] <- "genus"
    
    monitoring_lib_de$taxonRank <- "species"
    monitoring_lib_de$taxonRank[monitoring_lib_de$scientificName=="Acrocephalus"] <- "genus"
    
    monitoring_birds_de$taxonRank <- "species"
    monitoring_birds_de$taxonRank[monitoring_birds_de$scientificName=="Sympetrum"] <- "genus"
    
  #individualCount
    monitoring_veg_de$individualCount <- as.numeric(1)
    
    monitoring_lib_de$individualCount <- monitoring_lib_de$Amount
    monitoring_lib_de$individualCount[monitoring_lib_de$individualCount=="ca. 30-40"] <- 30
    monitoring_lib_de$individualCount <- as.numeric(gsub( ">", "", monitoring_lib_de$individualCount)) #delete > that was in this column
    
    monitoring_birds_de$individualCount <- monitoring_birds_de$Amount
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="1 + 1"] <- 2 
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="2 + 2"] <- 4
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="3 + 10(juvenile)"] <- 13
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="1+1"] <- 2 
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="3+2"] <- 5 
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="3+1"] <- 4 
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="2+1"] <- 3 
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="2+2"] <- 4 
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="2+3"] <- 5 
    monitoring_birds_de$individualCount[monitoring_birds_de$individualCount=="1+2"] <- 3 
    monitoring_birds_de$individualCount <- as.numeric(gsub( ">", "", monitoring_birds_de$individualCount)) #delete > that was in this column
    
  #decimalLatitude, https://www.latlong.net/degrees-minutes-seconds-to-decimal-degrees
    monitoring_birds_de$Area.stop <- paste(monitoring_birds_de$Area, monitoring_birds_de$Stop,sep="")
    info_birds_de$Area[info_birds_de$Area=="DS1 (am Randkanal)100"] <- "DS10"
    info_birds_de$Area[info_birds_de$Area=="DS2 (an der Hunte)100"] <- "DS20"
    info_birds_de$Area.stop <-  paste(info_birds_de$Area, info_birds_de$Meters,sep="")
    monitoring_birds_de <- merge(monitoring_birds_de, info_birds_de, by="Area.stop", all=T)
    monitoring_birds_de$decimalLatitude <- monitoring_birds_de$X.coordinates
    monitoring_birds_de$decimalLatitude <- as.character(monitoring_birds_de$decimalLatitude)
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.414' "] <- "52.86500000"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.393' "] <- "52.85916667"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.386'"] <- "52.85722222"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.442' "] <- "52.87277778"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.495' "] <- "52.88750000"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.535' "] <- "52.89861111"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.586' "] <- "52.91277778"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.548'"] <- "52.90222222"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.490' "] <- "52.88611111"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.447' "] <- "52.87416667"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="N52° 45.396' "] <- "52.86000000"
    monitoring_birds_de$decimalLatitude <- gsub( '"', "", monitoring_birds_de$decimalLatitude) #delete text that was in this column
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°32'37,078N "] <- "52.54363278"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°32'39,112N "] <- "52.54419778"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°32'43,976N "] <- "52.54554889"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°32'44,282N "] <- "52.54563389"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°32'45,804N "] <- "52.54605667"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°33'36,819N "] <- "52.56022750"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°33'42,569N"] <- "52.56182472"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°33'48,223N "] <- "52.56339528"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°33'54,571N "] <- "52.56515861"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°34'6,968N "] <- "52.56860222"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°32'46,497N "] <- "52.54624917"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°34'0,9N "] <- "52.56691667"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$decimalLatitude=="52°34'0,9N "] <- "52.56691667"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$Area.stop=="DS10"] <- "52.54363278"
    monitoring_birds_de$decimalLatitude[monitoring_birds_de$Area.stop=="DS20"] <- "52.56022750"
    
    
    monitoring_veg_de$Sub.location.area.PQ.number <- paste(monitoring_veg_de$Sub.location..Area., monitoring_veg_de$PQ.number,sep="")
    info_veg_de$PQ.number[info_veg_de$PQ.number==1.1] <-1
    info_veg_de$PQ.number[info_veg_de$PQ.number==2.1] <-2
    info_veg_de$PQ.number[info_veg_de$PQ.number==3.1] <-3
    info_veg_de$PQ.number[info_veg_de$PQ.number==4.1] <-4
    info_veg_de$Sub.location.area.PQ.number <-  paste(info_veg_de$Sub.location..Area., info_veg_de$PQ.number,sep="")
    info_veg_de$Sub.location.area.PQ.number[info_veg_de$Sub.location.area.PQ.number=="Altarm Vechte (V3)1"] <- "V31"
    info_veg_de$Sub.location.area.PQ.number[info_veg_de$Sub.location.area.PQ.number=="Altarm Vechte (V3)2"] <- "V32"
    info_veg_de$Sub.location.area.PQ.number[info_veg_de$Sub.location.area.PQ.number=="Altarm Vechte (V3)3"] <- "V33"
    info_veg_de$Sub.location.area.PQ.number[info_veg_de$Sub.location.area.PQ.number=="Altarm Vechte (V3)4"] <- "V34"
    monitoring_veg_de <- merge(monitoring_veg_de, info_veg_de, by="Sub.location.area.PQ.number", all=T)
    monitoring_veg_de$decimalLatitude <- monitoring_veg_de$X.coordinates
    monitoring_veg_de$decimalLatitude <- as.character(monitoring_veg_de$decimalLatitude)
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 45.419'"] <- "52.86638889"
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 45.575'"] <- "52.90972222"
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 45.444' "] <- "52.87333333"
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 45.659'"] <- "52.93305556"
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 32.668'"] <- "52.71888889"
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 32.683'"] <- "52.72305556"
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 33.848'"] <- "52.78555556"
    monitoring_veg_de$decimalLatitude[monitoring_veg_de$decimalLatitude=="N52° 33.870'"] <- "52.79166667"
    
    monitoring_lib_de$Area.Section <- paste(monitoring_lib_de$Area, monitoring_lib_de$Section,sep="")
    info_damselflies_de$Area.Section <-  paste(info_damselflies_de$Area, info_damselflies_de$Section,sep="")
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordA *dragonfly"] <- "Aschauteiche_nordA"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordB *dragonfly"] <- "Aschauteiche_nordB"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordC *dragonfly"] <- "Aschauteiche_nordC"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordD *dragonfly"] <- "Aschauteiche_nordD"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordE *dragonfly"] <- "Aschauteiche_nordE"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordA*dragonfly"] <- "Aschauteiche_nordA"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordB*dragonfly"] <- "Aschauteiche_nordB"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordD*dragonfly"] <- "Aschauteiche_nordD"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="Aschauteiche_nordE*dragonfly"] <- "Aschauteiche_nordE"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS1 (am Randkanal)B*dragonfly"] <- "DS1 (am Randkanal)B"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS1 (am Randkanal)C *dragonfly"] <- "DS1 (am Randkanal)C"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS1 (am Randkanal)B*dragonfly"] <- "DS1 (am Randkanal)B"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS1 (am Randkanal)A*dragonfly"] <- "DS1 (am Randkanal)A"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS1 (am Randkanal)D*dragonfly"] <- "DS1 (am Randkanal)D"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS1 (am Randkanal)E*dragonfly"] <- "DS1 (am Randkanal)E"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS2 (an der Hunte)B*dragonfly"] <- "DS2 (an der Hunte)B"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS2 (an der Hunte)E*dragonfly"] <- "DS2 (an der Hunte)E"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS2 (an der Hunte)A*dragonfly"] <- "DS2 (an der Hunte)A"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS2 (an der Hunte)C *dragonfly"] <- "DS2 (an der Hunte)C"
    monitoring_lib_de$Area.Section[monitoring_lib_de$Area.Section=="DS2 (an der Hunte)D*dragonfly"] <- "DS2 (an der Hunte)D"
    
    monitoring_lib_de <- merge(monitoring_lib_de, info_damselflies_de, by="Area.Section", all=T)
    monitoring_lib_de$decimalLatitude <- monitoring_lib_de$X.coordinates
    monitoring_lib_de$decimalLatitude <- as.character(monitoring_lib_de$decimalLatitude)
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="N52° 45.414' "] <- "52.86500000"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="N52° 45.403' "] <- "52.86194444"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="N52° 45.393' "] <- "52.85916667"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="N52° 45.411'"] <- "52.86416667"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="N52° 45.442' "] <- "52.87277778"
    #see coords_de2 for all of the X-Wert and Y-Wert coordinates
    #coords_de2[,c(2,4)]
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="X-Wert: 0454423"] <- "52.54363278" 
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0454460"] <- "52.54419778"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0454493"] <- "52.54554889"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0454536"] <- "52.54563389"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0454564 - 0454598"] <- "52.54605667"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="X-Wert: 0455259"] <- "52.56022750"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0455247"] <- "52.56182472"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0455250"] <- "52.56339528"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0455258"] <- "52.56515861"
    monitoring_lib_de$decimalLatitude[monitoring_lib_de$decimalLatitude=="x-Wert: 0455272 - 0455284"] <- "52.56860222"
    
  
  #decimalLongitude
    monitoring_birds_de$decimalLongitude <- monitoring_birds_de$Y.coordinates
    monitoring_birds_de$decimalLongitude <- as.character(monitoring_birds_de$decimalLongitude)
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.281'"] <- "10.34472222"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.199'"] <- "10.32194444"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude==" E10° 16.243'"] <- "10.33416667"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.180'"] <- "10.31666667"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.184'"] <- "10.31777778"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.195'"] <- "10.32083333"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.218'"] <- "10.32722222"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude==" E10° 16.245'"] <- "10.33472222"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.262'"] <- "10.33944444"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.275'"] <- "10.34305556"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="E10° 16.293'"] <- "10.34805556"
    monitoring_birds_de$decimalLongitude <- gsub( '"', "", monitoring_birds_de$decimalLongitude) #delete text that was in this column
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°19'32,869E "] <- "8.32579694"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°19'42,483E"] <- "8.32846750"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°19'49,81E "] <- "8.33050278"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°19'49,78E"] <- "8.33049444"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'10,765E"] <- "8.33632361"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'21,515E"] <- "8.33930972"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'33,573E"] <- "8.34265917"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'28,426E "] <- "8.34122944"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'23,601E"] <- "8.33988917"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'25,253E"] <- "8.34034806"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'26,403E"] <- "8.34066750"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$decimalLongitude=="8°20'24,558E"] <- "8.34015500"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$Area.stop=="DS10"] <- "8.32579694"
    monitoring_birds_de$decimalLongitude[monitoring_birds_de$Area.stop=="DS20"] <- "8.34265917"
    
    monitoring_veg_de$decimalLongitude <- monitoring_veg_de$Y.coordinates
    monitoring_veg_de$decimalLongitude <- as.character(monitoring_veg_de$decimalLongitude)
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude=="E10° 16.271'"] <- "10.34194444"
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude=="E10° 16.207'"] <- "10.32416667"
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude=="E10° 16.356'"] <- "10.36555556"
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude=="E10° 16.527'"] <- "10.41305556"
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude==" E8° 19.753'"] <- "8.52583333"
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude=="E8° 19.796'"] <- "8.53777778"
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude=="E8° 20.420'"] <- "8.45000000"
    monitoring_veg_de$decimalLongitude[monitoring_veg_de$decimalLongitude=="E8° 20.395'"] <- "8.44305556"
    
    monitoring_lib_de$decimalLongitude <- monitoring_lib_de$Y.coordinates
    monitoring_lib_de$decimalLongitude <- as.character(monitoring_lib_de$decimalLongitude)
  
  #see coords_de2 for all of the X-Wert and Y-Wert coordinates
  #coords_de2[,c(3,5)]
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="Y-Wert: 5821747"] <- "8.32579694"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5821780"] <- "8.32846750"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5821817"] <- "8.33050278"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5821849"] <- "8.33049444"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5821896 - 5821932"] <- "8.33632361"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="Y-Wert: 5823891"] <- "8.34265917"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5823938"] <- "8.34122944"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5823988"] <- "8.33988917"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5824037"] <- "8.34034806"
    monitoring_lib_de$decimalLongitude[monitoring_lib_de$decimalLongitude=="y-Wert: 5824081 - 5824130"] <- "8.34066750"
    
  
  #countryCode- e.g. NL, BE, DE
    monitoring_veg_de$countryCode <- "DE"
    monitoring_lib_de$countryCode <- "DE"
    monitoring_birds_de$countryCode <- "DE"

  #plant_Category (not DarwinCore but needed for other data)
    monitoring_veg_de$plantCategory <- monitoring_veg_de$Category
    monitoring_lib_de$plantCategory <- NA
    monitoring_birds_de$plantCategory <- NA
    
  #add project area and group
    monitoring_lib_de$project_area <- NA
    monitoring_lib_de$project_area[monitoring_lib_de$decimalLongitude<8.8 & monitoring_lib_de$decimalLongitude>8.2] <- 1
    monitoring_lib_de$project_area[monitoring_lib_de$decimalLongitude>8.8] <- 2
    monitoring_lib_de$project_area[monitoring_lib_de$decimalLatitude>52.58 & monitoring_lib_de$decimalLatitude<52.65 & monitoring_lib_de$decimalLongitude<6.95 & monitoring_lib_de$decimalLongitude>6.74] <- 3
    
    monitoring_birds_de$project_area <- NA
    monitoring_birds_de$project_area[monitoring_birds_de$decimalLongitude<8.8 & monitoring_birds_de$decimalLongitude>8.2] <- 1
    monitoring_birds_de$project_area[monitoring_birds_de$decimalLongitude>8.8] <- 2
    monitoring_birds_de$project_area[monitoring_birds_de$decimalLatitude>52.58 & monitoring_birds_de$decimalLatitude<52.65 & monitoring_birds_de$decimalLongitude<6.95 & monitoring_birds_de$decimalLongitude>6.74] <- 3
    
    monitoring_veg_de$project_area <- NA
    monitoring_veg_de$project_area[monitoring_veg_de$decimalLongitude<8.8 & monitoring_veg_de$decimalLongitude>8.2] <- 1
    monitoring_veg_de$project_area[monitoring_veg_de$decimalLongitude>8.8] <- 2
    monitoring_veg_de$project_area[monitoring_veg_de$decimalLatitude>52.58 & monitoring_veg_de$decimalLatitude<52.65 & monitoring_veg_de$decimalLongitude<6.95 & monitoring_veg_de$decimalLongitude>6.74] <- 3
    pa3_2021_veg$project_area <- 3
    
    monitoring_lib_de$Group <- "Libellen"
    monitoring_birds_de$Group <- "Birds"
    monitoring_veg_de$Group <- "Plants"

  #change columns order and combine datsets
  monitoring_veg_de <- monitoring_veg_de[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  monitoring_lib_de <- monitoring_lib_de[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  monitoring_birds_de <- monitoring_birds_de[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  pa3_2021_veg <- pa3_2021_veg[,c("basisOfRecord","eventDate","eventTime","scientificName","kingdom","taxonRank","individualCount","decimalLatitude","decimalLongitude","countryCode","plantCategory", "project_area", "Group")]
  de <-rbind(monitoring_veg_de, pa3_2021_veg, monitoring_lib_de, monitoring_birds_de)

  
#check missing values in German data
  #all missing values make sense based on original dataset, see below for summary
  subset(de, !complete.cases(de$basisOfRecord)) #2 missing  
  subset(de, !complete.cases(de$eventDate)) #2 missing, same as above
  length(subset(de, !complete.cases(de$eventTime))) #13 missing, all from plant surveys, see notes when reading in data, missing from Heiko's dataset and confirmed by Claudia
  length(subset(de, !complete.cases(de$scientificName))) #13 missing, 1 missing from lib data, 2 from veg, and others from birds (lost in translation)
  subset(de, !complete.cases(de$kingdom))  #2 missing, same as above
  subset(de, !complete.cases(de$taxonRank))  #2 missing, same as above
  subset(de, !complete.cases(de$individualCount)) #3 missing, same as above + 1 extra
  subset(de, !complete.cases(de$decimalLatitude)) #complete  
  subset(de, !complete.cases(de$decimalLongitude)) #complete 
  subset(de, !complete.cases(de$countryCode)) #complete    
  #remove the 2 missing observations
  de <- subset(de, complete.cases(de$kingdom))
  
#combine datasets from the three countries
  #change to date format to rbind
    be$eventDate <- as.Date(be$eventDate)
    nl$eventDate <- as.Date(nl$eventDate)
    de$eventDate <- as.Date(de$eventDate)
  all <- rbind(be, nl, de)

#add column occurenceID
  all$occurenceID <- paste("https://hetwaterschapshuis.sharepoint.com/sites/1084/Dashboard/Forms/AllItems.aspx?csf=1&web=1&e=Y5CqNG%2F&cid=672837e7%2D83d8%2D42ed%2D8bba%2D244d58943c2b&RootFolder=%2Fsites%2F1084%2FDashboard%2FNetherlands&FolderCTID=0x0120003F39983DD750B64A9A79D97E7303C9A7",formatC(1:nrow(all), width=4, flag = "0"), sep="/") 
  
#double check data for inconsistencies
  all$individualCount[all$individualCount=="Meerdere"] <- NA

  #fix some longitudes to decimal format
    all$decimalLongitude[all$decimalLongitude=="E10° 16.281'"] <-10.27135
    all$decimalLongitude[all$decimalLongitude=="E10° 16.239'"] <-10.27065
    all$decimalLongitude[all$decimalLongitude=="E10° 16.199'"] <-10.269983
    all$decimalLongitude[all$decimalLongitude=="E10° 16.181'"] <-10.269683
    all$decimalLongitude[all$decimalLongitude=="E10° 16.180'"] <-10.269667

  #check NAs
  subset(all, !complete.cases(all$basisOfRecord)) #complete
  nrow(subset(all, !complete.cases(all$eventDate))) #17 missing, from BE and NL, makes sense based on previous data checks
  nrow(subset(all, !complete.cases(all$eventTime))) #50 missing, all from DE, makes sense based on previous data checks
  nrow(subset(all, !complete.cases(all$scientificName))) #14 missing, from BE and DE, makes sense based on previous data checks
  subset(all, !complete.cases(all$kingdom)) #complete
  subset(all, !complete.cases(all$taxonRank)) #complete
  nrow(subset(all, !complete.cases(all$individualCount))) #1 missing, all from BE, makes sense based on previous data checks
  nrow(subset(all, !complete.cases(all$decimalLatitude))) #complete
  nrow(subset(all, !complete.cases(all$decimalLongitude))) #complete
  subset(all, !complete.cases(all$countryCode)) #complete
  
  nrow(all) #1352 total observations
  nrow(subset(all, complete.cases(all))) #74 with all columns complete (bc of plant categor)
  
  #check all coordinates make sense
    # loading the required packages
    library(sp)
    library(leaflet)
    # creating a sample data.frame with your lat/lon points
    longitude <- as.numeric(all$decimalLongitude)
    latitude <- as.numeric(all$decimalLatitude)
    df <- as.data.frame(cbind(longitude,latitude))
    df <- subset(df, complete.cases(df)==T)
    coordinates(df) <- ~longitude+latitude
    leaflet(df) %>% addMarkers() %>% addTiles() #map looks good!

#write csv file, LifeMICA_GBIF_2021_complete.csv includes NAs
  write.csv(all, file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/LifeMICA_GBIF_2021_complete.csv")

  #LifeMICA_GBIF_2021_subset.csv excludes NAs
  sub_all <- all[complete.cases(all), ]
  write.csv(sub_all, file="/Users/caitlinblack/Documents/UvA/Life MICA/2022 Biodiversity Report/data/2021/LifeMICA_GBIF_2021_subset_complete_cases.csv")


  #check why there is plant data missing from areas 3 and 10, clearly that isn't an issue here
all$decimalLongitude <- as.numeric(all$decimalLongitude)
pa1 <- subset(all, all$decimalLongitude<8.8 & all$decimalLongitude>8.2)
pa2 <- subset(all, all$decimalLongitude>8.8)
pa3 <- subset(all, all$decimalLatitude>52.58 & all$decimalLatitude<52.65 & all$decimalLongitude<6.95 & all$decimalLongitude>6.74)
pa4 <- subset(all, all$decimalLongitude<5.5)
pa5 <- subset(all, all$decimalLongitude<5.7 & all$decimalLongitude>5.5)
pa10 <- subset(all, all$decimalLatitude>51.7 & all$decimalLatitude<51.97 & all$decimalLongitude<8.2 & all$decimalLongitude>5.7)
length(unique(subset(pa3$scientificName, pa3$kingdom=="Plantae"))) #28
length(unique(subset(pa10, pa10$kingdom=="Plantae"))) #12

  
  
  
  
  
  
  
  















