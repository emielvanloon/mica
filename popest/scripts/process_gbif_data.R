# Script to count muskrat and coypu observations in GBIF for project areas
# uses functions from non-base R: dismo::gbif and sf::st_bbox
# Note: processing of the spatial extent needs refinement, but is good enough 
# for current use

load("../data/project_areas/project_areas.RDa")

areas <- c("LD_01_WGS", "AT_02_WGS", "VG_03_WGS", "SL_04_WGS", 
           "SM_05_WGS", "MV_06_WGS", "HS_07_WGS", "FR_08_WGS", 
           "NH_09_WGS", "GP_10_WGS", "VG_11_WGS" )

nrarea <- length(areas)
MR <- list(length=nrarea)
CO <- list(length=nrarea)

ev <- vector(length=nrarea)
catch_summary <- data.frame(MR2019=ev, MR2022=ev, CO2019=ev, CO2022=ev)

for(i in 1:nrarea){
  
  eval( parse( text=paste0( 'bb <- sf::st_bbox(', areas[i], ')' ) ) )
  MRa <- dismo::gbif(genus="Ondatra", species='zibethicus', ext=bb)
  COa <- dismo::gbif(genus="Myocastor", species='coypus', ext=bb)

  MRc <- MRa[,c('eventDate','year','lat','lon')]
  COc <- COa[,c('eventDate','year','lat','lon')]
  
  MR[[i]] <- MRc
  CO[[i]] <- COc
  
  catch_summary[i,] <- c(sum(MRc$year==2019), sum(MRc$year==2022),
                         sum(COc$year==2019), sum(COc$year==2022))  
}

save(list=c('MR','CO','catch_summary'),file="../data/gbif_data.RDa")
  
