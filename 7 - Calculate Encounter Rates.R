## R Script to Create Encounter Rate Grid and Calculate Final Restults
## Broken into 4 Stages
## 
## 1) Extract Effort Grid
## 2) Extract Sightings Grid
## 3) Merge Effort & Sightings 
## 4) Create Encounter Rate Column = Weight * (Sightings/Effort)
## 7) Output to Shapefile & CSV
##
## Author - grant.e.rogers@gmail.com

#Install and Load Required Packages as needed
packages <- c("sf","GISTools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{ install.packages(setdiff(packages, rownames(installed.packages()))) }
invisible(lapply(packages, require, character.only = TRUE))

#Set Working Directory to location of Current R File
tryCatch( expr = {setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) },
          error = function(e){ cat("Working Directory Not Set!\n")  } )

#Define Shapefile Output Directory & Create if not already exist
output_directory_root = "OUT//"
output_directory = "OUT//Encounter_Rates//"

if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }

#Define Season List for Looping
seasons_list <- c("ALL","WINTER","SPRING","SUMMER","AUTUMN")

Encounter_Rate_Summary_Table <- data.frame(Season=NA, ENC_RATE_SUM=NA)[numeric(0),]

for(season in seasons_list)
{
  #Load Empty Survey Grid Shapefile (2x2 Hexagons covering Survey Area)
  Effort_Grid <- st_read(paste("OUT/Boat_Land_Effort_Grid/Boat_Land_Survey_Effort_",season,".gpkg",sep=""),quiet = TRUE)
  
  Sightings_Grid <- st_read(paste("OUT/Boat_Land_Sightings_Grid/Boat_Land_Sightings_Grid_",season,".gpkg",sep=""),quiet = TRUE)
  
  #Merge Effort and Sightings Grid (will only work if both Grids are same length - which they should be)
  Encounter_Rate_Grid <- cbind(Effort_Grid, Sightings_Grid)
  
  #Filter only useful columns
  Columns_To_Keep <- c("id","Weight","CombinedEffort","CombinedSightings","geom")
  Encounter_Rate_Grid <- Encounter_Rate_Grid[,Columns_To_Keep]
   
  Encounter_Rate_Grid$ENC_Rate <-  Encounter_Rate_Grid$Weight * (Encounter_Rate_Grid$CombinedSightings/Encounter_Rate_Grid$CombinedEffort)
    
  #Convert NA's to 0 (though there should not be any)
  Encounter_Rate_Grid[is.na(Encounter_Rate_Grid)] <- 0
  
  #Convert any Infinities to 0 (this may happen if a sighting occurs far from a grid cell where effort has been logged
  # eg within one cell, 1 sighting, but 0 effort, creating a ENC Rate of Inf
  Encounter_Rate_Grid$ENC_Rate[!is.finite(Encounter_Rate_Grid$ENC_Rate)] <- 0
  
  #Create Shapefile for Effort Grid & Define Output Filename
  Encounter_Rate_Grid_Filename <- paste(output_directory,"Encounter_Rate_Grid_",season,".gpkg",sep="")
   
  #Output to GPKG Format
    if (file.exists(Encounter_Rate_Grid_Filename))
      file.remove(Encounter_Rate_Grid_Filename)
    st_write(Encounter_Rate_Grid,Encounter_Rate_Grid_Filename, driver = "gpkg",quiet=TRUE)
  
  #Create CSV for Effort Grid & Define Output Filename
  Encounter_Rate_Grid_CSV <- Encounter_Rate_Grid
  #Get Rid of Geometry Fields
  st_geometry(Encounter_Rate_Grid_CSV) <- NULL
  Encounter_Rate_Grid_CSV_Filename <- paste(output_directory,"Encounter_Rate_",season,".CSV",sep="")
  
  #Output to GPKG Format
  if (file.exists(Encounter_Rate_Grid_CSV_Filename))
    file.remove(Encounter_Rate_Grid_CSV_Filename)
  #st_write(Encounter_Rate_Grid,Encounter_Rate_Grid)
  write.csv(Encounter_Rate_Grid_CSV,Encounter_Rate_Grid_CSV_Filename )
  
  Encounter_Rate_Summary_Table[nrow(Encounter_Rate_Summary_Table) + 1,] <- c(season,round(sum(Encounter_Rate_Grid$ENC_Rate),6))
  
}

cat("\nEncounter Rate Data Calculated and Results Exported to ",output_directory,"\n")
print(Encounter_Rate_Summary_Table)
