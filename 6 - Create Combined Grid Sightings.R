## R Script to Create Combined Land & Boat Survey Sightings Grid
## Broken into 4 Stages
## 
## 1) Import Empty Hexagonal Grid 
## 2) Import Boat Sightings
## 3) Count points in polygon to get Boat Sightings in Each Grid Cell 
## 4) Import Land Sightings
## 5) Count points in polygon to get Land Sightings in Each Grid Cell 
## 6) Combine Sightings Together to Form Combined Sightings Column
## 7) Output to Combined Sightings Shapefile Grid for Each Season
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

#Load Empty Survey Grid Shapefile (2x2 Hexagons covering Survey Area)
Survey_Grid <- st_read("DATA/Full_Survey_Area_Grid_With_Weights.gpkg",quiet = TRUE)

#Define Shapefile Output Directory & Create if not already exist
output_directory_root = "OUT//"
output_directory = "OUT//Boat_Land_Sightings_Grid//"

if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }

#Define Season List for Looping
seasons_list <- c("ALL","WINTER","SPRING","SUMMER","AUTUMN")

for(season in seasons_list)
{
  #Duplicate Grid for Boat Effort to Preserve Original
  Boat_Land_Sightings_Grid <- Survey_Grid
  
  #Load Boat Route
  Boat_Sightings <- st_read(paste("OUT//Boat_Sightings//Boat_Sightings_",season,".gpkg",sep=""),quiet = TRUE)
  
  #Calculate Total Boat Effort in minutes per grid cells by counting points in polygon function
  Boat_Land_Sightings_Grid$BoatSightings <- unname(poly.counts(as_Spatial(Boat_Sightings),as_Spatial(Boat_Land_Sightings_Grid)))
  
  #Load Land Effort and Land Survey Areas Shapefile
  Land_Sightings <- st_read(paste("OUT//Land_Sightings//Land_Sightings_",season,".gpkg",sep=""),quiet = TRUE)
  
  #Convert Shapefiles to Dataframe
  Boat_Sightings_CSV <- as.data.frame(st_coordinates(Boat_Sightings))
  Land_Sightings_CSV <- as.data.frame(st_coordinates(Land_Sightings))
  
  #Combine Sightings
  Boat_Land_Sightings <- rbind(Boat_Sightings_CSV,Land_Sightings_CSV)
  
  #Calculate Total Boat Effort in minutes per grid cells by counting points in polygon function
  Boat_Land_Sightings_Grid$LandSightings <- unname(poly.counts(as_Spatial(Land_Sightings),as_Spatial(Boat_Land_Sightings_Grid)))
  
  #Calculate Combined Sightings
  Boat_Land_Sightings_Grid$CombinedSightings <- Boat_Land_Sightings_Grid$BoatSightings + Boat_Land_Sightings_Grid$LandSightings
  
  #Filter only useful columns
  Columns_To_Keep <- paste("id","Weight","BoatSightings","LandSightings","CombinedSightings","geom",sep="|")
  Boat_Land_Sightings_Grid <- Boat_Land_Sightings_Grid[grepl(Columns_To_Keep,colnames(Boat_Land_Sightings_Grid))]
   
  #Convert NA's to 0 (though there should not be any)
  Boat_Land_Sightings_Grid[is.na(Boat_Land_Sightings_Grid)] <- 0
  
  #Create Shapefile for Effort Grid & Define Output Filename
  Boat_Land_Sightings_Grid_Filename <- paste(output_directory,"Boat_Land_Sightings_Grid_",season,".gpkg",sep="")
   
  #Output to GPKG Format
   if (file.exists(Boat_Land_Sightings_Grid_Filename))
     file.remove(Boat_Land_Sightings_Grid_Filename)
   st_write(Boat_Land_Sightings_Grid,Boat_Land_Sightings_Grid_Filename, driver = "gpkg",quiet=TRUE)
}
  
cat("\nCombined Boat & Land Sightings Created & Exported to ",output_directory,"\n")