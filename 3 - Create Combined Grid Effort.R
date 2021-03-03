## R Script to Create Combined Land & Boat Survey Effort Grid
## Broken into 4 Stages
## 
## 1) Extract Empty Hexagonal Grid 
## 2) Extract Boat Route (each point a minute)
## 3) Count points in polygon to get Boat Effort in minutes in Each Grid Cell 
## 4) Extract Land Station Shapefiles (With effort included)
## 5) Spatial Join between Effort Grid & Land Station Shapefiles (& Handle overlapping areas)
## 6) Output to Combined Effort Shapefile
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
output_directory = "OUT//Boat_Land_Effort_Grid//"

if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }

#Define Season List for Looping
seasons_list <- c("ALL","WINTER","SPRING","SUMMER","AUTUMN")

for(season in seasons_list)
{
  #Duplicate Grid for Boat Effort to Preserve Original
  Boat_Effort_Grid <- Survey_Grid
  
  #Load Boat Route
  Boat_Route <- st_read(paste("OUT/Boat_Route/Boat_Route_",season,".gpkg",sep=""),quiet = TRUE)
  
  #Calculate Total Boat Effort in minutes per grid cells by counting points in polygon function
  Boat_Effort_Grid$BoatEffort <- unname(poly.counts(as_Spatial(Boat_Route),as_Spatial(Boat_Effort_Grid)))
  
  #Load Land Effort and Land Survey Areas Shapefile
  Land_Effort <- st_read(paste("OUT//Land_Effort//Land_Survey_Effort_",season,".gpkg",sep=""),quiet = TRUE)
  
  #Spatial Intersection Between the Survey Grid and Land Station Areas
  Combined_Boat_Land_Effort_Grid <- st_join(Boat_Effort_Grid, Land_Effort)
  
  #Make a list of overlapping grid cells between Land stations Bar & Utjeha
  Overlapping_Grid_Cells <- Combined_Boat_Land_Effort_Grid$id[duplicated(Combined_Boat_Land_Effort_Grid$id)]
  
  #Take the first two overlapping cells from the list
  Duplicate_Cells <- Combined_Boat_Land_Effort_Grid[Combined_Boat_Land_Effort_Grid$id==Overlapping_Grid_Cells[1],]
  #Add together Land Effort from each Station which can be applied to all overlapping cells
  Overlapped_Land_Effort <- Duplicate_Cells$LandEffort[1]  + Duplicate_Cells$LandEffort[2]
  
  #Update Land Effort in Combined_Boat_Land_Effort_Grid for all overlapping grid cells
  Combined_Boat_Land_Effort_Grid$LandEffort[Combined_Boat_Land_Effort_Grid$id %in% Overlapping_Grid_Cells] <- Overlapped_Land_Effort
  
  #Now Safe to Remove the Duplicate Geometries (one of the overlapping cells)
  Combined_Boat_Land_Effort_Grid <- Combined_Boat_Land_Effort_Grid[!duplicated(Combined_Boat_Land_Effort_Grid$geom),]
  
  #Filter only useful columns
  Columns_To_Keep <- paste("id","Weight","BoatEffort","LandEffort","geom",sep="|")
  Combined_Boat_Land_Effort_Grid <- Combined_Boat_Land_Effort_Grid[grepl(Columns_To_Keep,colnames(Combined_Boat_Land_Effort_Grid))]
  
  #Convert NA's to 0
  Combined_Boat_Land_Effort_Grid[is.na(Combined_Boat_Land_Effort_Grid)] <- 0
  
  #Create Combined Effort Column
  Combined_Boat_Land_Effort_Grid$CombinedEffort <- Combined_Boat_Land_Effort_Grid$BoatEffort + Combined_Boat_Land_Effort_Grid$LandEffort
  
  #Create Shapefile for Effort Grid & Define Output Filename
  Combined_Boat_Land_Effort_Grid_Filename <- paste(output_directory,"Boat_Land_Survey_Effort_",season,".gpkg",sep="")
  
  #Output to GPKG Format
  if (file.exists(Combined_Boat_Land_Effort_Grid_Filename))
    file.remove(Combined_Boat_Land_Effort_Grid_Filename)
  st_write(Combined_Boat_Land_Effort_Grid,Combined_Boat_Land_Effort_Grid_Filename, driver = "gpkg",quiet=TRUE)
}

cat("\nBoat & Land Effort Grid Created and exported to",output_directory,"\n")