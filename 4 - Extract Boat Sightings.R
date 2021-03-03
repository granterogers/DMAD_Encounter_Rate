#Script to Extract the Boat Sightings from the Montenegro Master Data
#Perform some cleanup, clip to MNE Survey Area and export to Shapefile 
#Seperated By Season

#Install and Load Required Packages as needed
packages <- c("excel.link","openxlsx","sf")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{install.packages(setdiff(packages, rownames(installed.packages())))}
invisible(lapply(packages, require, character.only = TRUE))

#Set Working Directory to location of Current R File
tryCatch( expr = {setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) },
          error = function(e){ cat("Working Directory Not Set!\n")  } )

#Load Empty Survey Grid Shapefile (2km Hexagons covering MNE Survey Area) (for clipping)
Survey_Grid <- st_read("DATA/Full_Survey_Area_Grid_With_Weights.gpkg",quiet = TRUE)

#Define Location of the Master Data Excel Sheet
master_data_file    <- "DATA\\MONTENEGRO MASTER DATA SET.xls"

# Extract and Export Dolphin Sightings 
Boat_Sightings <- xl.read.file(master_data_file, password = "DOGABAS!",xl.sheet="behaviour-boat")

#Filter by Species TT (Tursiops truncatus)
Boat_Sightings <- Boat_Sightings[Boat_Sightings$SPECIES=="TT",]

#Reformat Time Column
Boat_Sightings$TIME <- format(convertToDateTime(Boat_Sightings$TIME, origin = "1900-01-01"),"%H:%M:%S")

#Ensure Season Column is Upper Case
Boat_Sightings$SEASON <- toupper(Boat_Sightings$SEASON)

#Make any non numeric (empty) locations converted to NA
Boat_Sightings$LATITUDE <- as.numeric(Boat_Sightings$LATITUDE)
Boat_Sightings$LONGITUDE <- as.numeric(Boat_Sightings$LONGITUDE)

#Remove NA Locations
Boat_Sightings <- Boat_Sightings[!is.na(Boat_Sightings$LATITUDE),]
Boat_Sightings <- Boat_Sightings[!is.na(Boat_Sightings$LONGITUDE),]

#Define Shapefile Output Directory & Create if not already exist
output_directory_root = "OUT//"
output_directory = "OUT//Boat_Sightings//"

if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }

#Define CRS for Shapefile output as WGS84 Geographic
WGS_84_CRS <- st_crs("epsg:4326")

#Create Shapefile for Full Boat Route & Define Output Filename
Boat_Sightings_Shapefile               <- st_as_sf(Boat_Sightings, coords = c("LONGITUDE", "LATITUDE"), crs = WGS_84_CRS)

#Clip Sightings to within the Survey Grid (remove any land points)
Boat_Sightings_Shapefile <- Boat_Sightings_Shapefile[Survey_Grid,]

# Define Output Filename
Boat_Sightings_Shapefile_Filename      <- paste(output_directory,"Boat_Sightings_ALL",".gpkg",sep="")

#Output to GPKG Format
if (file.exists(Boat_Sightings_Shapefile_Filename))
  file.remove(Boat_Sightings_Shapefile_Filename)
st_write(Boat_Sightings_Shapefile,Boat_Sightings_Shapefile_Filename, driver = "gpkg",quiet=TRUE)

#Create Shapefiles for Boat Route per Season
seasons_list <- c("WINTER","SPRING","SUMMER","AUTUMN")

for(season in seasons_list)
{
  #Create Shapefile for Seasonal Boat Route & Define Output Filename
  Boat_Sightings_Shapefile          <- st_as_sf(Boat_Sightings[Boat_Sightings$SEASON==season,], coords = c("LONGITUDE", "LATITUDE"), crs = WGS_84_CRS)
  Boat_Sightings_Shapefile_Filename <- paste(output_directory,"Boat_Sightings_",season,".gpkg",sep="")
  
  #Clip Sightings to within the Survey Grid (remove any land points)
  Boat_Sightings_Shapefile <- Boat_Sightings_Shapefile[Survey_Grid,]
  
  #Output to GPKG Format
  if (file.exists(Boat_Sightings_Shapefile_Filename))
    file.remove(Boat_Sightings_Shapefile_Filename)
  st_write(Boat_Sightings_Shapefile,Boat_Sightings_Shapefile_Filename, driver = "gpkg",quiet=TRUE)
}

cat("\nBoat Sightings Extracted to ",output_directory,"\n")