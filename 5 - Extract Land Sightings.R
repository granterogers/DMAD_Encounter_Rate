#Script to Extract the Boat Sightings from the Montenegro Master Data
#and export to Shapefile seperated By Seasons

#Install and Load Required Packages as needed
packages <- c("excel.link","openxlsx","sf")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{install.packages(setdiff(packages, rownames(installed.packages())))}
invisible(lapply(packages, require, character.only = TRUE))

#Set Working Directory to location of Current R File
tryCatch( expr = {setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) },
          error = function(e){ cat("Working Directory Not Set!\n")  } )

#Load Land Survey Area Shapfile
#Contains the Geographic Boundaries of the Land Station Survey Areas
#As Determined by furthest recorded sightings
Land_Survey_Areas_Shapefile        <- st_read("DATA/Land_Station_Survey_Areas.gpkg",quiet = TRUE)

#Define Location of the Master Data Excel Sheet
Pythagoras_Data_File    <- "DATA\\PYTHAGORAS_MNE.xls"

# Extract and Export Dolphin Sightings 
Land_Sightings_Full <- xl.read.file(Pythagoras_Data_File,xl.sheet="FIX DATA")

#Filter to Tursiops Truncatus
Land_Sightings <- Land_Sightings_Full[Land_Sightings_Full$FixType=="TT",]

#Ensure Season Column is Upper Case
Land_Sightings$Season <- toupper(Land_Sightings$Season)

#Rename LAT/LON Columns
names(Land_Sightings)[names(Land_Sightings) == "Dec Lat"] <- "LATITUDE"
names(Land_Sightings)[names(Land_Sightings) == "Dec Long"] <- "LONGITUDE"

#Make any non numeric (empty) locations converted to NA
Land_Sightings$LATITUDE <- as.numeric(Land_Sightings$LATITUDE)
Land_Sightings$LONGITUDE <- as.numeric(Land_Sightings$LONGITUDE)

#Remove NA Locations
Land_Sightings <- Land_Sightings[!is.na(Land_Sightings$LATITUDE),]
Land_Sightings <- Land_Sightings[!is.na(Land_Sightings$LONGITUDE),]

#Define CRS for Shapefile output as WGS84 Geographic
WGS_84_CRS <- st_crs("epsg:4326")

#Create Shapefile of Land Sightings
Land_Sightings_Shapefile    <- st_as_sf(Land_Sightings, coords = c("LONGITUDE", "LATITUDE"), crs = WGS_84_CRS)

#Clip Sightings to within Land Station Survey Areas
#NB Survey Areas are themselves determined by the largest angular &
#Distance of further sightings, so shapefiles may have to be adjusted
Land_Sightings_Shapefile <- Land_Sightings_Shapefile[Land_Survey_Areas_Shapefile,]

#Define Shapefile Output Directory & Create if not already exist
output_directory_root = "OUT//"
output_directory = "OUT//Land_Sightings//"

if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }

#Define Output Filename for ALL Seasons Land Sightings Shapefile
Land_Sightings_Shapefile_Filename <- paste(output_directory,"Land_Sightings_ALL.gpkg",sep="")

#Output to GPKG Format
if (file.exists(Land_Sightings_Shapefile_Filename))
  file.remove(Land_Sightings_Shapefile_Filename)
st_write(Land_Sightings_Shapefile,Land_Sightings_Shapefile_Filename, driver = "gpkg",quiet=TRUE)

#Define Season List for Looping
seasons_list <- c("WINTER","SPRING","SUMMER","AUTUMN")

for(season in seasons_list)
{
  #Create Shapefile for Seasonal Land Sightings & Define Output Filename
  Land_Sightings_Shapefile_Season           <- Land_Sightings_Shapefile[Land_Sightings_Shapefile$Season==season,]
  Land_Sightings_Shapefile_Season_Filename  <- paste(output_directory,"Land_Sightings_",season,".gpkg",sep="")
  
  #Output to GPKG Format
  if (file.exists(Land_Sightings_Shapefile_Season_Filename))
    file.remove(Land_Sightings_Shapefile_Season_Filename)
  st_write(Land_Sightings_Shapefile_Season,Land_Sightings_Shapefile_Season_Filename, driver = "gpkg",quiet=TRUE)
}

cat("\nLand Sightings Extracted to ",output_directory,"\n")