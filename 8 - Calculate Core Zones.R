# R Script to Load Boat & Land Dolphin Sightings
# And perform Kernel Density Estimate using bandwidth plugin
# to produce 50% & 90% core zone estimates
# grant.e.rogers@gmail.com

#Install and Load Required Packages as needed
packages <- c("rgdal", "raster", "maptools","sf","ks" )
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{ install.packages(setdiff(packages, rownames(installed.packages()))) }
invisible(lapply(packages, require, character.only = TRUE))

#Set Working Directory to location of Current R File
tryCatch( expr = {setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) },
          error = function(e){ cat("Working Directory Not Set!\n")  } )

#Define Season List for Looping
seasons_list <- c("ALL","WINTER","SPRING","SUMMER","AUTUMN")

for(season in seasons_list)
{
  #Load Boat & Land Sightings 
  Boat_Sightings <- st_read(paste("OUT//Boat_Sightings//Boat_Sightings_",season,".gpkg",sep=""),quiet = TRUE)
  Land_Sightings <- st_read(paste("OUT//Land_Sightings//Land_Sightings_",season,".gpkg",sep=""),quiet = TRUE)
  
  #Convert Shapefiles to Dataframe
  Boat_Sightings_CSV <- as.data.frame(st_coordinates(Boat_Sightings))
  Land_Sightings_CSV <- as.data.frame(st_coordinates(Land_Sightings))
  
  #Combine Sightings
  Combined_Sightings <- rbind(Boat_Sightings_CSV,Land_Sightings_CSV)
  
  #Define WGS84 CRS
  WGS84_CRS <- CRS("+init=epsg:4326")
  
  #Load Kernel Density Estimation
  Kernel_Density_Estimate <- kde(Combined_Sightings)
  
  #gps_hpi<-Hpi(x=gps,pilot="samse",pre="scale")

  
  #Extract Contour Line Points
  contour.50 <- with(Kernel_Density_Estimate,contourLines(x=eval.points[[1]],y=eval.points[[2]],z=estimate,levels=cont["50%"])[[1]])
  contour.95 <- with(Kernel_Density_Estimate,contourLines(x=eval.points[[1]],y=eval.points[[2]],z=estimate,levels=cont["95%"])[[1]])

  #Extract Contour Levels
  contour_level_50 <-contourLevels(Kernel_Density_Estimate, prob = c(0.5))
  contour_level_95 <-contourLevels(Kernel_Density_Estimate, prob = c(0.05))
  
  #Create Contour Level Lines
  contour_lines_50<-contourLines(Kernel_Density_Estimate$eval.points[[1]],Kernel_Density_Estimate$eval.points[[2]],Kernel_Density_Estimate$estimate,level=contour_level_50[1])
  contour_lines_95<-contourLines(Kernel_Density_Estimate$eval.points[[1]],Kernel_Density_Estimate$eval.points[[2]],Kernel_Density_Estimate$estimate,level=contour_level_95[1])
  
  #Convert Counter Level Lines to Shapefiles
  contour_lines_SHP_50 <- ContourLines2SLDF(contour_lines_50,proj4string=WGS84_CRS)
  contour_lines_SHP_95 <- ContourLines2SLDF(contour_lines_95,proj4string=WGS84_CRS)
  
  #Create Combined Sightings Shapefile
  Combined_Sightings_SHP <- Combined_Sightings
  # Convert to Coordinate format & Define CRS
  coordinates(Combined_Sightings_SHP) <- ~X+Y
  proj4string(Combined_Sightings_SHP) <- WGS84_CRS
  
  #Convert Combined Sightings to Spatial Points DataFrame
  Combined_Sightings_SHP_SPDF <- SpatialPointsDataFrame(Combined_Sightings_SHP, data.frame(ID=1:length(Combined_Sightings_SHP)))
  
  # OUTPUT RESULTS
  
  #Define Shapefile Output Directory & Create if not already exist
  output_directory_root = "OUT//"
  output_directory = "OUT//Core_Zones//"
  
  if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
  if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }
  
  #Setup Filenames
  filename_50 <- paste(output_directory,"50_Percent_Core_Zone_",season,".gpkg",sep="")
  filename_90 <- paste(output_directory,"90_Percent_Core_Zone_",season,".gpkg",sep="")
  filename_combined <-paste(output_directory,"Combined_Sightings_",season,".gpkg",sep="") 
  
  # Write Core Zones to Shapefile
  writeOGR(contour_lines_SHP_50,filename_50,layer="50_Core_Zone",driver="GPKG",overwrite_layer = TRUE)
  writeOGR(contour_lines_SHP_95,filename_90,layer="95_Core_Zone",driver="GPKG",overwrite_layer = TRUE)
  
  #Write Combined Sightings to Shapefile
  writeOGR(Combined_Sightings_SHP_SPDF,filename_combined,layer="combined",driver="GPKG",overwrite_layer = TRUE)
  
  #Print Home Range Areas
  #cat("Season - ",season," - Area of 50% Core Zone:", KDE_Plugin.50.Contour$area,"\n")
 # cat("Season - ",season," - Area of 90% Core Zone:", KDE_Plugin.90.Contour$area,"\n")
}