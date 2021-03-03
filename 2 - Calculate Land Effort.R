## R Script to Determine total land survey effort in minutes
## Takes survey effort minute from master data sheet, performs a spatial join
## Between landstation names and the Land Station Survey Shapefiles
## Outputs LandStation Shapefile (& CSV) with Effort in minutes for all seasons (and combined)
## Author - grant.e.rogers@gmail.com

#Install and Load Required Packages as needed
packages <- c("excel.link","openxlsx","sf")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{install.packages(setdiff(packages, rownames(installed.packages())))}
invisible(lapply(packages, require, character.only = TRUE))

#Set Working Directory to location of Current R File
tryCatch( expr = {setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) },
          error = function(e){ cat("Working Directory Not Set!\n")  } )

#Define Location of the Master Data Excel Sheet
master_data_file    <- "DATA\\MONTENEGRO MASTER DATA SET.xls"

##### Import and Format Land Effort Data ####

# Make Duplicate for Error Checking Purposes
Land_Effort_Full <- xl.read.file(master_data_file, password = "DOGABAS!",xl.sheet="observation details")

# Remove Boat Stations
Land_Effort <- Land_Effort_Full[!Land_Effort_Full$STATION=="BOAT",]

# Comment out to Keep full data frame if necessary
rm(Land_Effort_Full)

# Extract Only Useful Columns
Land_Effort  <- Land_Effort[,grepl("DATE|STATION|SEASON|TIME",colnames(Land_Effort))]#

#Reformat Time Column
Land_Effort$`TOTAL TIME (h)` <- format(convertToDateTime(Land_Effort$`TOTAL TIME (h)`, origin = "1900-01-01"),"%H:%M:%S")

#Remove any rows containing NA
Land_Effort <- Land_Effort[!is.na(Land_Effort$`TOTAL TIME (h)`),]

# Remove Any trailing or leading whitespaces in station name
Land_Effort$STATION <- trimws(Land_Effort$STATION)

# Rename old (and misspelled) stations to allow merging of efforts

# Ulcinj Cafe
Land_Effort$STATION <- gsub("BALCONY DOWNSTAIRS", "ULCINJ CAFE", Land_Effort$STATION )
Land_Effort$STATION <- gsub("FORTRESS", "ULCINJ CAFE", Land_Effort$STATION )
Land_Effort$STATION <- gsub("BALCONY UPSTAIRS", "ULCINJ CAFE", Land_Effort$STATION )
Land_Effort$STATION <- gsub("ULCINJ CAFÉ", "ULCINJ CAFE", Land_Effort$STATION )
Land_Effort$STATION[Land_Effort$STATION=="ULCINJ"] <- gsub("ULCINJ", "ULCINJ CAFE", Land_Effort$STATION[Land_Effort$STATION=="ULCINJ"] )

# Bar
Land_Effort$STATION[Land_Effort$STATION=="BAR"] <- gsub("BAR", "BAR NEW", Land_Effort$STATION[Land_Effort$STATION=="BAR"] )
Land_Effort$STATION <- gsub("BAR \\(NEW\\)", "BAR NEW", Land_Effort$STATION )
Land_Effort$STATION <- gsub("BAR HIGHWAY", "BAR NEW", Land_Effort$STATION )
Land_Effort$STATION <- gsub("BUSAT", "BAR NEW", Land_Effort$STATION )

# Utjeha
Land_Effort$STATION[Land_Effort$STATION=="UTJEHA"] <- gsub("UTJEHA", "UTJEHA SOUVENIR", Land_Effort$STATION[Land_Effort$STATION=="UTJEHA"] )

# Herceg Novi
Land_Effort$STATION <- gsub("HERCEG-NOVI", "HERCEG NOVI", Land_Effort$STATION )

# Petrovac
Land_Effort$STATION <- gsub("BUDVA NEW", "PETROVAC", Land_Effort$STATION )
Land_Effort$STATION <- gsub("BUDVA \\(NEW\\)", "PETROVAC", Land_Effort$STATION )
Land_Effort$STATION[Land_Effort$STATION=="BUDVA"] <- gsub("BUDVA", "PETROVAC", Land_Effort$STATION[Land_Effort$STATION=="BUDVA"] )

# Remove Opportunistic Land Surveys
Land_Effort <- Land_Effort[!Land_Effort$STATION=="KOTOR",]
Land_Effort <- Land_Effort[!Land_Effort$STATION=="RANDOM",]
Land_Effort <- Land_Effort[!Land_Effort$STATION=="LONG BEACH",]
Land_Effort <- Land_Effort[!Land_Effort$STATION=="SAPORE DI MARE RESTAURANT",]
Land_Effort <- Land_Effort[!Land_Effort$STATION=="BAR BEACH",]

#### Calculate Total Time in Minutes for each Station

# Calculate Total time for combined seasons

# Declare the Data Frame to contain Total Survey Effort for each land station
Total_Land_Effort_All <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(Total_Land_Effort_All) <- c("STATION","LandEffort")

# Loop over each unique station
for (StationName in unique(Land_Effort$STATION))
{
  # Create Vector of Total Times recorded for each station
  Vector_Station_Total_Times <- Land_Effort$`TOTAL TIME (h)`[Land_Effort$STATION==StationName]
 
  # Calculate total time of survey effort in minutes (2014-01-01 is arbitrary)
  Station_Total_Time <- sum(as.numeric(as.POSIXct(paste("2014-01-01", Vector_Station_Total_Times))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/60
  
  # Create Temporary (2x1) Data Frame containing station Name & total effort in minutes
  Temp_DF <- data.frame(StationName,Station_Total_Time)
  colnames(Temp_DF) <- c("STATION","LandEffort")
  
  # Add results from this station to final effort data frame
  Total_Land_Effort_All <- rbind(Total_Land_Effort_All,Temp_DF)
}


##### OUTPUT RESULTS TO CSV & SHAPEFILE FOR ALL SEASONS

#Define CSV Output Directory & Create if not already exist
output_directory_root = "OUT//"
output_directory = "OUT//Land_Effort//"

if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }

write.csv(Total_Land_Effort_All,paste(output_directory,"Land_Effort_All.csv",sep=""),row.names = F)

#Load Land Survey Area Shapfile
#Contains the Geographic Boundaries of the Land Station Survey Areas
Land_Survey_Areas_Shapefile        <- st_read("DATA/Land_Station_Survey_Areas.gpkg",quiet = TRUE)

#Convert Land Station Names to Uppercase
Land_Survey_Areas_Shapefile$Station         <- toupper(Land_Survey_Areas_Shapefile$Station)

#Merge Shapefile with Effort Dataframe by Station Name
Land_Survey_Areas_Effort_Shapefile  <- merge(Land_Survey_Areas_Shapefile, Total_Land_Effort_All, by.x = "Station", by.y = "STATION")

#Create Shapefile for Land Survey Effort & Define Output Filename
Land_Survey_Areas_Effort_Filename <- paste(output_directory,"Land_Survey_Effort_All",".gpkg",sep="")

#Output to GPKG Format
if (file.exists(Land_Survey_Areas_Effort_Filename))
  file.remove(Land_Survey_Areas_Effort_Filename)
st_write(Land_Survey_Areas_Effort_Shapefile,Land_Survey_Areas_Effort_Filename, driver = "gpkg",quiet=TRUE)


#### Calculate Total time for each season ####
# & OUTPUT RESULTS TO CSV & SHAPEFILE FOR ALL SEASONS

# Declare the Data Frame to contain Total Survey Effort for each land station
Total_Land_Effort_Season <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(Total_Land_Effort_Season) <- c("STATION","LandEffort")

#Create Shapefiles for Boat Route per Season
seasons_list <- c("WINTER","SPRING","SUMMER","AUTUMN")

#Ensure Season Column is Upper Case
Land_Effort$SEASON <- toupper(Land_Effort$SEASON)

for(season in seasons_list)
{
  Land_Effort_Season <- Land_Effort[Land_Effort$SEASON==season,]
  # Loop over each unique station
  for (StationName in unique(Land_Effort$STATION))
  {
    # Create Vector of Total Times recorded for each station
    Vector_Station_Total_Times <- Land_Effort_Season$`TOTAL TIME (h)`[Land_Effort_Season$STATION==StationName]
    
    # Calculate total time of survey effort in minutes (2014-01-01 is arbitrary)
    Station_Total_Time <- sum(as.numeric(as.POSIXct(paste("2014-01-01", Vector_Station_Total_Times))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/60
    
    # Create Temporary (2x1) Data Frame containing station Name & total effort in minutes
    Temp_DF <- data.frame(StationName,Station_Total_Time)
    colnames(Temp_DF) <- c("STATION","LandEffort")
    
    # Add results from this station to final effort data frame
    Total_Land_Effort_Season <- rbind(Total_Land_Effort_Season,Temp_DF)
  }
  
  #Define Shapefile Output Directory & Create if not already exist
  output_directory_root = "OUT//"
  output_directory = "OUT//Land_Effort//"
  
  if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
  if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }
  
  write.csv(Total_Land_Effort_Season,paste(output_directory,"Land_Effort_",season,".csv",sep=""),row.names = F)
  
  #Merge Shapefile with Effort Dataframe by Station Name
  Land_Survey_Areas_Effort_Shapefile_Season  <- merge(Land_Survey_Areas_Shapefile, Total_Land_Effort_Season, by.x = "Station", by.y = "STATION")
  
  #Reset Total Land Effort Dataframe Values for Next Season
  Total_Land_Effort_Season <- data.frame(matrix(ncol = 2, nrow = 0))
  
  #Create Shapefile for Land Survey Effort & Define Output Filename
  Land_Survey_Areas_Effort_Season_Filename <- paste(output_directory,"Land_Survey_Effort_",season,".gpkg",sep="")
  
  #Output to GPKG Format
  if (file.exists(Land_Survey_Areas_Effort_Season_Filename))
    file.remove(Land_Survey_Areas_Effort_Season_Filename)
  st_write(Land_Survey_Areas_Effort_Shapefile_Season,Land_Survey_Areas_Effort_Season_Filename, driver = "gpkg",quiet=TRUE)

}

cat("\nLand Effort Calculation Completed and Exported to ",output_directory,"\n")