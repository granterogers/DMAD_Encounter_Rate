## R Script to Determine total boat survey effort in minutes
## Broken into 4 Stages
## Stage 0 - Import Data of Logged Boat Routes and Survey Effort and perform basic quality control
## Stage 1 - Filling in missing time rows between start and end time
## Stage 2 - Pad our extra rows to create a row for each minute
## Stage 3 - Interpolate the LAT/LON to have a location for each minute
##
## Author - grant.e.rogers@gmail.com

#Install and Load Required Packages as needed
packages <- c("dplyr", "padr", "lubridate", "zoo", "data.table", "sf","excel.link","openxlsx")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{ install.packages(setdiff(packages, rownames(installed.packages()))) }
invisible(lapply(packages, require, character.only = TRUE))

#Set Working Directory to location of Current R File
tryCatch( expr = {setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) },
          error = function(e){ cat("Working Directory Not Set!\n")  } )

#Define Location of the Master Data Excel Sheet
master_data_file    <- "DATA\\MONTENEGRO MASTER DATA SET.xls"

#### READ IN DATA ####

# LOAD BOAT ROUTE

#Extract Boat Route from Excel Sheet "Boat_Trackline"
cat("Loading Boat Route Data.. may take a while..")
Boat_Route     <- xl.read.file(master_data_file, password = "DOGABAS!",xl.sheet="Boat_Trackline")

#Make a duplicate for error checking
Boat_Route_Full <- Boat_Route

#Reformat both the Time and Date Columns
Boat_Route$TIME <- format(convertToDateTime(Boat_Route$TIME, origin = "1900-01-01"),"%H:%M:%S")
Boat_Route$DATE <- strptime(as.character(Boat_Route$DATE), "%Y-%m-%d")
Boat_Route$DATE <- format(Boat_Route$DATE,"%d/%m/%Y")

#Extract Only Useful Columns from Boat Route Data
Boat_Route          <- Boat_Route[,grepl("DATE|TIME|LATITUDE|LONGITUDE",colnames(Boat_Route))]

# Replace any 0 values with NA
Boat_Route[Boat_Route == 0] <- NA

# Create Combined DateTime Column (Needed for Interpolation)
Boat_Route$DATETIME <- paste(Boat_Route$DATE,Boat_Route$TIME)
# Format Date & Time Column as POSIXct
Boat_Route$DATETIME <- as.POSIXct(Boat_Route$DATETIME,format="%d/%m/%Y %H:%M:%S")

# LOAD BOAT EFFORT
# Import Boat Survey Effort Data  (needed to extract start and end time for each boat route)
# Make Duplicate for Error Checking Purposes
Boat_Survey_Effort <- xl.read.file(master_data_file, password = "DOGABAS!",xl.sheet="observation details")
Boat_Survey_Effort_Full <- Boat_Survey_Effort

#Reformat both the Time and Date Columns
Boat_Survey_Effort$DATE <- strptime(as.character(Boat_Survey_Effort$DATE), "%Y-%m-%d")
Boat_Survey_Effort$DATE <- format(Boat_Survey_Effort$DATE,"%d/%m/%Y")
Boat_Survey_Effort$`START (hh:mm)`  <- format(convertToDateTime(Boat_Survey_Effort$`START (hh:mm)`, origin = "1900-01-01"),"%H:%M:%S")
Boat_Survey_Effort$`FINISH (hh:mm)` <- format(convertToDateTime(Boat_Survey_Effort$`FINISH (hh:mm)`, origin = "1900-01-01"),"%H:%M:%S")

# Filter Survey effort to only include Boat Stations & Extract useful columns
Boat_Survey_Effort  <- Boat_Survey_Effort[Boat_Survey_Effort$STATION=="BOAT",]
Boat_Survey_Effort  <- Boat_Survey_Effort[,grepl("DATE|START|FINISH",colnames(Boat_Survey_Effort))]

#### Quality Check ####

# Define the Stage 0 Data Frame
Boat_Route_Stage0 <- data.frame()

#These dates should be removed (following a visual inspection of the data)
Boat_Route <- Boat_Route[!grepl("28/07/2017", Boat_Route$DATE),]
Boat_Route <- Boat_Route[!grepl("13/04/2017", Boat_Route$DATE),]
Boat_Route <- Boat_Route[!grepl("23/03/2017", Boat_Route$DATE),]
Boat_Route <- Boat_Route[!grepl("09/03/2017", Boat_Route$DATE),]
Boat_Route <- Boat_Route[!grepl("26/09/2016", Boat_Route$DATE),]
Boat_Route <- Boat_Route[!grepl("26/09/2017", Boat_Route$DATE),]

# Remove any dates for which there is only *one* row (land survey effort) (erroneously placed)
# Cycle through each unique data in the Data Frame
for (x in unique(Boat_Route$DATE))
{
  # If the length of the extracted date vector is 1 (1 Row) then remove this row from the Boat Route DF
  if (length(Boat_Route[Boat_Route$DATE==x,]$DATE) == 1) { Boat_Route <- Boat_Route[!grepl(x, Boat_Route$DATE),] }
}

# Identify which dates have time data missing (contain NA's) & therefore need temporal interpolation
# And place into Bad_Dates DataFrame
Bad_Dates <- unique(Boat_Route[is.na(Boat_Route$TIME),"DATE"])

# Identify Those dates that have both good & bad dates 
Good_and_Bad_Dates <- unique(Boat_Route[!is.na(Boat_Route$TIME),"DATE"])

# Calculate those dates with no time data missing
# Not Necessary but can be useful to know
Good_Dates <- setdiff(Good_and_Bad_Dates,Bad_Dates)

# Populate the Stage 0 Boat Roat Dataframe post Quality Check
Boat_Route_Stage0 <- Boat_Route

# Cleanup the Boat_Route Variable
rm(Boat_Route)

#### Stage 1 - Perform Temporal Interpolation for Missing Time Stamps ####

# Define the Stage 1 Data Frame
Boat_Route_Stage1 <- data.frame()

# Define a Data Frame that will contain interpolated time values
Interpolated_Routes_DF <- data.frame()

# Loop over unique dates
for (x in unique(Bad_Dates))
{
  # Create Temporary Data Frame for Temporal Interpolation
  Temp_DF                           <- Boat_Route_Stage0[Boat_Route_Stage0$DATE==x,]
  # Get Rows Numbers of Temp DF
  End_Time_Index                    <- length(Temp_DF$DATETIME)
  
  # Populate Start Time Row (in case not present)
  Temp_DF$TIME[1]                   <- Boat_Survey_Effort$`START (hh:mm)`[Boat_Survey_Effort$DATE==x]
  # Update Start DateTime Row
  Temp_DF$DATETIME[1]               <- as.POSIXct(paste(Temp_DF$DATE[1], Temp_DF$TIME[1]), format="%d/%m/%Y %H:%M:%S")
  
  # Populate End Time Row
  Temp_DF$TIME[End_Time_Index]      <- Boat_Survey_Effort$`FINISH (hh:mm)`[Boat_Survey_Effort$DATE==x]
  # Update End DateTime Row
  Temp_DF$DATETIME[End_Time_Index]  <- as.POSIXct(paste(Temp_DF$DATE[End_Time_Index], Temp_DF$TIME[End_Time_Index]), format="%d/%m/%Y %H:%M:%S")

  # Interpolate blank time cells with appropriate intervals
  Temp_DF$DATETIME                  <- seq(Temp_DF$DATETIME[1], Temp_DF$DATETIME[End_Time_Index],length.out = End_Time_Index)
  
  # Remove Extraneous Time Column
  Temp_DF$TIME                      <- NULL
  # Reorder Columns
  Temp_DF                           <- Temp_DF[,c("DATE","DATETIME","LATITUDE","LONGITUDE")]

  # Add to Dataframe containing Interpolated Routes
  Interpolated_Routes_DF            <- rbind(Interpolated_Routes_DF,Temp_DF)
}

# Create Data Frame containing uninterpolated (good) Boat Routes
Uninterpolated_Routes_DF <-  Boat_Route_Stage0[Boat_Route_Stage0$DATE %in% Good_Dates,]
# Remove Extraneous Time Column
Uninterpolated_Routes_DF$TIME <- NULL
# Reorder Columns
Uninterpolated_Routes_DF <- Uninterpolated_Routes_DF[,c("DATE","DATETIME","LATITUDE","LONGITUDE")]

# Populate Final Stage1 Data Frame containing both interpolated and good routes merged
Boat_Route_Stage1 <- rbind(Uninterpolated_Routes_DF,Interpolated_Routes_DF)
# Order by TimeStamp
Boat_Route_Stage1 <- Boat_Route_Stage1[order(Boat_Route_Stage1$DATETIME),]
# Remove Seconds From Timestamp - This creates multiple duplicate rows within DATETIME
Boat_Route_Stage1$DATETIME <- format(Boat_Route_Stage1$DATETIME,format = "%Y-%m-%d %H:%M")
Boat_Route_Stage1$DATETIME <- as.POSIXct(Boat_Route_Stage1$DATETIME, format = "%Y-%m-%d %H:%M")

#Cleanup Variables that are no longer needed
rm(Temp_DF,x,End_Time_Index,Good_and_Bad_Dates,Bad_Dates,Good_Dates)
rm(Uninterpolated_Routes_DF,Interpolated_Routes_DF)
#No Longer Needed
rm(Boat_Route_Stage0,Boat_Survey_Effort)

#### Final output of Stage 1 is "Boat_Route_Stage1" - All time value are filled where necessary

#### Stage 2 - Fill in any missing minutes OR remove duplicate rows ####

#Define Stage 2 Data Frame
Boat_Route_Stage2 <- data.frame()

# Loop over all dates
for (x in unique(Boat_Route_Stage1$DATE))
{
  #Create Temporary Data Frame for each unique data for manipulation
  Temp_DF           <- Boat_Route_Stage1[Boat_Route_Stage1$DATE==x,]
  
  # Pad Time by introducing extra rows that cover every minute between start and end date
  Temp_DF           <- pad(Temp_DF,interval="min")
  
  # Populate the Temp DF Date Column properly so no blanks in new rows
  Temp_DF$DATE      <- Temp_DF$DATE[1]
  
  # Trim Time - As seconds have already been zeroed, only need to remove duplidate datetimes
  Temp_DF           <- Temp_DF[!duplicated(Temp_DF$DATETIME),]
  
  # Added Temp Trimmed DF to Stage 2 DF
  Boat_Route_Stage2 <- rbind(Boat_Route_Stage2,Temp_DF)
}

#Cleanup Variables no longer needed
rm(Temp_DF,x)
rm(Boat_Route_Stage1)

#### Stage 3 - Interpolate LAT/LON for missing locations ####

# Define Stage 3 Data Frame
Boat_Route_Stage3 <- data.frame()

# Loop over all dates
for (x in unique(Boat_Route_Stage2$DATE))
{
  #Create Temporary Data Frame for each unique data for manipulation
  Temp_DF <- Boat_Route_Stage2[Boat_Route_Stage2$DATE==x,]

  # We now will remove any rows that have NA for LAT & LON only
  # at the *beginning* or *end* of data frame
  # This will be the First time we now subtract survey effort
  # Alternative is to extrapolate LAT/LON to the start and end times
  Temp_DF <- na.trim(Temp_DF)

  # We can use this command to check how many were trimmed (about 1/3)
  # if ( !isTRUE(identical(Temp_DF,na.trim(Temp_DF))) ) print(paste(x," DF modified"))

  #Fill in Missing Interal Na Values of LAT & LON by linear interpolation
  Temp_DF$NEWLAT    <- na.approx(Temp_DF$LATITUDE)
  Temp_DF$NEWLON    <- na.approx(Temp_DF$LONGITUDE)

  #Add Temporary DateFrame Result for each Date to Stage3 DataFrame
  Boat_Route_Stage3 <- rbind(Boat_Route_Stage3,Temp_DF)
}

#Cleanup Unused Variables
rm(Temp_DF,x)
rm(Boat_Route_Stage2)

#### Prepare for Export ####
#Create a Seasons Column from Scratch (Easier than to track from beginning)

Boat_Route_Stage3$SEASON <- ""
Boat_Route_Stage3[month(Boat_Route_Stage3$DATE) %in% c(12,1,2),]$SEASON   <- "Winter"
Boat_Route_Stage3[month(Boat_Route_Stage3$DATE) %in% c(3,4,5),]$SEASON    <- "Spring"
Boat_Route_Stage3[month(Boat_Route_Stage3$DATE) %in% c(6,7,8),]$SEASON    <- "Summer"
Boat_Route_Stage3[month(Boat_Route_Stage3$DATE) %in% c(9,10,11),]$SEASON  <- "Autumn"

#Remove the Old LAT/LON Columns
Boat_Route_Stage3$LATITUDE  <- NULL
Boat_Route_Stage3$LONGITUDE <- NULL

#### Export to CSV & Shapefile ####

output_directory_root = "OUT//"
output_directory = "OUT//Boat_Route//"

if (!dir.exists(output_directory_root)){ dir.create(output_directory_root,showWarnings = FALSE) }
if (!dir.exists(output_directory)){ dir.create(output_directory,showWarnings = FALSE) }

# Export to CSV
write.csv(Boat_Route_Stage3,paste(output_directory,"Boat_Route_Processed.csv",sep=""))

#Define CRS for Shapfile output as WGS84 Geographic
WGS_84_CRS <- st_crs("epsg:4326")

#Create Shapefile for Full Boat Route & Define Output Filename
Boat_Route_Shapefile_Full               <- st_as_sf(Boat_Route_Stage3, coords = c("NEWLON", "NEWLAT"), crs = WGS_84_CRS)
Boat_Route_Shapefile_Full_Filename      <- paste(output_directory,"Boat_Route_All",".gpkg",sep="")

#Output to GPKG Format
if (file.exists(Boat_Route_Shapefile_Full_Filename))
  file.remove(Boat_Route_Shapefile_Full_Filename)
st_write(Boat_Route_Shapefile_Full,Boat_Route_Shapefile_Full_Filename, driver = "gpkg",quiet=TRUE)

#Create Shapefiles for Boat Route per Season
seasons_list <- c("WINTER","SPRING","SUMMER","AUTUMN")

#Ensure Season Column is Upper Case
Boat_Route_Stage3$SEASON <- toupper(Boat_Route_Stage3$SEASON)

for(season in seasons_list)
{
  #Create Shapefile for Seasonal Boat Route & Define Output Filename
  Boat_Route_Shapefile_Season          <- st_as_sf(Boat_Route_Stage3[Boat_Route_Stage3$SEASON==season,], coords = c("NEWLON", "NEWLAT"), crs = WGS_84_CRS)
  Boat_Route_Shapefile_Season_Filename <- paste(output_directory,"Boat_Route_",season,".gpkg",sep="")
  
  #Output to GPKG Format
  if (file.exists(Boat_Route_Shapefile_Season_Filename))
    file.remove(Boat_Route_Shapefile_Season_Filename)
  st_write(Boat_Route_Shapefile_Season,Boat_Route_Shapefile_Season_Filename, driver = "gpkg",quiet=TRUE)
}

cat("\nBoat Effort Calculation Completed and Exported to ",output_directory,"\n")
