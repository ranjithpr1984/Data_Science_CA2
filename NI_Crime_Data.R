# Get list of files under directory NI_Crime_Data
files <- list.files(path = "NI_Crime_Data",
                    pattern = "*northern-ireland-street.csv",
                    full.names = TRUE,
                    recursive = TRUE)

# Read first file to data frame AllNICrimeData
AllNICrimeData = read.csv(files[1])

#Loop throght the 2nd to last file in the file list
for (i in 2:length(files)){
  # Read CSV to data frame NICrimeData
  NICrimeData = read.csv(files[i])
  #Merge data frame NICrimeData into AllNICrimeData
  rbind(AllNICrimeData,NICrimeData)
}

# Display count and structure of data frame AllNICrimeData
nrow(AllNICrimeData)
str(AllNICrimeData, strict.width = "cut")

# Write data frame AllNICrimeData to CSV
write.csv(AllNICrimeData, file = "AllNICrimeData.csv", row.names = FALSE)

# Remove columns CrimeID, Reported by, Falls within, LSOA code, 
#  LSOA name, last outcome and context from data frame AllNICrimeData
AllNICrimeData <- AllNICrimeData[-c(1, 3, 4, 8, 9, 11, 12)]

# Write data frame AllNICrimeData to CSV
write.csv(AllNICrimeData, file = "AllNICrimeData.csv"
          , row.names = FALSE)

# Decode Crime type to shorterned description and store to CSV
crime_type_char <- as.character(AllNICrimeData$Crime.type)

crime_type_char[crime_type_char == "Anti-social behaviour"] <- "ASBO"
crime_type_char[crime_type_char == "Bicycle theft"] <- "BITH"
crime_type_char[crime_type_char == "Burglary"] <- "BURG"
crime_type_char[crime_type_char == "Criminal damage and arson"] <- "CDAR"
crime_type_char[crime_type_char == "Drugs"] <- "DRUG"
crime_type_char[crime_type_char == "Other theft"] <- "OTTH"
crime_type_char[crime_type_char == "Public order"] <- "PUBO"
crime_type_char[crime_type_char == "Robbery"] <- "ROBY"
crime_type_char[crime_type_char == "Shoplifting"] <- "SHOP"
crime_type_char[crime_type_char == "Theft from the person"] <- "THPR"
crime_type_char[crime_type_char == "Vehicle crime"] <- "VECR"
crime_type_char[crime_type_char == "Violence and sexual offences"] <- "VISO"
crime_type_char[crime_type_char == "Other crime"] <- "OTCR"
crime_type_char[crime_type_char == "Possession of weapons"] <- "POSW"

AllNICrimeData$Crime.type <- crime_type_char

# Write data frame AllNICrimeData to CSV
write.csv(AllNICrimeData, file = "AllNICrimeData.csv"
          , row.names = FALSE)
str(AllNICrimeData, strict.width = "cut")

# Sort crime by frequency and plot
plot(x = sort(table(AllNICrimeData$Crime.type), decreasing = TRUE),
     type = "h", col = "Blue", main = "Crime Frequency",
     xlab = "Crime type",
     ylab = "Frequency")

# Remove string 'On or near' from Location
AllNICrimeData$Location <- sub(pattern = "On or near ", "",
                               as.character(AllNICrimeData$Location),
                               ignore.case = TRUE)

# Assign NA to blank location
attach(AllNICrimeData)
AllNICrimeData$Location[ Location == "" ] <- NA
detach(AllNICrimeData)

# Install and load package data.table
#install.packages("data.table")
library(data.table)

# Take only crime data with location is not NA and convert it to data.table object
AllNICrimeData_tab <- data.table(subset(AllNICrimeData,!is.na(Location)))

#Create random crime sample of size 5000
set.seed(100)
random_crime_sample <- AllNICrimeData_tab[sample(.N, 5000)]

# Load CSV CleanNIPostcodeData.csv to data frame NIPostcode
NIPostcode <- read.csv("CleanNIPostcodeData.csv")

# Function to find Town from NIPostcode using location
find_a_town <- function(p_location) {
  indx <- grep(p_location,
               NIPostcode$Primary_Thorfare,
               ignore.case = TRUE)
  if(length(indx) == 0)
    indx <- grep(p_location,
                 NIPostcode$Alt_Thorfare,
                 ignore.case = TRUE)
  if(length(indx) == 0)
    indx <- grep(p_location,
                 NIPostcode$Secondary_Thorfare,
                 ignore.case = TRUE)
  return(as.character(NIPostcode[indx[1],11]))
}

#Create and pupulate field Town in random sample crime data set
random_crime_sample$Town <- sapply(random_crime_sample$Location, find_a_town)
nrow(random_crime_sample)
str(random_crime_sample, strict.width = "cut")

#Load CSV VillageList.csv to data frame VillageList
VillageList <- read.csv("VillageList.csv")
names(VillageList)[1] <- "City_Town_Village"

# Function to get population from VillageList using Town
add_town_data <- function(p_town) {
  indx <- grep(p_town,
               VillageList$City_Town_Village,
               ignore.case = TRUE)
  # Convert the population to number before returning
  return(as.numeric(sub(",","",as.character(VillageList[indx[1],2]))))
}

# Create and Populate field Population in random sample dataset
random_crime_sample$Population <- sapply(tmp$Town, add_town_data)
nrow(random_crime_sample)
str(random_crime_sample, strict.width = "cut")

# Rename columns in random sample dataset
names(random_crime_sample)[5] <- "Crime type"
names(random_crime_sample)[6] <- "City-Town-Village"

#Convert random_crime_sample to pure data frame
setDF(random_crime_sample)
str(random_crime_sample, strict.width = "cut")

# Write random crime sample data frame to CSV
write.csv(random_crime_sample, file = "random_crime_sample.csv"
          , row.names = FALSE)

# Setup figure property to display 2 charts side by side
par(mfrow=c(1,2))

#Reduce the font size axis values to ensure all axis values are displayed
par(cex.axis=.7)

#Plot Belfast crime frequency sort by crime type count
barplot(height = sort(table(subset(random_crime_sample,City.Town.Village=="BELFAST", 
                           select = c(Crime.type))), decreasing = TRUE),
     col = "Blue", main = "Belfast crime frequency",
     xlab = "Crime type",
     ylab = "Frequency",
     las = 2
     #,ylim = c(0,500)
     )

#Plot Derry crime frequency sort by crime type count
barplot(height = sort(table(subset(random_crime_sample,City.Town.Village=="LONDONDERRY", 
                           select = c(Crime.type))), decreasing = TRUE),
     col = "Blue", main = "Derry crime frequency",
     xlab = "Crime type",
     ylab = "Frequency",
     las = 2
     #,ylim = c(0,500)
     )

