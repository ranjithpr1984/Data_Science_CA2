#Load CSV file to R data frame
NI_Postcodes <- read.csv(file = "NIPostcodes.csv")

# Show number of rows in dataset NI_Postcodes
nrow(NI_Postcodes)

#Show structure of dataset.
#strict.width is used to truncate the line to fit for documentation
str(NI_Postcodes, strict.width = "cut")

#Show first 10 rows in dataset 
head(NI_Postcodes, n = 10)

# Set meaningful names for the column in data frame
column_names <- c("Organisation_Name", "Sub_building_Name", "Building_Name",
                  "Building_Number", "Primary_Thorfare", "Alt_Thorfare",
                  "Secondary_Thorfare", "Locality", "Townland", "Town",
                  "County", "Postcode", "x_coordinates", "y_coordinates",
                  "Identifier")
colnames(NI_Postcodes) <- column_names

# Display structure of data frame which shows columns with blank("") values
str(NI_Postcodes, strict.width = "cut")

# Take count of addresses records without post code and building details
nrow(subset(NI_Postcodes, Organisation_Name == "" &
              Sub_building_Name == "" &
              Building_Name == "" &
              Building_Number == "" &
              Postcode == "" ))

# Delete rows with insufficient details from data frame
NI_Postcodes <- subset(NI_Postcodes, Organisation_Name != "" |
                               Sub_building_Name != "" |
                               Building_Name != "" |
                               Building_Number != "" |
                               Postcode != "" )

# Validate dataset and make sure all address records with insufficient details
#are removed.  Below code should return zero rows
nrow(subset(NI_Postcodes, Organisation_Name == "" &
              Sub_building_Name == "" &
              Building_Name == "" &
              Building_Number == "" &
              Postcode == "" ))
nrow(NI_Postcodes)

attach(NI_Postcodes)

# Assign NA to blank values
NI_Postcodes$Organisation_Name[Organisation_Name == ""] <- NA
NI_Postcodes$Sub_building_Name[Sub_building_Name == ""] <- NA
NI_Postcodes$Building_Name[Building_Name == ""] <- NA
NI_Postcodes$Building_Number[Building_Number == ""] <- NA
NI_Postcodes$Primary_Thorfare[Primary_Thorfare == ""] <- NA
NI_Postcodes$Alt_Thorfare[Alt_Thorfare == ""] <- NA
NI_Postcodes$Secondary_Thorfare[Secondary_Thorfare == ""] <- NA
NI_Postcodes$Locality[Locality == ""] <- NA
NI_Postcodes$Town[Town == ""] <- NA
NI_Postcodes$Postcode[Postcode == ""] <- NA

detach(NI_Postcodes)

# User defined function to find NAs in a Vector
na_count <- function (x) {sum(is.na(x))}

# Find NAs for all the fields in the vector
sapply(NI_Postcodes,na_count)

#Install and load package "mice"
#install.packages("mice", dependencies = TRUE)
#library("mice")

#install.packages("VIM", dependencies = TRUE)
#library(VIM)
#missing_values <- aggr(NI_Postcodes,
#                       prop = FALSE,
#                       numbers = TRUE)
