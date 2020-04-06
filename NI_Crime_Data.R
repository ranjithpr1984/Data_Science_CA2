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

