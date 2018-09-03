pollutantmean <- function(directory,pollutant, id = 1:332)
{
  
  library(readr)

  # In the id vector we have the indexes of the files to be considered
  # For instance if vector id = c(1,19,200), it means the files to be considered
  # are the following: 001.csv, 019.csv and 200.csv
  # That implies converting the integer numbers to character
  
  file_names<- vector(length = length(id))
  
  for (i in 1:length(id)){ #for every file
      # convert number from integer to string
      file_names[i] <- as.character(id[i])
      # add zeros to the left in case file name has less than 3 digits
      file_names[i] <- paste("000", file_names[i], sep="")
      # obtain the correct file number with 3 digits
      ncars<- nchar(file_names[i])
      file_names[i] <- substr(file_names[i],ncars-2, ncars )
      # adding .csv extension to file name
      file_names[i] <- paste(file_names[i], ".csv", sep="")
      # finally adding directory to file name 
      file_names[i] <- paste(getwd(), "/", directory, "/",  file_names[i], sep="")
  }
  
  # initialize variables for result
  measure <- 0
  number_of_rows <- 0
 
  # process every file 
   for (j in 1:length(file_names)) {
      
      # read file
      content <- read_csv(file_names[j],col_names = TRUE, col_types = list(col_date(), col_double(), col_double(), col_integer()))    
      # get only those rows wich are not NA for the respective pollutant
      goods <- !is.na(content[,pollutant])
      
      content2 <- content[goods,]
      if (nrow(content2) > 0){
        # adding measures and number of rows 
        measure <- measure + sum(content2[,pollutant])
        number_of_rows <- number_of_rows + nrow(content2)
      }

   }
  
  # final result for mean
  if (number_of_rows != 0){
    return(measure/number_of_rows)}
  else {
    return (0)
  }

  
  
}