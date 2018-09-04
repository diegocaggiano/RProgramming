complete <- function(directory,id = 1:332)
{
  
  library(readr)
  
  # In the id vector we have the indexes of the files to be considered
  # For instance if vector id = c(1,19,200), it means the files to be considered
  # are the following: 001.csv, 019.csv and 200.csv
  # That implies converting the integer numbers to character
  
  # initialize vector with file names
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
  
  #initialize result data frame
  df <- data.frame("id"=integer(0), "nobs"=integer(0))
  
  # process every file 
  for (j in 1:length(file_names)) {
    
    complete_cases = 0
    
    # read file
    content <- read_csv(file_names[j],col_names = TRUE, col_types = list(col_date(), col_double(), col_double(), col_integer()))    
    # get only complete cases
    vector_cc <- complete.cases(content)
    content2 <- content[vector_cc,]
    
    # number of complete records
    complete_cases <- nrow(content2)
    
    # preserving id of the current file
    file_id <- content[1, "ID"]
    
    # adding row to data frame
    df[j,] <- c(file_id, complete_cases)
    
  }
  
  
  # returning dataframe with results
  return(df)
  
  
  
}