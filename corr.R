corr <- function(directory, threshold = 0){
  
  library(readr)
  working_directory <- paste(getwd(), "/", directory, sep="")
  
  # initialize vector
  result_vector = vector(mode="double")
  
  # obtaining list of CSV files in directory
  # assuming all of them have the correct format 
  list_of_files <- list.files(working_directory, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
  
  if (length(list_of_files) > 0) {
    # for every file in the list
    for (j in 1:length(list_of_files)) {
      # read file
      content <- read_csv(list_of_files[j],col_names = TRUE, col_types = list(col_date(), col_double(), col_double(), col_integer()))    
      # get only complete cases
      vector_cc <- complete.cases(content)
      content2 <- content[vector_cc,]
    
      # number of complete records
      complete_cases <- nrow(content2)
      
      if (complete_cases > threshold){
        # calculate correlation
        correlation <- cor(content2[,"sulfate"], content2[,"nitrate"])       
        result_vector <- append(result_vector, correlation)
      } #end if
      
      
    } #end for
    
  } #end if
  
  return (result_vector)
  
}