rankall <- function(outcome, num ="best") {
  
  ## Read outcome data
  wdir<- "C:/Data_Science/RProgramming/ProgrammingAssignment3/ProgAssignment3-data"
  file_name <- paste(wdir, "/", "outcome-of-care-measures.csv",sep = "" )
  outcome_data <- read.csv(file_name, colClasses = "character", stringsAsFactors=FALSE)
  
  ## Check that outcome parameter is valid
  if (outcome == "heart attack") outcome_index <- 11          # 11th column of data set
  else if (outcome == "heart failure") outcome_index <- 17  # 17th column of data set
  else if (outcome == "pneumonia") outcome_index <- 23    # 23rd column of data set
  else stop("invalid outcome")  # bad input      
  
  ## We create the list of states contained in the dataset
  v_states <- !is.na(outcome_data[,7])
  list_of_states <- unique(outcome_data[v_states,7])
  
  ## get only those records which are not NA for the selected outcome
  outcome_data[, outcome_index] <- suppressWarnings(as.numeric(outcome_data[,outcome_index]))
  v_rate <- !is.na(outcome_data[,outcome_index])
  good_outcome_data <- outcome_data[v_rate,]
  
  ## initialize result dataframe
  result <- data.frame(hospital=character(), state=character())
  
  ## for every state in the dataset
  for (st in list_of_states) {
    ## filter only those records related to the current state value
    good_outcome_data_by_state <- subset(good_outcome_data, State==st)
    
    ## sort dataframe by the selected outcome in ascending order and then
    ## by hospital name (2nd column)
    outcome_data_sorted <- good_outcome_data_by_state[order(good_outcome_data_by_state[,outcome_index], good_outcome_data_by_state[,2]),]

    ## decide result based on the num parameter value
    if (nrow(outcome_data_sorted) == 0) {
          new_row<- data.frame(hospital = NA, state = st)
      
    }
    else {
      if (num == "best")  
        new_row <- data.frame(hospital = outcome_data_sorted[1,2], state = st)
      else if (num == "worst") 
        new_row <- data.frame(hospital = outcome_data_sorted[nrow(outcome_data_sorted),2], state = st)
      else if (num > nrow(outcome_data_sorted)) 
        new_row<- data.frame(hospital = NA, state = st)
      else
        new_row <- data.frame(hospital = outcome_data_sorted[num, 2], state = st)
    }
 
    # add new row to result dataframe
    result <- rbind(result, new_row)
    
  }
    
  # final result
  return (result)
  
}