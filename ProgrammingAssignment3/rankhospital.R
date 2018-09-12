rankhospital <- function(state, outcome, num ="best") {
  
  ## Read outcome data
  wdir<- "C:/Data_Science/RProgramming/ProgrammingAssignment3/ProgAssignment3-data"
  file_name <- paste(wdir, "/", "outcome-of-care-measures.csv",sep = "" )
  outcome_data <- read.csv(file_name, colClasses = "character", stringsAsFactors=FALSE)
  
  ## Check that state and outcome parameters are valid
  
  ## We assume state is valid if it is contained in the 7th column of data set
  v_states <- !is.na(outcome_data[,7])
  list_of_states <- outcome_data[v_states,7]
  if (state %in% list_of_states) state<- state
  else  stop("invalid state")
  
  ## outcome
  if (outcome == "heart attack") outcome_index <- 11          # 11th column of data set
  else if (outcome == "heart failure") outcome_index <- 17  # 17th column of data set
  else if (outcome == "pneumonia") outcome_index <- 23    # 23rd column of data set
  else stop("invalid outcome")  # bad input        
  
  
  ## Return hospital name in that state with the given rank 30-day death
  ## rate for the selected outcome
  
  ## get only those records which are not NA for the selected outcome
  outcome_data[, outcome_index] <- suppressWarnings(as.numeric(outcome_data[,outcome_index]))
  v_rate <- !is.na(outcome_data[,outcome_index])
  good_outcome_data <- outcome_data[v_rate,]
  
  ## filter only those records related to the state value given
  good_outcome_data_by_state <- subset(good_outcome_data, State==state)
  
  ## sort dataframe by the selected outcome in ascending order and then
  ## by hospital name (2nd column)
  outcome_data_sorted <- good_outcome_data_by_state[order(good_outcome_data_by_state[,outcome_index], good_outcome_data_by_state[,2]),]
  
  
  ## return result based on the num parameter value
  if (num == "best") return (outcome_data_sorted[1,2])
  if (num == "worst") return (outcome_data_sorted[nrow(outcome_data_sorted),2])
  if (num > nrow(outcome_data_sorted)) return (NA)
  else
      return(outcome_data_sorted[num,2])
    
}