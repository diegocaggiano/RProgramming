best <- function(state, outcome) {
  
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
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate for the selected outcome
  
  ## get only those records which are not NA for the selected outcome
  outcome_data[, outcome_index] <- suppressWarnings(as.numeric(outcome_data[,outcome_index]))
  v_rate <- !is.na(outcome_data[,outcome_index])
  good_outcome_data <- outcome_data[v_rate,]
  
  ## filter only those records related to the state value given
  good_outcome_data_by_state <- subset(good_outcome_data, State==state)
  
  ## obtain the min value for the selected outcome
  minvalue<- min(good_outcome_data_by_state[,outcome_index])
  
  ## get the list (vector) of best hospitals in the State
  vminh <- good_outcome_data_by_state[,c(outcome_index)] == minvalue
  dbesth <- good_outcome_data_by_state[vminh,]
  lbesth <- dbesth [,2] ## 2nd column has Hospital name
  
  # finally sort list in case there is more than one "best" hospital
  lbesth <- sort(lbesth)
  
  # final result
  lbesth[1]
  
}