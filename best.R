## Function to determine which hospital has the best outcome of care for a given condition 
## (heart attack, heart failure, or pneumonia) in a given state based on minimum mortality rates.
## If there is a tie, the earlier alphabetic hospital is given.

best <- function(this_state, outcome) {
  ## Read outcome data
  outcomes_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  alloutcomes <- c("heart attack", "heart failure", "pneumonia")

  if (!this_state %in% state.abb) {
    stop("invalid state")
  }
  if(!outcome %in% alloutcomes) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
    # Create subset based on state
    state_data <- subset(outcomes_data, State == this_state)
    # Drop factor levels on hospital name
    state_data$Hospital.Name <- as.character(state_data$Hospital.Name)
    # Create vector containing relevant columns
    outcome_select <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
  ## Function to determine minimum mortality rate in data set  
    find_min <- function(data, outcome) {
      this_outcome <- outcome_select[outcome]
      min_rate <- min(as.numeric(data[, this_outcome]), na.rm = TRUE)
      return (min_rate)
    }
  
  ## Function to find set of hospitals matching minimum rate
    find_best <- function(minimum, outcome) {
      this_outcome <- outcome_select[outcome]
      best_set <- state_data$Hospital.Name[as.numeric(state_data[, this_outcome]) == minimum]
      # remove NA values from hospital names
      best_set <- best_set[!is.na(best_set)]
      if (length(best_set) > 1) {
        sort(best_set)
      }
      return (best_set[1])
    }
    
    find_best(find_min(state_data, outcome), outcome)
}