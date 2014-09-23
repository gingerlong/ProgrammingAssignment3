setwd("/home/user/data_analysis/R_programming/project3")

best <- function(state, outcome) {
  ## Read outcome data and subset the effective information
  outcome_list <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_tmp <-outcome_list[,c(2, 7, 11, 17, 23)]
  
  
  state_name <- unique(outcome_list$State)
  colnames(outcome_tmp) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  outcome_name <- c("heart attack", "heart failure", "pneumonia")
 
  ## Check that state and outcome are valid
  if (!is.element(state, state_name))
    { stop("invalid state")
     }
 
  else if (!is.element(outcome, outcome_name))
    {
    stop("invalid outcome")
     }
    
  else {outcome_new <-outcome_tmp[outcome_tmp$State == state,]
  
  #(outcome_new1 <- as.data.frame(lapply(outcome_new,function(x) if(is.character(x)|is.factor(x)) gsub("Not Available","NA",x) else x)))
  # gsub("Not Available", "NA", outcome_new)
 
  temp <- outcome_new[, eval(outcome)]

  suppressWarnings(row_number<- which.min(temp))
  outcome_new[row_number, 1]

  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
}
