
setwd("/home/user/data_analysis/R_programming/project3")

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
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
        suppressWarnings(outcome_new[,outcome]<-as.numeric(outcome_new[,outcome]))
        data2<-subset(outcome_new,!is.na(outcome_new[outcome]))
        data3<- data2[order(data2[,outcome],data2[, "Hospital"]),]
        if(is.character(num)){
          if(num == 'best'){
            data3[1,"Hospital"]
          }else if(num == 'worst'){
            data3[nrow(data3), "Hospital"]
          }
        }else if(is.numeric(num)){
          data3[num, "Hospital"]
        }else
          NA 
}
}
