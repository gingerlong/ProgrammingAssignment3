rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcome_list <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome_tmp <-outcome_list[,c(2, 7, 11, 17, 23)]
 
  colnames(outcome_tmp) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  outcome_name <- c("heart attack", "heart failure", "pneumonia")
  state_name <- unique(outcome_list$state)
  
  ## Check that state and outcome are valid

  
  if (!is.element(outcome, outcome_name))
  {
    stop("invalid outcome")
  }
  
  else {
        suppressWarnings(outcome_tmp[,outcome]<-as.numeric(outcome_tmp[,outcome]))
        data2<-subset(outcome_tmp,!is.na(outcome_tmp[outcome]))
      #  data3<-split(data2,data2$state)
      #  data4<-lapply(data3, function(x) x<-x[order(x[, outcome],x[,"hospital"]),])
        
        for(item in state_name){
          frame <- as.data.frame(data4[item])}
        # by(data, factorlist, function)
        # example obtain variable means separately for
        # each level of byvar in data frame mydata
        data5<-by(data2, data2$state, function(x) x<-x[order(x[, outcome],x[,"hospital"]),])      
        
        result<- data.frame()
        if(is.character(num)){
          if(num == 'best'){
          for (i in 1:54){
            result<-rbind(result,data5[[i]][1,1:2])
          }
            return(result)
          }else if(num == 'worst'){
            for (i in 1:54){
              result<-rbind(result,data5[[i]][nrow(data5[[i]]),1:2])
            }
            return(result)
          }
        }else if(is.numeric(num)){
          for (i in 1:54){
            if (num > nrow(data5[[i]])){
              data5[[i]][num,2]<-names(data5)[i]
            }
          result<-rbind(result,data5[[i]][num,1:2])
          }
          return(result)
        }else
          NA 
  }
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)