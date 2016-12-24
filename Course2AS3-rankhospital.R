rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=F)
  outcome_data[,11]=as.numeric(outcome_data[,11])
  outcome_data[,17]=as.numeric(outcome_data[,17])
  outcome_data[,23]=as.numeric(outcome_data[,23])
  colnames(outcome_data)[11]="heart attack"
  colnames(outcome_data)[17]="heart failure"
  colnames(outcome_data)[23]="pneumonia"
  ## Check that state and outcome are valid
  outcome_vl=c("heart attack", "heart failure", "pneumonia")
  if(!(state %in% outcome_data$State)){
    stop("invalid state")
  }
  if(!(outcome %in% outcome_vl)){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  tmp=outcome_data[which(outcome_data$State==state),]
  tmp=tmp[!is.na(tmp[[outcome]]),]
  tmp_sort=tmp[order(tmp[[outcome]],tmp$Hospital.Name),]
  if(num=="best"){
    num=1
  }
  else if(num=="worst"){
    num=nrow(tmp_sort)
  }
  hr=tmp_sort[num,2]
  hr 
}