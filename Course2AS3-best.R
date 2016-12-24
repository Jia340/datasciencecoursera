best <- function(state, outcome) {
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
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  tmp=outcome_data[which(outcome_data$State==state),]
  bh=sort(tmp[which(tmp[[outcome]]==min(tmp[[outcome]],na.rm=T)),2])[1]
  bh
}