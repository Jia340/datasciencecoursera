rankall <- function(outcome, num = "best") {
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
  if(!(outcome %in% outcome_vl)){
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  hosrank <- function(df, oc, n){
    df=df[!is.na(df[[oc]]),]
    df_sort=df[order(df[[oc]],df$Hospital.Name),]
    if(n=="best"){
      n=1
    }
    else if(n=="worst"){
      n=nrow(df_sort)
    }
    hr=df_sort[n,2]
    hr 
  }
  hosr=by(outcome_data, outcome_data$State, function(x) hosrank(df=x, oc=outcome, n=num))
  hosr_df=cbind(hosr,levels(factor(outcome_data$State)))
  colnames(hosr_df)=c("hospital","state")
  rownames(hosr_df)=levels(factor(outcome_data$State))
  hosr_df
}