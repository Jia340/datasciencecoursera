complete <- function(directory, id=1:332){
  com_case=data.frame(id=numeric(),nobs=numeric())
  j=1
  for(i in id){
    tmp <- read.csv(paste(directory, "/",as.character(formatC(i, width=3, flag="0")),".csv",sep=""),header=T)
    com_case[j,]=c(i,nrow(tmp[complete.cases(tmp),]))
    j=j+1
  }
  com_case
}