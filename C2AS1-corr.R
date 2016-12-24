corr <-function(directory, threshold=0){
  cor_value=numeric()
  j=1
  for(i in 1:332){
    if(complete(directory, i)$nobs>threshold){
      tmp <- read.csv(paste(directory, "/",as.character(formatC(i, width=3, flag="0")),".csv",sep=""),header=T)
      tmp=tmp[complete.cases(tmp),]
      cor_value[j]=cor(tmp$sulfate,tmp$nitrate)
      j=j+1
    }
  }
  cor_value
}