pollutantmean <- function(directory, pollutant, id=1:332){
  total_count=0
  total_sum=0
  for(i in id){
#    print(paste(directory, "/",as.character(formatC(i, width=3, flag="0")),".csv",sep=""))
    tmp <- read.csv(paste(directory, "/",as.character(formatC(i, width=3, flag="0")),".csv",sep=""),header=T)
    a=tmp[[pollutant]]
    a=a[!is.na(a)]
    total_count=total_count+length(a)
    total_sum=total_sum+sum(a)
  }
  total_sum/total_count
}