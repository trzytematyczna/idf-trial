require(jsonlite)
require(timeDate)


fromYear <- 2016
toYear <- 2019
data<-fromJSON(txt=paste(getwd(),"/data/guardian-data/guardian_2016-01-01-2016-01-31",sep=""))
data2<-fromJSON(txt=paste(getwd(),"/data/guardian-data/guardian_2016-02-01-2016-02-29",sep=""))
data <- data.frame()
for(gyear in fromYear:toYear){
  print(gyear)
  for(gmonth in 1:12){
    print(gmonth)
    fromDate <- paste(gyear,"-", sprintf("%02d", gmonth),"-01", sep="")
    print(gmonth)
    toDate <- timeLastDayInMonth(fromDate)
    print(gmonth)
    data <- rbind(data, fromJSON(txt = paste(getwd(),"/data/guardian-data/guardian_", fromDate,"-", toDate, sep="")))
    print(gmonth)
  }
}
