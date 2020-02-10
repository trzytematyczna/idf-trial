require(httr)
require(jsonlite)
require(timeDate)

# response <- GET("https://penelope.vub.be/guardian-climate-change-data/articles/2016-01-01/2016-12-31/200")

# json <- content(response,"text")

fromYear <- 2016
toYear <- 2019
delayTime <- 1 #seconds
maxArticles <- 10000

for(gyear in fromYear:toYear){
  for(gmonth in 1:12){
    fromDate <- paste(gyear,"-", sprintf("%02d", gmonth),"-01", sep="")
    toDate <- timeLastDayInMonth(fromDate)
    response <- GET(paste("https://penelope.vub.be/guardian-climate-change-data/articles/",fromDate, "/",paste(toDate),"/",maxArticles, sep=""))
    write(content(response,"text",encoding="UTF-8"), paste("guardian_", fromDate,"-", toDate, sep=""))
  }
  Sys.sleep(delayTime)
}
  