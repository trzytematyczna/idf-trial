require(jsonlite)
require(timeDate)
require(tidyr)
require(purrr)
require(dplyr)
require(ggplot2)

fromYear <- 2016
toYear <- 2019

data <- data.frame()
for(gyear in fromYear:toYear){
  print(gyear)
  for(gmonth in 1:12){
    fromDate <- paste(gyear,"-", sprintf("%02d", gmonth),"-01", sep="")
    toDate <- timeLastDayInMonth(fromDate)
    newData <- read.csv2(paste(getwd(),"/data/guardian_csv/guardian_", fromDate,"-", toDate, ".csv", sep=""), sep=",")
    data <- rbind(data, newData)
  }
}

data$date_published..date<-as.POSIXct((data$date_published..date/1000), origin="1970-01-01")

data %>% write.csv2(paste(getwd(),"/data/guardian-articles-with-comments.csv", sep=''), row.names = FALSE)

data$og_fields.og.description <-NULL
data$og_fields.og.image<-NULL
data$og_fields.og.title<-NULL
data$og_fields.og.type<-NULL
data$og_fields.og.url<-NULL
data$comments<-NULL

data %>% write.csv2(paste(getwd(),"/data/guardian-articles.csv", sep=''), row.names = FALSE)


############distribution per month
grouped <- data %>% 
  count(format(date_published..date,'%y-%m'))

colnames(grouped)<-c("art_date_published","art_count")

p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + geom_col() 

p + theme(axis.text.x = element_text(angle = 45))
