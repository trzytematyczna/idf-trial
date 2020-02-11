require(jsonlite)
require(timeDate)
require(tidyr)
require(purrr)
require(dplyr)

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
    newData <- fromJSON(txt = paste(getwd(),"/data/guardian-data/guardian_", fromDate,"-", toDate, sep=""))
    newData$og_fields<-NULL
    data <- rbind(data, newData)
    print(gmonth)
  }
}

data_tib <- as_tibble(data, validate = FALSE)
data_raw <- enframe(unlist(data))

data$og_fields<-NULL
asd <-  data %>% 
        discard(is_empty) %>% 
        map_if(is.data.frame, list) %>% 
        as_tibble()


