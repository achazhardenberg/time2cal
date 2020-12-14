time2cal <- function(x,output = "timetable.ics"){
library(dplyr)
library(stringr)
library(lubridate)
library(anytime)

timetable <- data.frame(readLines(file(x)))
timetable<-data.frame(timetable[ grep("^Tasks This Week", timetable[,1], invert = TRUE) , ])
timetable[,2]<-timetable[,1]
timetable[,1]<-ifelse(substring(timetable[,2], 1,1) %in% c(0:9), lag(timetable[,2]), timetable[,2])
timetable[,1]<-ifelse(substring(timetable[,2], 1,1) %in% c(0:9), lag(timetable[,1]), timetable[,1])
colnames(timetable)<-c("Date","col2")

timetable<-timetable[!(timetable[,1]==timetable[,2]),]

timetable$time<-substr(timetable[,2], start = 1, stop = 19)
timetable$event<-NA
for(i in 1:length(timetable$col2)){
timetable$event[i] <- gsub(timetable$time[i],"",timetable$col2[i])
}
timetable$col2<-NULL

timetable$notes<-NA
for(i in 1:length(timetable$event)){
  timetable$notes[i] <- substring(timetable$event[i], regexpr(" in ", timetable$event[i]) + 1)
}
for(i in 1:length(timetable$event)){
  timetable$event[i] <- gsub(timetable$notes[i],"",timetable$event[i])
}
for(i in 1:length(timetable$notes)){
  timetable$notes[i] <- gsub("in ","",timetable$notes[i])
}
timetable$to<-NA
for(i in 1:length(timetable$time)){
  timetable$to[i] <- substring(timetable$time[i], regexpr("-", timetable$time[i]) + 1)
}
timetable$from<-NA
for(i in 1:length(timetable$to)){
  timetable$from[i] <- gsub(timetable$to[i],"",timetable$time[i])
}
for(i in 1:length(timetable$from)){
  timetable$from[i] <- gsub("-","",timetable$from[i])
}
timetable$time<-NULL
timetable$Date <-mdy(timetable$Date)
timetable$from<- format(strptime(timetable$from, "%I:%M %p"), "%H%M")
timetable$to<- format(strptime(timetable$to, "%I:%M %p"), "%H%M")
timetable$Date<- format(strptime(timetable$Date,"%Y-%M-%d"),"%Y%M%d")
timetable$starttime<-paste(timetable$Date,"T",timetable$from,"00",sep="")
timetable$endtime<-paste(timetable$Date,"T",timetable$to,"00",sep="")
# create an ics-file by iterating over an csv-file.

timetable<-data.frame(starttime=timetable$starttime,endtime=timetable$endtime,summary=timetable$event,description=timetable$notes)
timetable$location<-NA

#code from https://github.com/cutterkom/convert-csv-to-ics-in-R :

df<-timetable

# import ics templates
ics_header <- readLines("ics_template/template_header.ics", warn = F)
ics_body <- readLines("ics_template/template_body.ics", warn = F)
ics_footer <- readLines("ics_template/template_footer.ics", warn = F)

# iterate over events and insert events data
ics_events <- ""

for(i in 1:nrow(df)) {

  ics_body <- str_replace(ics_body, "SUMMARY:.*", paste0("SUMMARY:", df$summary[i]))
  ics_body <- str_replace(ics_body, "LOCATION:.*", paste0("LOCATION:", df$location[i]))
  ics_body <- str_replace(ics_body, "DESCRIPTION:.*", paste0("DESCRIPTION:", df$description[i]))
  ics_body <- str_replace(ics_body, "DTSTART:.*", paste0("DTSTART:", df$starttime[i]))
  ics_body <- str_replace(ics_body, "DTEND:.*", paste0("DTEND:", df$endtime[i]))
  # create unique identifier
  ics_body <- str_replace(ics_body, "UID:.*", paste0("UID:", paste0(df$starttime[i], df$endtime[i])))
  ics_events <- append(ics_events, ics_body)
}

# combine template parts to one vector
ics_events <- append(ics_header, ics_events)
ics_events <- append(ics_events, ics_footer)

write(ics_events, file = output)
}


