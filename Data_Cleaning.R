#---------------------------------------------------------
#Name: Yaou Duan
#Partner: Claudia Chang
#Stat133 Final Project 
#Data cleaining
#8/10/15
#---------------------------------------------------------
raw_data<-read.csv("../rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat", header = FALSE)
library(stringr)
#storm.csv-------------------------------------------------------------

#extract all headders:
raw_header<-raw_data[grep(pattern = "M=", raw_data[ ,1]), ]

#ID:
storms_id<-str_sub(raw_header, start = 1, end = 5)

#Date
storms_date<-str_extract(raw_header, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
storms_date1<-as.Date(storms_date, "%m/%d/%Y")

#Days
x<-str_extract(raw_header, "M=.[0-9]+")
storms_days<-as.numeric(str_replace_all(x, "M=", ""))

#name
a<-str_sub(raw_header, 36)
storms_name<-str_extract(a, "^[A-Z]+")

#creating storm table
clean_storm<-data.frame(id=seq_along(storms_id), date=storms_date1, 
                        days=storms_days, name=storms_name)
# write .csv
write.csv(clean_storm, "storms.csv")


#tracks.csv-------------------------------------------------------
#take out trailer(not selecting trailer)
raw_tracks<-raw_data[-grep("SRC=", raw_data[,1]), ] # only header and daily

#id
header_num<-grep(pattern = "M=", raw_data[,1]) 
trailer_num<-grep("SRC=", raw_data[,1])
daily_tracks<-raw_data[-c(header_num, trailer_num), ] #remove hoth header and trailer

x1<-c()
x<-seq(1:1777) #1777 storms according to storm.cvs
for(i in 1:length(storms_days)){
  x1=c(x1, rep(x[i], times=storms_days[i]))
}
tracks_id2<-rep(x1, each=4) #each has 4 periods
tail(tracks_id2)
#Date
tracks_date<-str_extract(raw_tracks, "[0-9]{2}/[0-9]{2}/?[0-9]?[0-9]?[0-9]?[0-9]?")
header_num2<-grep("[0-9]{2}/[0-9]{2}/[0-9]{4}", tracks_date)

for(i in 1:length(tracks_date)){
  if(nchar(tracks_date[i])!=10){
    tracks_date[i]<-paste0(tracks_date[i], str_sub(tracks_date[i-1], 6, 10))
  } 
}
b<-as.data.frame(tracks_date)
tracks_date2<-b[-header_num2, ]
tracks_date3<-as.Date(tracks_date2, "%m/%d/%Y")

tracks_date4<-rep(tracks_date3, each = 4) #each date has 4 periods
head(tracks_date4)
#period
p<-c("00h", "06h", "12h", "18h")
tracks_period<-rep(p, times=length(tracks_date2))
tracks_period

#stage
stage<-str_extract_all(daily_tracks, "[*SEWL]", simplify = TRUE)
tracks_stage<-stage[, -5] #remove the last symbol
tracks_stage<-str_replace_all(tracks_stage, "\\*", "cyclone") #replace symbol with name
tracks_stage<-str_replace_all(tracks_stage, "S", "subtropical")
tracks_stage<-str_replace_all(tracks_stage, "E", "extratropical")
tracks_stage<-str_replace_all(tracks_stage, "W", "wave")
tracks_stage<-str_replace_all(tracks_stage, "L", "remanent low")
tracks_stage<-matrix(tracks_stage, ncol=4, byrow = FALSE)

tracks_stage1 = c() #convert matrix to vector by row
for (i in seq(1:nrow(tracks_stage))){
  tracks_stage1 = c(tracks_stage1, tracks_stage[i,])
}
tail(tracks_stage1)


#Lat
Latx<-cbind(str_sub(daily_tracks, 13, 15),
            str_sub(daily_tracks, 30, 32),
            str_sub(daily_tracks, 47, 49),
            str_sub(daily_tracks, 64, 66))
tracks_latx = c() #convert matrix to vector by row
for (i in seq(1:nrow(Latx))){
  tracks_latx = c(tracks_latx, Latx[i,])
}

tracks_latx<-paste0(str_sub(tracks_latx, 1, 2),
                    '.', str_sub(tracks_latx, 3,3)) #decimal
tracks_latx<-as.numeric(tracks_latx)
head(tracks_latx)
tail(tracks_latx)
tracks_latx
#Long
Lon<-str_extract_all(daily_tracks, "[*SEWL][0-9]{7}", simplify = TRUE)
Lon<-str_sub(Lon, 5, 8)
tracks_Lon<-paste0(str_sub(Lon, 1, 3), ".", str_sub(Lon, 4,4))
tracks_Lon<-as.numeric(tracks_Lon)
tracks_Lon<-str_replace_na(tracks_Lon, "0.0")
tracks_Lon<-as.numeric(tracks_Lon)

Lonx<-cbind(str_sub(daily_tracks, 16, 19),
            str_sub(daily_tracks, 33, 36),
            str_sub(daily_tracks, 50, 53),
            str_sub(daily_tracks, 67, 70))

tracks_Lonx = c() #convert matrix to vector by row
for (i in seq(1:nrow(Lonx))){
  tracks_Lonx = c(tracks_Lonx, Lonx[i,])
}

tracks_Lonx<-paste0(str_sub(tracks_Lonx, 1, 3), ".", 
                   str_sub(tracks_Lonx, 4,4)) #decimal

tracks_Lonx<-as.numeric(tracks_Lonx)

for (j in 1: length(tracks_Lonx)){   #convert from 0-360 to -180 to 180
  if (tracks_Lonx[j]>180){
    tracks_Lonx[j]<-tracks_Lonx[j]-360
  }else{
    tracks_Lonx[j]<-tracks_Lonx[j]
  }
}

head(tracks_Lonx)
tail(tracks_Lonx)
tracks_Lonx

#Wind
windx<-cbind(str_sub(daily_tracks, 21, 23 ),
  str_sub(daily_tracks, 38, 40),
  str_sub(daily_tracks, 55, 57),
  str_sub(daily_tracks, 72, 74))

tracks_windx = c() #convert matrix to vector by row
for (i in seq(1:nrow(windx))){
  tracks_windx = c(tracks_windx, windx[i,])
}
tracks_windx<-as.numeric(tracks_windx)
head(tracks_windx)
tail(tracks_windx)

#press
pressx<-cbind(str_sub(daily_tracks, 25, 28),
              str_sub(daily_tracks, 42, 45),
              str_sub(daily_tracks, 59, 62),
              str_sub(daily_tracks, 76, 79))
              
tracks_pressx = c() #convert matrix to vector by row
for (i in seq(1:nrow(pressx))){
  tracks_pressx = c(tracks_pressx, pressx[i,])
}

tracks_pressx<-as.numeric(tracks_pressx)
head(tracks_pressx)
tail(tracks_pressx)

#create table
clean_tracks<-data.frame(id=tracks_id2, date=tracks_date4,
                         period=tracks_period, stage=tracks_stage1,
                         lat=tracks_latx, long=tracks_Lonx,
                         wind=tracks_windx, press=tracks_pressx)
# remove rows where lat, long, wind, and press are all 0
clean_tracks1<-clean_tracks[-which(clean_tracks$lat==0 & clean_tracks$long==0 & 
        clean_tracks$wind==0 & clean_tracks$press ==0), ]

#write in .csv 
write.csv(clean_tracks1, "tracks.csv")



