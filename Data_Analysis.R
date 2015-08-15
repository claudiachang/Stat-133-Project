#---------------------------------------------------------
#Name: Yaou Duan
#Partner: Claudia Chang
#Stat133 Final Project 
#Data Analysis
#8/10/15
#---------------------------------------------------------

storms_data<-read.csv("../data/storms.csv")
tracks_data<-read.csv("../data/tracks.csv")
library(stringr)

#storms.csv data from 1980 to 2010
y<-str_sub(storms_data$date, 1, 4) #extract year
y<-as.numeric(y)
storms1980_2010<-storms_data[which(y>=1980 & y<=2010), ]

#tracks.csv data from 1980 to 2010
tracks1980_2010<-tracks_data[which(tracks_data$id>=min(storms1980_2010$id) &
                                     tracks_data$id<=max(storms1980_2010$id)),]
#max wind speed by id
max_wind_id<-tapply(tracks1980_2010$wind, tracks1980_2010$id, max)

# 1.) Number of storms per year---------------------------
#extract year of each storm
storms_year<-str_extract(storms_data$date, "^[0-9]{4}")
storms_year1<-as.numeric(storms_year)
#from 1980 to 2010
storms_year2<-storms_year1[which(storms_year1 >= 1980 & storms_year1 <= 2010)]
# frequency of each storms by year
table(storms_year2)
# Barplot
png("../images/number_of_storms_per_year.png")
plot(as.factor(storms_year2))
title("Number of storms per year")
dev.off()

pdf("../images/number_of_storms_per_year.pdf")
plot(as.factor(storms_year2))
title("Number of storms per year")
dev.off()
#---------------------------------------------------------

# 2.) Number of storms per year with winds >= 35 knots----
#max wind of each storm by id
date35<-storms1980_2010$date[which(max_wind_id>=35)]#match id to date
year35<-str_extract(date35, "^[0-9]{4}") #extract year
#frequency
table(year35)
#bartplot
png("../images/Number_of_storms_per_year_with_winds_greater_35_knots.png")
plot(as.factor(year35))
title("Number of storms per year with winds greater 35 knots")
dev.off()

pdf("../images/Number_of_storms_per_year_with_winds_greater_35_knots.pdf")
plot(as.factor(year35))
title("Number of storms per year with winds greater 35 knots")
dev.off()
#---------------------------------------------------------

# 3.) Number of storms per year with winds >= 64 knots----
date64<-storms1980_2010$date[which(max_wind_id>=64)] #match id to date
year64<-str_extract(date64, "^[0-9]{4}") #extract year
#frequency
table(year64)
#barplot
png("../images/Number_of_storms_per_year_with_winds_greater_64_knots.png")
plot(as.factor(year64))
title("Number of storms per year with winds greater 64 knots")
dev.off()

pdf("../images/Number_of_storms_per_year_with_winds_greater_64_knots.pdf")
plot(as.factor(year64))
title("Number of storms per year with winds greater 64 knots")
dev.off()
#---------------------------------------------------------

# 4.) Number of storms per year with winds >= 96 knots----
date96<-storms1980_2010$date[which(max_wind_id>=96)] #match id to date
year96<-str_extract(date96, "^[0-9]{4}") #extract year
#frequency
table(year96)
#barplot
png("../images/Number_of_storms_per_year_with_winds_greater_96_knots.png")
plot(as.factor(year96))
title("Number of storms per year with winds greater 96 knots")
dev.off()

pdf("../images/Number_of_storms_per_year_with_winds_greater_96_knots.pdf")
plot(as.factor(year96))
title("Number of storms per year with winds greater 96 knots")
dev.off()
#---------------------------------------------------------

# 5.) number of storms per month--------------------------
month1980_2010<-storms_data$date[which(storms_year1 >= 1980 & storms_year1 <= 2010)] 
storms_month<-str_sub(month1980_2010, 6, 7) #extract month from date
#frequency
table(storms_month)
# barplot
png("../images/Number_of_storms_per_month.png")
plot(as.factor(storms_month))
title("number of storms per month")
dev.off()

pdf("../images/Number_of_storms_per_month.pdf")
plot(as.factor(storms_month))
title("number of storms per month")
dev.off()
#---------------------------------------------------------

# 6.) Number of storms per month with wind >= 35 knots----
month35<-str_sub(date35, 6, 7)
#frequency
table(month35)
#barplot
png("../images/Number_of_storms_per_month_with_wind_greater_35_knots.png")
plot(as.factor(month35))
title("Number of storms per month with wind greater 35 knots")
dev.off()

pdf("../images/Number_of_storms_per_month_with_wind_greater_35_knots.pdf")
plot(as.factor(month35))
title("Number of storms per month with wind greater 35 knots")
dev.off()
#---------------------------------------------------------

# 7.) Number of storms per month with wind >= 64 knots----
month64<-str_sub(date64, 6, 7)
#frequency
table(month64)
#barplot
png("../images/Number_of_storms_per_month_with_wind_greater_64_knots.png")
plot(as.factor(month64))
title("Number of storms per month with wind greater 64 knots")
dev.off()

pdf("../images/Number_of_storms_per_month_with_wind_greater_64_knots.pdf")
plot(as.factor(month64))
title("Number of storms per month with wind greater 64 knots")
dev.off()
#---------------------------------------------------------

# 8.) Number of storms per month with wind >=96 knots-----
month96<-str_sub(date96, 6, 7)
#freqency
table(month96)
#barplot
png("../images/Number_of_storms_per_month_with_wind_greater_96_knots.png")
plot(as.factor(month96))
title("Number of storms per month with wind greater 96 knots")
dev.off()

pdf("../images/Number_of_storms_per_month_with_wind_greater_96_knots.pdf")
plot(as.factor(month96))
title("Number of storms per month with wind greater 96 knots")
dev.off()
#---------------------------------------------------------

# 9.) Avg number of storms >=35 knots---------------------
#avg
avg35<-mean(table(year35))
avg35<-as.numeric(sprintf("%.1f", avg35)) #change decimal
avg35
# std dev
std35<-sd(table(year35))
std35<-as.numeric(sprintf("%.2f", std35))#change decimal
std35
#25th quantile
q1_35<-quantile(table(year35), 0.25)
q1_35
#50th quantile
q2_35<-quantile(table(year35), 0.50)
q2_35
#75th quantile
q3_35<-quantile(table(year35), 0.75)
q3_35
#---------------------------------------------------------

# 10.) Avg number of storms >=64 knots--------------------
#avg
avg64<-mean(table(year64))
avg64<-as.numeric(sprintf("%.1f", avg64)) #change decimal
avg64
#std dev
std64<-sd(table(year64))
std64<-as.numeric(sprintf("%.2f", std64)) #change decimal
std64
#25th quantile
q1_64<-quantile(table(year64), 0.25)
q1_64
#50th quantile
q2_64<-quantile(table(year64), 0.50)
q2_64
#75th quantile
q3_64<-quantile(table(year64), 0.75)
q3_64
#---------------------------------------------------------

# 11.) Avg number of storms >=96 knots--------------------
#avg
avg96<-mean(table(year96))
avg96<-as.numeric(sprintf("%.1f", avg96)) #change decimal
avg96
#std dev
std96<-sd(table(year96))
std96<-as.numeric(sprintf("%.2f", std96)) #change decimal
std96
#25th quantile
q1_96<-quantile(table(year96), 0.25)
q1_96
#50th quantile
q2_96<-quantile(table(year96), 0.50)
q2_96
#75th quantile
q3_96<-quantile(table(year96), 0.75)
q3_96
#---------------------------------------------------------

# summary table-------------------------------------------
summ_year<-data.frame("Wind"=c("35 knots", "64 knots", "96 knots"),
                      "Avg"=c(avg35, avg64, avg96), "StdDev"=
                        c(std35, std64, std96), "25th"= 
                        c(q1_35, q1_64, q1_96), "50th"=
                        c(q2_35, q2_64, q2_96), "75th"=
                        c(q3_35, q3_64, q3_96))
summ_year

#---------------------------------------------------------

#Regression Analysis 1------------------------------------
#mean pressure for each storm(by id)
mean_press<-tapply(tracks1980_2010$press, tracks1980_2010$id, mean)
mean_press1<-mean_press[-which(mean_press==0)] #remove 0 obs
mean_press1

#mean wind for each storm(by id)
mean_wind<-tapply(tracks1980_2010$wind, tracks1980_2010$id, mean)
mean_wind1<-mean_wind[-which(mean_press==0)]
mean_wind1
#regression
regression1<-lm(mean_wind1~mean_press1)
regression1

png("../images/regression1.png")
plot(mean_press1, mean_wind1, xlab = "mean pressure", 
     ylab = "mean wind speed", xlim = c(960, 1050))
abline(regression1, col="#FF0000")
title("mean press vs mean wind speed plot")
dev.off()

pdf("../images/regression1.pdf")
plot(mean_press1, mean_wind1, xlab = "mean pressure", 
     ylab = "mean wind speed", xlim = c(960, 1050))
abline(regression1, col="#FF0000")
title("mean press vs mean wind speed plot")
dev.off()
#---------------------------------------------------------

#Regression Analysis 2------------------------------------
#median pressure for each storm (by id)
median_press<-tapply(tracks1980_2010$press, tracks1980_2010$id, median)
median_press1<-median_press[-which(median_press==0)] #remove 0 obs
median_press1

#median wind for each storm (by id)
median_wind<-tapply(tracks1980_2010$wind, tracks1980_2010$id, median)
median_wind1<-median_wind[-which(median_press==0)]
median_wind1

#regression
regression2<-lm(median_wind1~median_press1)
regression2

png("../images/regression2.png")
plot(median_press1, median_wind1, xlab = "meidan pressure",
     ylab = "median wind speed")
abline(regression2, col="#FF0000")
title("median press vs median wind speed plot")
dev.off()

pdf("../images/regression2.pdf")
plot(median_press1, median_wind1, xlab = "meidan pressure",
     ylab = "median wind speed")
abline(regression2, col="#FF0000")
title("median press vs median wind speed plot")
dev.off()

#---------------------------------------------------------
