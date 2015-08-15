# ===========================================
#Name: Yaou Duan
#Partner: Claudia Chang
#Title: Stat 133 Final Project 
#Description: Visualization
#Date: 8/14/15
# ===========================================

library(stringr)
library(maps)
library(ggplot2)


# loading files

download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv",
              "Basin.EP.csv")

download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv",
              "Basin.NA.csv")

# naming files

storms_EP <- read.csv(file = "Basin.EP.csv", header = F, skip = 3)
storms_NA <- read.csv(file = "Basin.NA.csv", header = F, skip = 3)


# ===========================================
# Data preprocessing
# ===========================================


# combine both basins

all_storms <- rbind(storms_EP, storms_NA)


# rename columns

col_names <- c("Serial_Num", 
               "Season", 
               "Num", 
               "Basin", 
               "Sub_basin", 
               "Name", 
               "ISO_time", 
               "Nature", 
               "Latitude", 
               "Longitude", 
               "Wind_kt", 
               "Pressure_mb",
               "Center",
               "Wind_percent",
               "Pressure_percent",
               "Track_type")

colnames(all_storms) <- col_names 


# remove white space

all_storms$Basin <- str_replace(all_storms$Basin, " ", "")


# add storm ID

all_storms$ID <- paste(all_storms$Name, all_storms$Season, sep= "_")


# set Wind_kt as numeric

all_storms$Wind_kt <- as.numeric(all_storms$Wind_kt)




# ===========================================
# Global visualizations
# ===========================================


# -------------------------------------------
# trajectory of all storms (1980-2010)
# -------------------------------------------


# function to extract the year from ISO_time

get_year <- function(x) {
  time_date = str_split(x, " ")
  ISO_date = unlist(lapply(time_date, function(x) x[1]))
  ISO_year = as.numeric(str_sub(ISO_date, 1, 4))
}


# create column of years

all_storms$year <- get_year(all_storms$ISO_time)


# extract only storms 1980-2010

all_storms <- subset(all_storms, year >= 1980 & year <= 2010) 


# factor conversion

all_storms$Wind_kt <- factor(all_storms$Wind_kt) 

# merge levels for a simple legend
levels(all_storms$Wind_kt) <- c(0,
                                rep("10-50", 10), 
                                rep("55-100", 13), 
                                rep("105-165", 13)) 

# map of storms 1980-2010

map <- ggplot(all_storms, 
              aes(x = Longitude, y = Latitude, group = all_storms$ID)) +
  geom_polygon(data = map_data("world"),
               mapping = aes(x = long, y = lat, group = group),
               fill = "#333333", color = "#111111", size = 0.2) +
  geom_path(data = all_storms,
            mapping = aes(group = ID, color = Wind_kt),
            size = 0.5) +
  scale_color_manual(values = c("#b3cde070", "#6497b170", "#005b9670", "#03396c70")) +
  ggtitle("Hurricane Trajectories 1980-2010") +
  labs(x = "", y = "", color = "Wind \n(knots)") +
  theme(aspect.ratio = 1)


# save maps

png(file="All_Storms.png", width=800, height=500) # as PNG
map
dev.off()

pdf(file="All_Storms.pdf", width=800, height=500) # as PDF
map
dev.off()




# -------------------------------------------
# trajectory of all storms (1980-2010)
# -------------------------------------------


# function to extract the month from ISO_time

get_month <- function(x) {
  time_date = str_split(x, " ")
  ISO_date = unlist(lapply(time_date, function(x) x[1]))
  ISO_month = str_sub(ISO_date, 6, 7)
  factor(ISO_month, labels = c(month.name))  
}


# create column of months

all_storms$month <- get_month(all_storms$ISO_time)


# faceted map

map_month <- ggplot(all_storms, 
                    aes(x = Longitude, y = Latitude, group = all_storms$ID)) +
  geom_polygon(data = map_data("world"),
               mapping = aes(x = long, y = lat, group = group),
               fill = "#333333", color = "#111111", size = 0.2) +
  geom_path(data = all_storms,
            mapping = aes(group = ID, color = Wind_kt),
            size = 0.5) +
  scale_color_manual(values = c("#b3cde070", "#6497b170", "#005b9670", "#03396c70")) +
  labs(x = "", y = "", color = "Wind \n(knots)") +
  theme(aspect.ratio = 1) + 
  facet_wrap(~ month, scales = "free") +
  ggtitle("Hurricane Trajectories 1980-2010 by Month") 


# save maps

png(file="All_Storms_by_Month.png", width=800, height=500) # as PNG
map_month
dev.off()

pdf(file="All_Storms_by_Month.pdf", width=800, height=500) # as PDF
map_month
dev.off()



# ===========================================
# Visualizations per decade
# ===========================================

# -------------------------------------------
# 1980's
# -------------------------------------------

# extract storms in 1980's

storms_1980s <- subset(all_storms, year >= 1980 & year < 1990)


# faceted map by year

map_1980s <- ggplot(all_storms, 
                     aes(x = Longitude, y = Latitude, group = ID)) +
  geom_polygon(data = map_data("world"),
               mapping = aes(x = long, y = lat, group = group),
               fill = "#333333", color = "#111111", size = 0.2) +
  geom_path(data = storms_1980s,
            mapping = aes(group = ID, color = Wind_kt),
            size = 0.5) +
  scale_color_manual(values = c("#b3cde070", "#6497b170", "#005b9670", "#03396c70")) +
  labs(x = "", y = "", color = "Wind \n(knots)") +
  theme(aspect.ratio = 1) +
  facet_wrap(~ Season, scales = "free") +
  ggtitle("Hurricane Trajectories 1980's by Year") 



# save maps

png(file="Storms_1980s_by_Year.png", width=800, height=500) # as PNG
map_1980s
dev.off()

pdf(file="Storms_1980s_by_Year.pdf", width=800, height=500) # as PDF
map_1980s
dev.off()


# -------------------------------------------
# 1990's
# -------------------------------------------

# extracted storms in 1990's

storms_1990s <- subset(all_storms, year >= 1990 & year < 2000)


# faceted map by year

map_1990s <- ggplot(all_storms, 
                    aes(x = Longitude, y = Latitude, group = all_storms$ID)) +
  geom_polygon(data = map_data("world"),
               mapping = aes(x = long, y = lat, group = group),
               fill = "#333333", color = "#111111", size = 0.2) +
  geom_path(data = storms_1990s,
            mapping = aes(group = ID, color = Wind_kt),
            size = 0.5) +
  scale_color_manual(values = c("#b3cde070", "#6497b170", "#005b9670", "#03396c70")) +
  labs(x = "", y = "", color = "Wind \n(knots)") +
  theme(aspect.ratio = 1) +
  facet_wrap(~ Season, scales = "free") +
  ggtitle("Hurricane Trajectories 1990's by Year") 


# save maps

png(file="Storms_1990s_by_Year.png", width=800, height=500) # as PNG
map_1990s
dev.off()

pdf(file="Storms_1990s_by_Year.pdf", width=800, height=500) # as PDF
map_1990s
dev.off()


# -------------------------------------------
# 2000's
# -------------------------------------------

# extracted storms in 2000's

storms_2000s <- subset(all_storms, year >= 2000 & year <= 2010)


# faceted map by year

map_2000s <- ggplot(all_storms, 
                    aes(x = Longitude, y = Latitude, group = all_storms$ID)) +
  geom_polygon(data = map_data("world"),
               mapping = aes(x = long, y = lat, group = group),
               fill = "#333333", color = "#111111", size = 0.2) +
  geom_path(data = storms_2000s,
            mapping = aes(group = ID, color = Wind_kt),
            size = 0.5) +
  scale_color_manual(values = c("#b3cde070", "#6497b170", "#005b9670", "#03396c70")) +
  labs(x = "", y = "", color = "Wind \n(knots)") +
  theme(aspect.ratio = 1) +
  facet_wrap(~ Season, scales = "free") +
  ggtitle("Hurricane Trajectories 2000's by Year") 


# save maps

png(file="Storms_2000s_by_Year.png", width=800, height=500) # as PNG
map_2000s
dev.off()

pdf(file="Storms_2000s_by_Year.pdf", width=800, height=500) # as PDF
map_2000s
dev.off()

