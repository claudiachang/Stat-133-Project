#---------------------------------------------------------
#Name: Yaou Duan
#Partner: Claudia Chang
#Stat133 Final Project 
#Skeleton
#8/10/15
#---------------------------------------------------------
# create 6 folders in project
dir.create("./code")
dir.create("./rawdata")
dir.create("./data")
dir.create("./resources")
dir.create("./report")
dir.create("./images")

#download data
download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat",
              "./rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat")

download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv",
              "./rawdata/Basin.EP.csv")

download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv",
              "./rawdata/Basin.NA.csv")
#create readme
file.create("./README.md")
