# cloud_amount_master.r

options(scipen=3)
require(doParallel)
registerDoParallel(cores = 3)

setwd("/media/jim/FAT323/Projects/cloud_amount")
require(data.table)
metar <- fread("HM01X_Data_040842_999999999445572.txt"
  # , nrow = 0
  # , header = FALSE
  , stringsAsFactors = FALSE
  , data.table = FALSE)
# metar < - as.data.frame(metar, optional = TRUE)
head(metar)

setnames(metar, "hm", "hm")
setnames(metar, "Station Number", "stnnumb")
setnames(metar, "Day/Month/Year in DD/MM/YYYY format", "dateloc")
setnames(metar, "Hour24:Minutes  in HH24:MI format in Local standard time", "timeloc")
setnames(metar, "Precipitation in last 10 minutes in mm", "preci10")
setnames(metar, "Quality of precipitation in last 10 minutes", "prec10q")
setnames(metar, "Precipitation since 9am local time in mm", "prec9am")
setnames(metar, "Quality of precipitation since 9am local time", "prec9aq")
setnames(metar, "Air Temperature in degrees C", "tempera")
setnames(metar, "Quality of air temperature", "tempqua")
setnames(metar, "Wet bulb temperature in degrees C", "wetbulb")
setnames(metar, "Quality of Wet bulb temperature", "wetbulq")
setnames(metar, "Dew point temperature in degrees C", "dewpntt")
setnames(metar, "Quality of dew point temperature", "dewpntq")
setnames(metar, "Relative humidity in percentage %", "relhumi")
setnames(metar, "Quality of relative humidity", "relhumq")
setnames(metar, "Vapour pressure in hPa", "vappres")
setnames(metar, "Quality of vapour pressure", "vappreq")
setnames(metar, "Saturated vapour pressure in hPa", "satvapp")
setnames(metar, "Quality of saturated vapour pressure", "satvapq")
setnames(metar, "Wind speed in km/h", "windkmh")
setnames(metar, "Wind speed quality", "windkmq")
setnames(metar, "Wind direction in degrees true", "winddir")
setnames(metar, "Wind direction quality", "winddrq")
setnames(metar, "Speed of maximum windgust in last 10 minutes in  km/h", "windgkm")
setnames(metar, "Quality of speed of maximum windgust in last 10 minutes", "wingkmq")
setnames(metar, "Cloud amount(of first group) in eighths", "cldoct1")
setnames(metar, "Quality of first group of cloud amount", "cldamq1")
setnames(metar, "Cloud type(of first group) in in_words", "cldtyp1")
setnames(metar, "Quality of first group of cloud type", "cldtyq1")
setnames(metar, "Cloud height (of first group) in feet", "cldhgt1")
setnames(metar, "Quality of first group of cloud height", "cldhtq1")
setnames(metar, "Cloud amount(of second group) in eighths", "cldoct2")
setnames(metar, "Quality of second group of cloud amount", "cldamq2")
setnames(metar, "Cloud type(of second group) in in_words", "cldtyp2")
setnames(metar, "Quality of second group of cloud type", "cldtyq2")
setnames(metar, "Cloud height (of second group) in feet", "cldhgt2")
setnames(metar, "Quality of second group of cloud height", "cldhtq2")
setnames(metar, "Cloud amount(of third group) in eighths", "cldoct3")
setnames(metar, "Quality of third group of cloud amount", "cldamq3")
setnames(metar, "Cloud type(of third group) in in_words", "cldtyp3")
setnames(metar, "Quality of third group of cloud type", "cldtyq3")
setnames(metar, "Cloud height (of third group) in feet", "cldhgt3")
setnames(metar, "Quality of third group of cloud height", "cldhtq3")
setnames(metar, "Cloud amount(of fourth group) in eighths", "cldoct4")
setnames(metar, "Quality of fourth group of cloud amount", "cldamq4")
setnames(metar, "Cloud type(of fourth group) in in_words", "cldtyp4")
setnames(metar, "Quality of fourth group of cloud type", "cldtyq4")
setnames(metar, "Cloud height (of fourth group) in feet", "cldhgt4")
setnames(metar, "Quality of fourth group of cloud height", "cldhtq4")
setnames(metar, "Ceilometer cloud amount(of first group)", "cldocc1")
setnames(metar, "Quality of first group of ceilometer cloud amount", "cldacq1")
setnames(metar, "Ceilometer cloud height (of first group) in feet", "cldhtc1")
setnames(metar, "Quality of first group of ceilometer cloud height", "cldhcq1")
setnames(metar, "Ceilometer cloud amount(of second group)", "cldocc2")
setnames(metar, "Quality of second group of ceilometer cloud amount", "cldacq2")
setnames(metar, "Ceilometer cloud height (of second group) in feet", "cldhtc2")
setnames(metar, "Quality of second group of ceilometer cloud height", "cldhcq2")
setnames(metar, "Ceilometer cloud amount(of third group)", "cldocc3")
setnames(metar, "Quality of third group of ceilometer cloud amount", "cldacq3")
setnames(metar, "Ceilometer cloud height (of third group) in feet", "cldhtc3")
setnames(metar, "Quality of third group of ceilometer cloud height", "cldhcq3")
setnames(metar, "Horizontal visibility in km", "visobsk")
setnames(metar, "Quality of horizontal visibility", "visobsq")
setnames(metar, "Direction of minimum visibility in degrees", "visobmn")
setnames(metar, "Quality of direction of minimum visibility", "visobmq")
setnames(metar, "AWS visibility in km", "visawsk")
setnames(metar, "Quality of AWS(Automatic Weather Station) visibility", "visawsq")
setnames(metar, "Present weather in text", "presewx")
setnames(metar, "Quality of present weather", "preswxq")
setnames(metar, "Intensity of first present weather in text", "prwxin1")
setnames(metar, "Quality of intensity of first present weather", "prwxiq1")
setnames(metar, "Descriptor of first present weather in text", "prwxde1")
setnames(metar, "Quality of descriptor of first present weather", "prwxdq1")
setnames(metar, "Type of first present weather in text", "prwxty1")
setnames(metar, "Quality of type of first present weather", "prwxtq1")
setnames(metar, "Intensity of second present weather in text", "prwxin2")
setnames(metar, "Quality of intensity of second present weather", "prwxiq2")
setnames(metar, "Descriptor of second present weather in text", "prwxiq2")
setnames(metar, "Quality of descriptor of second present weather", "prwxdq2")
setnames(metar, "Type of second present weather in text", "prwxty2")
setnames(metar, "Quality of type of second present weather", "prwxtq2")
setnames(metar, "Intensity of third present weather in text", "prwxin3")
setnames(metar, "Quality of intensity of third present weather", "prwxiq3")
setnames(metar, "Descriptor of third present weather in text", "prwxde3")
setnames(metar, "Quality of descriptor of third present weather", "prwxdq3")
setnames(metar, "Type of third present weather in text", "prwxty3")
setnames(metar, "Quality of type of third present weather", "prwxtq3")
setnames(metar, "Descriptor of first recent weather in text", "rewxde1")
setnames(metar, "Quality of descriptor of first recent weather", "rewxdq1")
setnames(metar, "Type of first recent weather in text", "rewxty1")
setnames(metar, "Quality of type of first recent weather", "rewxtq1")
setnames(metar, "Descriptor of second recent weather in text", "rewxde2")
setnames(metar, "Quality of descriptor of second recent weather", "rewxdq2")
setnames(metar, "Type of second recent weather in text", "rewxty2")
setnames(metar, "Quality of type of second recent weather", "rewxtq2")
setnames(metar, "Descriptor of third recent weather in text", "rewxde3")
setnames(metar, "Quality of descriptor of third recent weather", "rewxdq3")
setnames(metar, "Type of third recent weather in text", "rewxty3")
setnames(metar, "Quality of type of third recent weather", "rewxtq3")
setnames(metar, "AWS present weather in text", "prwxaws")
setnames(metar, "Quality of AWS present weather", "prwxawq")
setnames(metar, "AWS weather for last 15 minutes in text", "awwx15m")
setnames(metar, "Quality of AWS weather for last 15 minutes", "awwx15q")
setnames(metar, "AWS weather for last 60 minutes in text", "awwx60m")
setnames(metar, "Quality of AWS weather for last 60 minutes", "awwx60q")
setnames(metar, "Mean sea level pressure in hPa", "mslphPa")
setnames(metar, "Quality of mean sea level pressure", "mslphPq")
setnames(metar, "Station level pressure in hPa", "stprehP")
setnames(metar, "Quality of station level pressure", "stprehq")
setnames(metar, "QNH pressure in hPa", "QNHphPa")
setnames(metar, "Quality of QNH pressure", "QNHhPaq")
setnames(metar, "AWS Flag", "awsflag")
setnames(metar, "#", "x")

metar$cldhgt1 <- as.numeric(metar$cldhgt1)
metar$cldhgt2 <- as.numeric(metar$cldhgt2)
metar$cldhgt3 <- as.numeric(metar$cldhgt3)
metar$cldhgt4 <- as.numeric(metar$cldhgt4)

metar <- metar[any(-is.na(c(metar$cldhgt1, metar$cldhgt2, metar$cldhgt3, metar$cldhgt4))), ]
head(metar)

metar <- metar[is.na(metar$cldhgt1) | metar$cldhgt1 <= 5000, ]
metar <- metar[is.na(metar$cldhgt2) | metar$cldhgt2 <= 5000, ]
metar <- metar[is.na(metar$cldhgt3) | metar$cldhgt3 <= 5000, ]
metar <- metar[is.na(metar$cldhgt4) | metar$cldhgt4 <= 5000, ]

metar$datetime_loc <- paste(metar$dateloc, " ", metar$timeloc, sep = "")
head(metar)

metar <- metar[, c("datetime_loc"
  , "dateloc"
  , "timeloc"
  , "cldoct1"
  , "cldamq1"
  , "cldtyp1"
  , "cldtyq1"
  , "cldhgt1"
  , "cldhtq1"
  , "cldoct2"
  , "cldamq2"
  , "cldtyp2"
  , "cldtyq2"
  , "cldhgt2"
  , "cldhtq2"
  , "cldoct3"
  , "cldamq3"
  , "cldtyp3"
  , "cldtyq3"
  , "cldhgt3"
  , "cldhtq3"
  , "cldoct4"
  , "cldamq4"
  , "cldtyp4"
  , "cldtyq4"
  , "cldhgt4"
  , "cldhtq4"
  , "cldocc1"
  , "cldacq1"
  , "cldhtc1"
  , "cldhcq1"
  , "cldocc2"
  , "cldacq2"
  , "cldhtc2"
  , "cldhcq2"
  , "cldocc3"
  , "cldacq3"
  , "cldhtc3"
  , "cldhcq3")]

setwd("/media/jim/FAT323/Projects/cloud_amount")
require(data.table)
upperair <- fread("UA01D_Data_040842_999999999446605.txt"
  # , nrow = 0
  # , header = FALSE
  , stringsAsFactors = FALSE
  , data.table = FALSE)
head(upperair)

setnames(upperair, "ua", "ua" )
setnames(upperair, "Station Number", "stnnumb" )
setnames(upperair, "Station Name", "stnname" )
setnames(upperair, "Locality", "localit" )
setnames(upperair, "State", "stateau" )
setnames(upperair, "Latitude", "latitud" )
setnames(upperair, "Longitude", "longitu" )
setnames(upperair, "Month/Year site opened (MM/YYYY)", "monyeao" )
setnames(upperair, "Month/Year site closed (MM/YYYY)", "monyeac" )
setnames(upperair, "WMO Index Number", "wmoindn" )
setnames(upperair, "Rainfall district code", "raindst" )
setnames(upperair, "River station ID", "rivstid" )
setnames(upperair, "Aviation ID", "aviatid" )
setnames(upperair, "Height above MSL", "habvmsl" )
setnames(upperair, "Day/Month/Year Hour24:Minutes in DD/MM/YYYY HH24:MI format in Local Time", "datetime_loc" )
setnames(upperair, "Day/Month/Year Hour24:Minutes in DD/MM/YYYY HH24:MI format in Local Standard Time", "datetime_std" )
setnames(upperair, "Day/Month/Year Hour24:Minutes in DD/MM/YYYY HH24:MI format in UTC - Coordinated Universal Time", "datetime_utc" )
setnames(upperair, "Air temperature in Degrees C", "tempera" )
setnames(upperair, "Quality of air temperature", "tempqua" )
setnames(upperair, "Dew point temperature in Degrees C", "dewpntt" )
setnames(upperair, "Quality of dew point temperature", "dewpntq" )
setnames(upperair, "Relative humidity in percentage %", "relhumi" )
setnames(upperair, "Quality of relative humidity", "relhumq" )
setnames(upperair, "Wind speed measured in knots", "windkno" )
setnames(upperair, "Quality of wind speed", "windknq" )
setnames(upperair, "Wind direction measured in degrees", "winddir" )
setnames(upperair, "Quality of wind direction", "winddiq" )
setnames(upperair, "Pressure in hPa", "preshPa" )
setnames(upperair, "Quality of pressure", "preshPq" )
setnames(upperair, "Geopotential height in gpm to nearest 0.1m", "geopgpm" )
setnames(upperair, "Quality of geopotential height", "geopgpq" )
setnames(upperair, "Level type", "levelt" )
setnames(upperair, "#", "x" )

upperair$dateutc <- substr(upperair$datetime_utc, start = 1, stop = 10)

# for(name in names(upperair)) {
#   print(name)
# }

upperair$geopgpf <- 3.28084*as.numeric(upperair$geopgpm)
upperair$datetime_loc <- as.character(upperair$datetime_loc)

upperairs <- upperair[is.na(upperair$geopgpf) | upperair$geopgpf <= 10000, ]

metar <- metar[metar$datetime_loc %in% upperair$datetime_loc, ]
head(metar)

metar <- data.table(metar)
upperair <- data.table(upperair)

require(foreach)
# relhum_list <- foreach(datetime_loc = unique(metar$datetime_loc)[1:100]) %dopar% {
relhum_list <- foreach(datetime_loc = unique(metar$datetime_loc)) %dopar% {

  data.frame(datetime_loc = datetime_loc
    , relhum1 = relhum(datetime_loc, 1)
    , relhum2 = relhum(datetime_loc, 2)
    , relhum3 = relhum(datetime_loc, 3)
    , relhum4 = relhum(datetime_loc, 4)
    )

}
length(relhum_list)
head(relhum_list)

require(plyr)
relhum_dat <- ldply(relhum_list, data.table)
head(relhum_dat)
nrow(relhum_dat)

metar <- as.data.table(metar)
relhum_dat <- as.data.table(relhum_dat)

metar$datetime_loc <- as.character(metar$datetime_loc)
relhum_dat$datetime_loc <- as.character(relhum_dat$datetime_loc)

setkey(metar, datetime_loc)
setkey(relhum_dat, datetime_loc)

data <- metar[relhum_dat]
# data <- relhum_dat[metar]

plot(data$cldoct1, data$relhum1)

setwd("/home/jim/Dropbox/Projects/cloud_amount")
fwrite(data, "data.txt")


