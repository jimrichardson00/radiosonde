# radiosonde_master.r

setwd("/home/jim/Dropbox/Python/radiosonde")

source("radiosonde_functions.r")

region = "pac"
YEAR = "2016"
MONTH = "01"
DAY = "01"
HOUR = "00"
FROM = paste(DAY, HOUR, sep = "")
TO = paste(DAY, HOUR, sep = "")
STNM = "94578"
STATION = "YBBN"
filename = paste(STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")

# few, sct, bkn, ovc

# ------------------------------------

year <- 2010
month <- 10
day <- 7
hour <- 0

FEW <- vector()
SCT <- vector()
BKN <- vector()
OVC <- vector()

# for(year in seq(2010, 2010, 1)) {
for(year in seq(2010, 2016, 1)) {

  for(month in seq(1, 12, 1)) {

    print(paste(year, month))

    for(day in seq(1, 31)) {

      for(hour in c(0)) {

        region = "pac"
        YEAR = as.character(year)
        MONTH = formatC(month, width = 2, flag = "0")
        DAY = formatC(day, width = 2, flag = "0")
        HOUR = formatC(hour, width = 2, flag = "0")
        FROM = paste(DAY, HOUR, sep = "")
        TO = paste(DAY, HOUR, sep = "")
        STNM = "94578"
        STATION = "YBBN"
        filename = paste(STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")

        # print(filename)

        tryCatch(

        {

          paste("sed", filename, "_data.txt", sep = "")
          file.exists(paste("sed", filename, "_data.txt", sep = "")) == FALSE
          if(file.exists(paste(filename, "_data.txt", sep = "")) == FALSE) {
            extract_data(region = region, YEAR = YEAR, MONTH = MONTH, DAY = DAY, HOUR = HOUR, FROM = FROM, TO = TO, STNM = STNM, STATION = STATION, filename = filename)
          }

          data <- NA
          data <- read.csv(paste("sed", filename, "_data.txt", sep = ""))
          data <- na.omit(data)
          head(data)

          data$DEWDEP <- data$TEMP - data$DWPT
          data$HGHTft <- data$HGHT*3.28084
          data <- na.omit(data)

          # restrict to less than 5000ft
          data <- data[data$HGHTft <= 5000, ]
          data

          interp <- NA
          interp <- approxfun(x = data$HGHTft, y = data$DEWDEP)

          paste("sky", filename, "_metar.txt", sep = "")
          file.exists(paste("sky", filename, "_metar.txt", sep = "")) == FALSE
          if(file.exists(paste("sky", filename, "_metar.txt", sep = "")) == FALSE) {
            extract_metar_sky(region = region, YEAR = YEAR, MONTH = MONTH, DAY = DAY, HOUR = HOUR, FROM = FROM, TO = TO, STNM = STNM, STATION = STATION, filename = filename)
          }

          metar <- NA
          metar <- read.csv(file = paste("sky", filename, "_metar.txt", sep = ""))
          metar

          require(stringr)
          metar <- metar[, as.vector(na.omit(str_match(pattern = "sky.+", names(metar))[, 1]))]
          metar

          sky = NA
          sky = data.frame(skyc = c(as.character(metar$skyc1), as.character(metar$skyc2), as.character(metar$skyc3), as.character(metar$skyc4)),
            skyl = c(as.numeric(as.character(metar$skyl1)), as.numeric(as.character(metar$skyl2)), as.numeric(as.character(metar$skyl3)), as.numeric(as.character(metar$skyl4))))
          nrow(sky)
          sky

          if(nrow(sky) == 4) {

            i <- 1
            for(i in seq(1, nrow(sky), 1)) {

              is.na(sky[i, 2]) == FALSE & sky[i, 2] <= 5000
              if(is.na(sky[i, 2]) == FALSE & sky[i, 2] <= 5000) {

                (sky[i, 1] == "FEW")
                if(sky[i, 1] == "FEW") {

                  # print(paste("FEW", predict(object = smth, x = as.numeric(sky[i, 2]))$y, sep = " "))
                  # FEW <- c(FEW, predict(object = smth, x = as.numeric(sky[i, 2]))$y)

                  # x = as.numeric(sky[i, 2]) + seq(-500, 500, 50)
                  # FEW <- c(FEW, predict(object = smth, x = x)$y)

                  x = as.numeric(sky[i, 2]) + seq(0, 1000, 21)
                  FEW <- c(FEW, interp(x))

                  } else if (sky[i, 1] == "SCT") {

                    # print(paste("SCT", predict(object = smth, x = sky[i, 2])$y), sep = " ")
                    # SCT <- c(SCT, predict(object = smth, x = sky[i, 2])$y)

                    # x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                    # SCT <- c(SCT, predict(object = smth, x = x)$y)

                    x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                    SCT <- c(SCT, interp(x))

                    } else if (sky[i, 1] == "BKN") {

                      # print(paste("BKN", predict(object = smth, x = sky[i, 2])$y), sep = " ")
                      # BKN <- c(BKN, predict(object = smth, x = sky[i, 2])$y)

                      # x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                      # BKN <- c(BKN, predict(object = smth, x = x)$y)

                      x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                      BKN <- c(BKN, interp(x))

                      # if(predict(object = smth, x = sky[i, 2])$y > 20) {
                      #   print(doesnt_exist)
                      # }

                      if(interp(sky[i, 2]) > 20) {
                        print(doesnt_exist)
                      }
                      
                      interp(x)

                      } else if (sky[i, 1] == "OVC") {

                        # print(paste("OVC", predict(object = smth, x = sky[i, 2])$y), sep = " ")
                        # OVC <- c(OVC, predict(object = smth, x = sky[i, 2])$y)

                        # x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                        # OVC <- c(OVC, predict(object = smth, x = x)$y)

                        x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                        OVC <- c(OVC, interp(x))

                      }

                    }

                  }

                }

          # if(file.exists(paste(filename, "_info.txt", sep = "")) == FALSE) {

          #   extract_info(region = region, YEAR = YEAR, MONTH = MONTH, FROM = FROM, TO = TO, STNM = STNM, filename = filename)

          # }


        }
        ,
        
        error = function(e) {cat(paste(YEAR, MONTH, DAY, sep = "-"), "ERROR :",conditionMessage(e), "\n")}
        
        )

      }
    }
  }
}

length(seq(-500, 500, 50))

FEWm <- matrix(FEW, ncol = 21, byrow = TRUE)
FEWm <- as.data.frame(FEWm)
FEWm$Cover <- rep("FEW", nrow(FEWm))
FEW <- na.omit(FEW)

SCTm <- matrix(SCT, ncol = 21, byrow = TRUE)
SCTm <- as.data.frame(SCTm)
SCTm$Cover <- rep("SCT", nrow(SCTm))
SCT <- na.omit(SCT)
BKNm <- matrix(BKN, ncol = 21, byrow = TRUE)
BKNm <- as.data.frame(BKNm)
BKNm$Cover <- rep("BKN", nrow(BKNm))
BKN <- na.omit(BKN)
OVCm <- matrix(OVC, ncol = 21, byrow = TRUE)
OVCm <- as.data.frame(OVCm)
OVCm$Cover <- rep("OVC", nrow(OVCm))
OVC <- na.omit(OVC)

data_train <- rbind(FEWm, SCTm, BKNm, OVCm)
data_train <- na.omit(data_train)
class(data_train$Cover)

formula <- as.formula(paste("factor(Cover) ~ ", paste(paste("V", seq(1, 21, 1), sep = ""), collapse = " + "), sep = ""))
formula

?randomForest
rndf <- randomForest(formula, data = data_train)
rndf

plot(data$HGHTft, data$DEWDEP, type = "l")
points(x, predict(object = smth, x = x)$y)
predict(object = smth, x = x)


plot(hist(FEW))
plot(hist(SCT))
plot(hist(BKN))
plot(hist(OVC))

print(length(FEW))
print(length(SCT))
print(length(BKN))
print(length(OVC))

plot(density(FEW), col = "black", type = "l", ylim = c(0, 0.5))
lines(density(SCT), col = "blue", ylim = c(0, 0.5))
lines(density(BKN), col = "red", ylim = c(0, 0.5))
lines(density(OVC), col = "green", ylim = c(0, 0.5))

data_train = data.frame(DEWDEP = c(FEW, SCT, BKN, OVC), 
  Cover = c(rep("FEW", length(FEW)), rep("SCT", length(SCT)), rep("BKN", length(BKN)), rep("OVC", length(OVC))))

require(randomForest)
rndf <- randomForest(formula = Cover ~ DEWDEP, data = data_train)
rndf


# require(fitdistrplus)
# require(logspline)

# FEWp <- FEW
# FEWp[FEWp <= 0] <- 0.01
# SCTp <- SCT
# SCTp[SCTp <= 0] <- 0.01
# BKNp <- BKN
# BKNp[BKNp <= 0] <- 0.01
# OVCp <- OVC
# OVCp[OVCp <= 0] <- 0.01

# max <- max(c(FEWp, SCTp, BKNp, OVCp))

# FEWp <- FEWp/max
# SCTp <- SCTp/max
# BKNp <- BKNp/max
# OVCp <- OVCp/max

# descdist(FEW)
# descdist(SCT)
# descdist(BKN)
# descdist(OVC)


# fit <- fitdist(data = FEWp, distr = "lnorm", start = NULL)
# plot(fit)
# fit$aic

# fit <- fitdist(data = FEWp, distr = "gamma", start = NULL)
# plot(fit)


# fit <- fitdist(data = FEWp, distr = "norm", start = NULL)
# plot(fit)

# fit <- fitdist(data = FEWp, distr = "weibull", start = NULL)
# plot(fit)
# names(fit)

# # extract_data(region = region, YEAR = YEAR, MONTH = MONTH, FROM = FROM, TO = TO, STNM = STNM, filename = filename)

# # extract_info(region = region, YEAR = YEAR, MONTH = MONTH, FROM = FROM, TO = TO, STNM = STNM, filename = filename)

# # --------------------------------------

# # ts at ybbn

# setwd("/home/jim/Dropbox/Python/radiosonde")

# source("radiosonde_functions.r")

# # region = "pac"
# # YEAR = "2016"
# # MONTH = "01"
# # DAY = "01"
# # HOUR = "00"
# # FROM = paste(DAY, HOUR, sep = "")
# # TO = paste(DAY, HOUR, sep = "")
# # STNM = "94578"
# # STATION = "YBBN"
# # filename = paste(STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")

# # ts at the airport

# year <- 2010
# month <- 11
# day <- 31
# hour <- 0

# data_train <- vector()

# # for(year in seq(2010, 2010, 1)) {
# for(year in seq(2010, 2016, 1)) {

#   for(month in seq(1, 12, 1)) {

#     print(paste(year, month))

#     for(day in seq(1, 31, 1)) {

#       for(hour in c(0)) {

#         region = "pac"
#         YEAR = as.character(year)
#         MONTH = formatC(month, width = 2, flag = "0")
#         DAY = formatC(day, width = 2, flag = "0")
#         HOUR = formatC(hour, width = 2, flag = "0")
#         FROM = paste(DAY, HOUR, sep = "")
#         TO = paste(DAY, HOUR, sep = "")
#         STNM = "94578"
#         STATION = "YBBN"
#         filename = paste(STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")

#         tryCatch(

#         {

#           paste(filename, "_info.txt", sep = "")
#           file.exists(paste(filename, "_info.txt", sep = "")) == FALSE
#           if(file.exists(paste(filename, "_info.txt", sep = "")) == FALSE) {

#             extract_info(region = region, YEAR = YEAR, MONTH = MONTH, DAY = DAY, HOUR = HOUR, FROM = FROM, TO = TO, STNM = STNM, filename = filename)

#           }

#           data <- read.csv(paste(filename, "_info.txt", sep = ""))
#           data

#           TS <- extract_metar_TS(region = region, YEAR = YEAR, MONTH = MONTH, DAY = DAY, HOUR = HOUR, FROM = FROM, TO = TO, STNM = STNM, STATION = STATION, filename = filename)
          
#           data$TS <- TS

#           data_train <- rbind(data_train, data)

#         }
#         ,
        
#         error = function(e) {cat(paste(YEAR, MONTH, DAY, sep = "-"), "ERROR :",conditionMessage(e), "\n")}
        
#         )

#       }
#     }
#   }
# }

# write.csv(data_train, "data_train.txt")

# save(x = data_train, file = "data_train.RData")

# head(data

#   )
