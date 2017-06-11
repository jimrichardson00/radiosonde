# cloud_amount_master.r

master_dir = "/home/jim/Dropbox/Python/cloud_amount"
data_dir = "/home/jim/Dropbox/Python/cloud_amount/data"

setwd(master_dir)

source("cloud_amount_functions.r")

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
for(year in seq(2000, 2016, 1)) {

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
        # filename = paste("\\data\\", STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")

        # print(filename)

        tryCatch(

        {

          paste("sed", filename, "_data.txt", sep = "")

          setwd(data_dir)
          file.exists(paste("sed", filename, "_data.txt", sep = "")) == FALSE
          if(file.exists(paste("", filename, "_data.txt", sep = "")) == FALSE) {
            extract_data(region = region, YEAR = YEAR, MONTH = MONTH, DAY = DAY, HOUR = HOUR, FROM = FROM, TO = TO, STNM = STNM, STATION = STATION, filename = filename)
          }
          setwd(master_dir)

          data <- NA
          setwd(data_dir)
          data <- read.csv(paste("sed", filename, "_data.txt", sep = ""))
          setwd(master_dir)
          data <- na.omit(data)
          head(data)

          data$DEWDEP <- data$TEMP - data$DWPT
          data$HGHTft <- data$HGHT*3.28084
          data <- na.omit(data)

          # restrict to less than 10000ft
          data <- data[data$HGHTft <= 10000, ]
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

          diff = seq(-420, 2100 - 420, length = 21)
          diff

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

                  x = as.numeric(sky[i, 2]) + diff
                  FEW <- c(FEW, interp(x))

                  } else if (sky[i, 1] == "SCT") {

                    # print(paste("SCT", predict(object = smth, x = sky[i, 2])$y), sep = " ")
                    # SCT <- c(SCT, predict(object = smth, x = sky[i, 2])$y)

                    # x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                    # SCT <- c(SCT, predict(object = smth, x = x)$y)

                    x = as.numeric(sky[i, 2]) + diff
                    SCT <- c(SCT, interp(x))

                    } else if (sky[i, 1] == "BKN") {

                      # print(paste("BKN", predict(object = smth, x = sky[i, 2])$y), sep = " ")
                      # BKN <- c(BKN, predict(object = smth, x = sky[i, 2])$y)

                      # x = as.numeric(sky[i, 2]) + seq(0, 1000, length = 21)
                      # BKN <- c(BKN, predict(object = smth, x = x)$y)

                      x = as.numeric(sky[i, 2]) + diff
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

                        x = as.numeric(sky[i, 2]) + diff
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

# ----------------------------------

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
head(data_train)

formula <- as.formula(paste("factor(Cover) ~ ", paste(paste("V", seq(1, 21, 1), sep = ""), collapse = " + "), sep = ""))
formula

# ?randomForest
rndf <- randomForest(formula, data = data_train)
rndf

train <- sample(seq(1, nrow(data_train), 1), size = ceiling(nrow(data_train)*(9/10)))
data_tra <- data_train[train, ]
data_tes <- data_train[-train, ]

# ?randomForest
rndf <- randomForest(factor(Cover) ~ V5, data = data_tra)
rndf

output <- as.character(predict(object = rndf, newdata = data_tes))
output

data_tes$output <- output

output == "FEW"

output <- data.frame(cbind(output[, c("FEW", "SCT", "BKN", "OVC")], data_tes$Cover, data_tes$V5))
head(output)

write.csv(output, "ouput.csv")

png("/home/jim/Dropbox/Python/cloud_amount/2.png")
plot(density(data_tes[output == "BKN", ]$V5), col = "red", ylim = c(0, 0.5))
lines(density(data_tes[output == "SCT", ]$V5), col = "blue", ylim = c(0, 0.5))
lines(density(data_tes[output == "FEW", ]$V5), col = "green", ylim = c(0, 0.5))
dev.off()


lm <- lm() ~ V5, data = data_tra)

predict(object = lm, newdata = data_tes)

tapply(data_tes$V5, data_tes$output, summary)




rndf$result


importance(rndf, type = 1)

weights = 1680 - abs(diff)
weights = weights/sum(weights)
weights
sum(weights)

DEWDEP = weights[1]*data_train[, "V1"] + weights[2]*data_train[, "V2"] + weights[3]*data_train[, "V3"] + weights[4]*data_train[, "V4"] + weights[5]*data_train[, "V5"] + weights[6]*data_train[, "V6"] + weights[7]*data_train[, "V7"] + weights[8]*data_train[, "V8"] + weights[9]*data_train[, "V9"] + weights[10]*data_train[, "V10"] + weights[11]*data_train[, "V11"] + weights[12]*data_train[, "V12"] + weights[13]*data_train[, "V13"] + weights[14]*data_train[, "V14"] + weights[15]*data_train[, "V15"] + weights[16]*data_train[, "V16"] + weights[17]*data_train[, "V17"] + weights[18]*data_train[, "V18"] + weights[19]*data_train[, "V19"] + weights[20]*data_train[, "V20"] + weights[21]*data_train[, "V21"]
data_train$DEWDEP <- DEWDEP

index = which(diff == 0)

plot(hist(FEW))
plot(hist(SCT))
plot(hist(BKN))
plot(hist(OVC))

print(length(FEW))
print(length(SCT))
print(length(BKN))
print(length(OVC))

png("/home/jim/Dropbox/Python/cloud_amount/1.png")
plot(density(data_train[data_train$Cover == "OVC", ]$DEWDEP), col = "black", type = "l", ylim = c(0, 0.6), xlim = c(0, 20))
lines(density(data_train[data_train$Cover == "BKN", ]$DEWDEP), col = "red", ylim = c(0, 0.5))
lines(density(data_train[data_train$Cover == "SCT", ]$DEWDEP), col = "blue", ylim = c(0, 0.5))
lines(density(data_train[data_train$Cover == "FEW", ]$DEWDEP), col = "green", ylim = c(0, 0.5))
dev.off()

png("/home/jim/Dropbox/Python/cloud_amount/2.png")
plot(density(data_train[data_train$Cover == "OVC", ]$V5), col = "black", type = "l", ylim = c(0, 0.6), xlim = c(0,20))
lines(density(data_train[data_train$Cover == "BKN", ]$V5), col = "red", ylim = c(0, 0.5))
lines(density(data_train[data_train$Cover == "SCT", ]$V5), col = "blue", ylim = c(0, 0.5))
lines(density(data_train[data_train$Cover == "FEW", ]$V5), col = "green", ylim = c(0, 0.5))
dev.off()

tapply(data_train$V5, data_train$Cover, summary)

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

# setwd("/home/jim/Dropbox/Python/cloud_amount")

# source("cloud_amount_functions.r")

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
