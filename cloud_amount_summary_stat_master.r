# cloud_amount_master.r

master_dir = "/home/jim/Dropbox/Python/cloud_amount"

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

require(hash)
station_dict = hash(keys = c("YBBN", "YMML", "YPPH", "YPAD", "YPDN"), 
  values = c("94578", "94866", "94610", "94672", "94120"))

keys(station_dict)

station = "YBBN"
year <- 2010
month <- 5
day <- 7
hour <- 0
for(station in keys(station_dict)) {

  print("")
  print(station)

  FEW <- vector()
  SCT <- vector()
  BKN <- vector()
  OVC <- vector()

  # for(year in seq(2016, 2016, 1)) {
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
          STATION = station
          STATION
          STNM = station_dict[[station]]
          STNM

          filename = paste(STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")
          # print(filename)

          data_dir = paste(master_dir, "/data/", STATION, sep = "")
          data_dir

          tryCatch(

          {

            paste("sed", filename, "_data.txt", sep = "")

            setwd(data_dir)
            file.exists(paste("sed", filename, "_data.txt", sep = "")) == FALSE
            if(file.exists(paste("", filename, "_data.txt", sep = "")) == FALSE) {
              extract_data(region = region, YEAR = YEAR, MONTH = MONTH, DAY = DAY, HOUR = HOUR, FROM = FROM, TO = TO, STNM = STNM, STATION = STATION, filename = filename)
            }

            data <- NA
            setwd(data_dir)
            data <- read.csv(paste("sed", filename, "_data.txt", sep = ""))
            data <- na.omit(data)

            data$DEWDEP <- data$TEMP - data$DWPT
            data$HGHTft <- data$HGHT*3.28084
            data <- na.omit(data)

            # restrict to less than 10000ft
            data <- data[data$HGHTft <= 10000, ]
            data

            interpDEWDEP <- NA
            interpDEWDEP <- approxfun(x = data$HGHTft, y = data$DEWDEP)
            interpPRES <- NA
            interpPRES <- approxfun(x = data$HGHTft, y = data$PRES)
            interpTEMP <- NA
            interpTEMP <- approxfun(x = data$HGHTft, y = data$TEMP)
            interpDWPT <- NA
            interpDWPT <- approxfun(x = data$HGHTft, y = data$DWPT)
            interpRELH <- NA
            interpRELH <- approxfun(x = data$HGHTft, y = data$RELH)
            interpMIXR <- NA
            interpMIXR <- approxfun(x = data$HGHTft, y = data$MIXR)
            interpDRCT <- NA
            interpDRCT <- approxfun(x = data$HGHTft, y = data$DRCT)
            interpSKNT <- NA            
            interpSKNT <- approxfun(x = data$HGHTft, y = data$SKNT)  

            setwd(data_dir)
            paste("sky", filename, "_metar.txt", sep = "")
            file.exists(paste("sky", filename, "_metar.txt", sep = "")) == FALSE
            if(file.exists(paste("sky", filename, "_metar.txt", sep = "")) == FALSE) {
              extract_metar_sky(region = region, YEAR = YEAR, MONTH = MONTH, DAY = DAY, HOUR = HOUR, FROM = FROM, TO = TO, STNM = STNM, STATION = STATION, filename = filename)
            }

            metar <- NA
            setwd(data_dir)
            metar <- read.csv(file = paste("sky", filename, "_metar.txt", sep = ""))
            metar

            require(stringr)
            metar <- metar[, as.vector(na.omit(str_match(pattern = "sky.+", names(metar))[, 1]))]
            metar

            sky = NA
            sky = data.frame(skyc = c(as.character(metar$skyc1), as.character(metar$skyc2), as.character(metar$skyc3), as.character(metar$skyc4)),
              skyl = c(as.numeric(as.character(metar$skyl1)), as.numeric(as.character(metar$skyl2)), as.numeric(as.character(metar$skyl3)), as.numeric(as.character(metar$skyl4))))
            sky

            if(nrow(sky) == 4) {

              i <- 1
              for(i in seq(1, nrow(sky), 1)) {

                is.na(sky[i, 2]) == FALSE & sky[i, 2] <= 5000
                if(is.na(sky[i, 2]) == FALSE & sky[i, 2] <= 5000) {

                  (sky[i, 1] == "FEW")
                  if(sky[i, 1] == "FEW") {

                    x = as.numeric(sky[i, 2])
                    FEW <- c(FEW,
                      interpDEWDEP(x), 
                      interpPRES(x), 
                      interpTEMP(x), 
                      interpDWPT(x), 
                      interpRELH(x), 
                      interpMIXR(x), 
                      interpDRCT(x), 
                      interpSKNT(x))

                    } else if (sky[i, 1] == "SCT") {

                      x = as.numeric(sky[i, 2])
                      SCT <- c(SCT,
                        interpDEWDEP(x), 
                        interpPRES(x), 
                        interpTEMP(x), 
                        interpDWPT(x), 
                        interpRELH(x), 
                        interpMIXR(x), 
                        interpDRCT(x), 
                        interpSKNT(x))

                      } else if (sky[i, 1] == "BKN") {

                        x = as.numeric(sky[i, 2])
                        BKN <- c(BKN,
                          interpDEWDEP(x), 
                          interpPRES(x), 
                          interpTEMP(x), 
                          interpDWPT(x), 
                          interpRELH(x), 
                          interpMIXR(x), 
                          interpDRCT(x), 
                          interpSKNT(x))

                        # if(interp(sky[i, 2]) > 20) {
                        #   print(doesnt_exist)
                        # }
                        

                        } else if (sky[i, 1] == "OVC") {

                          x = as.numeric(sky[i, 2])
                          OVC <- c(OVC,
                            interpDEWDEP(x), 
                            interpPRES(x), 
                            interpTEMP(x), 
                            interpDWPT(x), 
                            interpRELH(x), 
                            interpMIXR(x), 
                            interpDRCT(x), 
                            interpSKNT(x))

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

  FEWm <- matrix(FEW, ncol = 8, byrow = TRUE)
  FEWm <- as.data.frame(FEWm)
  FEWm$Cover <- rep("FEW", nrow(FEWm))
  FEWm <- na.omit(FEWm)

  SCTm <- matrix(SCT, ncol = 8, byrow = TRUE)
  SCTm <- as.data.frame(SCTm)
  SCTm$Cover <- rep("SCT", nrow(SCTm))
  SCTm <- na.omit(SCTm)

  BKNm <- matrix(BKN, ncol = 8, byrow = TRUE)
  BKNm <- as.data.frame(BKNm)
  BKNm$Cover <- rep("BKN", nrow(BKNm))
  BKNm <- na.omit(BKNm)

  OVCm <- matrix(OVC, ncol = 8, byrow = TRUE)
  OVCm <- as.data.frame(OVCm)
  OVCm$Cover <- rep("OVC", nrow(OVCm))
  OVCm <- na.omit(OVCm)

  data_train <- rbind(FEWm, SCTm, BKNm, OVCm)
  data_train <- na.omit(data_train)
  names(data_train) <- c("DEWDEP", 
    "PRES", 
    "TEMP", 
    "DWPT", 
    "RELH", 
    "MIXR", 
    "DRCT", 
    "SKNT",
    "Cover"
    )
  setwd(master_dir)
  save(x = data_train, file = paste("data_train", STATION, ".txt", sep = ""))

  png(paste("/home/jim/Dropbox/Python/cloud_amount/", STATION ,"RH vs cloud amount.png", sep = ""))
  plot(density(data_train[data_train$Cover == "OVC", ]$RELH), col = "black", xlim = c(0, 100), ylim = c(0, 0.2), 
    main = paste(STATION, "RH v.s. cloud amount (FEW, SCT, BKN, OVC)", sep = ""),
    xlab = "RH",
    ylab = "Density")
  lines(density(data_train[data_train$Cover == "BKN", ]$RELH), col = "red", ylim = c(0, 0.2))
  lines(density(data_train[data_train$Cover == "SCT", ]$RELH), col = "blue", ylim = c(0, 0.2))
  lines(density(data_train[data_train$Cover == "FEW", ]$RELH), col = "green", ylim = c(0, 0.2))
  legend("topleft", legend = c("OVC", "BKN", "SCT", "FEW"), fill = c("black", "red", "blue", "green"))
  dev.off()

  summary_stats = tapply(data_train$RELH, data_train$Cover, summary)

  summary_stats_df <- rbind(summary_stats$FEW, summary_stats$SCT, summary_stats$BKN, summary_stats$OVC)
  summary_stats_df = as.data.frame(summary_stats_df)
  summary_stats_df
  summary_stats_df$cloud_amount <- c("FEW", "SCT", "BKN", "OVC")
  summary_stats_df


  file.exists(paste(master_dir, "/", STATION, "_summary_stats.txt", sep = "")) == FALSE
  if(paste(master_dir, "/", STATION, "_summary_stats.txt", sep = "") == TRUE) {
    file.remove(paste(master_dir, "/", STATION, "_summary_stats.txt", sep = ""))
  }
  sink(file = paste(master_dir, "/", STATION, "_summary_stats.txt", sep = ""), append = TRUE)
  print("FEW")
  summary_stats[["FEW"]]
  print("")
  print("SCT")
  summary_stats[["SCT"]]
  print("")
  print("BKN")
  summary_stats[["BKN"]]
  print("")
  print("OVC")
  summary_stats[["OVC"]]
  print("")
  sink()

}

# ----------------------------------

# # ---------------------------------------------
# # - Cross validation. For each i = 1,... N, randomly remove one night with fog, train classifier on the rest of the data and test on result, print results

# Outputs <- "Cover"
# Inputs <- c("DEWDEP", 
#   "PRES", 
#   "TEMP", 
#   "DWPT", 
#   "RELH", 
#   "MIXR", 
#   "DRCT", 
#   "SKNT"
#   )

# Outputs_f <- vector()
# for(Output in Outputs) {
#   Outputs_f_o <- unique(data_train[, Output])
#   for(Output_f_o in Outputs_f_o) {
#     data_train[, paste(Output, Output_f_o, sep = "")] <- as.numeric(ifelse(data_train[, Output] == Output_f_o, 1, 0))
#     print(paste(Output, " ", Output_f_o, sep = ""))
#     Outputs_f <- c(Outputs_f, paste(Output, Output_f_o, sep = ""))
#   }
#   data_train[, Output] <- factor(data_train[, Output])
# }

# # creates empty vector for each classifier to fill with percent correct values
# per_correct_arnns <- vector()
# per_correct_rndfs <- vector()
# per_correct_nbays <- vector()
# per_correct_modes <- vector()

# print("Starting: cross validation")
# i <- 1 
# require(foreach)
# N <- 100
# # results <- foreach(i = seq(1, N, 1)) %dopar% {
# results <- for(i in seq(1, N, 1)) {

#   print(paste("Cross validation: ", i, sep = ""))

#   train <- sample(seq(1, nrow(data_train), 1), size = ceiling(nrow(data_train)*(2/3)))
#   data_tra <- data_train[train, ]
#   head(data_tra)
#   data_tes <- data_train[-train, ]
#   head(data_tes)

#   tryCatch({

#     # random forest and naive bays
#     rndf_result <- matrix(NA, nrow = nrow(data_tes), ncol = length(Outputs_f))
#     rndf_result <- as.data.frame(rndf_result)
#     names(rndf_result) <- Outputs_f
#     head(rndf_result)

#     o <- 1
#     for(o in seq(1, length(Outputs), 1)) {

#       Output <- Outputs[o]
#       Outputs_f_o <- as.vector(Outputs_f[is.na(str_match(Outputs_f, Output)) == FALSE])
#       Outputs_f_o

#       # train random forest on training data
#       require(randomForest)
#       Inputs
#       rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
#       , data = data_tra
#       , replace = TRUE
#       , strata = factor(rep(unique(data_tra[, Outputs]), nrow(data_tra)))
#       )
#       rndf

#       # run classifier on test data, and store result
#       output <- as.data.frame(predict(object = rndf, newdata = data_tes, type = "prob"))
#       names(output) <- paste(Output, names(output), sep = "")
#       for(Output_f_o in Outputs_f) {
#         rndf_result[, Output_f_o] <- output[, Output_f_o]
#       }
#       head(rndf_result)

#     }

#   },
#   error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
#   )


#   n_correct <- 0
#   for(i in seq(1, nrow(data_tes), 1)) {
#     n_correct <- n_correct + sum(which.max(rndf_result[1, ]) == which.max(data_tes[1, c("CoverFEW", "CoverSCT", "CoverBKN", "CoverOVC")]))
#     }

#   # percent correct for each classifier
#   # per_correct_arnns <- c(per_correct_arnns, sum(arnn_result[2] > P_val))
#   per_correct_rndfs <- c(per_correct_rndfs, n_correct/nrow(data_tes))
#   # per_correct_nbays <- c(per_correct_nbays, sum(nbay_result[2] > P_val))
#   # per_correct_modes <- c(per_correct_modes, sum(mode_result[2] > P_val))

# }

# per_correct_rndfs
# # list containing percent correct for each iteration in each classifier
# per_correct_resu <- list(
#   per_correct_arnns = per_correct_arnns, 
#   per_correct_rndfs = per_correct_rndfs,
#   per_correct_nbays = per_correct_nbays, 
#   per_correct_modes = per_correct_modes
#   )

# per_correct_rndfs

# # print mean percent correct for each classifier
# print(paste("per_correct_arnn: ", formatC(round(mean(per_correct_arnns), digits = 3), format = "f", digits = 3), sep = ""))
# print(paste("per_correct_rndf: ", formatC(round(mean(per_correct_rndfs), digits = 3), format = "f", digits = 3), sep = ""))
# print(paste("per_correct_nbay: ", formatC(round(mean(per_correct_nbays), digits = 3), format = "f", digits = 3), sep = ""))
# print("")

# per_correct_resu <- list(
#   per_correct_arnns = per_correct_arnns, 
#   per_correct_rndfs = per_correct_rndfs,
#   per_correct_nbays = per_correct_nbays, 
#   per_correct_modes = per_correct_modes
# )

#  # saves percent correct information
# print(paste("per_correct", paste(Outputs, collapse = ""), ".RData", sep = ""))
# save(per_correct_resu, file = paste("per_correct_", paste(Outputs, collapse = ""), ".RData", sep = ""))
