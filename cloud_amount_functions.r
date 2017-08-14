# radiosonde.r


relhum <- function(datetime_loc, cldgrpn) {

  index_upp <- which(upperair$datetime_loc == datetime_loc)
  index_upp

  index_met <- which(metar$datetime_loc == datetime_loc)
  index_met

  col <- which(names(metar) == paste("cldhgt", cldgrpn, sep = ""))
  col

  metar[index_met[1], ]
  cldhgt_cldgrpn <- metar[index_met[1], col, with = FALSE]
  cldhgt_cldgrpn <- as.numeric(cldhgt_cldgrpn)
  head(cldhgt_cldgrpn)

  is.na(cldhgt_cldgrpn)
  if(is.na(cldhgt_cldgrpn)) {

    return(NA)

  } else {

      geopgpf <- upperair[index_upp, "geopgpf"]
      geopgpf <- as.numeric(as.vector(geopgpf$geopgpf))
      head(geopgpf)

      min(na.omit(geopgpf)) > cldhgt_cldgrpn | max(na.omit(geopgpf)) < cldhgt_cldgrpn
      if(min(na.omit(geopgpf)) > cldhgt_cldgrpn | max(na.omit(geopgpf)) < cldhgt_cldgrpn) {

        return(NA)

      } else {

        upperair[index_upp, ]
        relhumi <- upperair[index_upp, "relhumi"]
        relhumi <- as.numeric(as.vector(relhumi$relhumi))
        head(relhumi)

        head(cldhgt_cldgrpn - geopgpf)
        min(abs(cldhgt_cldgrpn - geopgpf))
        index_min <- which.min(abs(cldhgt_cldgrpn - geopgpf))
        index_min

        (cldhgt_cldgrpn - geopgpf)[index_min]
        (cldhgt_cldgrpn - geopgpf)[index_min] > 0
        if((cldhgt_cldgrpn - geopgpf)[index_min] > 0) {

          geopgpf1 <- geopgpf[index_min]
          geopgpf2 <- geopgpf[index_min + 1]

          relhumi1 <- relhumi[index_min]
          relhumi2 <- relhumi[index_min + 1]

        } else {

          geopgpf1 <- geopgpf[index_min - 1]
          geopgpf1

          geopgpf2 <- geopgpf[index_min]
          geopgpf2

          relhumi1 <- relhumi[index_min - 1]
          relhumi2 <- relhumi[index_min]

      }  

    }


  ((relhumi2 - relhumi1)/(geopgpf2 - geopgpf1))*(cldhgt_cldgrpn - geopgpf1) + relhumi1
  return(((relhumi2 - relhumi1)/(geopgpf2 - geopgpf1))*(cldhgt_cldgrpn - geopgpf1) + relhumi1)

  }

}


# ----------------------

# region = "pac"
# YEAR = "2016"
# MONTH = "01"
# DAY = "01"
# HOUR = "00"
# FROM = "0100"
# TO = "0100"
# STNM = "94578"

# filename = paste(STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")

extract_data <- function(region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename) {

  setwd(data_dir)
  paste("raw", filename, "_info.txt", sep = "")
  file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE) {

    require(rPython)
    setwd(master_dir)
    python.load("extract_data.py")
    python.call("extract_data", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

  setwd(data_dir)
  file.exists(paste("raw", filename, "_data.txt", sep = "")) == TRUE
  if(file.exists(paste("raw", filename, "_data.txt", sep = "")) == TRUE) {

    setwd(data_dir)
    file.exists(paste("sed", filename, "_data.txt", sep = "")) == FALSE
    if(file.exists(paste("sed", filename, "_data.txt", sep = "")) == FALSE) {

      lines <- readLines(paste("raw", filename, "_data.txt", sep = ""))
      lines[3]

      require(stringr)
      ends <- as.vector(as.data.frame(str_locate_all(pattern = "\\S\\s", string = lines[3]))[, 2])
      ends <- sort(ends, decreasing = TRUE)
      ends

      col_delim <- paste("-e 's/./\\,/", paste(ends, collapse = "' -e 's/./\\,/"), "'", sep = "")
      col_delim

      paste("sed -e '/^\\s*$/d'" # deletes blank lines
      , " -e '/^-*$/d' " #deletes --- lines
      , col_delim #replaces with, at specific positions
      , " -e 's/ //g'" #replace space with blank
      , " -e '4d'" #deletes 4th line
      , paste(" raw", filename, "_data.txt", sep = "")
      , paste(" > sed", filename, "_data.txt", sep = "")
      , sep = "")

      system(
        paste("sed -e '/^\\s*$/d'" # deletes blank lines
        , " -e '/^-*$/d' " #deletes --- lines
        , col_delim #replaces with, at specific positions
        , " -e 's/ //g'" #replace space with blank
        , " -e '4d'" #deletes 4th line
        , paste(" raw", filename, "_data.txt", sep = "")
        , paste(" > sed", filename, "_data.txt", sep = "")
        , sep = "")
          )


    }

  }

}

# extract_data(region = region, YEAR = YEAR, MONTH = MONTH, FROM = FROM, TO = TO, STNM = STNM, filename = filename)

extract_info <- function(region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, filename) {

  setwd(data_dir)
  paste("raw", filename, "_info.txt", sep = "")
  file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE) {

    require(rPython)
    setwd(master_dir)
    python.load("extract_info.py")
    python.call("extract_info", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

  setwd(data_dir)
  file.exists(paste("raw", filename, "_info.txt", sep = ""))
  if(file.exists(paste("raw", filename, "_info.txt", sep = ""))) {

    lines <- readLines(paste("raw", filename, "_info.txt", sep = ""))
    lines

    system(
      paste("sed -e '/^\\s*$/d'" # deletes blank lines
      , " -e '/^-*$/d' " #deletes --- lines
      , " -e 's/\\s//g'" #replace space with blank
      , " -e 's/:/,/g'" #replace space with blank
      , paste(" raw", filename, "_info.txt", sep = "")
      , paste(" > sed", filename, "_info.txt", sep = "")
      , sep = "")
        )

    data <- read.csv(file = paste("sed", filename, "_info.txt", sep = ""), header = FALSE)
    data <- as.data.frame(data)

    info <- as.data.frame(matrix(NA, nrow = 1, ncol = nrow(data)))
    info[1, ] <- as.vector(data[, 2])
    names(info) <- as.vector(as.character(data[, 1]))

    setwd(data_dir)
    write.csv(x = info, file = paste(filename, "_info.txt", sep = ""))      
    save(x = info, file = paste(filename, "_info.RData", sep = ""))

  }

}

# extract_info(region = region, YEAR = YEAR, MONTH = MONTH, FROM = FROM, TO = TO, STNM = STNM, filename = filename)

# returns 1 if TS on UTC calendar day
TS_f <- function(metar) {
  metar <- as.data.frame(metar)
  presentwx <- metar$presentwx
  require(stringr)
  n_TS <- length(na.omit(as.vector(str_match(pattern = ".*TS.*", string = presentwx))))
  if(n_TS == 0){
    return(0)
  } else {
    return(1)
  }
}

extract_metar_sky <- function(region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename) {

  setwd(data_dir)
  paste("raw", filename, "_metar.txt", sep = "")
  file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE) {

    require(rPython)
    setwd(master_dir)
    python.load("extract_metar.py")
    python.call("extract_metar", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

  setwd(data_dir)
  paste("sky", filename, "_metar.txt", sep = "")
  file.exists(paste("sky", filename, "_metar.txt", sep = "")) == FALSE
  if(file.exists(paste("sky", filename, "_metar.txt", sep = "")) == FALSE) {

    metar <- read.csv(paste("raw", filename, "_metar.txt", sep = ""), skip = 5)
    metar

    metar$valid <- as.character(metar$valid)

    require(stringr)
    metar <- metar[which(metar$valid == paste(YEAR, "-", MONTH, "-", DAY, " ", HOUR, ":00", sep = "")), ]
    metar

    write.csv(x = metar, file = paste("sky", filename, "_metar.txt", sep = ""))
    save(x = metar, file = paste("sky", filename, "_metar.RData", sep = ""))

  }

}

extract_metar_TS <- function(region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename) {

  setwd(data_dir)
  paste("raw", filename, "_metar.txt", sep = "")
  file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE) {

    require(rPython)
    setwd(master_dir)
    python.load("extract_metar.py")
    python.call("extract_metar", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

  setwd(data_dir)
  paste(filename, "_metar.txt", sep = "")
  file.exists(paste(filename, "_metar.txt", sep = "")) == FALSE
  if(file.exists(paste(filename, "_metar.txt", sep = "")) == FALSE) {

    metar <- read.csv(paste("raw", filename, "_metar.txt", sep = ""), skip = 5)
    metar

    TS <- TS_f(metar)
    TS

    return(TS)

  } else {

    return(NA)
  
  }

}
