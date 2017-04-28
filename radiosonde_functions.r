# radiosonde.r

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

  paste("raw", filename, "_info.txt", sep = "")
  file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE) {

    require(rPython)
    python.load("extract_info.py")
    python.call("extract_info", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

  file.exists(paste("raw", filename, "_data.txt", sep = "")) == TRUE
  if(file.exists(paste("raw", filename, "_data.txt", sep = "")) == TRUE) {

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

  paste("raw", filename, "_info.txt", sep = "")
  file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_info.txt", sep = "")) == FALSE) {

    require(rPython)
    python.load("extract_info.py")
    python.call("extract_info", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

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

  paste("raw", filename, "_metar.txt", sep = "")
  file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE) {

    require(rPython)
    python.load("extract_metar.py")
    python.call("extract_metar", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

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

  paste("raw", filename, "_metar.txt", sep = "")
  file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE
  if(file.exists(paste("raw", filename, "_metar.txt", sep = "")) == FALSE) {

    require(rPython)
    python.load("extract_metar.py")
    python.call("extract_metar", region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename)

  }

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