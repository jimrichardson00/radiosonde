# radiosonde.r

setwd("/home/jim/Dropbox/Python/radiosonde")

region = "pac"
YEAR = "2016"
MONTH = "01"
DAY = "01"
HOUR = "00"
FROM = paste(DAY, HOUR, sep = "")
TO = paste(DAY, HOUR, sep = "")
STNM = "94578"
filename = paste(STNM, "_", YEAR, "_", MONTH, "_", DAY, "_", HOUR, sep = "")

extract_data <- function(region, YEAR, MONTH, FROM, TO, STNM, filename) {

  require(rPython)
  python.load("extract_data.py")
  python.call("extract_data", region, YEAR, MONTH, FROM, TO, STNM, filename)

  lines <- readLines(paste("raw", filename, "_data.txt", sep = ""))
  lines

  data = data.frame(matrix(NA, ncol = 11, nrow = 118 - 6))
  data

  require(stringr)
  names <- as.vector(as.data.frame(str_match_all(pattern = "[:alnum:]+", string = lines[3]))[, 1])
  names

  i = 68
  j = 5
  for(i in seq(6, length(lines))) {
    for(j in seq(1, length(names))) {
      line = lines[i]
      line
      name = names[j]
      name
      end = str_locate(pattern = name, string = lines[3])[2]
      end
      row = i - 5
      row
      col = j
      col
      line
      substr(line, start = end, stop = end)
      substr(line, start = end, stop = end) == " "
      if(substr(line, start = end, stop = end) == " ") {
          data[row, col] <- NA
        } else {
          substr(line, start = 0, stop = end)
          matches = str_match_all(pattern = " +(\\S+)", string = substr(line, start = 0, stop = end))
          matches
          matches = as.data.frame(matches)
          matches
          matches = as.vector(matches[, 2])
          matches
          match = matches[length(matches)]
          match
          data[row, col] <- as.numeric(match)
        }
    }



  names(data) <- names
  data[is.na(data)] <- NA

  write.csv(x = data, file = paste(filename, "_data.txt"))
  
  save(x = data, file = paste(filename, "_data.RData"))

}

extract_info <- function(region, YEAR, MONTH, FROM, TO, STNM, filename) {

  require(rPython)
  python.load("extract_info.py")
  python.call("extract_info", region, YEAR, MONTH, FROM, TO, STNM, filename)

  lines <- readLines(paste("raw", filename, "_info.txt", sep = ""))
  lines

  names <- vector()

  data = data.frame(matrix(NA, ncol = length(lines), nrow = 1))
  data

  i = 2
  j = 5
  for(i in seq(2, length(lines))) {

    start = as.data.frame(str_locate_all(pattern = "\\s+\\S", lines[i]))[1, 2]
    end = as.data.frame(str_locate_all(pattern = "\\S+\\:", lines[i]))[1, 2] - 1

    name <- substr(x = lines[i], start = start, stop = end)
    name <- str_replace_all(string = name, pattern = " ", replacement = "_")
    names <- c(names, name)

    value <- str_match(pattern = ".+\\:\\s+(\\S+)", lines[i])[, 2]

    data[1, i] <- value

    }

    names(data) <- names
    data[is.na(data)] <- NA

    write.csv(x = data, file = paste(filename, "_info.txt"))
    
    save(x = data, file = paste(filename, "_info.RData"))

  }

}

extract_data(region = region, YEAR = YEAR, MONTH = MONTH, FROM = FROM, TO = TO, STNM = STNM, filename = filename)

extract_info(region = region, YEAR = YEAR, MONTH = MONTH, FROM = FROM, TO = TO, STNM = STNM, filename = filename)




