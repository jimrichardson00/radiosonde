# cloud_amount_analysis.r

setwd(master_dir)
require(data.table)
data <- fread("data.txt")
head(data)

oct = 1
interp_oct <- foreach(oct = seq(1, 8, 1)) %dopar%  {

  data_oct <- data[data$cldoct1 == oct | data$cldoct2 == oct | data$cldoct3 == oct | data$cldoct4 == oct, ]

  relhum_oct <- c(data_oct$relhum1, data_oct$relhum2, data_oct$relhum3, data_oct$relhum4)
  head(relhum_oct)
  unique(relhum_oct)

  dens_oct <- density(na.omit(relhum_oct))

  dens_oct$y <- dens_oct$y/sum(dens_oct$y)

  interp_oct <- approxfun(x = dens_oct$x, y = dens_oct$y)

  interp_oct

}

interp_oct[[1]](80)

oct_probs <- foreach(relhum = seq(70, 100, 1)) %dopar% {

  total <- sum(unlist(lapply(seq(1, 8, 1), function(i) interp_oct[[i]](relhum))))

  oct = which.max(c(
        interp_oct[[1]](relhum)
        , interp_oct[[2]](relhum)
        , interp_oct[[3]](relhum)
        , interp_oct[[4]](relhum)
        , interp_oct[[5]](relhum)
        , interp_oct[[6]](relhum)
        , interp_oct[[7]](relhum)
        , interp_oct[[8]](relhum)
        )
      )

  data.frame(relhum = relhum

    , oct = oct

    , cls = oct_probs_df$cls <- unlist(lapply(oct, 
        FUN = function(oct)
        if(oct >= 0 & oct < 3) {
          return("FEW")
        } else if(oct >= 3 & oct < 5) {
          return("SCT")
        } else if(oct >= 5 & oct < 8) {
          return("BKN")
        } else if(oct >= 8) {
          return("OVC")
        }
        )
    )

    , probmorethan1oct = sum(unlist(lapply(seq(1, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    , probmorethan2oct = sum(unlist(lapply(seq(2, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    , probmorethan3oct = sum(unlist(lapply(seq(3, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    , probmorethan4oct = sum(unlist(lapply(seq(4, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    , probmorethan5oct = sum(unlist(lapply(seq(5, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    , probmorethan6oct = sum(unlist(lapply(seq(6, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    , probmorethan7oct = sum(unlist(lapply(seq(7, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    , probmorethan8oct = sum(unlist(lapply(seq(8, 8, 1), function(i) interp_oct[[i]](relhum))))/total
    )

}

oct_probs_df <- lapply(X = oct_probs, FUN = rbind)

oct_probs_df <- do.call(rbind, oct_probs)
head(oct_probs_df)

write.csv(oct_probs_df, "oct_probs_df.csv")

# plot(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[1]]), type = "l", ylim = c(0, 0.02))
# lines(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[2]]), type = "l")
# lines(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[3]]), type = "l")
# lines(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[4]]), type = "l")
# lines(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[5]]), type = "l")
# lines(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[6]]), type = "l")
# lines(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[7]]), type = "l")
# lines(seq(70, 100, 1), lapply(X = seq(70, 100, 1), FUN = interp_oct[[8]]), type = "l")

