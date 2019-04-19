# # # # # # # # # # # # # # # # # # # # #
# # Handles the plotting for urban.R  # #
# # # # # # # # # # # # # # # # # # # # #

#TODO: specify that the list returned from urban.R is requried to use this
# function.

# This develops the main plots for the IDF function after all rural and urban
# locations have been processed
returns.plot <- function(returns, loc_names, stationary) {
  # color scheme
  colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")

  # other initializations
  x <- durations
  rp <- c(2,20,100) #return return periods

  i <- length(x) #should be # of durations
  j <- length(rp) #should be # of return return periods

  make.plot <- function(file, tail, locs, z_lim) {
    jpeg(file, width=500, height=750)
    it <- 1
    for (loc in locs) {
      # Variable init
      df <- returns[[loc]]
      col <- colors[it] #holds a different color for each location

      # holds data $ confidence interval
      z = ci_l = ci_u <- rep(0, i)

      # Handle data for each duration
      for(m in 1:i) {
        if(tail) {
          z[m] <- tail(df[[m]][[n]]$y, n=1)
          ci_l[m] <- tail(df[[m]][[n]]$ci_l, n=1)
          ci_u[m] <- tail(df[[m]][[n]]$ci_u, n=1)
        } else {
          z[m] <- df[[m]][[n]]$y[1]
          ci_l[m] <- df[[m]][[n]]$ci_l[1]
          ci_u[m] <- df[[m]][[n]]$ci_u[1]
        }
      }
      if (it == 1) {
        plot(x, z, ylim=z_lim, ylab=expression(paste("Temperature [",degree,"F]")),
            xlab="Duration (days)", main = paste(rp[n], "-Year Return Levels Curve",
            dir, sep=""), type = "o", col=col)
      } else {
      lines(x, z, type = "o", col=col)
      }
      arrows(x, ci_l, x, ci_u, length=0.05, angle=90, code=3, col=col)
      it <- it + 1
    }
    legend("bottomright", legend = locs, lty = 1, col = colors[1:it])
    dev.off()
  }

  locs  <- c() #this will hold the locations of non-empty IDF analyses
  for (n in 1:j) {
    # These will be the zlim's (the data lim)
    z_lim <- c(1000000,-1000000)
    # First we must populate this...
    locs <- c()
    for (loc in loc_names) {
      df <- returns[[loc]]
      if (!is.null(df)) {
        locs <- append(locs, loc)
        for (m in 1:i) {
          tmp_z <- c(min(df[[m]][[n]]$ci_l), max(df[[m]][[n]]$ci_u))
          z_lim[1] <- min(z_lim[1], tmp_z[1])
          z_lim[2] <- max(z_lim[2], tmp_z[2])
        }
      }
    }

    # Go ahead and delcare the plot file for this return period
    if (stationary) {
      file1 <- file.path("Output",paste(city, "_R", sep=""), paste(rp[n],
        "yr_main_IDF.jpeg", sep=""))
      make.plot(file1, FALSE, locs, z_lim)
    } else {
      file1 <- file.path("Output",paste(city, "_R", sep=""), paste("then-",
        rp[n], "yr_main_IDF.jpeg", sep=""))
      file2 <- file.path("Output",paste(city, "_R", sep=""), paste("now-",
        rp[n], "yr_main_IDF.jpeg", sep=""))
      make.plot(file1, FALSE, locs, z_lim)
      make.plot(file2, TRUE, locs, z_lim)
    }
  }
}
