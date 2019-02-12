library(tidyverse)
library(extRemes)
library(zoo)
library(lubridate)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # IDF : This function developes a IDF Curve for the daily observations of
# # #     some data type by utilizing the GEV distribution family to develop
# # #     expected non-stationary return levels over a given return period at
# # #     various day-long durations. While this function will fill in missing
# # #     data, it is suggested your data be mostly complete for optimal
# # #     estimations.
# # # # # # # # # # #
# Inputs:
#    data - This should be a list or dataframe with the first column containing
#       the data as a POSIXct type or a character type in %Y-%m-%d
#       format, and the second column containing the data to analyze.
#    durations - This should be the a vector containing the various day-long
#       durations you wish to use in your IDF. The default is c(1:7,10)
#    return_periods - This should be a vector containing the return periods which
#       you want to calculate the return level's (and IDF) for.
#    season - This input should be a single word character string or integer
#       containing one the four seasons you want to calculate the IDF for. If
#       using an integer, the seasons begin with "winter".
#    extreme - This refers to the type of extreme you are looking at, and should
#       be a character string of either "min" or "max".
#    forceGEV - If you wish to always have a shape paramter (ignore the
#       potential of a Gumbel type) mark this as TRUE, else say FALSE.
#    dir - A directory name (or path) in which to put the plots. ONLY enter a
#       value if you wish for plots to be developed.
#
# Outputs:
#
# Author: April Walker
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

warn_fits <<- list()
warn_data <<- list()
w <<- 1
error <<- c(0,0,0,0)
names(error) <- c("CI", "CI-R", "PLOT", "FEVD")

IDF <- function(data, durations=c(1:7,10), return_periods=c(2, 20, 100),
        season, extreme = "max", forceGEV = FALSE, dir = NULL,
        polyFits = FALSE, alpha = 0.05) {

  # This season determines the start and end day of the observation based on
  # what season is indicated to be studied
  season.Setup <- function() {
    if ((season == 1) | (season == "winter")) {
      start_day <- "2999-12-01"
      end_day <- "2999-02-28"
      season <- "winter"
    } else if ((season == 2) | (season == "spring")) {
      start_day <- "2999-03-01"
      end_day <- "2999-05-31"
      season <- "spring"
    } else if ((season == 3) | (season == "summer")) {
      start_day <- "2999-06-01"
      end_day <- "2999-08-31"
      season <- "summer"
    } else if ((season = 4) | (season == "fall") | (season == "autumn")) {
      start_day <- "2999-09-01"
      end_day <- "2999-11-30"
      season <- "fall"
    }
    return(list(start=start_day, end=end_day, season=season))
  }

  # This function develops the directories and filenames if the user chooses
  # to plot the IDF findings
  file.Setup <- function() {
    season = season.Setup()$season
    extra <- file.path(dir, "extra")
    idf <- file.path(dir, "idf")

    season_extra <- file.path(extra, season)
    season_idf <- file.path(idf, season)

    trend <- file.path(season_extra, "trends")
    rl <- file.path(season_extra, "return")
    dir.create(dir)
    dir.create(extra)
    dir.create(idf)
    dir.create(season_extra)
    dir.create(season_idf)
    dir.create(trend)
    dir.create(rl)
    trend_file = c()
    rl_file = c()
    idf_file = c()
    for (i in 1:length(durations)) {
      trend_file[i] = paste(trend, "/trend-", durations[i], "day.jpeg", sep="")
      rl_file[i] = paste(rl, "/rlplot-", durations[i], "day.jpeg", sep="")
      for (j in 1:length(return_periods)) {
        idf_file[(i - 1)*length(return_periods) + j] = paste(season_idf, "/idf-",
            return_periods[j], "year-", durations[i], "day.jpeg", sep = "")
      }
    }
    return(list(rl=rl_file, idf=idf_file, trend=trend_file))
  }

  # This function reads in the data and determines the year range
  year.Range <- function(days) {
    start <- as.Date(min(data$DATE))
    end <- as.Date(max(data$DATE))
    # Check the starting year has the data we need
    if (format(start, format="%m-%d") < days$start) {
      start <- format(start, format = "%Y")
    } else {
      start <- format(start, format = "%Y")
      start <- as.numeric(start) + 1
    }
    # Check the ending year has the data we need
    if (format(end, format="%m-%d") > days$end) {
      end <- format(end, format = "%Y")
    } else {
      end <- format(end, format = "%Y")
      end <- as.numeric(end) - 1
    }
    start <- as.numeric(start)
    end <- as.numeric(end)
    return(start:end)
  }

  # This devlops the start and end day for a particular duration, and assumes
  # the rolling average is calculated to be centered around the period of
  # interest
  duration.Setup <- function(duration, days) {
      if (duration%%2 == 1) {
        adj <- (duration-1)/2
        dur_start <- format((as.Date(days$start) - adj), format="%m-%d")
        dur_end <- format((as.Date(days$end) + adj), format="%m-%d")
      } else if (duration%%2 == 0) {
        adj <- duration/2
        dur_start <- format((as.Date(days$start) - adj + 1), format="%m-%d")
        dur_end <- format((as.Date(days$end) + adj), format="%m-%d")
      }
      return(list(start=dur_start, end=dur_end))
  }

  # This function determines the maximum or minimum value for each year in a
  # dataset, then returns a new dataset with this information
  rolling.BlockMaxima <- function(years, days, duration) {
    block_max = c()
    for (yr in years) {
      # Grab the dataset for each year
      if ((days$start) <= "12-01" && (days$start) > "09-01") {
        if((leap_year(yr+1)) && (days$start == "12-01")) {
          dates <- seq(as.Date(paste(yr, days$start, sep="-")),
              as.Date(paste(yr + 1, "02-29", sep="-")), by = "day")
        }
        dates <- seq(as.Date(paste(yr, days$start, sep="-")),
            as.Date(paste(yr + 1, days$end, sep="-")), by = "day")
      } else {
        dates <- seq(as.Date(paste(yr, days$start, sep="-")),
            as.Date(paste(yr, days$end, sep="-")), by = "day")
      }
      tmp <- subset(data, as.Date(DATE,format = "%m-%d-%Y") %in% dates)
      # Double check for complete data
      tmp <- tmp %>%
          mutate(DATE = as.Date(DATE)) %>%
          complete(DATE = dates) %>%
          fill(DATA)
      # Find rolling mean and add save max value for the year
      tmp <- rollmean(tmp$DATA, duration, align = "center")
      block_max <- rbind(block_max, cbind.data.frame(yr, get(extreme)(tmp)))
    }
    names(block_max) <- c("YEAR", "DATA")
    return(block_max)
  }

  # make.Fits takes in information from the IDF function to develop fevd fits
  # for the dataset
  make.Fits <- function(df, .forceGEV = forceGEV, forceType = NULL) {
    years = df$YEAR
    p <- cor.test(df$YEAR, df$DATA, method="kendall")$p.value
    j <- 1
    fits = list()

    if ((!.forceGEV) && is.null(forceType)) {
      j <- 2
      warn_data <<- df
      fits[[1]] <- fevd(df$DATA, df, type="Gumbel", method = "Bayesian", iter = 1000)
      fits[[2]] <- fevd(df$DATA, df, type="GEV", method = "Bayesian", iter = 1000)
      lr = lr.test(fits[[1]], fits[[2]])
      if (lr$p.value < 0.05) {
        type <- "GEV"
        shape <- ci(fits[[2]] , type="parameter", which.par=3)[2]
        if (shape < 0) {
          GEV_type <- "Weibull"
        } else if (shape > 0) {
          GEV_type <- "Frechet"
        }
      } else {
        type <- "Gumbel"
        GEV_type <- "Gumbel"
      }
    } else if (.forceGEV) {
      fits[[1]] <- fevd(df$DATA, df, type="GEV", method = "Bayesian", iter = 1000)
      print(class(fits[[1]]))
      if (class(fits[1]) == "character") {
        error[4] <<- error[4] + 1
        warn_data[w] <<- df
        w <<- w + 1
      }
      type <- "GEV"
      GEV_type <- "NA"
    } else {
      type <- forceType
      fits[[1]] <- fevd(df$DATA, df, type=forceType, method = "Bayesian", iter = 1000)
    }
    data_years <- length(years)
    # If p < 0.05 the data is considered nonstationary
    if (p < 0.05) {
      ### Fits with location ~ year or ~year^2 ###
      fits[[j+1]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE))
      fits[[j+2]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE))

      ### Fits with scale ~ year ###
      fits[[j+3]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
        scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1), use.phi = TRUE)

      ### Fits with location ~ year and scale ~ year ###
      fits[[j+4]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          use.phi = TRUE)
      fits[[j+5]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
          scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          use.phi = TRUE)
      if (polyFits) {
        ### Fits with scale ~ year^2 ###
        fits[[j+6]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
            scale.fun = ~ poly(YEAR, 2), use.phi = TRUE)
        fits[[j+7]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
            location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            scale.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            use.phi = TRUE)
        fits[[j+8]] <- fevd(df$DATA, df, type = type, method = "Bayesian", iter = 1000,
            location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
            scale.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            use.phi = TRUE)
      }
    }
    return(fits)
  }

  # Given a vector of fits, this functon detemines and returns the best fit
  # based on AIC, or if unavaliable, MLE
  estimate.GOF <- function(fits) {
    # if fits only contains one fit
    if(!is.null(fits$call)) {
      best_fits <- fits
      return(best_fits)
    }
    # else determine the best fit
    AIC <- c()
    MLE <- c()
    for (i in 1:length(fits)) {
      tmp <- fits[[i]]
      if ((length(tmp) > 1) && is.null(tmp$results$value)) {
        best_fit <- estimate.GOF(tmp)
        return(best_fit)
      } else {
        MLE <- append(MLE, tmp$results$value)
        try({AIC <- append(AIC, as.double(summary(tmp$AIC)))})
      }
      if (!is.na(sum(AIC))) {
        best_fit <- fits[[which.min(AIC)]]
      } else {
        if(is.null(MLE)) {
          print(fits)
        }
        best_fit <- fits[[which.min(MLE)]]
      }
    return(best_fit)
    }
  }

  best.Fit <- function(fits) {
    num_params <- c()
    best_fits <- list()
    for (j in 1:length(fits)) {
      num_params[j] <- length(fits[[j]]$results$par)
    }
    k <- 1
    for (j in min(num_params):max(num_params)) {
      index <- which(num_params %in% j)
      if(!(length(index) == 0)) {
          names(fits) <- seq(1, length(fits))
          tmp <- subset(fits, names(fits) %in% index)
          names(fits) <- NULL
          names(tmp) <- NULL
          best_fits[[k]] <- estimate.GOF(tmp)
          k <- k + 1
      }
    }
    new_best <- best_fits[[1]]
    for (j in 2:length(best_fits)) {
      lr <- lr.test(new_best, best_fits[[j]])
      if (lr$p.value < 0.05) {
        new_best <- best_fits[[j]]
      }
    }
    return(new_best)
  }

  # This function develops the qcov matrix for a given fit, nonstationary or
  # stationary
  qcov.Developer <- function(years, fit) {
    scaled_range <- (years - years[1])/length(years)
    lin_seq <- scaled_range
    poly_seq <- scaled_range^2
    lin_par <- c("mu1", "phi1")
    quad_par <- c("mu2", "phi2")
    params <- intersect(lin_par, names(fit$results$par))
    if (is_empty(params)) {
      qcov <- make.qcov(fit)
    } else {
      vals <- list()
      for (val in params) {
        assign(val, lin_seq)
        vals[[val]] <- get(val)
      }
      params <- intersect(quad_par, names(fit$results$par))
      for (val in params) {
        assign(val, poly_seq)
        vals[[val]] <- get(val)
      }
      if (is.null(fit$num.pars$shape)) {
        fit$num.pars$shape <- 0
      }
      qcov <- make.qcov(fit, vals = vals)
    }
    return(qcov)
  }

  # Given either a stationary or nonstationary fevd object, a qcov, and a
  # level of confidence (alpha) this function determines the return level and
  # confidence level for a given return period
  return.Level <- function(df, fit, qcov, rp) {
    # Find significance and year range
    p <- cor.test(df$YEAR, df$DATA, method="kendall")$p.value
    # Calculate the confidence interval
    ci <- tryCatch(ci(fit, alpha = alpha, return.period = rp, qcov = qcov),
        error = function(e) print("CI Calc Error"))
    # Error handling
    if(class(ci) == "character") {
      error[1] <<- error[1] + 1
      if(fit$type == "GEV") {
        type = "Gumbel"
      } else {
        type = "GEV"
      }
      .fits = make.Fits(df, forceType=type)
      if(p < 0.05) {
        new_best = best.Fit(.fits)
        v <- qcov.Developer(df$YEAR, new_best)
      } else {
        new_best = fevd(df$DATA, df, type=type)
      }
      ci <- tryCatch(ci(fit, alpha = alpha, return.period = rp, qcov = qcov),
          error = function(e) print("Retry CI Calc Error"))
      if(class(ci) == "character") {
        error[2] <<- error[2] + 1
        return(list(x=0, y=0, ci_l=0, ci_u=0, err=0, warn=TRUE, ns=FALSE))
      }
    }

    # Develop confidence interval data
    x <- df$YEAR
    if (is.null(dim(ci))) {
      nonstationary = FALSE
      y <- rep(ci[2], length(x))
      ci_l <- rep(ci[1], length(x))
      ci_u <- rep(ci[3], length(x))
      err <- (ci_u-ci_l)/(qnorm(((1-alpha)/2)+alpha))
    } else {
      nonstationary = TRUE
      y <- ci[,2]
      ci_l <- ci[,1]
      ci_u <- ci[,3]
      err <- ci[,4]
    }
    # Error handling
    if(is.na(mean(err))) {
      print("Error! Irregular Fits" )
      return(list(x=0, y=0, ci_l=0, ci_u=0, err=0, warn=TRUE, ns=FALSE))
    } else if (mean(err) < 1e-4) {
      print("CI from fit unusually small")
      warning = FALSE
    } else {
      warning = FALSE
    }
    return(list(x=x, y=y, ci_l=ci_l, ci_u=ci_u, err=err, warn=warning, ns=nonstationary))
  }

  # # # # # # # # # # # # # ####################### # # # # # # # # # # # # #
  # # # # # # # # # # # # # #### MAIN FUNCTION #### # # # # # # # # # # # # #
  # # # # # # # # # # # # # ####################### # # # # # # # # # # # # #

  # # # #
  # # Simple Setup Stuff
  # # # #

  # variable initialization
  extremes <- list()
  fits <- list()
  return_vals <- list()

  # Data fixes and other setup
  names(data) <- c("DATE", "DATA")
  data$DATE <- as.POSIXct(data$DATE)
  if (!is.null(dir)) {
    file <- file.Setup()
  }

  # # # # # # # # # # # # # # # #
  # # Begin Analysis for IDF  # #
  # # # # # # # # # # # # # # # #

  # For each duration calculate the IDF
  for (i in 1:length(durations)) {
    return_vals[[i]] <- list()
    dur_init <- duration.Setup(durations[i], season.Setup())

    # Apply rolling mean block maxima
    extremes[[i]] <- rolling.BlockMaxima(year.Range(season.Setup()), dur_init,
        durations[i])

    # Begin trend tests
    mk <- cor.test(extremes[[i]]$YEAR, extremes[[i]]$DATA, method="kendall")
    tau <- as.double(mk$estimate)
    p <- mk$p.value

    # # # #
    # # Block Maxima Trend Plot
    # # # #
    if(!is.null(dir)) {
      jpeg(file$trend[i])
      max_locs = order(extremes[[i]]$DATA, decreasing=TRUE)[1:3]
      max_data = extremes[[i]][max_locs, 2]
      plot(extremes[[i]]$YEAR, extremes[[i]]$DATA,
        col=ifelse(extremes[[i]]$DATA == max_data[1] | extremes[[i]]$DATA ==
            max_data[2], "red", "black"),
        pch=ifelse(extremes[[i]]$DATA == max_data[1] | extremes[[i]]$DATA ==
            max_data[2], 19, 1))
      trend = lm(extremes[[i]]$DATA ~ extremes[[i]]$YEAR)
      intercept = signif(coef(trend)[1], 4)
      slope = signif(coef(trend)[2], 4)
      eq = paste("DATA =", slope, "YEAR +", intercept)
      abline(trend, col="#e66101")
      mtext(eq, 3, line=-2)

      trend = lm(extremes[[i]]$DATA ~ poly(extremes[[i]]$YEAR, 2, raw = TRUE))
      predicted = predict(trend, data.frame(x=extremes[[i]]$YEAR),
          interval='confidence', level=0.95)
      lines(extremes[[i]]$YEAR, predicted[,1], col="#5e3c99")
      dev.off()
    }

    # # # #
    # # Model Fitting and Determining Return Levels
    # # # #
    tmp_fits <- make.Fits(extremes[[i]])
    #TODO: Do a lot of checking before proceeding...

    fits[[i]] <- tmp_fits
    new_best = best.Fit(fits[[i]])
    v <- qcov.Developer(extremes[[i]]$YEAR, new_best)

    for (j in 1:length(return_periods)) {
      rl <- return.Level(extremes[[i]], new_best, v, return_periods[j])

      # # # #
      # # IDF and Return Level PLots
      # # # #

      # Given our model is nonstationary, plotting is wanted, and no warnings
      if (!is.null(dir)) {
        if(rl$ns && !rl$warn) {
          jpeg(file$idf[(i - 1)*length(return_periods) + j], width=500, height=750)
          plot(rl$x, rl$y, ylim=range(c(rl$ci_l, rl$ci_u)),
              ylab="DATA", xlab="YEAR", main = paste(return_periods[j],
              "-Year Return Levels for ", durations[i], " Day Events in ", dir,
              sep=""), type = "o")
          arrows(rl$x, rl$ci_l, rl$x, rl$ci_u, length=0.05, angle=90, code=3)
          dev.off()
        # Otherwise just plot the typical RL plot
        } else {
          jpeg(file$idf[(i - 1)*length(return_periods) + j], width=500, height=750)
          result = tryCatch(plot(new_best, "rl"), error = function(e)
              print("Plot (RL) Error"))
          dev.off()
          if(class(result) == "character" || rl$warn) {
            error[3] <<- error[3] + 1
            warn_fits[[w]] <<- new_best
            warn_data[[w]] <<- extremes[[i]]
            w <<- w + 1
          }
        }
      }
      return_vals[[i]][[j]] <- rl
    }
  }
  return(return_vals)
}

# # # #
# # Testing
# # # #

# declare data directory
options(error = function() traceback(2))

dir <- "Data"
sys_call <- paste("ls", dir, "| grep .csv", sep = " ")
date_cols <- c("YEAR", "MO", "DA")
data_cols <- c("TEMP", "PRCP", "MAX", "MIN")
all_cols = c(date_cols, data_cols)

# declare dataset with header based on cities in dataset
files <- system(sys_call, intern = TRUE)
cities <- unlist(strsplit(files, "\\."))[2*(1:length(files))-1]
data <- vector("list", length(files))
names(data) <- cities

for (i in 1:length(files)) {
  path <- file.path(dir, files[i])
  data[[cities[i]]] <- read.csv(path, header = TRUE)
  data[[cities[i]]] <- select(data[[cities[i]]], one_of(all_cols))
}

durations <- c(1,2,3,4,5,6,7,10)
seasons <- c(1,2,3,4)

print("Begin analysis:")

# For now let's just focus on temp
val = "TEMP"
# city = "Kansas"
# for(val in data_cols)
for(city in cities) {
  print(paste("Analyzing data from", city))
  for (s in seasons) {
    #Make some directories
    directory <- "Output"
    dir.create(directory)
    directory <- file.path(directory, city)
    dir.create(directory)
    directory <- file.path(directory, val)
    #Data prep
    cols <- c(date_cols, val)
    test <- select(data[[city]], cols)
    test <- unite(test, DATE, c(YEAR, MO, DA), sep="-", remove = TRUE)
    test$DATE <- as.POSIXct(test$DATE)
    returns <- IDF(data=test, season=s, dir=directory)
    # returns <- tryCatch(IDF(test, durations, c(2, 20, 100), season, "max", TRUE,
    #     dir, FALSE), error = function(e) print("IDF Function Error"))
    # if(class(returns) == "character") {
    #   print(city)
    #   print(s)
    #   print(head(test))
    # }
  }
}
# }

print("End analysis")
print(error)
