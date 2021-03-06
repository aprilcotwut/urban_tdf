library(tidyverse)
library(extRemes)
library(zoo)
library(Hmisc)
library(lubridate)

source("R/setup.R")
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
#       format, the second column containing the data to analyze, and any
#       following colomns should be potential covariates.
#    durations - This should be the a vector containing the various day-long
#       durations you wish to use in your IDF. The default is c(1:7,10). NOTE:
#       this should be from the smallest duration to the longest.
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
#    polyFits - This is a boolean which indicates whether to allow polynomial
#       covariates.
#    alpha - This input should be a single numerical input indicating the
#       he confidence level. Default is 0.05, recommended value between
#       0.01 and 0.1.
#    stationary - This is a boolean which indicates whether to consider
#       covariates when fitting data.
#    use.phi - Uses a logarithmic derivation of scale, see extRemes
#       documentation for more info.
#    exhaustive - A boolean indicating whether all models should be fit or
#       whether assumptions should be made to "cut out" unlikely canidates.
# Outputs:
#
# Author: April Walker
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#TODO: impliment exhaustive component to cut down run time, impliment additional
# covariates.

# Some global variables that should be reformatted to be part of a test file
warn_fits <<- list()
warn_data <<- list()
w <<- 1
.i <<- 1
error <<- c(0,0,0,0,0)
new_test <<- list()
factors <<- c()
names(error) <- c("CI", "CI-R", "PLOT", "FEVD", "BFs")

# # #
# Begin IDF function
# # #
IDF <- function(data, durations=c(1:7,10), return_periods=c(2, 20, 100),
        season, extreme = "max", forceGEV = FALSE, method = NULL, dir = NULL,
        polyFits = FALSE, alpha = 0.05, stationary = FALSE, use.phi = TRUE,
        exhaustive = TRUE) {
  # Early data cleanup
  names(data) <- c("DATE", "DATA")

  # Important object declarations
  days <- season.Setup(season)
  files <- file.Setup(days, dir, return_periods, durations)
  years <- year.Range(days, data)


  # This function returns the season data for a given year
  get.Seasonal.Data <- function(yr, duration, time) {
    if (class(time) != "timeframe") {
      stop("invalid 'time' input for get.Seasonal.Data")
    }
    # Grab the dataset for each year
    if ((season == "winter") & (duration > 1)) {
      if((leap_year(yr+1)) && (time$start == "12-01")) {
        dates <- seq(as.Date(paste(yr, time$start, sep="-")),
            as.Date(paste(yr + 1, "02-29", sep="-")), by = "day")
      }
      dates <- seq(as.Date(paste(yr, time$start, sep="-")),
          as.Date(paste(yr + 1, time$end, sep="-")), by = "day")
    } else {
      dates <- seq(as.Date(paste(yr, time$start, sep="-")),
          as.Date(paste(yr, time$end, sep="-")), by = "day")
    }
    tmp <- subset(data, as.Date(DATE,format = "%m-%d-%Y") %in% dates)
    return(tmp)
  }

  # This function handles incomplete data.
  complete.Data <- function() {
    time <- duration.Setup(max(durations), days)
    duration <- max(durations)
    for (yr in years) {
      tmp <- get.Seasonal.Data(yr, duration, time)

      sink(log, append = TRUE)
      print(paste("NA data for duration ", duration, ":", sep=""))
      print(paste(mean(is.na(tmp$DATA)), "% NA in ", yr), sep = "")
      sink()
      # If less than 25% of the data is missing, impute predicted values
      # as given by Hmisc's transcan "impute" function
      if (mean(is.na(tmp$DATA)) < 0.25) {
        tmp$DATA <- as.numeric(impute(tmp$DATA))
        data[match(tmp$DATE, data$DATE),] <- tmp
      }
    }

    return(data)
  }

  # This function determines the maximum or minimum value for each year in a
  # dataset, then returns a new dataset with this information
  rolling.BlockMaxima <- function(duration, time) {
    block_max = c()
    log <- files$info
    for (yr in years) {
      tmp <- get.Seasonal.Data(yr, duration, time)
      # Find rolling mean and add save max value for the year
      tmp <- rollmean(tmp$DATA, duration, align = "center")
      # Given NA values exist, the extreme will be NA, else max/min will be found
      block_max <- rbind(block_max, cbind.data.frame(yr, get(extreme)(tmp)))
    }
    names(block_max) <- c("YEAR", "DATA")
    return(block_max)
  }

  # make.Fits takes in information from the IDF function to develop fevd fits
  # for the dataset
  make.Fits <- function(df, .forceGEV = forceGEV, type = NULL, it = NULL, p = NULL) {
    # Method setup #
    if(is.null(method)) {
      .method <- "MLE"
    } else {
      .method <- method
      .forceGEV = TRUE
    }
    if (is.null(type)) {
      # Enter in info for managing PP, etc
    }
    # Other Setup #
    log <- files$info
    years <- df$YEAR
    if (is.null(p)) {
      p <- cor.test(df$YEAR, df$DATA, method="kendall")$p.value
    }
    j <- 1
    fits <- list()
    # Main function #
    if ((!.forceGEV) && is.null(type)) {
      j <- 2
      # warn_data <<- df
      fits[[1]] <- fevd(df$DATA, df, type="Gumbel", method = .method, iter = it,
          na.action = na.omit, use.phi = use.phi)
      fits[[2]] <- fevd(df$DATA, df, type="GEV", method = .method, iter = it,
          na.action = na.omit, use.phi= use.phi)
      lr = lr.test(fits[[1]], fits[[2]])
      if (lr$p.value < alpha) {
        type <- "GEV"
      } else {
        type <- "Gumbel"
      }
    } else if (.forceGEV) {
      fits[[1]] <- fevd(df$DATA, df, type="GEV", method = .method, iter = it,
          na.action = na.omit, use.phi = use.phi)
      if (class(fits[1]) == "character") {
        error[4] <<- error[4] + 1
        warn_data[w] <<- df
        w <<- w + 1
      }
      type <- "GEV"
    } else {
      fits[[1]] <- fevd(df$DATA, df, type=type, method = .method, iter = it,
          na.action = na.omit, use.phi = use.phi)
    }
    data_years <- length(years)
    # If p < 0.05 the data is considered nonstationary
    if ((p < alpha) && (stationary == FALSE)) {
      ### Fits with location ~ year or ~year^2 ###
      fits[[j+1]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          use.phi = use.phi, na.action = na.omit)
      fits[[j+2]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
          use.phi = use.phi, na.action = na.omit)

      ### Fits with scale ~ year ###
      fits[[j+3]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
        scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
        use.phi = use.phi, na.action = na.omit)

      ### Fits with location ~ year and scale ~ year ###
      fits[[j+4]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          use.phi = use.phi, na.action = na.omit)
      fits[[j+5]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
          scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          use.phi = use.phi, na.action = na.omit)
      if (polyFits) {
        ### Fits with scale ~ year^2 ###
        fits[[j+6]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
            scale.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            use.phi = use.phi, na.action = na.omit)
        fits[[j+7]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
            location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            scale.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            use.phi = use.phi, na.action = na.omit)
        fits[[j+8]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
            location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
            scale.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            use.phi = use.phi, na.action = na.omit)
      }
    }
    names(fits) <- seq(1, length(fits))
    for (val in fits) {
      if(.method == "Bayesian") {
        tmp <- as.double(summary(val)$DIC)
        sink(log, append = TRUE)
        print(tmp)
        sink()
      } else {
        tmp <- as.double(summary(val)$AIC)
        tmp <- as.double(summary(val)$BIC)
        sink(log, append = TRUE)
        print(tmp)
        sink()
      }
    }
    return(fits)
  }

  # Given a vector of fits, this functon detemines and returns the best fit
  # based on AIC, or if unavaliable, MLE
  best.Fit <- function(fits) {
    # Interval function to determine best fit between models of
    # similar complexity.
    estimate.GOF <- function(fits) {
      # if fits only contains one fit
      if(!is.null(fits$call)) {
        best_fits <- fits
        return(best_fits)
      }
      # else determine the best fit
      if (is.null(method) || method == "MLE") {
        AIC <- c()
        MLE <- c()
        for (i in 1:length(fits)) {
          tmp <- fits[[i]]
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
        } else if (method == "Bayesian") {
          DIC <- c()
          if(length(fits) > 1) {
            for(i in 1:length(fits)) {
              tmp <- fits[[i]]
              try({DIC <- append(DIC, as.double(summary(tmp$DIC)))})
            }
            if (!is.na(sum(DIC))) {
              best_fit <- fits[[which.min(DIC)]]
            } else {
              best_fit <- fits[[1]]
            }
          } else {
            best_fit <- fits[[1]]
          }
        return(best_fit)
      }
    }

    # Begin function
    if(length(fits) == 1) {
      return(fits[[1]])
    }
    num_params <- c()
    best_fits <- list()
    if (is.null(method) || method == "MLE") {
      for (j in 1:length(fits)) {
        num_params[j] <- length(fits[[j]]$results$par)
      }
    } else if (method == "Bayesian") {
      for (j in 1:length(fits)) {
        num_params[j] <- ncol(fits[[j]]$results)
      }
    }
    k <- 1
    for (j in min(num_params):max(num_params)) {
      index <- which(num_params %in% j)
      if(!(length(index) == 0)) {
        tmp <- subset(fits, names(fits) %in% index)
        best_fits[[k]] <- estimate.GOF(tmp)
        k <- k + 1
      }
    }
    new_best <- best_fits[[1]]
    if (is.null(method) || method == "MLE") {
      for (j in 2:length(best_fits)) {
        lr <- lr.test(new_best, best_fits[[j]])
        if (lr$p.value < alpha) {
          new_best <- best_fits[[j]]
        }
      }
      return(new_best)
    } else if (method == "Bayesian") {
      tmp <- as.double(summary(new_best)$DIC)
      for (j in 2:length(best_fits)) {
        factor <- as.numeric(BayesFactor(new_best, best_fits[[j]],
          method = "laplace")$statistic)
        if (is.nan(factor)) {
          factor <- as.numeric(BayesFactor(new_best, best_fits[[j]],
            method = "harmonic")$statistic)
        }
        factors <<- append(factors, factor)
        if (is.nan(factor)) {
          DIC_nb <- as.double(summary(new_best)$DIC)
          DIC_fit <- as.double(summary(best_fits[[j]])$DIC)
          if(is.nan(DIC_nb)) {
            error[5] <<- error[5] + 1
            new_best <- best_fits[[j]]
          }
          if (DIC_nb > DIC_fit) {
            new_best <- best_fits[[j]]
          }
        }
        else if (factor > 1) {
          new_best <- best_fits[[j]]
        }
      }
      return(new_best)
    }
  }

  # This function develops the qcov matrix for a given fit, nonstationary or
  # stationary
  qcov.Developer <- function(fit) {
    # Setup
    scaled_range <- (years - years[1])/length(years)
    lin_seq <- scaled_range
    poly_seq <- scaled_range^2
    lin_par <- c("mu1", "phi1")
    quad_par <- c("mu2", "phi2")

    # For linear parameters...
    if (method == "Bayesian") {
      params <- intersect(lin_par, colnames(fit$results))
    } else {
      params <- intersect(lin_par, names(fit$results$par))
    }
    if (is_empty(params)) {
      qcov <- make.qcov(fit)
    } else {
      vals <- list()
      for (val in params) {
        assign(val, lin_seq)
        vals[[val]] <- get(val)
      }

      # For quadratic parameters...
      if (method == "Bayesian") {
        params <- intersect(quad_par, colnames(fit$results))
      } else {
        params <- intersect(quad_par, names(fit$results$par))
      }
      for (val in params) {
        assign(val, poly_seq)
        vals[[val]] <- get(val)
      }
      if (method != "Bayesian") {
        if (is.null(fit$num.pars$shape)) {
          fit$num.pars$shape <- 0
        }
      }
      qcov <- make.qcov(fit, vals = vals)
    }
    return(qcov)
  }

  # Given either a stationary or nonstationary fevd object, a qcov, and a
  # level of confidence (alpha) this function determines the return level and
  # confidence level for a given return period
  return.Level <- function(df, fit, qcov, rp, p = NULL) {
    # Find significance and year range
    if (is.null(p)) {
      p <- cor.test(df$YEAR, df$DATA, method="kendall")$p.value
    }
    # Calculate the confidence interval
    ci <- ci(fit, alpha = alpha, return.period = rp, qcov = qcov)

    # Develop confidence interval data
    x <- df$YEAR
    if (is.null(dim(ci))) {
      nonstationary = FALSE
      y <- rep(ci[2], length(x))
      ci_l <- rep(ci[1], length(x))
      ci_u <- rep(ci[3], length(x))
      # err <- (ci_u-ci_l)/(qnorm(((1-alpha)/2)+alpha))
    } else {
      nonstationary = TRUE
      y <- ci[,2]
      ci_l <- ci[,1]
      ci_u <- ci[,3]
      # err <- ci[,4]
    }
    # Error handling
    if (mean(abs(ci_u - ci_l)) < 1e-4) {
      print("CI from fit unusually small")
      warning = FALSE
    } else {
      warning = FALSE
    }
    return(list(x=x, y=y, ci_l=ci_l, ci_u=ci_u, warn=warning, ns=nonstationary,
    data=df))
  }

  # These function handles plot development for this codebase
  # Develops the trend plots for the data
  trend.Plot <- function(df, dur_it) {
    # Determine linear trend and significance
    mk <- cor.test(df$YEAR, df$DATA, method="kendall")
    tau <- as.double(mk$estimate)
    p <- mk$p.value
    # Begin plot
    jpeg(files$trend[dur_it])
    # Indicate locations of extremes
    if (extreme == "max") {
      extr_locs <- order(df$DATA, decreasing=TRUE)[1:2]
      extr_data <- df[extr_locs, 2]
    } else {
      extr_locs <- order(df$DATA, decreasing=FALSE)[1:2]
      extr_data <- df[extr_locs, 2]
    }
    # Plot data with extremes indicated in red
    plot(df$YEAR, df$DATA,
      col=ifelse(df$DATA == extr_data[1] | df$DATA ==
          extr_data[2], "red", "black"),
      pch=ifelse(df$DATA == extr_data[1] | df$DATA ==
          extr_data[2], 19, 1))
    # Add linear plot line and equation
    trend <- lm(df$DATA ~ df$YEAR)
    intercept <- signif(coef(trend)[1], 4)
    slope <- signif(coef(trend)[2], 4)
    eq <- paste("DATA =", slope, "YEAR +", intercept)
    abline(trend, col="#e66101")
    mtext(eq, 3, line=-2)
    # Add quadratic plot line
    trend <- lm(df$DATA ~ poly(df$YEAR, 2, raw = TRUE))
    predicted <- predict(trend, data.frame(x=df$YEAR),
        interval='confidence', level=(1-alpha))
    lines(df$YEAR, predicted[,1], col="#5e3c99")
    dev.off()
  }

  # Develops the extra IDF/RL plots developed for each duration as a check
  extra.Plot <- function(dur_it, rp_it, rl_vals, fit) {
    if(rl_vals$ns && !rl_vals$warn) {
      # IDF
      jpeg(files$idf[(dur_it - 1)*length(return_periods) + rp_it], width=500, height=750)
      plot(rl_vals$x, rl_vals$y, ylim=range(c(rl_vals$ci_l, rl_vals$ci_u)),
          ylab="DATA", xlab="YEAR", main = paste(return_periods[j],
          "-Year Return Levels for ", durations[dur_it], " Day Events in ", dir,
          sep=""), type = "o")
      arrows(rl_vals$x, rl_vals$ci_l, rl_vals$x, rl_vals$ci_u, length=0.05, angle=90, code=3)
      dev.off()
      # RL
      jpeg(files$rl[(dur_it)], width=500, height=750)
      result <- tryCatch(plot(fit), error = function(e) print("RL Plot error"))
      dev.off()
    # Otherwise
    } else {
      # Just plot standard rl graph
      jpeg(files$rl[dur_it], width=500, height=750)
      result <- tryCatch(plot(fit), error = function(e) print("RL Plot error"))
      dev.off()
      if(class(result) == "character" || rl_vals$warn) {
        error[3] <<- error[3] + 1
        warn_fits[[w]] <<- fit
        w <<- w + 1
      }
    }
  }

  # Develops the main plots
  main.Plot <- function(rl_vals) {
    x <- durations

    warn <- FALSE

    i <- length(rl_vals) #should be the number of durations
    j <- length(rl_vals[[1]]) #should be the number of return periods
    k <- length(rl_vals[[1]][[1]]$x) #should be the number of years of data

    m <- 1
    n <- 1
    stop <- FALSE
    while(!stop && (m <= i) && (n <= j)) {
      if(rl_vals[[m]][[n]]$ns) {
        stop <- TRUE
      } else {
        m <- m + 1
        n <- n + 1
      }
    }
    ns <- stop
    rm(m,n)
    # If all RL models are stationary, devlop the 2D IDF curve
    if(!ns) {
      # Repeat for each return period
      for(n in 1:j) {
        z = ci_l = ci_u <- rep(0, i) # holds data & confidence interval
        # Handle data for each duration
        for(m in 1:i) {
          z[m] <- rl_vals[[m]][[n]]$y[1]
          ci_l[m] <- rl_vals[[m]][[n]]$ci_l[1]
          ci_u[m] <- rl_vals[[m]][[n]]$ci_u[1]
        }

        jpeg(files$idf2[n], width=500, height=750)
        plot(x, z, ylim=range(c(ci_l, ci_u)), ylab="DATA", xlab="DURATION",
            main = paste(return_periods[n], "-Year Return Levels Curve", dir,
            sep=""), type = "o")
        arrows(x, ci_l, x, ci_u, length=0.05, angle=90, code=3)
        dev.off()
      }

    # Else develop the 3d IDF curve and adjust stationary findings
    } else {
      # Repeat for each return period
      for(n in 1:j) {
        z = ci_l = ci_u <- matrix(0, nrow=k, ncol=i) # holds data & confidence interval
        y <- rl_vals[[1]][[n]]$x
        # Handle data for each duration
        for(m in 1:i) {
          z[,m] <- rl_vals[[m]][[n]]$y
          ci_l[,m] <- rl_vals[[m]][[n]]$ci_l
          ci_u[,m] <- rl_vals[[m]][[n]]$ci_u
        }

        jpeg(files$idf2[n], width=500, height=750)
        persp(x, y, t(z), phi=20, theta=30, ticktype="detailed")
        dev.off()
      }

    }

  }

  # # # # # # # # # # # # # ####################### # # # # # # # # # # # # #
  # # # # # # # # # # # # # #### MAIN FUNCTION #### # # # # # # # # # # # # #
  # # # # # # # # # # # # # ####################### # # # # # # # # # # # # #
  print(paste("Beginning IDF development for", season, "dataset"))
  # # # #
  # # Simple Setup Stuff
  # # # #

  # variable initialization
  extremes <- list()
  fits <- list()
  best_fits <- list()
  return_vals <- list()

  # # # #
  # # Data Fixes
  # # # #
  # Prepare log to be written to
  log <- files$info
  close(file(log, open="w"))
  # If dataset is empty or unuseable, skip it...
  tmp <- mean(is.na(data$DATA))
  if (tmp > 0.25) {
    sink(log, append = TRUE)
    print("Location skipped due to excessive lack of data")
    sink()
    return(NULL)
  } else {
    sink(log, append = TRUE)
    print(paste("NA data for whole dataset: ", tmp, sep=""))
    sink()
  }
  # Fill in empty dates with "NA"
  data <- data %>%
    mutate(DATE = as.Date(DATE)) %>%
    complete(DATE = seq.Date(min(DATE), max(DATE), by="day"))
  data$DATE <- as.POSIXct(data$DATE)
  # Call more complexe data completion function
  new_test[[1]] <<- data
  data <- complete.Data()
  new_test[[2]] <<- data
  .i <<- 3


  # # # # # # # # # # # # # # # #
  # # Begin Analysis for IDF  # #
  # # # # # # # # # # # # # # # #

  # Determine the yearly maximum's trend
  block_max <- rolling.BlockMaxima(duration = 1, time = duration.Setup(1, days))

  p_abs <- cor.test(block_max$YEAR, block_max$DATA, method = "kendall")$p.value

  cat(paste("Yearly Seasonal Extreme Year/Value Correlation P_val:", signif(p_abs, 4),
      "\n"), file = log, append = TRUE)

  if(!is.null(dir)) {
    trend.Plot(block_max, 1)
  }

  # For each duration calculate the IDF
  for (i in 1:length(durations)) {
    print(paste("Analyzing Duration", durations[i]))
    return_vals[[i]] <- list()
    time <- duration.Setup(durations[i], days)
    # Apply rolling mean block maxima
    print("Applying Block Maxima...")
    extremes[[i]] <- rolling.BlockMaxima(duration = durations[i], time = time)
    new_test[[.i]] <<- extremes[[i]]
    .i <<- .i + 1

    # Plot trends (as given by Mann Kendall Test, lm(), and a prediction of the
    # quadratic fit)
    print("Plotting trends...")
    if(!is.null(dir)) {
      trend.Plot(extremes[[i]], i)
    }
    # Fit models and determine return levels
    cat(paste("DIC vals for duration", durations[i], ":\n", sep=" "), file = log,
        append = TRUE)
    print("Developing fits...")
    tmp_fits <- make.Fits(extremes[[i]], it = 10000, p = p_abs)
    #TODO: Do a lot of checking before proceeding...
    fits[[i]] <- tmp_fits
    new_best = best.Fit(fits[[i]])

    jpeg(files$trace[i])
    plot(new_best, type="trace")
    dev.off()

    print("Finding return levels and CI...")
    v <- qcov.Developer(new_best)

    # For each return period determine the predicted return levels
    for (j in 1:length(return_periods)) {
      rl <- return.Level(extremes[[i]], new_best, v, return_periods[j], p = p_abs)
      # Then plot the findings
      if (!is.null(dir)) {
        extra.Plot(i, j, rl, new_best)
      }
      return_vals[[i]][[j]] <- rl
    }
    best_fits[[i]] <- new_best
  }
  if (length(return_vals) >= 1) {
    main.Plot(return_vals)
  }
  print("Error analysis:")
  print(error)
  return_vals$fits <- best_fits
  return_vals$factors <- factors
  return_vals$stationary <- stationary
  return_vals$warn_data <- warn_data
  return_vals$warn_fits <- warn_fits
  return(return_vals)
}
