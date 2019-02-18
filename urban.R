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
error <<- c(0,0,0,0,0)
names(error) <- c("CI", "CI-R", "PLOT", "FEVD", "BFs")

IDF <- function(data, durations=c(1:7,10), return_periods=c(2, 20, 100),
        season, extreme = "max", forceGEV = FALSE, method = NULL, dir = NULL,
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
    info_file = paste(extra, "info_file.txt")
    for (i in 1:length(durations)) {
      trend_file[i] = paste(trend, "/trend-", durations[i], "day.jpeg", sep="")
      rl_file[i] = paste(rl, "/rlplot-", durations[i], "day.jpeg", sep="")
      for (j in 1:length(return_periods)) {
        idf_file[(i - 1)*length(return_periods) + j] = paste(season_idf, "/idf-",
            return_periods[j], "year-", durations[i], "day.jpeg", sep = "")
      }
    }
    return(list(rl=rl_file, idf=idf_file, trend=trend_file, info=info_file))
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
    log <- file.Setup()$info
    years <- df$YEAR
    if (is.null(p)) {
      p <- cor.test(df$YEAR, df$DATA, method="kendall")$p.value
    }
    j <- 1
    fits <- list()
    # Main function #
    if ((!.forceGEV) && is.null(type)) {
      j <- 2
      warn_data <<- df
      fits[[1]] <- fevd(df$DATA, df, type="Gumbel", method = .method, iter = it)
      fits[[2]] <- fevd(df$DATA, df, type="GEV", method = .method, iter = it)
      lr = lr.test(fits[[1]], fits[[2]])
      if (lr$p.value < 0.05) {
        type <- "GEV"
      } else {
        type <- "Gumbel"
      }
    } else if (.forceGEV) {
      fits[[1]] <- fevd(df$DATA, df, type="GEV", method = .method, iter = it)
      if (class(fits[1]) == "character") {
        error[4] <<- error[4] + 1
        warn_data[w] <<- df
        w <<- w + 1
      }
      type <- "GEV"
    } else {
      fits[[1]] <- fevd(df$DATA, df, type=type, method = .method, iter = it)
    }
    data_years <- length(years)
    # If p < 0.05 the data is considered nonstationary
    if (p < 0.05) {
      ### Fits with location ~ year or ~year^2 ###
      fits[[j+1]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE))
      fits[[j+2]] <- fevd(df$DATA, df, type = type, method = .method, iter = 1000,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE))

      ### Fits with scale ~ year ###
      fits[[j+3]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
        scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1), use.phi = TRUE)

      ### Fits with location ~ year and scale ~ year ###
      fits[[j+4]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          use.phi = TRUE)
      fits[[j+5]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
          location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
          scale.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
          use.phi = TRUE)
      if (polyFits) {
        ### Fits with scale ~ year^2 ###
        fits[[j+6]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
            scale.fun = ~ poly(YEAR, 2), use.phi = TRUE)
        fits[[j+7]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
            location.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            scale.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            use.phi = TRUE)
        fits[[j+8]] <- fevd(df$DATA, df, type = type, method = .method, iter = it,
            location.fun = ~ poly(((YEAR - years[1])/ data_years), 1, raw = TRUE),
            scale.fun = ~ poly(((YEAR - years[1])/ data_years), 2, raw = TRUE),
            use.phi = TRUE)
      }
    }
    names(fits) <- seq(1, length(fits))
    for (val in fits) {
      tmp <- as.double(summary(val)$DIC)
      sink(log, append = TRUE)
      print(tmp)
      sink()
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

  best.Fit <- function(fits) {
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
        if (lr$p.value < 0.05) {
          new_best <- best_fits[[j]]
        }
      }
      return(new_best)
    } else if (method == "Bayesian") {
      tmp <- as.double(summary(new_best)$DIC)
      for (j in 2:length(best_fits)) {
        factor <- as.numeric(BayesFactor(new_best, best_fits[[j]])[1])
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
        else if (factor > 1.001) {
          new_best <- best_fits[[j]]
        }
      }
      return(new_best)
    }
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
  return.Level <- function(df, fit, qcov, rp, p = NULL) {
    # Find significance and year range
    if (is.null(p)) {
      p <- cor.test(df$YEAR, df$DATA, method="kendall")$p.value
    }
    # Calculate the confidence interval
    ci = ci(fit, alpha = alpha, return.period = rp, qcov = qcov)
    # Error handling temp deleted

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
    return(list(x=x, y=y, ci_l=ci_l, ci_u=ci_u, warn=warning, ns=nonstationary))
  }

  # These function handles plot development for this codebase
  # Develops the IDF plot
  IDF.Plot <- function(dur_it, rp_it, rl_vals, fit, files = file.Setup()) {
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
    # Otherwise just plot the typical RL plot
    } else {
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

  # Develops the trend plots for the data
  trend.Plot <- function(df, dur_it, files = file.Setup()) {
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
        interval='confidence', level=0.95)
    lines(df$YEAR, predicted[,1], col="#5e3c99")
    dev.off()
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
  log <- file.Setup()$info
  close(file(log, open="w"))

  # # # # # # # # # # # # # # # #
  # # Begin Analysis for IDF  # #
  # # # # # # # # # # # # # # # #

  # Determine the yearly maximum's trend
  block_max <- rolling.BlockMaxima(year.Range(season.Setup()),
      duration.Setup(1, season.Setup()), 1)
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
    dur_init <- duration.Setup(durations[i], season.Setup())
    # Apply rolling mean block maxima
    extremes[[i]] <- rolling.BlockMaxima(year.Range(season.Setup()), dur_init,
        durations[i])
    # Plot trends (as given by Mann Kendall Test, lm(), and a prediction of the
    # quadratic fit)
    if(!is.null(dir)) {
      trend.Plot(extremes[[i]], i)
    }
    # Fit models and determine return levels
    cat(paste("DIC vals for duration", durations[i], ":\n", sep=" "), file = log,
        append = TRUE)
    tmp_fits <- make.Fits(extremes[[i]], it = 2000, p = p_abs)
    #TODO: Do a lot of checking before proceeding...
    fits[[i]] <- tmp_fits
    new_best = best.Fit(fits[[i]])
    if (is.null(method) || method == "MLE") {
      v <- qcov.Developer(extremes[[i]]$YEAR, new_best)
    } else {
      v <- NULL
    }
    # For each return period determine the predicted return levels
    for (j in 1:length(return_periods)) {
      rl <- return.Level(extremes[[i]], new_best, v, return_periods[j], p = p_abs)
      # Then plot the findings
      if (!is.null(dir)) {
        IDF.Plot(i, j, rl, new_best)
      }
      return_vals[[i]][[j]] <- rl
    }
  }
  return(return_vals)
}





# # # # # # # # # # # # # # # # # # # # # #
# # Testing for urban.R's IDF Function  # #
# # # # # # # # # # # # # # # # # # # # # #

# declare data directory
options(error = function() traceback(2))

# # Update based on rural city only analysis # #
# dir <- "Data"
dir <- file.path("Data", "Rural")
sys_call <- paste("ls", dir, "| grep .csv", sep = " ")

date_cols <- c("YEAR", "MO", "DA")
data_cols <- c("TEMP", "PRCP", "MAX", "MIN")
all_cols = c(date_cols, data_cols)

durations <- c(1,2,3,4,5,6,7,10)
seasons <- c(1,2,3,4)

# declare dataset with header based on cities in dataset
files <- system(sys_call, intern = TRUE)
cities <- unlist(strsplit(files, "\\."))[2*(1:length(files))-1]

# # Uncomment For Cities Only # #
# data <- vector("list", length(files))
# names(data) <- cities
# for (i in 1:length(files)) {
#   path <- file.path(dir, files[i])
#   data[[cities[i]]] <- read.csv(path, header = TRUE)
#   data[[cities[i]]] <- select(data[[cities[i]]], one_of(all_cols))
# }

# # Uncomment For Rural Analysis # #
tmp <- list()
for (i in 1:length(files)) {
  path <- file.path(dir, files[i])
  tmp[[i]] <- read.csv(path, header = TRUE)
  tmp[[i]] <- select(tmp[[i]], one_of(all_cols))
}








print("Begin analysis:")



# # # #
# # Running IDF
# # # #

# For now let's just focus on temp
val = "TEMP"

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
    returns <- IDF(data=test, season=s, method = "Bayesian", dir=directory)
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
