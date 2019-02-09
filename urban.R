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
#    r_periods - This should be a vector containing the return periods which
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

IDF <- function(data, durations=c(1:7,10), r_periods=c(2, 20, 100),
        season = 3, extreme = "max", forceGev = FALSE, dir = NULL) {

  # This function develops the directories and filenames if the user chooses
  # to plot the IDF findings
  file.Setup <- function(dir, durations, r_periods, season) {
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
      trend_file[i] = paste(trend, "trend-", durations[i], "day.jpeg")
      rl_file[i] = paste(rl, "rlplot-", durations[i], "day.jpeg")
      for (p in r_periods) {
        idf_file[i] = paste(idf, "idf-", r_periods, "year-", durations[i],
            "day.jpeg", sep = "")
      }
    }
    return(list(rl=rl_file, idf=idf_file, trend=trend_file))
  }

  # This function reads in the data and determines the year range
  year.Range <- function(data) {
    start <- format(as.Date(min(data$DATE)), format="%Y")
    end <- format(as.Date(max(data$DATE)), format="%Y")
    return(start:end)
  }

  # This season determines the start and end day of the observation based on
  # what season is indicated to be studied
  season.Setup <- function(season) {
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
  rolling.BlockMaxima <- function(years, days, duration, data, extreme="max") {
    extremes = c()
    for (yr in years) {
      # Grab the dataset for each year
      if (days$start == "12-01") {
        days$end <- "02-28"
        if(leap_year(yr+1)) {
          days$end <- "02-29"
        }
      }
      dates <- seq(as.Date(paste(yr, days$start, sep="-")),
          as.Date(paste(yr + 1, days$end, sep="-")), by = "day")
      tmp <- subset(data, as.Date(DATE,format = "%m-%d-%Y") %in% dates)
      # Double check for complete data
      tmp <- tmp %>%
          mutate(DATE = as.Date(DATE)) %>%
          complete(DATE = dates) %>%
          fill(DATA)
      # Find rolling mean and add save max value for the year
      tmp <- rollmean(tmp$DATA, duration, align = "center")
      extremes <- tryCatch(rbind(extremes, cbind.data.frame(yr,
          get(extreme)(tmp))), error = function(e) print("try-error"))
      # if (class(extremes) == "character") {
      #   extremes <- cbind.data.frame(yr, get(extreme)(tmp))
      #   extremes$DATA <- as.double(extremes$DATA)
      # }
    }
    names(extremes) <- c("YEAR", "DATA")
    return(extremes)
  }

  # Given a vector of fits, this functon detemines and returns the best fit
  # based on AIC, or if unavaliable, MLE
  estimate.GOF <- function(fits) {
    # if fits only contains one fit
    if(!is.null(fits$call)) {
      best_fits = fits
      return(best_fits)
    }
    # else determine the best fit
    AIC <- c()
    MLE <- c()
    for (i in 1:length(fits)) {
      tmp <- fits[[i]]
      print(tmp)
      MLE <- append(MLE, tmp$results$value)
      try({AIC <- append(AIC, as.double(summary(tmp$AIC)))})
    }
    if (length(AIC) != 0) {
      best_fit = fits[[which.min(AIC) + 2]]
    } else {
      best_fit = fits[[which.min(MLE) + 2]]
    }
    return(best_fit)
  }

  # Given either a stationary or nonstationary fevd object, a qcov, and a
  # level of confidence (alpha) this function determines the return level and
  # confidence level for a given return period
  return.Level <- function(fit, qcov, alpha, r_period) {
    ci = ci(fit, alpha = alpha, return.period = r_period,
        qcov = qcov)
    x = year_range
    for (val in ci) {
      print(val)
    }
    if (is.null(dim(c))) {
      y = ci[2]
      ci_l = ci[1]
      ci_u = ci[3]
      err = (ci_u-ci_l)/(qnorm(((1-alpha)/2)+alpha)
    } else {
      y = ci[,2]
      ci_l = ci[,1]
      ci_u = ci[,3]
      err = ci[,4]
    }
    return(x=x, y=y, ci_l=ci_l, ci_u=ci_u, err = err)
  }

  # # # # # # # # # # # # # ####################### # # # # # # # # # # # # #
  # # # # # # # # # # # # # #### MAIN FUNCTION #### # # # # # # # # # # # # #
  # # # # # # # # # # # # # ####################### # # # # # # # # # # # # #

  # Variable init and gen setup
  extremes <- list()
  fits <- list()
  return_vals = list()

  # Data fixes and generalizations
  names(data) <- c("DATE", "DATA")
  data$DATE <- as.POSIXct(data$DATE)

  # Season setup
  season_init <- season.Setup(season)
  year_range <- year.Range(data)
  if (!is.null(dir)) {
    file = file.Setup(dir, durations, r_periods, season_init$season)
  }
  if ((season == 1) | season=="winter") {
    year_range = year_range[1:(length(year_range)-1)]
  }
  data_years = length(year_range)
  # Duration setup and block maxima
  for (i in 1:length(durations)) {
    return_vals[[i]] = list()
    dur_init <- duration.Setup(durations[i], season_init)
    # Apply rolling mean block maxima
    extremes[[i]] <- rolling.BlockMaxima(year_range, dur_init, durations[i], data)
    # Begin trend tests
    mk <- cor.test(extremes[[i]]$YEAR, extremes[[i]]$DATA, method="kendall")
    tau <- as.double(mk$estimate)
    p <- mk$p.value
    # Begin fitting to models
    fits[[i]] <- list()
    j <- 1
    if (!forceGev) {
      j <- 2
      fits[[i]][[1]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type="Gumbel", units="deg F")
      fits[[i]][[2]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type="GEV", units="deg F")
      lr = lr.test(fits[[i]][[1]], fits[[i]][[2]])
      if (lr$p.value < 0.05) {
        type <- "GEV"
        shape <- ci(fits[[i]][[2]] , type="parameter", which.par=3)[2]
        if (shape < 0) {
          GEV_type <- "Weibull"
        } else if (shape > 0) {
          GEV_type <- "Frechet"
        }
      } else {
        type <- "Gumbel"
        GEV_type <- "Gumbel"
      }
    } else {
      fits[[i]][[1]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type="GEV", units="deg F")
      type <- "GEV"
      GEV_type <- "NA"
    }
    # If p < 0.05 the data is considered nonstationary
    if (p < 0.05) {
      # Fits with location ~ year
      fits[[i]][[j+1]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type = type, location.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j+2]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type = type, location.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 2, raw = TRUE), units = "deg F")
      # fits with scale ~ year
      fits[[i]][[j+3]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type = type, scale.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 1), units = "deg F")
      fits[[i]][[j+4]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type = type, scale.fun =
          ~ poly(YEAR, 2), units = "deg F")
      # Fits with location ~ year and scale ~ year
      fits[[i]][[j+5]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type = type, location.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 1, raw = TRUE), scale.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j+6]] = fevd(extremes[[i]]$DATA, extremes[[i]], type = type, location.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 2, raw = TRUE), scale.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j+7]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type = type, location.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 2, raw = TRUE), scale.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 2, raw = TRUE), units = "deg F")
      fits[[i]][[j+8]] <- fevd(extremes[[i]]$DATA, extremes[[i]], type = type, location.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 1, raw = TRUE), scale.fun =
          ~ poly(((YEAR - year_range[1])/ data_years), 2, raw = TRUE), units = "deg F")
    }
    # Find best fit
    num_params = c()
    best_fits = list()
    for (j in 1:length(fits[[i]])) {
      num_params[j] = length(fits[[i]][[j]]$results$par)
    }
    k = 1
    for (j in min(num_params):max(num_params)) {
      index = which(num_params %in% j)
      if(!(length(index) == 0)) {
          tmp <- fits[[i]][[index]]
          best_fits[[k]] <- estimate.GOF(tmp)
          k = k + 1
      }
    }
    new_best = best_fits[[1]]
    for (j in 2:length(best_fits)) {
      lr = lr.test(new_best, best_fits[[j]])
      if (lr$p.value < 0.05) {
        new_best = best_fits[[j+1]]
      }
    }
    scaled_range = (year_range - year_range[1])/data_years
    threshold = seq()
    lin_par = c("mu1", "sigma1")
    quad_par = c("mu2", "sigma2")
    params = intersect(lin_par, names(new_best$results$par))
    if (is_empty(params)) {
      v = make.qcov(new_best)
    } else {
      vals = list()
      for (val in params) {
        assign(val, lin_seq)
        vals[[val]] = get(val)
      }
      params = intersect(quad_par, names(new_best$results$par))
      for (val in params) {
        assign(val, poly_seq)
        vals[[val]] = get(val)
      }
      if (is.null(new_best$num.pars$shape)) {
        new_best$num.pars$shape = 0
      }
      v = make.qcov(new_best, vals = vals)
    }
    for (j in 1:length(r_periods)) {
      rl = return.Level(new_best, v, 0.05, r_periods[j])
      return_vals[[i]][[j]]
      if (!is.null(dir)) {
        jpeg(file$idf, width=500, height=750)
        plot(rl$x, rl$y, ylim=range(c(rl$ci_l, rl$ci_u)), ylab="DATA", xlab="YEAR",
            main = paste(r_periods[j], "-Year Return Levels for ", durations[i],
            " Day Events in ", dir, sep=""), type = "o")
        arrows(rl$x, rl$ci_l, rl$ci_u, length=0.05, andlge=90, code=3)
        dev.off()
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

dir <- "Historical_daily_obs"
sys_call <- paste("ls", dir, "| grep .csv", sep = " ")
data_cols = c("YEAR", "MO", "DA", "TEMP", "PRCP", "MAX", "MIN")

# declare dataset with header based on cities in dataset
files <- system(sys_call, intern = TRUE)
cities <- unlist(strsplit(files, "\\."))[2*(1:length(files))-1]
data <- vector("list", length(files))
names(data) = cities

for (i in 1:length(files)) {
  path <- file.path(dir, files[i])
  data[[cities[i]]] <- read.csv(path, header = TRUE)
  data[[cities[i]]] <- select(data[[cities[i]]], one_of(data_cols))
}

test <- select(data$SLC, c("YEAR", "MO", "DA", "TEMP"))
test <- unite(test, DATE, c(YEAR, MO, DA), sep="-", remove = TRUE)
test$DATE <- as.POSIXct(test$DATE)

durations = c(1,2,3,4,5,6,7,10)
seasons = c(1,2,3,4)
print("Begin analysis:")
for(season in seasons) {
  returns = IDF(test, durations, 2, season, "max", FALSE, "SLC")
}
