# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Author: April Walker
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(tidyr)
library(dplyr)
library(extRemes)
library(zoo)

year.Range <- function(data) {
  return(1980:2018)
}

season.Setup <- function(season) {
  if ((season == 1) | (season == "winter")) {
    start_day <- "2999-12-01"
    end_day <- "2999-02-31"
  } else if ((season == 2) | (season == "spring")) {
    start_day <- "2999-03-01"
    end_day <- "2999-05-31"
  } else if ((season == 3) | (season == "summer")) {
    start_day <- "2999-06-01"
    end_day <- "2999-08-31"
  } else if ((season = 4) | (season == "fall")) {
    start_day <- "2999-09-01"
    end_day <- "2999-11-31"
  }
  return(list(start=start_day, end=end_day))
}

duration.Setup <- function(duration, days) {
    if (duration%%2 == 1) {
      adj <- (duration-1) /2
      dur_start <- format((as.Date(days$start) - adj), format="%m-%d")
      dur_end <- format((as.Date(days$end) + adj), format="%m-%d")
    } else if (duration%%2 == 0) {
      adj <- duration/2
      dur_start <- format((as.Date(days$start) - adj + 1), format="%m-%d")
      dur_end <- format((as.Date(days$end) + adj), format="%m-%d")
    }
    return(list(start=dur_start, end=dur_end))
}

rolling.BlockMaxima <- function(years, days, duration, data, extreme="max") {
  for (yr in years) {
    # Grab the dataset for each year
    if (days$start == "2999-12-01") {
      tmp <- subset(data, (format(as.Date(DATE,format = "%m-%d-%Y"),"%Y") == yr)
          | (format(as.Date(DATE,format = "%m-%d-%Y"),"%Y") == yr + 1))
      dates <- seq(as.Date(paste(yr, days$start, sep="-")),
          as.Date(paste(yr, days$end, sep="-")), by = "day"))
    } else {
      tmp <- subset(data, format(as.Date(DATE,format = "%m-%d-%Y"),"%Y") == yr)
      dates <- seq(as.Date(paste(yr, days$start, sep="-")),
          as.Date(paste(yr, days$end, sep="-")), by = "day"))
    }
    # Double check for complete data
    tmp <- tmp %>%
        mutate(DATE = as.Date(DATE)) %>%
        complete(DATE = dates) %>%
        fill(DATA)
    # Find rolling mean and add save max value for the year
    tmp <- rollmean(tmp$DATA, duration, align = "center")
    extremes <- tryCatch(rbind(extremes, cbind.data.frame(yr,
        get(extreme)(tmp))), error = function(e) print("try-error"))
    if (class(extremes) == "character") {
      extremes <- cbind.data.frame(yr, get(extreme)(tmp))
      names(extremes) <- c("YEAR", "DATA")
      extremes$DATA <- as.double(extremes$DATA)
    }
  }
  return(extremes)
}

estimateGOF <- function(fits) {
  AIC <- c()
  MLE <- c()
  for (j in 1:length(fits)) {
    MLE[j] = fits[[j]]$results$value
    try({AIC[j] <- as.double(summary(fits[[j]])$AIC)})
    if(is.null(AIC[j]) | is.na(AIC[j])) {
      AIC[j] = NaN
    }
  } if (min(AIC) == NA) {
    print(i)
    best_fit = fits[[which.min(MLE) + 2]]
  } else {
    best_fit = fits[[which.min(AIC) + 2]]
  }
  return(best_fit)
}


# TDF takes in data, durations, and season. The data should contain a column of
# dates in the first column (mm-dd-yyyy format) and a column of data in the
# second column. The durations can be a vector of day lengths which you wish to
# determine the rolling average for. The season should be "winter" "spring"
# "summer" or "fall" or a numerical value from 1:4 corresponding to a season in
# that order. The extreme should me "max" or "min". The forceGev qualifier ignores
# the lines which lr test between Gumbel and GEV.
IDF <- function(data, durations, season, extreme = "max", forceGev = TRUE) {
  # Variable init and gen setup
  extremes <- list()
  fits <- list()
  bf <- list()
  names(data) <- c("DATE", "DATA")
  # Season setup
  season_init <- seasonSetup(season)
  year_range <- yearRange(data)
  if ((season == 1) | season=="winter") {
    year_range = year_range[2:length(year_range)]
  }
  # Duration setup and block maxima
  for (i in 1:length(durations)) {
    dur_init <- durationSetup(durations[i], season_init)
    # Save the data from within this window to develop the rolling means
    data <- subset(data, format(as.Date(DATE,format = "%m-%d-%Y"),"%m-%d") >=
        dur_init$start & format(as.Date(DATE,format = "%m-%d-%Y"),"%m-%d") <=
        dur_init$end)
    # Apply rolling mean block maxima
    extremes[[i]] <- rollingBlockMaxima(year_range, dur_init, durations[i], data)
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
      fits[[i]][[j+1]] <- fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j+2]] <- fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), units = "deg F")
      # fits with scale ~ year
      fits[[i]][[j+3]] <- fevd(val[[j]]$TMAX, val[[j]], type = type, scale.fun =
          ~ poly(((Year - start_year)/ data_years), 1), units = "deg F")
      fits[[i]][[j+4]] <- fevd(val[[j]]$TMAX, val[[j]], type = type, scale.fun =
          ~ poly(Year, 2), units = "deg F")
      # Fits with location ~ year and scale ~ year
      fits[[i]][[j+5]] <- fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j+6]] = fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j+7]] <- fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), units = "deg F")
      fits[[i]][[j+8]] <- fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), units = "deg F")
    }
    # Find best fit
    num_params = c()
    best_fits = list()
    for (j in 1:length(fits[[i]])) {
      num_params[j] = length(fits[[i]][[j]]$results$value)
    }
    for (j in min(num_params):max(num_params)) {
      index = which(num_params %in% j)
      if(!(length(index) == 0)) {
        for(k in index) {
          best_fits <- append(best_fits, GOF(fits[[i]][[index]]))
        }
      }
    }
    for (j in 1:(length(best_fits)-1)) {
      lr = lr.test(best_fits[[j]], best_fits[[j+1]])
      if (test$p.value < 0.05) {
        new_best = best_fits[[j+1]]
      }
    }
  }
}


















# # # # # # # # #
# MAIN FUNCTION #
# # # # # # # # #

# declare data directory
dir <- "Historical_daily_obs"
sys_call <- paste("ls", dir, "| grep .csv", sep = " ")
data_cols = c("YEAR", "MO", "DA", "TEMP", "PRCP", "MAX", "MIN")

# declare dataset with header based on cities in dataset
files <- system(sys_call, intern = TRUE)
cities <- unlist(strsplit(files, "\\."))[2*(1:length(files))-1]
data <- vector("list", length(files))
names(data) = cities

for (i in 1:length(files)) {
  path <- paste(dir, files[i], sep="/")
  data[[cities[i]]] <- read.csv(path, header = TRUE)
  data[[cities[i]]] = subset(data[[cities[i]]], names(data[[cities[i]]]) == data_cols)
}

durations = c(1,2,3,4,5,6,7,10)
seasons = c(1,2,3,4)
