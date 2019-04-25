# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # setup.R : This file handles the setup for urban.R, including file setup,  #
# # # season setup, etc.                                                      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


### Time Series Handling ###

# # # #
# # season.Setup
# # # #
# This function determines the start and end day of the observation based on
# what season is indicated to be studied. The season is also returned in a
# more normalized fashion.
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
  } else if ((season == 4) | (season == "fall") | (season == "autumn")) {
    start_day <- "2999-09-01"
    end_day <- "2999-11-30"
    season <- "fall"
  } else if ((season == 5) | (season == "all")) {
    start_day <- "2999-01-01"
    end_day <- "2999-12-31"
    season <- "all"
  }
  days <- list(start=start_day, end=end_day, season=season)
  class(days) <- "days"
  return(days)
}

# # # #
# year.Range
# # # #
# This function determines the total range of the series under the criteria of
# adequate data for analysis
year.Range <- function(days, data) {
  if (class(days) != "days") {
    stop("Invalid 'days' input for year.Range")
  }
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
  season <- days$season
  if (class(days) != "days") {
    stop("Invalid 'days' input for year.Range")
  }
  if (season != "all") {
    if (duration%%2 == 1) {
      adj <- (duration-1)/2
      dur_start <- format((as.Date(days$start) - adj), format="%m-%d")
      dur_end <- format((as.Date(days$end) + adj), format="%m-%d")
    } else if (duration%%2 == 0) {
      adj <- duration/2
      dur_start <- format((as.Date(days$start) - adj + 1), format="%m-%d")
      dur_end <- format((as.Date(days$end) + adj), format="%m-%d")
    }
  } else if (season == "all") {
    dur_start <- format((as.Date(days$start) - adj + 1), format="%m-%d")
    dur_end <- format((as.Date(days$end) + adj), format="%m-%d")
  }
  time <- list(start=dur_start, end=dur_end)
  class(time) <- "timeframe"
  return(time)
}

# This function develops the directories and filenames if the user chooses
# to plot the IDF findings
file.Setup <- function(days, dir, return_periods, durations) {
  season <- days$season
  extra <- file.path(dir, "extra")
  idf <- file.path(dir, "idf")

  season_extra <- file.path(extra, season)
  season_idf <- file.path(idf, season)

  dir.create(dir)
  dir.create(extra)
  dir.create(idf)
  dir.create(season_extra)
  dir.create(season_idf)

  trend_file <- c()
  rl_file <- c()
  trace_file <- c()
  idf_file <- c()
  idf2_file <- c()
  info_file <- file.path(extra, "info_file.txt")
  for (i in 1:length(durations)) {
    trend_file[i] <- paste(season_extra, "/trend-", durations[i], "day.jpeg", sep="")
    rl_file[i] <- paste(season_extra, "/rlplot-", durations[i], "day.jpeg", sep="")
    trace_file[i] <- paste(season_extra, "/trace-", durations[i], "day.jpeg", sep="")
    for (j in 1:length(return_periods)) {
      idf_file[(i - 1)*length(return_periods) + j] <- paste(season_extra, "/idf-",
          return_periods[j], "year-", durations[i], "day.jpeg", sep = "")
    }
  }
  for (j in 1:length(return_periods)) {
    idf2_file[j] <- paste(season_idf, "/idf-", return_periods[j], "year.jpeg",
        sep = "")
  }
  return(list(rl=rl_file, idf=idf_file, idf2=idf2_file, trend=trend_file,
      info=info_file, trace=trace_file))
}
