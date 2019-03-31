# # # # # # # # # # # # # # # # # # # # # #
# # Main for urban.R's IDF Function  # #
# # # # # # # # # # # # # # # # # # # # # #
start <- Sys.time()

# declare data directory
options(error = function() traceback(2))

# # Update based on rural city only analysis # #
# dir <- "Data"
dir <- file.path("Data", "Rural")
sys_call <- paste("ls", dir, "| grep .csv", sep = " ")
stationary <- TRUE

durations <- c(1,2,3,4,5,6,7,10)
seasons <- c(1,2,3,4)

# declare dataset with header based on cities in dataset
files <- system(sys_call, intern = TRUE)
cities <- unlist(strsplit(files, "\\."))[2*(1:length(files))-1]
data <- vector("list", length(files))

#color scheme
colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")

# # # # # # # # # # # # # # # # #
# # Uncomment For Cities Only # #
# # # # # # # # # # # # # # # # #

# date_cols <- c("YEAR", "MO", "DA")
# data_cols <- c("TEMP", "PRCP", "MAX", "MIN")
# all_cols = c(date_cols, data_cols)
# names(data) <- cities
# for (i in 1:length(files)) {
#   path <- file.path(dir, files[i])
#   data[[cities[i]]] <- read.csv(path, header = TRUE)
#   data[[cities[i]]] <- select(data[[cities[i]]], one_of(all_cols))
# }

# # #
# Running IDF
# # #

# print("Begin analysis:")
#
# # For now let's just focus on temp
# val = "TEMP"
# # for(val in data_cols)
# for(city in cities) {
#   print(paste("Analyzing data from", city))
#   for (s in seasons) {
#     #Make some directories
#     directory <- "Output"
#     dir.create(directory)
#     directory <- file.path(directory, city)
#     dir.create(directory)
#     directory <- file.path(directory, val)
#     #Data prep
#     cols <- c(date_cols, val)
#     test <- select(data[[city]], cols)
#     test <- unite(test, DATE, c(YEAR, MO, DA), sep="-", remove = TRUE)
#     test$DATE <- as.POSIXct(test$DATE)
#     returns <- IDF(data=test, season=s, method = "Bayesian", dir=directory,
#         stationary = stationary)
#   }
# }

# # # # # # # # # # # # # # # # # #
# Uncomment For Rural Analysis  # #
# # # # # # # # # # # # # # # # # #
cities <- unlist(strsplit(cities, "\\_"))[2*(1:length(cities))-1]
names(data) <- cities
data_cols <- c("TAVG", "PRCP", "TMAX", "TMIN")
all_cols = c("DATE", data_cols)
seasons = c("winter", "spring", "summer", "fall")
tmp <- list()
for (i in 1:length(files)) {
  path <- file.path(dir, files[i])
  tmp[[i]] <- read.csv(path, header = TRUE)
  data[[cities[[i]]]] <- list()
  if (cities[i] == "NYC") {
    data[[cities[i]]]$MAIN <- tmp[[i]] %>%
        filter(grepl("NY CITY", NAME))
    tmp[[i]] <- tmp[[i]] %>%
        filter(!grepl("NY CITY", NAME))
  } else {
    data[[cities[i]]]$MAIN <- tmp[[i]] %>%
        filter(grepl(toupper(cities[i]), NAME))
    tmp[[i]] <- tmp[[i]] %>%
        filter(!grepl(toupper(cities[i]), NAME))
  }
  rural <- as.character(unique(tmp[[i]]$NAME))
  rural <- unlist(strsplit(rural, ","))[2*(1:length(rural))-1]
  r_names <- gsub(" ", "_", rural)
  for (j in 1:length(rural)) {
    data[[cities[[i]]]][[r_names[j]]] <- tmp[[i]] %>%
        filter(grepl(rural[j], NAME))
  }
}

print("Begin analysis:")

# # For now let's just focus on temp
val <- "TMAX"
s <- "summer"
# for(val in data_cols)
# for(city in cities) {
city <- cities[2]
returns <- list()
  print(paste("Analyzing data from", city))
  # loc <- "MAIN"
  warn <- FALSE
  for (loc in names(data[[city]])) {
    print(paste("Analyzing", loc))
    # for (s in seasons) {
      #Make some directories
      directory <- "Output"
      dir.create(directory)
      city_R <- paste(city, "_R", sep = "")
      directory <- file.path(directory, city_R)
      dir.create(directory)
      directory <- file.path(directory, loc)
      dir.create(directory)
      directory <- file.path(directory, val)
      dir.create(directory)
      #Data prep
      cols <- c("DATE", val)
      test <- select(data[[city]][[loc]], cols)
      test$DATE <- as.POSIXct(test$DATE)
      print(head(test))
      returns[[loc]] <- IDF(data=test, season=s, method = "Bayesian",
          dir=directory, stationary = stationary)
    }
  if (stationary && !warn) {
    x <- c(1:7,10) #durations
    rp <- c(2,20,100) #return return periods

    i <- length(x) #should be # of durations
    j <- length(rp) #should be # of return return periods

    # make a new plot for each return period
    for (n in 1:j) {
      file <- file.path("Output",paste(city, "_R", sep=""), paste(rp[n],
        "yr_main_IDF.jpeg", sep=""))
      jpeg(file, width=500, height=750)
      plot(-1, ylim=c(90,110), xlim=c(0,10))
      it <- 1
      locs <- c()
      for (loc in names(data[[city]])) {
        df <- returns[[loc]]
        if (!is.null(df)) {
          locs <- append(locs, loc)
          col <- colors[it]
          years <- df[[1]][[1]]$x
          k <- length(years) #should be # of years of data
          z = ci_l = ci_u <- rep(0, i) # holds data & confidence interval
          # Handle data for each duration
          for(m in 1:i) {
            z[m] <- df[[m]][[n]]$y[1]
            ci_l[m] <- df[[m]][[n]]$ci_l[1]
            ci_u[m] <- df[[m]][[n]]$ci_u[1]
          }
          if (it == 1) {
          plot(x, z, ylim=range(c((ci_l-5), (ci_u+5))), ylab="DATA", xlab="YEAR",
              main = paste(rp[n], "-Year Return Levels Curve", dir,
              sep=""), type = "o", col=col)
          } else {
          plot(x, z, type = "o", col=col, add = TRUE)
          }
          arrows(x, ci_l, x, ci_u, length=0.05, angle=90, code=3, col=col)
          it <- it + 1
        }
      }
      legend("bottomright", legend = locs, lty = 1, col = colors[1:it])
      dev.off()
    }
  }
# }

# source("urban_test.R")
# final.Test(names(data[[city]]), returns)

print("End analysis")
print(error)

end <- Sys.time()
print(end-start)
