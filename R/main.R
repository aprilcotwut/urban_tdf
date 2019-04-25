# # # # # # # # # # # # # # # # # # # # # #
# # Main for urban.R's IDF Function  # # #
# # # # # # # # # # # # # # # # # # # # # #
start <- Sys.time()
source("R/urban.R")
source("R/plot.R")

# declare data directory
options(error = function() traceback(2))

# # Update based on rural city only analysis # #
# dir <- "Data"
dir <- file.path("Data", "Rural")
# dir <- file.path("Data", "Long")
sys_call <- paste("ls", dir, "| grep .csv", sep = " ")
stationary <- FALSE

durations <- c(1,2,3,4,5,6,7,10)
seasons <- c(1,2,3,4)

# declare dataset with header based on cities in dataset
files <- system(sys_call, intern = TRUE)
cities <- unlist(strsplit(files, "\\."))[2*(1:length(files))-1]
data <- vector("list", length(files))

# this gathers the sites info to use as covariates
site_dir <- file.path("Data", "Sites_Info")
sys_call <- paste("ls", site_dir, "| grep .csv", sep = " ")
site_files <- system(sys_call, intern = TRUE)


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
#
# # # #
# # Running IDF
# # # #
#
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
for(city in cities) {
# city <- cities[3]
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
      returns[[loc]] <- IDF(data=test, season=s, method = "MLE", alpha = 0.1,
          dir=directory, stationary = stationary)
    }
    returns.plot(returns, names(data[[city]]), stationary)
}

# source("urban_test.R")
# final.Test(names(data[[city]]), returns)

print("End analysis")

end <- Sys.time()
time <- end - start
print(time)

save(returns, file = file.path("Output", paste0(format(end,
  "%Y%m%d_%H%M%S_"), "data.RData")))
