# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Author: April Walker
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# declare data directory
dir <- "Historical_daily_obs"
sys_call <- paste("ls", dir, "| grep .csv", sep = " ")

# declare dataset with header based on cities in dataset
files <- system(sys_call, intern = TRUE)
cities <- unlist(strsplit(files, "\\."))[2*(1:length(files))-1]
data <- vector("list", length(files))
names(data) = cities

for (i in 1:length(files)) {
  path <- paste(dir, files[1], sep="/")
  data[i] = read.csv(path, header = TRUE)
}
