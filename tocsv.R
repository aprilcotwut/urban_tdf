# # # # # # # # # # # # #
# converts xlsx -> csv  #
# # # # # # # # # # # # #
library(rio)

dir <- "Data"
sys_call <- paste("ls", dir, "| grep .xlsx", sep = " ")

files <- system(sys_call, intern = TRUE)

for (file in files) {
  path <- paste(dir, file, sep="/")
  created <- mapply(convert, path, gsub("xlsx", "csv", path))
}
