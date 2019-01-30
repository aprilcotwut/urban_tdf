# # # # # #
# Fuck u  #
# # # # # #
library(xlsx)

files = system("ls | grep .xlsx", intern = TRUE)

for (file in files) {
  data = read.xlsx2(file, sheetName = "Sheet1")
  file_name = paste(strplit(file, "\\.")[[1]][1], ".csv", sep = "")
  write.csv(data, file = file_name, sep=",")
}
