# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script handles testing for urban.R                                     #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# This script is a final check on the returned values from IDF. TODO: In the
# future, this shoudl be replaced with individual unit tests which run with
# IDF().
final.Test <- function(locations, df) {
  for (loc in locations) {
    data <- list()
    for (i in 1:length(df[[loc]]))
      data[[i]] <- df[[loc]][[i]][[1]]$data
    for (i in 2:length(df[[loc]])) {
      if (max(data[[i]]$DATA > max(data[[i-1]]$DATA)) {
        print("ERROR")
    #   }
    # }
  }
}

test <- function(a) {
  test2 <- function() {
    a + 1
  }
  a <- 5
  a <- test2()
  b <- 7
  test3 <- function() {
    a + b
  }
  a <- test3()
  return(a)
}

test <- test %>%
  mutate(DATE = as.Date(DATE)) %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), by="day"))

for (loc in names(data[[city]])) {
  df <- returns[[loc]]
  if (!is.null(df)) {
    locs <- append(locs, loc)
  }
  print(locs)
}
