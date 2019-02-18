library(tidyverse)
library(extRemes)
library(zoo)
library(lubridate)

# # # #
# # Basic extReme's Testing
# # # #

data("PORTw", package = "extRemes")
fit1 <- fevd(TMX1, PORTw)
fit0 <- fevd(TMX1, PORTw, type = "Gumbel")
fit2 <- fevd(TMX1, PORTw, location.fun ~AOindex)
fitb <- fevd(TMX1, PORTw, method = "Bayesian", iter = 1000)
fitb2 <- fevd(TMX1, PORTw, location.fun ~AOindex,
    method = "Bayesian", iter = 1000)
BayesFactor(fitb, fitb2, burn.in = 100, method = "harmonic")


fitb <- fevd(TMX1, PORTw, method = "Bayesian", iter = 600)
fitb2 <- fevd(TMX1, PORTw, location.fun = ~AOindex,
method = "Bayesian", iter = 600)
fitb3 <- fevd(TMX1, PORTw, scale.fun = ~AOindex^2,
method = "Bayesian", iter = 600)
fitb4 <- fevd(TMX1, PORTw, location.fun = ~AOindex, scale.fun = ~AOindex,
method = "Bayesian", iter = 600)
BayesFactor(fitb3, fitb4, burn.in = 500, method = "harmonic")
