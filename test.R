data("PORTw", package = "extRemes")
fit1 <- fevd(TMX1, PORTw)
fit0 <- fevd(TMX1, PORTw, type = "Gumbel")

fitb <- fevd(TMX1, PORTw, method = "Bayesian", iter = 1000)
