
#### this is a little helper for plotting old models, imported from FLAGG V1 (MSc Thesis version)
# only for replotting old Aare and Rhine (Klettgau) profile models

plotOldModel <- function(...) {
x <- model$model[[2]][,1]
wx <- c(min(model$model[[2]][,1]), max(model$model[[2]][,1]))
new.x <- seq(wx[1], wx[2], length.out = length(model$fitted.values))
pred <- predict(model, new = data.frame(x = new.x), interval = "conf", level = 0.95)
lines(x,pred[,"fit"],...)
}

