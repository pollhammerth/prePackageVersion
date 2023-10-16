#### pmt.model() and pmt.plotModel()
# pmt.model(): creates lin or poly models, offering more convenience than typing lm() function
# pmt.plotModel(): plots a lin or poly model. (Does not work with log models!)
# pmt.plotModel() depends on empty.plot() 

pmt.model <- function(data, deg = 1){
  if (deg == 1) {
    model <- with(
      data,
      lm(y ~ x)
    )
  } else if (deg > 1) {
    model <- with(
      data,
      lm(y ~ poly(x,deg,raw = TRUE))
    )
  }
  return(model)
}

pmt.plotModel <- function(model = m, col = "black", conf = T, lev = 0.95, lwd = 2, lty = 1, add = T, elev = 0, extrapol = T, dashed = T, highlight = F, colhl = "#ffffff99"){
  
  # create empty plot
  if (add == F) {
    pmt.empty()  
  }
  
  # prepare model for plotting
  if (extrapol == T) {
    wx <- par("usr")[1:2] # get the x axis extent 
  } else {
    if (names(model$model)[2] == "x") { # if its x, the model is linear, else, the model is presumed to be polynomial
      wx <- c(min(model$model$x), max(model$model$x)) # read x-values from linear model
    } else {
      wx <- c(min(model$model[[2]][,1]), max(model$model[[2]][,1])) # read x-values from polynomial model
    }
  }
  new.x <- seq(wx[1], wx[2], length.out = length(model$fitted.values))
  pred <- predict(model, new = data.frame(x = new.x), interval = "conf", level = lev)
  
  # plot buffer to highlight the model
  if (isTRUE(highlight) == T) {
    lines(new.x,pred[,"fit"]+elev,lwd=lwd*3,col=colhl)
  }
  
  # plot the model
  lines( new.x, pred[,"fit"]+elev, lwd=lwd, col=col, lty = lty )
  
  # plot the model on top as dashed black line (useful, when a light, hardly visible color is used)
  if( dashed == T ) { 
    lines( new.x, pred[,"fit"]+elev, lwd=lwd, col="black", lty = 3 )
  }

  # plot confidence interval
  if (conf == T) {
    lines(new.x,pred[,"lwr"]+elev,lty=3, col=col)
    lines(new.x,pred[,"upr"]+elev,lty=3, col=col)
  }
  
  # prompt some stuff
  cat("> The model has been plotted. \n> Coefficients: ")
  cat("", paste0(model$coefficients, collapse = ", "), "\n")
  cat("> r^2: ",round(summary(model)$r.squared,2))
}
