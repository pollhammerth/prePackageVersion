#### pmt.modelToLine
# will convert a model to a line object, that can be used as input for selecting pixels close to the model in the profile view


pmt.modelToLine <- function(model = m, extrapol = F, output = "S4"){
  
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
  pred <- predict(model, new = data.frame(x = new.x), interval = "conf", level = 0.95)
  

  # plot the model
  # lines( new.x, pred[,"fit"] )
  

  # create line object
  line.df <- data.frame( x = new.x, y = pred[,"fit"] )
  # execute some conversions, to create a spatial lines object
  l <- as.matrix(line.df) %>% # convert to matrix
    st_linestring() %>% # convert matrix to linestring
    st_sfc() %>% # convert linestring to simple feature collection
    as_Spatial() # convert simple feature collection to Spatial Lines Data Frame
  
  # return function output
  if (output == "S4") {
    return(l)
  } else if (output == "df") {
    return(line.df)
  }
  
  # prompt some stuff
  cat("> The model has been converted to a line. \n> Coefficients: ")
  cat("", paste0(model$coefficients, collapse = ", "), "\n")
  cat("> r^2: ",round(summary(model)$r.squared,2))
}

