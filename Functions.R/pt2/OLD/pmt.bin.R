####
# pmt.bin() replaces calculate.m()
# requires pmt.id() if the option bid = "id" is used (takes "bin" or "id")
# creates representative y values for x bins or polygons
# available value options are "median", "mean", "min" or "max".
# usage with other functions e.g. pmt.model(pmt.bin(eval(tds)))

# Dependencies:
# pmt::pmt.extent()



pmt.bin <- function(data, interval = 500, value = "median", mode = "bin", cth = NA, sth = NA){

  
  #### calculate statistics for x-axis bins
  if (mode == "bin") {
    
  # calculate no of intervals
  pmt.extent(data)
  interval.count <- max(extent[["xlim"]])/interval
  
  # Calculate amount of pixels per bin and the standard deviation
  c <- length(data[which(data[["x"]] < interval),][["y"]])
  for (i in 2:ceiling(interval.count)) {
    c <- c(c, length(data[which(data[["x"]] < interval*i & data[["x"]] >= interval*(i-1)),][["y"]]))
  }
  s <- round(sd(data[which(data[["x"]] < interval),][["y"]]),1)
  for (i in 2:ceiling(interval.count)) {
    s <- c(s, round(sd(data[which(data[["x"]] < interval*i & data[["x"]] >= interval*(i-1)),][["y"]]),1))
  }
  
  # calculate y values for each x-axis bin
  if (value == "median") {
    y <- median(data[which(data[["x"]] < interval),][["y"]])
    for (i in 2:ceiling(interval.count)) {
      y <- c(y, median(data[which(data[["x"]] < interval*i & data[["x"]] >= interval*(i-1)),][["y"]]))
    }
  } else if (value == "min") {
    y <- min(data[which(data[["x"]] < interval),][["y"]])
    for (i in 2:ceiling(interval.count)) {
      y <- c(y, min(data[which(data[["x"]] < interval*i & data[["x"]] >= interval*(i-1)),][["y"]]))
    }
  } else if (value == "max") {
    y <- max(data[which(data[["x"]] < interval),][["y"]])
    for (i in 2:ceiling(interval.count)) {
      y <- c(y, max(data[which(data[["x"]] < interval*i & data[["x"]] >= interval*(i-1)),][["y"]]))
    }
  } else if (value == "mean") {
    y <- mean(data[which(data[["x"]] < interval),][["y"]])
    for (i in 2:ceiling(interval.count)) {
      y <- c(y, mean(data[which(data[["x"]] < interval*i & data[["x"]] >= interval*(i-1)),][["y"]]))
    }
  } else {"> The specified value is not valid. Please choose between: 'median', 'mean', 'min' or 'max'."}
  
  # calculate bin-centered x values
  x <- interval/2
  for (i in 2:ceiling(interval.count)) {
    x <- c(x, interval * i - interval/2)
  }
  
  
  
  #### calculate statistics for polygon ids
  } else if (mode != "bin") {
    
    # Calculate amount of pixels per polygon and the standard deviation
    c <- with(data[which(data[[mode]] == pmt.id(data)[i]),],length(y))
    for (i in 2:length(pmt.id(data))) {
      c <- c(c,with(data[which(data[[mode]] == pmt.id(data)[i]),], length(y)))
    }
    s <- with(data[which(data[[mode]] == pmt.id(data)[i]),],round(sd(y),1))
    for (i in 2:length(pmt.id(data))) {
      s <- c(s,with(data[which(data[[mode]] == pmt.id(data)[i]),], round(sd(y),1)))
    }
    id <- with(data[which(data[[mode]] == pmt.id(data)[i]),], pmt.id(data)[i])
    for (i in 2:length(pmt.id(data))) {
      id <- c(id,with(data[which(data[[mode]] == pmt.id(data)[i]),], pmt.id(data)[i]))
    }
    
    
    # calculate y values for each polygon
    if (value == "median") {
    y <- with(data[which(data[[mode]] == pmt.id(data)[i]),],median(y))
    for (i in 2:length(pmt.id(data))) {
      y <- c(y,with(data[which(data[[mode]] == pmt.id(data)[i]),], median(y)))
    }
    } else if (value == "min") {
      y <- with(data[which(data[[mode]] == pmt.id(data)[i]),],min(y))
      for (i in 2:length(pmt.id(data))) {
        y <- c(y,with(data[which(data[[mode]] == pmt.id(data)[i]),], min(y)))
      }
    } else if (value == "max") {
      y <- with(data[which(data[[mode]] == pmt.id(data)[i]),],max(y))
      for (i in 2:length(pmt.id(data))) {
        y <- c(y,with(data[which(data[[mode]] == pmt.id(data)[i]),], max(y)))
      }
    } else if (value == "mean") {
      y <- with(data[which(data[[mode]] == pmt.id(data)[i]),],mean(y))
      for (i in 2:length(pmt.id(data))) {
        y <- c(y,with(data[which(data[[mode]] == pmt.id(data)[i]),], mean(y)))
      }
    }
    
    # calculate polygon-centered x values
    x <- with(data[which(data[[mode]] == pmt.id(data)[i]),],(max(x)-min(x))/2+min(x))
    for (i in 2:length(pmt.id(data))) {
      x <- c(x,with(data[which(data[[mode]] == pmt.id(data)[i]),],(max(x)-min(x))/2+min(x)))
    }
    
  } else {"> No valid value was supplied for mode. Please choose \"bin\" or \"name of polygon id column\""}
    
  
  
  
  # result
  r <- data.frame(x = x,y = y,c = c,s = s,id = if(mode != "bin"){id}else{x/interval*2})[order(x),]
  
  # threshold of min value count per bin or polygon
  if (!is.na(cth) == TRUE) {
    r <- r[r$c > cth,]
  }
  # threshold of max standard deviation per bin or polygon
  if (!is.na(sth) == TRUE) {
    r <- r[r$s < sth,]
  }
  
    
  # return data (values lower than -1000 may be removed here, to get rid of eventual no data values.)
  r <- r[is.na(r[["y"]]) == FALSE,]
#  r <- r[which(r[["y"]] > -1000),]
  
  r
  
}



