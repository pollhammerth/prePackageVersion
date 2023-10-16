#### calculate.m function
# replaces calculate.mmmm function
# does the same as the old function, but leaves out mean when calling:
# calculate.m(interval = 1000)
# the function can be used for single dataframes and
# it can output the result dataframe directly, without saving it
# if input = "all", saving is automatically activated

# requires:
# terrace 
# td

# creates:
# td.m - but is is not stored to global variable space!
# if compatibility issues arise, maybe activate storing of td.m


calculate.m <- function(input = "all", interval = 1000, save = FALSE) {
    
  if (input == "all") {
    save <- TRUE
    for (i in 1:length(td)) {
      input <- td[i]
      
      
      # get data
      expr <- paste0("df <- ", noquote(input)); eval(parse(text = expr))
      # convert data to data.frame, if it is a S4 object
      if (mode(df) == "S4") {
        df <- as.data.frame(df); df$x.1 <- NULL; df$y.1 <- NULL
      } else {}
      # calculate no of intervals
      interval.count <- max(x.limit)/interval
      
      # calculate vector of median values per interval
      median <- median(subset(x = df, 
                              subset = x < interval, 
                              select = y)[,1])
      for (i in 2:ceiling(interval.count)) {
        median <- c(median, median(subset(x = subset(x = df, subset = x < interval * i), 
                                          subset = x > interval * (i - 1), 
                                          select = y)[,1])
        )
      }
      # calcualte vector of max values per interval
      max <- max(subset(x = df, 
                        subset = x < interval, 
                        select = y)[,1])
      for (i in 2:ceiling(interval.count)) {
        max <- c(max, max(subset(x = subset(x = df, subset = x < interval * i), 
                                 subset = x > interval * (i - 1), 
                                 select = y)[,1])
        )
      }
      # calculate vector of min values per interval
      min <- min(subset(x = df, 
                        subset = x < interval, 
                        select = y)[,1])
      for (i in 2:ceiling(interval.count)) {
        min <- c(min, min(subset(x = subset(x = df, subset = x < interval * i), 
                                 subset = x > interval * (i - 1), 
                                 select = y)[,1])
        )
      }
      # calculate vector of centered x values per inteval
      x <- interval/2
      for (i in 2:ceiling(interval.count)) {
        x <- c(x, interval * i - interval/2)
      }
      
      
      # save output to df if save = TRUE
      td.m <<- NULL
      for (i in 1:length(td)) {
        td.m[i] <<- paste0('m',interval,'_',td[i])
        #eval(parse(text = paste0(td.m[i], ' <- m',interval,'_',td[i], ')')))
      }
       if (save == TRUE) {
         expr <- paste0("m",interval, "_", gsub(".*td_","",input), " <<- subset(x = data.frame(x, median, min, max), 
                   subset = median > -1000)"); eval(parse(text = expr))
       } else {}
    }} else {
      
      
      #### about the same procedure as above, but for only one given dataframe
      # get data
      expr <- paste0("df <- ", noquote(input)); eval(parse(text = expr))
      # convert data to data.frame, if it is a S4 object
      if (mode(df) == "S4") {
        df <- as.data.frame(df); df$x.1 <- NULL; df$y.1 <- NULL
      } else {}
      # calculate no of intervals
      interval.count <- max(x.limit)/interval
      
      # calculate vector of median values per interval
      median <- median(subset(x = df, 
                              subset = x < interval, 
                              select = y)[,1])
      for (i in 2:ceiling(interval.count)) {
        median <- c(median, median(subset(x = subset(x = df, subset = x < interval * i), 
                                          subset = x > interval * (i - 1), 
                                          select = y)[,1])
        )
      }
      # calcualte vector of max values per interval
      max <- max(subset(x = df, 
                        subset = x < interval, 
                        select = y)[,1])
      for (i in 2:ceiling(interval.count)) {
        max <- c(max, max(subset(x = subset(x = df, subset = x < interval * i), 
                                 subset = x > interval * (i - 1), 
                                 select = y)[,1])
        )
      }
      # calculate vector of min values per interval
      min <- min(subset(x = df, 
                        subset = x < interval, 
                        select = y)[,1])
      for (i in 2:ceiling(interval.count)) {
        min <- c(min, min(subset(x = subset(x = df, subset = x < interval * i), 
                                 subset = x > interval * (i - 1), 
                                 select = y)[,1])
        )
      }
      # calculate vector of centered x values per inteval
      x <- interval/2
      for (i in 2:ceiling(interval.count)) {
        x <- c(x, interval * i - interval/2)
      }
      
      
      
      # save output to df if save = TRUE
      if (save == TRUE) {
        expr <- paste0("m",interval, "_", input, " <<- subset(x = data.frame(x, median, min, max), 
                  subset = median > -1000)"); eval(parse(text = expr))
      } else if (save == FALSE) {
        # create output data.frame, if save != TRUE
        subset(x = data.frame(x, median, min, max), 
               subset = median > -1000) # the > - 1000 is to get rid of larger no data areas (e.g. outside of raster extent)
      } else {}
    } 
  
  
}


