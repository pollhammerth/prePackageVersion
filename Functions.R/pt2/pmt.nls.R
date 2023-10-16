
#### non-linear parametric regression and kernel regression (non-parametric, requires package "sm") as coded by Sebastian Fuchs
#### implemented in pmt by Thomas Pollhammer
# arguments:
# alpha
# bdata = c(0,1) Factor of data extent
# bextra =c(20000,23000) x range in [m] where the model should be extrapolated and confidence interval is calculated



###
###--- modeling terrace + plot
# nichtlineare parametrische Regression (ueber kompletten Datensatz)
###

pmt.nls <- function(data, intercept = NA) {
  #--- Regression (using data)
  #--- step 1: find intercept
  m1 <- lm(y~poly(x, order = 2), data = data)
  new <- data.frame(x=rep(0,1))
  new$y <- predict(m1, newdata=new)
  #--- step 2: transform data
  B <- data.frame(x=data$x, y=rep(0,nrow(data)))
  B$y <- exp(new$y[1]/data$y)
  #--- step 3: quadratic regression in transformed space
  m2 <- lm(y ~ poly(x, 2, raw=TRUE), data=B)
  #--- step 4: use estimates as start values
  if (is.na(intercept)) { d <- new$y[1] } else {d <- intercept}
  model.nls  <- nls(y ~ d/(log(a*x^2+b*x+c)),
                    data=data, 
                    start=list(a=m2$coefficients[3],
                               b=m2$coefficients[2],
                               c=m2$coefficients[1]))#,weights= data$weight)
  
  # function output
  #--- regression function f(x) = d/(log(a*x^2+b*x+c)
  formu <- model.nls$m$formula()
  #--- coefficients and plot
  coef <- data.frame(a=coef(model.nls)[1],b=coef(model.nls)[2],c=coef(model.nls)[3],d=d); coef
  
  # prompt some stuff
  #--- regression function f(x) = d/(log(a*x^2+b*x+c)
  cat("\nformula:","\ny =", as.character(formu), "\n")
  #--- coefficients
  cat("\ncoefficients:","\na =",coef[[1]], "\nb =", coef[[2]], "\nc =", coef[[3]], "\nd =", coef[[4]])
  
  return(model.nls)
}
#A <- eval(HADS) %>% pmt.bin(interval = 500, value = "median", mode = "bin", idfield = "terrace", cth = NA, sth = NA)
#mnls <- pmt.nls(A)


pmt.plotNls <- function(model = m, col = "black", lwd = 2, lty = 1, add = T, elev = 0, extrapol = T, dashed = T, highlight = F, colhl = "#ffffff99", xlim = NA){
  
  # arguments conf = T, lev = 0.95 need to be implemented

  # create empty plot
  if (add == F) {
    pmt.empty()  
  }
  
  # prepare model for plotting
  if (extrapol == T) {
    wx <- par("usr")[1:2] # get the x axis extent 
  } else if (is.na(xlim[1])) {
    cat("if you do not want to extrapolate over whole plot, provide xlim!") 
  } else {
    wx <- c(min(xlim), max(xlim))
  }

  # predict over new x values
  new.x <- seq(wx[1], wx[2], length.out = 1000)
  pred <- predict(model, newdata = data.frame(x = new.x))#, interval = "conf", level = lev)
  
  # plot buffer to highlight the model
  if (isTRUE(highlight) == T) {
    lines(new.x,pred+elev,lwd=lwd*3,col=colhl)
  }
  
  # plot the model
  lines( new.x, pred+elev, lwd=lwd, col=col, lty = lty )
  
  # plot the model on top as dashed black line (useful, when a light, hardly visible color is used)
  if( dashed == T ) { 
    lines( new.x, pred+elev, lwd=lwd, col="black", lty = 3 )
  }
  
  # prompt some stuff
  #--- regression function f(x) = d/(log(a*x^2+b*x+c)
  cat("\nformula:","\ny =", as.character(model$m$formula())[3], "\n")
  # #--- coefficients
  # coef <- data.frame(a=coef(model)[1],b=coef(model)[2],c=coef(model)[3],d=d)
  # cat("\ncoefficients:","\na =",coef[[1]], "\nb =", coef[[2]], "\nc =", coef[[3]], "\nd =", coef[[4]])
}

#pmt.plotNls(mnls, add = T, extrapol = F, xlim = A$x, col = "blue")






###
###--- modeling confluence via non-linear parametric regression
# Extrapolation mittels nichtlinearer parametrischer Regression
###

pmt.nlsConf <- function(data, bdata = c(0,1), bextra = c(74000,85000), alpha = 0.05) {
#--- bounds 
# lower_vis <- 0.5*(max(data$x)-min(data$x)) + min(data$x)             # lower bound plot
# upper_vis <- confluence[2] + 5000                           # upper bound plot

lower_data <- min(bdata)*(max(data$x)-min(data$x)) + min(data$x)            # lower bound used data
# upper_data <- max(bdata)*(min(bextra)-min(data$x)) + min(data$x)       # upper bound used data
upper_data <- max(bdata)*(max(data$x)-min(data$x)) + min(data$x)       # upper bound used data

# lower_extra <- max(bdata)*(min(bextra)-min(data$x)) + min(data$x)      # lower bound extrapolation
lower_extra <- max(bdata)*(max(data$x)-min(data$x)) + min(data$x)      # lower bound extrapolation
upper_extra <- max(bextra)                                # upper bound extrapolation

R <- 2000   # repetitions
leng <- 100
X <- seq(lower_extra,upper_extra, length=leng)
E <- data.frame(x=rep(X,R), y=rep(NA,leng*R))

#--- non-linear parametric regression, R repetitions, sample size = 1/10*n
for(i in 1:R){
  AA <- data[data$x >= lower_data & data$x <=upper_data,] #extra,] # im Original _extra. Sollte hier nicht _data stehen?
  AA <- AA[sample(1:nrow(AA), nrow(AA)/10, replace = F),]
  #--- step 1: find intercept
  m1 <- lm(y~poly(x, order = 2), data = AA)
  new <- data.frame(x=rep(0,1))
  new$y <- predict(m1, newdata=new)
  #--- step 2: transform data
  B <- data.frame(x=AA$x, y=rep(0,nrow(AA)))
  B$y <- exp(new$y[1]/AA$y)
  #--- step 3: quadratic regression in transformed space
  m2 <- lm(y ~ poly(x, 2, raw=TRUE), data=B)
  #--- step 4: use estimates as start values
  d <- new$y[1]
  try({model.nls  <- nls(y ~ d/(log(a*x^2+b*x+c)),
                         data=AA, 
                         start=list(a=m2$coefficients[3],
                                    b=m2$coefficients[2],
                                    c=m2$coefficients[1]));
  E[c((leng*i-leng+1):(leng*i)),2] <- predict(model.nls, newdata=data.frame(x=X))}, silent=T)
}
#--- missing values possible 
sum(is.na(E))
E <- na.omit(E)

#--- calculate lower and upper quantiles - alpha = 0.05, 1-alpha = 0.95
EE <- data.frame(x=X,lower_q=rep(0,leng),upper_q=rep(0,leng))  
# alpha <- 0.05
for(i in 1:leng){
  EE[i,2] <- as.numeric(quantile(E[E$x==X[i],]$y,probs = c(alpha/2))) 
  EE[i,3] <- as.numeric(quantile(E[E$x==X[i],]$y,probs = c(1-alpha/2))) }

#--- (prepare) plot data
# data_C <- orig.data[orig.data$x >= lower_vis,]
# C <- data[data$x >= lower_vis,]

#--- confidence region
EE[EE$x > min(bextra),]

#--- function output
return(EE)

}

#b <- data %>% pmt.bin(interval = 500, value = "median", mode = "bin", idfield = "terrace", cth = NA, sth = NA)
#conf <- b %>% pmt.nlsConf()


pmt.plotNlsConf <- function(data = conf, border = "black", fill = NA, ...) {
  if (!is.na(fill)) {
    polygon(x=c(data$x,rev(data$x)), y = c(data$lower_q,rev(data$upper_q)), col = fill, border = NA)
  }
  if (!is.na(border)) {
    lines(x = data$x, y = data$upper_q, col = border, ...)
    lines(x = data$x, y = data$lower_q, col = border, ...)
  }
}

#pmt.plotNlsConf(fill = "#aa667766")







###
###--- modeling confluence via kernel regression
# Extrapolation mittels Kernregression (Kernel Regression)
# (Gewichtung der Datenpunkte nach Naehe zu Schaetzpunkt)
###

# library(sm)
# data <- wertach1
# para <- -2.5
# #--- confluence data
# confluence <- wertach1_Conf$confluence    
# elev <- wertach1_Conf$elev
# 
# #--- apply functions b() and sf()
# A <- b(sf(data),digits=para)
# A <- na.omit(A[,1:2])
# n <- nrow(A)



pmt.kernelReg <- function(data, bdata = c(0,1), bextra = c(74000,85000), alpha = 0.05) {

#--- bounds 
#lower_vis <- 0.5*(max(A$x)-min(A$x)) + min(A$x)             # lower bound plot
#upper_vis <- confluence[2] + 5000                           # upper bound plot

  lower_data <- min(bdata)*(max(data$x)-min(data$x)) + min(data$x)            # lower bound used data
  # upper_data <- max(bdata)*(min(bextra)-min(data$x)) + min(data$x)       # upper bound used data
  upper_data <- max(bdata)*(max(data$x)-min(data$x)) + min(data$x)       # upper bound used data
  
  # lower_extra <- max(bdata)*(min(bextra)-min(data$x)) + min(data$x)      # lower bound extrapolation
  lower_extra <- max(bdata)*(max(data$x)-min(data$x)) + min(data$x)      # lower bound extrapolation
  upper_extra <- max(bextra)                                # upper bound extrapolation
  
R <- 2000   # repetitions
leng <- 100
X <- seq(lower_extra,upper_extra, length=leng)
E <- data.frame(x=rep(X,R), y=rep(NA,leng*R))

#--- kernel regression, R repetitions, sample size = 1/10*n
for(i in 1:R){
  AA <- data[data$x >= lower_data & data$x <=upper_extra,] #extra,] # hier stand extra. Sollte hier nicht data stehen?
  AA <- AA[sample(1:nrow(AA), nrow(AA)/10, replace = F),] # hier wird ein Sampledatensatz aus den Daten gezogen, der 10% der Originaldaten umfasst.
  #--- kernel regression
  nreg <- sm.regression(AA$x, AA$y, eval.points=X, display="none") # bandwith = nreg$h; choose bandwith h= # nicht-parametrische Regression
  E[c((leng*i-leng+1):(leng*i)),2] <- nreg$estimate
}
#--- missing values possible - sm.regression may not provide values due to extrapolation
sum(is.na(E))
E <- na.omit(E)

#--- calculate lower and upper quantiles - alpha/2 = 0.025, 1-alpha = 0.975
EE <- data.frame(x=X,lower_q=rep(0,leng),upper_q=rep(0,leng))  
# alpha <- 0.05 # wird in function (pmt.kernelReg) als Argument gegeben
for(i in 1:leng){
  EE[i,2] <- as.numeric(quantile(E[E$x==X[i],]$y,probs = c(alpha/2))) 
  EE[i,3] <- as.numeric(quantile(E[E$x==X[i],]$y,probs = c(1-alpha/2))) }

#--- (prepare) plot
#data_C <- data[data$x >= lower_vis,]
#C <- A[A$x >= lower_vis,]

#--- confidence region
EE[EE$x > min(bextra),]

#--- function output
return(EE)

}








