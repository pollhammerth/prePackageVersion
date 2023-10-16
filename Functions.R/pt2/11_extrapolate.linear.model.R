####################################################################################################################################
#### Lineares Modell extrapolieren #################################################################################################
####################################################################################################################################

#### Content:
# extrapolate.linear.model()     # [1]<-col,[2]<-lwd,[3]<-lty
#### Benoetigt:
# model (type: linear)

extrapolate.linear.model<-function(col="red",lwd=1,lty=3,mod="model"){
  temp<-paste0('abline(',mod,'$coefficients[1],',mod,'$coefficients[2], col=col, lwd=lwd, lty=lty )');eval(parse(text=temp))
}
