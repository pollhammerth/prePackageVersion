####################################################################################################################################
#### Draw abline interactively by clicking 2 points in plot then press ESC #########################################################
####################################################################################################################################

#### Content:
# quick.abline()       # [1]<-col,[2]<-lwd,[3]<-lty
#### Benoetigt:
#
#### Output:
#




quick.abline<-function(col="black",lwd=1,lty=5){
  print('Bitte 2 Punkte waehlen, die eine Linie aufspannen sollen (Bestaetigen mit ESC):')
  b<-data.frame(locator())
  model<-lm(b$y ~ b$x)
  abline(model, col=col, lwd=lwd, lty=lty)
  
  print('Ergebnis:')
  quick.abline<-model
  lm(formula=quick.abline)
}