####################################################################################################################################
#### Funktion zur interaktiven Gefaelleberechnung in anzugebendem x-Achsenintervall #################################################
####################################################################################################################################

#### Content:
# quick.slope("m200",1,"median")     # [1]<-datatype,[2]<-i(terrace.no),[3]<-y,[4]<-x
#### Benoetigt:
# terrace
#### Output:
#



quick.slope<-function(datatype=td,i=1,y="y",x="x",clipboard.windows=FALSE){
  print('quick.slope!')
  
  y<-paste0(datatype,'_',i,'.',terrace[i],'$',y)
  print('Zu analysierende Daten:')
  print(y)
  y<-eval(parse(text=y))
  x<-paste0(datatype,'_',i,'.',terrace[i],'$',x)
  x<-eval(parse(text=x))
  print("Bitte x-Achsenintervall fuer Gefaellsberechnung in Graphik festlegen (Bestaetigen mit ESC)")
  b<-data.frame(locator())
  string=paste0(as.vector(b[1]),collapse=', ');print(string)
  if (clipboard.windows==TRUE){
    writeClipboard(print(string))
  } else {}
  print('Danke.')
  
  model<-lm(y[x > min(b[1]) & x < max(b[1])] ~ x[x > min(b[1]) & x < max(b[1])])
  print('Ergebnis:')
  model$coefficients
}





