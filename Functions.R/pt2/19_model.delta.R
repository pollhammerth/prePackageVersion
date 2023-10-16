####################################################################################################################################
#### y Werte aus model berechnen ###################################################################################################
####################################################################################################################################

#### Content:
# model.delta()
#### Benoetigt:
# model
#### Output:
# Wert fuer y

model.delta<-function(model1=NA,model2=NA,x=0,clipboard.windows=FALSE){
  print("Funktionswerte von Modell 1 und 2:")
  result.model.delta<<-model.predict(model1,x,clip=FALSE)-model.predict(model2,x,clip=FALSE)
  print("Differenz der y-Werte an Position x:")
  if (clipboard.windows==TRUE){
  print("(Ergebnis wurde in Zwischenablage kopiert.)")
  writeClipboard(as.character(result.model.delta))
  } else {}
  result.model.delta
#  print(result.model.delta)
}

