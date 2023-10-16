####################################################################################################################################
#### y Werte aus model berechnen ###################################################################################################
####################################################################################################################################

#### Content:
# predict.model()
#### Benoetigt:
# model
#### Output:
# Wert fuer y


model.predict<-function(use.model=model,x=0,clipboard.windows=FALSE){
  a<-data.frame(use.model$coefficients);a<-as.vector(a[,1])
  if (length(a)==2){
    result.predict.model<<-a[1] + a[2]*x
  } else if (length(a)==3){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2
  } else if (length(a)==4){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3
  } else if (length(a)==5){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3 + a[5]*x^4
  } else if (length(a)==6){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3 + a[5]*x^4 + a[6]*x^5
  } else if (length(a)==7){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3 + a[5]*x^4 + a[6]*x^5 + a[7]*x^6
  } else if (length(a)==8){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3 + a[5]*x^4 + a[6]*x^5 + a[7]*x^6 + a[8]*x^7
  } else if (length(a)==9){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3 + a[5]*x^4 + a[6]*x^5 + a[7]*x^6 + a[8]*x^7 + a[9]*x^8
  } else if (length(a)==10){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3 + a[5]*x^4 + a[6]*x^5 + a[7]*x^6 + a[8]*x^7 + a[9]*x^8 + a[10]*x^9
  } else if (length(a)==11){
    result.predict.model<<-a[1] + a[2]*x + a[3]*x^2 + a[4]*x^3 + a[5]*x^4 + a[6]*x^5 + a[7]*x^6 + a[8]*x^7 + a[9]*x^8 + a[10]*x^9 + a[11]*x^10
  } else {}
  

  
  if (clipboard.windows==TRUE){
  writeClipboard(as.character(result.predict.model))
  } else {}

  print(result.predict.model)
  
}
