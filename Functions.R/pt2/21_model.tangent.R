#### Tangente an Modell legen und Koeffizienten ausgeben


model.tangent<-function(model="model",x=NA,col="black",dir="lr"){
  x.value<-x
  s<<-model.slope(x=x.value)
  y<-model.predict(x=x.value)
  i<<-x*s*(-1)+y
  
  if (dir=="lr"){
  abline(i,s,lty=3,col=col)
  } else if (dir=="r"){
    x2<-500000
    segments(x.value, i+s*x.value, x2, i+s*x2, col=col, lty=3)
  } else if (dir=="l"){
    x2<-0
    segments(x.value, i+s*x.value, x2, i+s*x2, col=col, lty=3)
  } else {}

  print("Tangent coefficients:")
  print(i)
  print(s)
}

