



model.slope<-function(use.model=m,x=0,clipboard.windows=FALSE){
  a<-data.frame(use.model$coefficients);a<-as.vector(a[,1])
  if (length(a)==2){
    result.model.slope<-a[2]
  } else if (length(a)==3){
    result.model.slope<-a[2] + a[3]*2*x
  } else if (length(a)==4){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2
  } else if (length(a)==5){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2 + a[5]*4*x^3
  } else if (length(a)==6){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2 + a[5]*4*x^3 + a[6]*5*x^4
  } else if (length(a)==7){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2 + a[5]*4*x^3 + a[6]*5*x^4 + a[7]*6*x^5
  } else if (length(a)==8){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2 + a[5]*4*x^3 + a[6]*5*x^4 + a[7]*6*x^5 + a[8]*7*x^6
  } else if (length(a)==9){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2 + a[5]*4*x^3 + a[6]*5*x^4 + a[7]*6*x^5 + a[8]*7*x^6 + a[9]*8*x^7
  } else if (length(a)==10){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2 + a[5]*4*x^3 + a[6]*5*x^4 + a[7]*6*x^5 + a[8]*7*x^6 + a[9]*8*x^7 + a[10]*9*x^8
  } else if (length(a)==11){
    result.model.slope<-a[2] + a[3]*2*x + a[4]*3*x^2 + a[5]*4*x^3 + a[6]*5*x^4 + a[7]*6*x^5 + a[8]*7*x^6 + a[9]*8*x^7 + a[10]*9*x^8 + a[11]*10*x^9
  } else {}
  
  
  
  if (clipboard.windows==TRUE){
    writeClipboard(as.character(result.model.slope))
  } else {}
  
#  return(result.model.slope)
  permil <- paste0(round(abs(result.model.slope)*1000,digits = 1)," Promille")
  return(permil)
    
}
