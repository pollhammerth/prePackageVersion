


pmt.drawSlope<-function(mod = m,locate=TRUE,x1=NA,y1=NA,x2=NA,y2=NA,x3=NA,y3=NA,lwd=1,ann=TRUE,tcex=1){
  # get points from plot (3 clicks)
  if (locate==TRUE){ 
    xy<-data.frame(locator())
    x1<-as.numeric(round(xy[1,1],digits=1))
    y1<-as.numeric(round(xy[1,2],digits=1))
    x2<-as.numeric(round(xy[2,1],digits=1))
    y2<-as.numeric(round(xy[2,2],digits=1))
    x3<-as.numeric(round(xy[3,1],digits=1))
    y3<-as.numeric(round(xy[2,2],digits=1))
  } else {}
  
  # draw an arrow
  arrows(x2,y2,x1,y1, lwd=lwd, angle = 20)
  segments(x2,y2,x3,y3, lwd=lwd)

  # get the slope value and annotate it
  if (ann==TRUE){
    # get coefficients
    a<-data.frame(mod$coefficients);a<-as.vector(a[,1])
    # calculate local slope for appropriate polynomial order
    if (length(a)==2){
      result.model.slope<-a[2]
    } else if (length(a)==3){
      result.model.slope<-a[2] + a[3]*2*x1
    } else if (length(a)==4){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2
    } else if (length(a)==5){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2 + a[5]*4*x1^3
    } else if (length(a)==6){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2 + a[5]*4*x1^3 + a[6]*5*x1^4
    } else if (length(a)==7){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2 + a[5]*4*x1^3 + a[6]*5*x1^4 + a[7]*6*x1^5
    } else if (length(a)==8){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2 + a[5]*4*x1^3 + a[6]*5*x1^4 + a[7]*6*x1^5 + a[8]*7*x1^6
    } else if (length(a)==9){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2 + a[5]*4*x1^3 + a[6]*5*x1^4 + a[7]*6*x1^5 + a[8]*7*x1^6 + a[9]*8*x1^7
    } else if (length(a)==10){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2 + a[5]*4*x1^3 + a[6]*5*x1^4 + a[7]*6*x1^5 + a[8]*7*x1^6 + a[9]*8*x1^7 + a[10]*9*x1^8
    } else if (length(a)==11){
      result.model.slope<-a[2] + a[3]*2*x1 + a[4]*3*x1^2 + a[5]*4*x1^3 + a[6]*5*x1^4 + a[7]*6*x1^5 + a[8]*7*x1^6 + a[9]*8*x1^7 + a[10]*9*x1^8 + a[11]*10*x1^9
    } else {}
    #  save result in permil as character for annotation
    permil <- paste0(round(abs(result.model.slope)*1000,digits = 1)," permil")
    # annotate
    text(x3,y3,permil,pos=if(x3 >= x2){4}else{2},offset=1,cex=tcex*0.84)
  } else {}
  
  # prompt function call to repeat the function
  string<-paste0("m %>% pmt.drawSlope(locate=FALSE,x1=",x1,",y1=",y1,",x2=",x2,",y2=",y2,",x3=",x3,",y3=",y3,",ann=TRUE)")
  cat(string)
}

