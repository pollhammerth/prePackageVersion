####################################################################################################################################
#### Zeichnen von Pfeilen ##########################################################################################################
####################################################################################################################################

#### Content:
# draw.arrow()       # 2 clicks = gerade, 3 clicks = abgewinkelt [1]text,[2]locate,[3:8]xy,[9]lwd,[10]annotate
# draw.slope().      # Wie draw.arrow(), nur [1]mod=model
# draw.spacing()     # Doppelpfeil parallel y-Achse. Erster click definiert x-Position.[1]locate,[2:7]xy,[8]lwd,[9]annotate
#### Benoetigt:
# require(shape)




draw.arrow<-function(text="",locate=TRUE,x1=NA,y1=NA,x2=NA,y2=NA,x3=NA,y3=NA,lwd=1,ann=TRUE,clwin=FALSE,cex=2){
  if (locate==TRUE){ 
    xy<-data.frame(locator())
    x1<-as.numeric(round(xy[1,1],digits=1))
    y1<-as.numeric(round(xy[1,2],digits=1))
    x2<-as.numeric(round(xy[2,1],digits=1))
    y2<-as.numeric(round(xy[2,2],digits=1))
    x3<-as.numeric(round(xy[3,1],digits=1))
    y3<-as.numeric(round(xy[2,2],digits=1))
  } else {}
  arrows(x2,y2,x1,y1, lwd=lwd)
  segments(x2,y2,x3,y3, lwd=lwd)
  xy.vector<-c(x1,y1,x2,y2,x3,y3)
  string=paste0(xy.vector,collapse=", ");print(paste0(",locate=FALSE,",string))
  if (clwin==TRUE){
  writeClipboard(print(paste0(",locate=FALSE,",string)))
  } else {}
  if (ann==TRUE){
    text(x3,y3,text,pos=4,offset=1,cex=cex)
  } else {}
}


draw.spacing<-function(locate=TRUE,x1=NA,y1=NA,x2=NA,y2=NA,x3=NA,y3=NA,lwd=1,measure=TRUE,clwin=FALSE,cex=2){
  if (locate==TRUE){ 
    xy<-data.frame(locator())
    x1<-as.numeric(round(xy[1,1],digits=1))
    y1<-as.numeric(round(xy[1,2],digits=1))
    x2<-as.numeric(round(xy[1,1],digits=1))
    y2<-as.numeric(round(xy[2,2],digits=1))
    x3<-as.numeric(round(xy[1,1],digits=1))
    y3<-as.numeric(round(xy[3,2],digits=1))
  } else {}
  arrows(x1,y1,x2,y2, lwd=lwd)
  arrows(x2,y2,x1,y1, lwd=lwd)
  segments(x2,y2,x3,y3, lwd=lwd)
  xy.vector<-c(x1,y1,x2,y2,x3,y3)
  string=paste0(xy.vector,collapse=", ");print(paste0("locate=FALSE,",string))
  if (clwin==TRUE){
  writeClipboard(print(paste0("locate=FALSE,",string)))
  } else {}
  if (measure==TRUE){
    y.spacing<-c(y1,y2)
    text<-as.character(round(max(y.spacing) - min(y.spacing),digits=1))
    text(x3,y3,text,pos=4,offset=1,cex=cex)
  } else {}
}


draw.slope<-function(mod="m",locate=TRUE,x1=NA,y1=NA,x2=NA,y2=NA,x3=NA,y3=NA,lwd=1,ann=TRUE,clwin=FALSE,tcex=1){
  if (locate==TRUE){ 
    xy<-data.frame(locator())
    x1<-as.numeric(round(xy[1,1],digits=1))
    y1<-as.numeric(round(xy[1,2],digits=1))
    x2<-as.numeric(round(xy[2,1],digits=1))
    y2<-as.numeric(round(xy[2,2],digits=1))
    x3<-as.numeric(round(xy[3,1],digits=1))
    y3<-as.numeric(round(xy[2,2],digits=1))
  } else {}
  arrows(x2,y2,x1,y1, lwd=lwd)
  segments(x2,y2,x3,y3, lwd=lwd)
#  xy.vector<-c(x1,y1,x2,y2,x3,y3)
#  string=paste0(xy.vector,collapse=", ");print(paste0(",locate=FALSE,",string))
  if (clwin==TRUE){
  writeClipboard(print(paste0(",locate=FALSE,",string)))
  } else {}
  if (ann==TRUE){
  	temp<-paste0("model.slope(",mod,",x=x1)");text<-eval(parse(text=temp))
    text(x3,y3,text,pos=4,offset=1,cex=tcex)
  } else {}
  
  string<-paste0("draw.slope(mod='",mod,"',locate=FALSE,x1=",x1,",y1=",y1,",x2=",x2,",y2=",y2,",x3=",x3,",y3=",y3,",ann=TRUE,clwin=FALSE)")
  print(string)
}
