####################################################################################################################################
#### Annotation ####################################################################################################################
####################################################################################################################################

#### Content:
# annotation()              # [1]<-text,[2]<-locate=TRUE,[3]<-x,[4]<-y



annotation<-function(text="Enter text as function argument.",locate=TRUE,x=NA,y=NA,pos=4,cex=2,offset=1,clwin=FALSE){
  if (locate==TRUE){
    b<-data.frame(locator())
    x<-as.numeric(round(b[1]))
    y<-as.numeric(round(b[2]))
    string=paste0("x=",round(b[1]),",y=",round(b[2]));print(paste0("annotation(text='",text,"',locate=FALSE,",string,")"))
    if (clwin==TRUE){
      writeClipboard(print(paste0(",locate=FALSE,",string)))
    } else {}
  } else {}
  text(x,y,text,pos=pos,offset=offset,cex=cex)
}


