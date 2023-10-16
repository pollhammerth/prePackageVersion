####################################################################################################################################
#### Export der aktuellen Graphik als PNG. (Laengenangaben in R std-maessig in inch.) ##############################################
####################################################################################################################################

#### Content:
# export.png()              # [1]<-file.name,[2]<-breite,[3]<-hoehe,[4]<-dev.off
#### Benoetigt:
# wd
#### Output:
# *.png Datei



export.png<-function(file.name="plot.png",breite=56,hoehe=23,dev.off=FALSE,res=500){
  wd<-paste0(getwd(),"/")
#  breite<-32
#  hoehe<-16
  dev2bitmap(file = paste0(wd,file.name),width=breite,height=hoehe,units="in",type="png16m",res=res,method="pdf")
  
  if (dev.off==TRUE){
    dev.off()
  } else {}
}