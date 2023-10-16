#### Einzelne Tabelle aus ArcGIS aufsplitten nach Stratigraphischer Information in anzugebender Spalte
#### Die Daten werden in einzelne csv Dateien in xyz.data abgelegt und koennen regulaer via import.td eingelesen werden



separate.csv<-function(path="xyz.data/original/",input="table.csv",column="name",fullDEM=TRUE,prefix="",out.path="xyz.data/"){

  input.short<-gsub(".csv","",input)
  
  temp<-paste0('imported.data<-read.table("',path,input,'")')
  eval(parse(text=temp))

  # Tabelle mit allen DEM Punkten speichern  
  if (fullDEM==TRUE){
    temp<-paste0('write.table(imported.data,"',out.path,'R_full_DEM.csv")')
    eval(parse(text=temp))
  } else {}
  
#  # RID zu character wandeln
#  RID<-as.character(imported.data$RID)
#  imported.data$RID<-NULL
#  imported.data<-data.frame(imported.data,RID)
  
  # Zu trennende Levels auslesen und in strat speichern
  temp<-paste0('strat<-unique(imported.data$',column,')')
  eval(parse(text=temp))
  strat<-strat[!is.na(strat)]
  strat<-strat[strat!=" "]
  strat<-strat[strat!=""]
  
  # Getrennte Tabelle je Level speichern  
  for (i in 1:length(strat)){
    
#    temp<-paste0('data_',strat[i],'<-imported.data[imported.data$',column,'=="',strat[i],'",]')
#    eval(parse(text=temp))

    temp<-paste0('data_',strat[i],'<-subset(imported.data, imported.data$',column,' == "',strat[i],'")')
    eval(parse(text=temp))
    
    temp<-paste0('write.table(data_',strat[i],',"',out.path,prefix,'_',column,'___',strat[i],'.csv",)')
    eval(parse(text=temp))

  }
}





