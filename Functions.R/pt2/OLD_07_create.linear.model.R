####################################################################################################################################
#### Funktion zur Erstellung eines linearen Modells ################################################################################
####################################################################################################################################

#### Content:
# create.linear.model()       # [1]<-datatype,[2]<-i(terrace.no),[3]<-y,[4]<-interval,[5]<-use.int.no,[6]<-x,[7]<-plot=FALSE,TRUE
#### Benoetigt:
# model.intervals
#### Output:
# model, model.x.axis, model.x.axis.interval


create.linear.model<-function(datatype=td,i=1,y="y",use.int.no=1,interval="full",x="x",plot=FALSE,col="black",conf=FALSE,save=FALSE){
  # model_intervals.csv einlesen (Template muss vorhanden sein in r.data. Dient zum speichern der Intervalle und wird ueberschrieben!)
  model.intervals<-read.csv("r.data/model_intervals.csv")
  
  yn<-paste0(datatype,'_',i,'.',terrace[i])
  xn<-paste0(datatype,'_',i,'.',terrace[i])
  yyn<-paste0(datatype,'_',i,'.',terrace[i],'$',y)
  xxn<-paste0(datatype,'_',i,'.',terrace[i],'$',x)
  print('Zu analysierende Daten:'); print(yyn)
  yv<-eval(parse(text=yyn))
  xv<-eval(parse(text=xxn))
  if (interval=="set"){
    # Ggf. vorhandenen gleichnamigen alten Intervalleintrag in model.intervals loeschen
    temp<-paste0('model.intervals$',xn,'.interval.linear.',use.int.no,'<<-NULL')
    eval(parse(text=temp))
    # Intervall auf Diagramm waehlen und in model.intervals<<-speichern
    axis.interval<-data.frame(locator())
    temp<-paste0(xn,'.interval.linear.',use.int.no,'<-as.vector(t(axis.interval[1]))')
    eval(parse(text=temp))
    temp<-paste0('model.intervals<<-data.frame(model.intervals,',xn,'.interval.linear.',use.int.no,')')
    eval(parse(text=temp))
    # Vorhandenes Intervall auslesen
    temp<-paste0('model.x.axis.interval<<-model.intervals$',xn,'.interval.linear.',use.int.no)
    eval(parse(text=temp))
  } else if (interval=="full"){
    # Gesamten Datenrange ermitteln
    temp<-paste0('c(min(',xn,')-1,max(',xn,')+1)')
    model.x.axis.interval<<-eval(parse(text=temp))
  } else if (interval=="use"){
    # Vorhandenes Intervall auslesen
    temp<-paste0('model.x.axis.interval<<-model.intervals$',xn,'.interval.linear.',use.int.no)
    eval(parse(text=temp))
  } else {}

  # Lineares Modell berechnen (f?r plot.model())
  model<<-lm(yv[xv > min(model.x.axis.interval) & xv < max(model.x.axis.interval)] ~ xv[xv > min(model.x.axis.interval) & xv < max(model.x.axis.interval)])

  # Benanntes Modell abspeichern
  if (save==TRUE){
    temp<-paste0(yn,'.linear.model<<-model');eval(parse(text=temp))
  }
  
  # Name der Daten-x-Achsenwerte speichern. Wird von plot.recent.model() benoetigt.  
  model.x.axis<<-c(paste0(datatype,'_',i,'.',terrace[i],'$',x))
  
  if (plot==TRUE){
    plot.recent.model(col=col,conf=conf)
  } else {}
  
  write.csv(model.intervals,"r.data/model_intervals.csv")
}

