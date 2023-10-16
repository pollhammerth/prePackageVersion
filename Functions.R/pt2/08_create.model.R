####################################################################################################################################
#### Funktionen zur Erstellung von Modellen innerhalb eines x-Achsen-Intervalls ####################################################
####################################################################################################################################

#### Content:
# create.model()       # [1]datatype,[2]i(terrace.no),[3]y,[4]order,[5]interval="full","set","use",[6]use.int.no,[7]x,[8]plot
# elevate.model()          # additional argument [9]elevate=0
#### Benoetigt:
# model.intervals
#### Output:
# model, model.axis, model.x.axis.interval, model.intervals[adds.column]


create.model<-function(data="td",y="y",order=3,interval="full",use.int.no=1,x="x",plot=FALSE,col="black",conf=FALSE,save=FALSE){
# model_intervals.csv einlesen (Template muss vorhanden sein in r.data. Dient zum speichern der Intervalle und wird ?berschrieben!)
  model.intervals<<-read.csv("r.data/model.intervals.csv")
  
  eval(parse(text = paste0(data ," <- as.data.frame(", data, ")")))  # this line has been added to allow compatibilty with S4 objects!
  
  yn<-paste0(data)
  xn<-paste0(data)
  yyn<-paste0(data,'$',y)
  xxn<-paste0(data,'$',x)
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
    # Neue Intervalle abspeichern als csv
    write.csv(model.intervals,"r.data/model.intervals.csv")
  } else if (interval=="full"){
    # Gesamten Datenrange ermitteln
    temp<-paste0('c(min(',xn,')-1,max(',xn,')+1)')
    model.x.axis.interval<<-eval(parse(text=temp))
  } else if (interval=="use"){
    # Vorhandenes Intervall auslesen
    temp<-paste0("model.x.axis.interval<<-model.intervals$",xn,".interval.linear.",use.int.no)
    eval(parse(text=temp))
  } else {}
  
  # Lineares Modell berechnen
  model<<-lm(yv[xv > min(model.x.axis.interval) & xv < max(model.x.axis.interval)] ~ 
               poly(xv[xv > min(model.x.axis.interval) & xv < max(model.x.axis.interval)],order,raw=TRUE))

  # Benanntes Modell abspeichern
  if (save==TRUE){
    temp<-paste0(yn,'.poly.',order,'.model<<-model');eval(parse(text=temp))
  }
  
  # Name der Daten-x-Achsenwerte speichern. Wird von plot.recent.model() benoetigt.  
  model.x.axis<<-c(paste0(data,'$',x))
  
  if (plot==TRUE){
    plot.recent.model(col=col,conf=conf)
  } else {}
}



# Create a model, that is elevated. Additional argument: elevate=0.
elevate.model <- function(data="td",y="median",order=3,interval="full",use.int.no=1,x="x",plot=TRUE,col="black",conf=FALSE,save=FALSE,elevate=0) {

    eval(parse(text = paste0(data ," <- as.data.frame(", data, ")")))  # this line has been added to allow compatibilty with S4 objects!
    name<-paste0(data,'$',y)
    temp<-paste0(name,' <- ',name,' + ',elevate); eval(parse(text=temp)) # Wert addieren
    
    
    
    # model_intervals.csv einlesen (Template muss vorhanden sein in r.data. Dient zum speichern der Intervalle und wird ?berschrieben!)
    model.intervals<<-read.csv("r.data/model.intervals.csv")
    
    yn<-paste0(data)
    xn<-paste0(data)
    yyn<-paste0(data,'$',y)
    xxn<-paste0(data,'$',x)
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
      # Neue Intervalle abspeichern als csv
      write.csv(model.intervals,"r.data/model.intervals.csv")
    } else if (interval=="full"){
      # Gesamten Datenrange ermitteln
      temp<-paste0('c(min(',xn,')-1,max(',xn,')+1)')
      model.x.axis.interval<<-eval(parse(text=temp))
    } else if (interval=="use"){
      # Vorhandenes Intervall auslesen
      temp<-paste0("model.x.axis.interval<<-model.intervals$",xn,".interval.linear.",use.int.no)
      eval(parse(text=temp))
    } else {}
    
    # Lineares Modell berechnen
    model<<-lm(yv[xv > min(model.x.axis.interval) & xv < max(model.x.axis.interval)] ~ 
                 poly(xv[xv > min(model.x.axis.interval) & xv < max(model.x.axis.interval)],order,raw=TRUE))
    
    # Benanntes Modell abspeichern
    if (save==TRUE){
      temp<-paste0(yn,'.poly.',order,'.model<<-model');eval(parse(text=temp))
    }
    
    # Name der Daten-x-Achsenwerte speichern. Wird von plot.recent.model() benoetigt.  
    model.x.axis<<-c(paste0(data,'$',x))
    
    if (plot==TRUE){
      plot.recent.model(col=col,conf=conf)
    } else {}
}




