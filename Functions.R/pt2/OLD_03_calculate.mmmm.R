####################################################################################################################################
#### Statistische Berechnungen ausfuehren und Ergebnisse speichern in neuem Datenframe je td[i] #####################################
####################################################################################################################################

#### Content:
# calculate.mmmm()                                                  # Min, Median, Mean, Max je [1]<-x-Achsenintervall (Std=200)
#### Benoetigt:
# td, terrace
#### Output:
# mxxx_* Dataframes, # td.m


calculate.mmmm<-function(x.interval.size=500){
  #### Berechnung der Anzahl an x-Achsenintervallen und Intervallzuordnung der Werte in td_* Dataframes ####
  ##########################################################################################################
  x.interval.count<-ceiling(x.limit[2]/x.interval.size)
  for (i in 1:length(td)){
    for (j in 1:x.interval.count){
      temp<-paste0(td[i],"$x.interval.no[",
                              paste0(td[i],"$x<x.interval.size*j & ", 
                                     paste0(td[i],"$x>x.interval.size*(j-1)")),"]<<-j")
      eval(parse(text=temp))
    }
  }
  #### Erstellen leerer temporaerer Vektoren mit Namen analog td[i] + Suffix ####
  ###############################################################################
  for (i in 1:length(td)){temp<-paste0('df_',i,'.<-NULL'); eval(parse(text=temp))}
  statistical.parameters<-c("median","mean","min","max")
  stat.result.column.suffixes<-c(statistical.parameters,"x")
  for (i in 1:length(td)){
    for (j in 1:length(stat.result.column.suffixes)){
      temp<-paste0('assign(paste0(td[i], ".", stat.result.column.suffixes[j]),', "df_", i, ".[[1]])")
      eval(parse(text=temp))
    }
  }
  rm(df.)
  #### statistical.parameters[i] berechnen und in temporaere Vektoren speichern ####
  ##################################################################################
  for (i in 1:length(td)){
    for (k in 1:length(statistical.parameters)){
      for (j in 1:x.interval.count){
        temp<-paste0(td[i],".",statistical.parameters[k],"[j]<-",
                                paste0(statistical.parameters[k],"(",
                                       paste0(td[i],"$y[",
                                              paste0(td[i],"$x.interval.no==j])"))))
        eval(parse(text=temp))
      }
    }
  }
  #### Berechnen der x-Achsenintervall-Mittenwerte und speichern in Vektor x.interval.centre ####
  ###############################################################################################
  for (i in 1:length(td)){
    for (j in 1:x.interval.count){
      temp<-paste0(td[i],".",stat.result.column.suffixes[length(stat.result.column.suffixes)],"[j]<-j*x.interval.size-x.interval.size/2")
      eval(parse(text=temp))
    }
  }
  #### Zusammenfuehren der temporaeren statistischen Daten in stat.result Dataframes ####
  #######################################################################################
  for (i in 1:length(td)){
    temp<-paste0(td[i],".stat.results<-c(1:x.interval.count)")
    eval(parse(text=temp))
  }
  for (i in 1:length(td)){
    for (j in 1:length(stat.result.column.suffixes)){
      temp<-paste0(td[i],".stat.results<-data.frame(",
                              paste0(td[i],".stat.results,"),
                              paste0(td[i],".",stat.result.column.suffixes[j]),")")
      eval(parse(text=temp))
    }
  }
  #### Loeschen von Zeilen mit NA/NaN/-Inf Werten und Output der stat.result Dataframes in global variable space ####
  ###################################################################################################################
  for (i in 1:length(td)){
    temp<-paste0('m',x.interval.size,'_',i,'.', terrace[i],"<<-",
                            paste0(td[i],".stat.results[",
                                   paste0(td[i],".stat.results$",
                                          paste0(td[i],".max"," != -Inf,]"))))
    eval(parse(text=temp))
  }
  #### Speichern der m_* Dataframe-Namen in eine Liste ####
  ############################################################
  td.m<<-NULL
  for (i in 1:length(terrace)){
    td.m[i]<<-paste0('m',x.interval.size,'_',i,'.',terrace[i])
  }
  #### Loeschen der bei der Zusammenfuehrung erstellten Behelfsspalte aus den Ergebnisdataframes ####
  ###################################################################################################
  for (i in 1:length(td)){ 
    temp<-paste0(td.m[i],'$',td[i],'.stat.results<<-NULL')
    eval(parse(text=temp))
  }
  #### Spalten der exportierten Dataframes fuer besseres Handling umbenennen ####
  ###############################################################################
  for (i in 1:length(td)){
    for (j in 1:length(stat.result.column.suffixes)){
    temp<-paste0("names(",td.m[i],")[names(",td.m[i],") == '",td[i],".",stat.result.column.suffixes[j],"'] <<- '",stat.result.column.suffixes[j],"'")
    eval(parse(text=temp))
  }
  } 
}