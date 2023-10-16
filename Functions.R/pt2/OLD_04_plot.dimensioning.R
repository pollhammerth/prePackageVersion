####################################################################################################################################
#### Berechnung der noetigen Plot-Achsenausdehnung, um alle Datenpunkte darzustellen und Funktion fuer leeren Plot ###################
####################################################################################################################################

#### Content:
#plot.dimensioning()                      # Achsendimensionierung des Plots

#### Benoetigt:
# td                                # Wird erzeugt durch import.terraces() oder global.variables()



# Benoetigt: td
plot.dimensioning<-function(){
  #### Intervall (x,y,z.limit) f?r alle Achsen ermitteln ####
  ###########################################################
  x.max.vector<-NULL
  for (i in 1:length(td)){
    expression.temp<-paste0('max(',paste0(td[i],'$x)'))
    x.max.vector<-c(x.max.vector,eval(parse(text=expression.temp)))
  }
  x.min.vector<-NULL
  for (i in 1:length(td)){
    expression.temp<-paste0('min(',paste0(td[i],'$x)'))
    x.min.vector<-c(x.min.vector,eval(parse(text=expression.temp)))
  }
  x.limit<<-c(min(x.min.vector),max(x.max.vector))
  y.max.vector<-NULL
  for (i in 1:length(td)){
    expression.temp<-paste0('max(',paste0(td[i],'$y)'))
    y.max.vector<-c(y.max.vector,eval(parse(text=expression.temp)))
  }
  y.min.vector<-NULL
  for (i in 1:length(td)){
    expression.temp<-paste0('min(',paste0(td[i],'$y)'))
    y.min.vector<-c(y.min.vector,eval(parse(text=expression.temp)))
  }
  y.limit<<-c(min(y.min.vector),max(y.max.vector))
  z.max.vector<-NULL
  for (i in 1:length(td)){
    expression.temp<-paste0('max(',paste0(td[i],'$z)'))
    z.max.vector<-c(z.max.vector,eval(parse(text=expression.temp)))
  }
  z.min.vector<-NULL
  for (i in 1:length(td)){
    expression.temp<-paste0('min(',paste0(td[i],'$z)'))
    z.min.vector<-c(z.min.vector,eval(parse(text=expression.temp)))
  }
  z.limit<<-c(min(z.min.vector),max(z.max.vector))
}
