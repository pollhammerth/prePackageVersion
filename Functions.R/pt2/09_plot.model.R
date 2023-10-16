####################################################################################################################################
#### Funktionen zur Erstellung von Modellen innerhalb eines x-Achsen-Intervalls ####################################################
####################################################################################################################################

#### Content:
# plot.recent.model()       # [1]<-col,[2]<-lwd,[3]<-plot.conf,[4]<-conf.level
#### Benoetigt:
# model, model.x.axis, model.x.axis.interval
#### Output:
# 



plot.recent.model<-function(col="red",lwd=2,plot.conf=TRUE,conf.level=0.99){
  
  temp<-paste0('model.predicted.intervals<-predict(model,data.frame(x=',model.x.axis,'[',model.x.axis,' > min(model.x.axis.interval) & ',
                                                                                          model.x.axis,' < max(model.x.axis.interval)])',
                                     ',interval="confidence"', 
                                     ',level=',conf.level,')'
              )
  eval(parse(text=temp))    
  
  if (plot.conf==TRUE){
    temp<-paste0('lines(',model.x.axis,'[',model.x.axis,' > min(model.x.axis.interval) & ',model.x.axis,' < max(model.x.axis.interval)]',
                 ',model.predicted.intervals[,2]',
                 ',col="grey"',
                 ',lwd=','1',
                 ')'
    )
    eval(parse(text=temp))
    temp<-paste0('lines(',model.x.axis,'[',model.x.axis,' > min(model.x.axis.interval) & ',model.x.axis,' < max(model.x.axis.interval)]',
                 ',model.predicted.intervals[,3]',
                 ',col="grey"',
                 ',lwd=','1',
                 ')'
    )
    eval(parse(text=temp))
  } else {}
  
  temp<-paste0('lines(',model.x.axis,'[',model.x.axis,' > min(model.x.axis.interval) & ',model.x.axis,' < max(model.x.axis.interval)]',
               ',model.predicted.intervals[,1]',
               ',col="',col,'"',
               ',lwd=',lwd,
               ')'
  )
  eval(parse(text=temp))

}
