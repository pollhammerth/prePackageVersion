####################################################################################################################################
#### Setzen und auswerten von manuellen Interpretationspunkten #####################################################################
####################################################################################################################################

#### Content:
# create.points(1,2)        # Punkt Dataframe per Mausclick [1]<-Nummer der interpretierten Terasse,[2]<-Nummer der Interpretation
#### Benoetigt:
# terrace
#### Output:
# int* Dataframe



create.points<-function(interpretation=1,i=1,overwrite=TRUE,savedir=""){
  print('create.points!')
  dn<-paste0('int',interpretation,'_',i,'.',terrace[i])
  if (overwrite==TRUE){
    print('Beliebig viele Punkte in Diagramm clicken und mit ESC bestaetigen:')
    clicked.points<-locator()
    temp<-paste0(dn,'<<-data.frame(clicked.points)')
    eval(parse(text=temp))
    print('Danke! Die Punkte wurden gespeichert in:')
    print(dn)
  } else {
    clicked.points<-locator()
    temp<-paste0(dn,'<<-rbind(',dn,',clicked.points)')
    eval(parse(text=temp))
    print('Danke! Die Punkte wurden angehaengt an:')
    print(dn)
  }
  # Punkte entlang der x-Achse absteigend sortieren
  temp<-paste0(dn,"<-arrange(",dn,",desc(",dn,"$x))");eval(parse(text=temp))
  # Punkte als csv exportieren
  temp<-paste0('write.csv(',dn,',"',savedir,dn,'.csv")');eval(parse(text=temp))
  # Befehl zum erneuten laden der Punkte generieren
  print("Die Punkte wurden zusaetzlich als csv gespeichert. Zum erneut laden:")
  string<-paste0(dn,"<-read.csv('",savedir,dn,".csv');",dn,"$X<-NULL")
  print(string)
}

