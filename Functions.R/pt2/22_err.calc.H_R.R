#### Funktion zur darstellung von Fehlerbalken der Donauhoehen.
#### Inhalt:
# err.calc.H_R()
# Variablen (Legende):
# b               Winkel zwischen Tributaerprofil und Donau		
# b_              Maximal- und Minimalwert fuer obigen Winkel		
# H_R             Relative Hoehe ueber Vorfluter, ermittelt am Vorfluter Taleingang		
# D               Distanz der dem Vorfluter am naechsten gelgenen Tributaerterrasse		
# H_T             Hoehe der oben genannten Tributaerterrasse		
# H_V             Hoehe des Vorfluters am Profilschnittpunkt		
# gV              Gefaelle des Vorfluters		
# gT              Interpoliertes Gefaelle der Tributaerterrassen		
# plot=TRUE       Erstellt einen Plot der Fehlerfunktionswerte zur visuellen Ueberpruefung
# ch_plus         Fehler Plus durch Kanalbreite, von moderner Konfluenz Tribut?rgerinne aufw?rts.
# ch_minus        Fehler Minus durch Kanalbreite
# csv_name        Wenn Name angegeben, werden die Fehlerbalkenwerte als absolute Hoehen exportiert.
#### Funktionsinterne Variablen (Legende):
#       Berechnung von Zwischenergebnissen				
# a               Gegenueberliegender Winkel zu b in Rechtwinkeligem Dreieck		
# E               Haeufig verwendeter Ausdruck		
# D90             Abstand D bei b = 90		
# dH_R_gV_min     Aenderung der relativen Hoehe ueber Vorfluter bei minimalem b		
#       Zwischenergebnis-Vektoren mit Funktionswerten:
# dH_R_vec        Aenderung der relativen Hoehe ueber Vorfluter (kumulativ)
# dH_R_D_vec	    (mit Distanzaenderung, bzw. Hoehenaenderung der extrapolierten Tributaerterrasse durch b)
# dH_R_gV_vec	    (mit Hoehenaenderung des Vorfluters durch b)
#### Output:
# Werte zur Erstellung eines Fehlerbalkens fuer einen relative Vorfluterhoehen-Plot.



# Handhabung:
# b und b_ in ArcGIS messen und eingeben.
# gV aus Donauprofil an Konfluenz messen und eingeben.
# gT Ergebnis aus aktuellem R Profil eingeben.
# locator=TRUE -> interaktives Auswaehlen weiterer Parameter.
# csv_name angeben damit das Ergebnis im Arbeitsverzeichnis abgespeichert wird.


err.calc.H_R<-function(b=69,b_=c(60,90),gV=0.0008,gT=0.003,locator=FALSE,csv_name=FALSE,plot=FALSE,H_R=56,D=8000,H_T=480,H_V=400,ch_plus=4000,ch_minus=1000,col="black",ch_plus_paleochannel=ch_plus,ch_minus_paleochannel=ch_minus){
if(locator==TRUE){
  # Daten generieren:
  print("x,y Position Vorfluter ermitteln. Bestaetigen mit [ESC]")
  v<-locator()
  print("x,y Position der, dem Vorfluter naechstgelegenen Terrasse ermitteln. Bestaetigen mit [ESC]")
  t<-locator()
  print("Extrapolierte relative Hoehe ueber modernem Vorfluter ermitteln. Bestaetigen mit [ESC]")
  r<-locator()
  H_R<-as.numeric(r[2])-as.numeric(v[2])
  H_V<-as.numeric(v[2])
  H_T<-as.numeric(t[2])
  D<-as.numeric(v[1])-as.numeric(t[1])
  
  # Fehler durch Kanalbreite:
  print("max Position der Kanal-Raender ermitteln. 2x click, bestaetigen mit [ESC]")
  ch<-locator()
  ch<-as.data.frame(ch[1])
  ch_plus<-as.numeric(v[1])-min(ch)
  ch_minus<-max(ch)-as.numeric(v[1])
} else {}
  
  
  # Zwischenberechnungen:
  a<-90-b
  E<-sqrt(1+tan(a*pi/180)^2)
  D90<-D/E
  # Zwischenberechnung von Vektoren:
  b_vec<-seq(min(b_),max(b_),length.out=100)
  a_vec<-90-b_vec
  E_vec<-sqrt(1+tan(a_vec*pi/180)^2)

  # Berechnung der Fehler-Funktionswerte:
  dH_R_D_vec<-H_T-H_V-tan(gT)*E_vec*D90-H_R
  dH_R_gV_vec<-(tan((90-min(b_))*pi/180)*D90-tan((90-b)*pi/180)*D90)*gV; for (i in 1:99) {dH_R_gV_vec<-c(dH_R_gV_vec,dH_R_gV_vec[i]-gV*(tan(a_vec[i]*pi/180)*D90-tan(a_vec[i+1]*pi/180)*D90))}
  dH_R_vec<-dH_R_D_vec+dH_R_gV_vec
  # Berechnung von Relativen Hoehen-Funktionswerten:
  H_R_D_vec<-H_R+dH_R_D_vec
  H_R_gV_vec<-H_R+dH_R_gV_vec
  H_R_vec<-H_R+dH_R_vec

  # Korrekturwerte durch Kanalbreite:
#  ch_plus <- ch_plus - (ch_plus + ch_minus) / 2 # Korrektur auf halbe Talbreite. Annahme, dass der Vorfluter zumindest die Haelfte der heutigen Talbreite ausgemacht hat. Der dem Tributaergerinne gegenueber liegende Vorfluter Talrand wird als fixiert angenommen! Achtung: Kann Fehlerwerte ausserhalb des Messwertes bewirken! Wenn Vorfluter am Trinutaer naechsten Talrand vrelaeuft! (Also der Messpunkt nicht innerhalb des angenommenen Palaeokanals liegt.)
  chy_plus<<-ch_plus*gT
  chy_minus<<-ch_minus*gT
#  chy_plus<<-0
#  chy_minus<<-0

  # Korrekturwerte um Lage des Vorfluterkanals entlang des Profils. Sofern jeweilige aeltere Kanalgrenzen feststellbar sind

if (ch_plus_paleochannel != ch_plus & ch_minus_paleochannel == ch_minus) {

  ch_shift_plus <- ch_plus_paleochannel - ch_plus
  chy_shift <- c(ch_shift_plus * gT, 0)

} else if (ch_plus_paleochannel == ch_plus & ch_minus_paleochannel != ch_minus) {

  ch_shift_minus <- ch_minus - ch_minus_paleochannel
  chy_shift <- c(0, ch_shift_minus * gT)

} else if (ch_plus_paleochannel != ch_plus & ch_minus_paleochannel != ch_minus) {

  ch_shift_plus <- ch_plus_paleochannel - ch_plus
  ch_shift_minus <- ch_minus - ch_minus_paleochannel
  chy_shift <- c(ch_shift_plus * gT, ch_shift_minus * gT)

} else if (ch_plus_paleochannel == ch_plus & ch_minus_paleochannel == ch_minus) {

  chy_shift <- c(0,0)

} else {print("Sh**. Sth with the paleochannel plus minus is not working...")}



  # Berechnung des Fehlerbalkens aus Vektoren (Ergebnis):
  H_R_bar<-c(min(H_R_vec)-chy_minus+chy_shift[2],max(H_R_vec)+chy_plus+chy_shift[1])
  Errorplusminus<-c(max(H_R_bar)-H_R,min(H_R_bar)-H_R)
  Errorwidth<-max(H_R_vec)-min(H_R_vec)+chy_plus+chy_minus
  # Ausgabe des Ergebnisses:
  print("Fehler +/-:")
  print(Errorplusminus)
  print("Gesamtfehlerbreite:")
  print(Errorwidth)
  print("Relative Hoehenwerte ueber Vorfluter:")
  print(paste0(c(round(H_R_bar,digits=2),H_R),collapse=","))
  
  # Berechnung von H_R_m. Entspricht H_R gemessen in der mitte des Wuermzeitlichen Vorfluter Kanales:
      m <- (ch_plus_paleochannel - (ch_minus_paleochannel + ch_plus_paleochannel) / 2) * gT
  H_R_m <- (ch_plus_paleochannel - (ch_minus_paleochannel + ch_plus_paleochannel) / 2) * gT + H_R
  H_R_m_chfix <- (ch_plus - (ch_minus + ch_plus) / 2) * gT + H_R


  if (plot==TRUE) {
  # Kontrollplot zur Ueberpruefung der Plausibilit?t des Ergebnisses:
  xe<-c(max(b_vec),min(b_vec))
  ye<-c(max(H_R_vec,H_R_bar),min(H_R_vec,H_R_bar))
  plot(xe,ye,col="white", main="Relative Elevation Change Plot",ylab="Relative Height above base-level",xlab="Angle between recieving stream and profile.")
  points(b_vec,H_R_vec,type = "l", col = "red")
#  points(b_vec,H_R_D_vec, type = "l", col = "green")
#  points(b_vec,H_R_gV_vec, type = "l", col = "blue")
  points(c(b,b),H_R_bar,type="l",lwd=4,col="black")
  points(b,H_R_m,pch=16,cex=4.5,col="black"); points(b,H_R_m,pch=16,cex=3,col=col); points(b,H_R_m,pch=3,cex=3,col="black")
  } else {}
  
  if(csv_name!=FALSE){
  # Data Output:
  result<-c(col,col,col)                                                                                     # Farbe fuer kumulativen Vorfluterplot.
  result<-data.frame(result,c(max(Errorplusminus)+H_R+H_V,H_R+H_V,min(Errorplusminus)+H_R+H_V)) # Messwert und Fehler durch var. Breite Vorfluterkanal (+ chy_shift).
  result<-data.frame(result,c(H_R,H_V,H_V+H_R))                    # Relative Hoehe, Vorfluterhoehe, abs. Tributaerhoehe.
  result<-data.frame(result,c(H_R_m,H_V,H_V+H_R_m))                # Relative Hoehe, Vorfluterhoehe, abs. Tributaerhoehe. Var. Kanal. An Kanalmitte.
  result<-data.frame(result,c(max(Errorplusminus)+H_R+H_V-chy_shift[1],H_R+H_V,min(Errorplusminus)+H_R+H_V-chy_shift[2]))    # Messwert und Fehler bei Annahme -> LGM Kanal.
  result<-data.frame(result,c(H_R,H_V,H_V+H_R))                                                        # Relative Hoehe, Vorfluterhoehe, abs. Tributaerhoehe.
  result<-data.frame(result,c(H_R_m_chfix,H_V,H_V+H_R_m_chfix))                                        # Relative Hoehe, Vorfluterhoehe, abs. Tributaerhoehe. LGM Kanal. An Kanalmitte.

  temp<-paste0('write.csv(result,"',csv_name,'")')
  eval(parse(text=temp))
  } else {}

  # Erneuter Aufruf:
  print("Zum erneuten Aufruf der Berechnung folgende Funktion eingeben:")
  string<-paste0('err.calc.H_R(b=',b,',b_=c(',min(b_),',',max(b_),'),gV=',gV,',gT=',gT,',locator=',locator,',H_R=',H_R,',D=',D,',H_T=',H_T,',H_V=',H_V,',ch_plus=',ch_plus,',ch_minus=',ch_minus,',plot=',plot,",csv_name='",csv_name,"')")
  print(string)
}



