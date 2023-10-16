####################################################################################################################################
#### Vertical spacing calculation by clicking 2 points in plot then press ESC ######################################################
####################################################################################################################################

#### Content:
# quick.spacing()          # [1]<-"y" or "x" for vertical or horizontal spacing respectively
#### Benoetigt:
#
#### Output:
#



quick.spacing<-function(axis="y",clipboard.windows=FALSE){
  print('Bitte 2 Punkte fuer Hoehenunterschiedsberechnung waehlen (Bestaetigen mit ESC):')
  b<-data.frame(locator())
  
  if (axis=="y"){
    print("y spacing coords")
    string=paste0(as.vector(b[2]),collapse=", ");print(string)
    if (clipboard.windows==TRUE){
      writeClipboard(print(string))
    } else {}
  } else {}
  if (axis=="x"){
    print("x spacing coords")
    string=paste0(as.vector(b[1]),collapse=", ");print(string)
    if (clipboard.windows==TRUE){
      writeClipboard(print(string))
    } else {}
  } else {}
  
  print('Ergebnis:')
  temp<-paste0('b$',axis,'[1] - b$',axis,'[2]')
  eval(parse(text=temp))
}