# Data management functions


# Decrease search radius
clip.radius<-function(name="all",radius=3000){
  if (name=="all"){
    for (i in 1:length(td)){
      temp<-paste0('test<-',td[i],'[',td[i],'$z < ',radius,',]'); eval(parse(text=temp))
      temp<-paste0(td[i],'<<-test[test$z > -',radius,',]'); eval(parse(text=temp))
    }
  } else {
    temp<-paste0('test<-',name,'[',name,'$z < ',radius,',]'); eval(parse(text=temp))
    temp<-paste0(name,'<<-test[test$z > -',radius,',]'); eval(parse(text=temp))
  }
}


# clip to z range
clip.z.range<-function(name="all",range=c(-3000,3000)){
  if (name=="all"){
    for (i in 1:length(td)){
      temp<-paste0('test<-',td[i],'[',td[i],'$z < ',max(range),',]'); eval(parse(text=temp))
      temp<-paste0(td[i],'<<-test[test$z > ',min(range),',]'); eval(parse(text=temp))
    }
  } else {
    temp<-paste0('test<-',name,'[',name,'$z < ',max(range),',]'); eval(parse(text=temp))
    temp<-paste0(name,'<<-test[test$z > ',min(range),',]'); eval(parse(text=temp))
  }
}



# Delete NoData values from input table
delete.no.data<-function(name="all",y.nodata.value=-9999){
  if (name=="all"){
    for (i in 1:length(td)){
      temp<-paste0(td[i],'<<-',td[i],'[',td[i],'$y > ',y.nodata.value,',]')
      eval(parse(text=temp))
    }
  } else {
    temp<-paste0(name,'<<-',name,'[',name,'$y > ',y.nodata.value,',]')
    eval(parse(text=temp))
  }
}


# Clean route fringes
clean.fringes<-function(name="all"){
  if (name=="all"){
    for (i in 1:length(td)){
      temp<-paste0(td[i],'<<-',td[i],'[',td[i],'$x > min(',td[i],'$x) & ',td[i],'$x < max(',td[i],'$x),]')
      eval(parse(text=temp))
    }
  } else {
    temp<-paste0(name,'<<-',name,'[',name,'$x > min(',name,'$x) & ',name,'$x < max(',name,'$x),]')
    eval(parse(text=temp))
  }
}


# Clip data.frame to x-axis range
clip.x.range<-function(name="all",range=c(0,10000)){
  if (name=="all"){
    for (i in 1:length(td)){
      temp<-paste0(td[i],'<<-',td[i],'[',td[i],'$x > ',min(range),' & ',td[i],'$x < ',max(range),',]')
      eval(parse(text=temp))
    }
  } else {
    temp<-paste0(name,'<<-',name,'[',name,'$x > ',min(range),' & ',name,'$x < ',max(range),',]')
    eval(parse(text=temp))
  }
}


# Sort Table ascending x-values
sort.by.x<-function(name="all"){
  if (name=="all"){
    for (i in 1:length(td)){
      temp<-paste0(td[i],'<<-',td[i],'[order(',td[i],'$x),]')
      eval(parse(text=temp))
    }
  } else {
    temp<-paste0(name,'<<-',name,'[order(',name,'$x),]')
    eval(parse(text=temp))
  }
}



# Erase x-axis range
erase.x.range<-function(name="all",range=c(0,10000)){
  if (name=="all"){
    for (i in 1:length(td)){
      temp<-paste0(td[i],'<<-',td[i],'[',td[i],'$x < ',min(range),' | ',td[i],'$x > ',max(range),',]')
      eval(parse(text=temp))
    }
  } else {
    temp<-paste0(name,'<<-',name,'[',name,'$x < ',min(range),' | ',name,'$x > ',max(range),',]')
    eval(parse(text=temp))
  }
}


# Add value to y variable (adapt elevation):
elevate <- function(type="m500",i=2,y="median",value=+3){
  temp<-paste0('df.name<-gsub("td_","',type,'_",td[',i,'])')
  eval(parse(text=temp))
  temp<-paste0(df.name,'$',y,'<<-',df.name,'$',y,'+(',value,')')
  eval(parse(text=temp))
}



# Interactively erase values between multiple x ranges:
batch.erase.x.range <- function(name="NA",locator="TRUE",x=c(1,2)){
  if (name == "NA") {
    print("Please enter data.frame name in function call!")
  } else {
    if (locator == "TRUE") {
      print("Please select x ranges, in between which data should be erased...")
      locs<-locator()
      x<-locs$x
      len<-length(x)/2
      for(i in 1:len){
        xi<-c(x[i+i-1],x[i+i])
        temp<-paste0('erase.x.range("',name,'",xi)');eval(parse(text=temp))
      }
      print("Recall function with this line:")
      print(paste0('batch.erase.x.range(name="',name,'",locator=FALSE,x=c(',paste(round(x,2),collapse=","),'))'))
    } else {
      len<-length(x)/2
      for(i in 1:len){
        xi<-c(x[i+i-1],x[i+i])
        temp<-paste0('erase.x.range("',name,'",xi)');eval(parse(text=temp))
      }
    }
  }
}




