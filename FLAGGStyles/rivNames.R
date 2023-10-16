# custom expressions, to filter specific river traces


rivNames <- c(
  "Donau", "Rhein", "Aare", "Wutach", "Thur", "Reuss"
)

riv <- list(
  all = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "rivers.label", 
               regard.elements = c(if(is.na(riv.add)==FALSE){riv.add}else{rivNames}), 
               disregard.column = "rivers.label", 
               disregard.elements = riv.remove,
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  Donau = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "rivers.label", 
               regard.elements = c("Donau", if(is.na(riv.add)==FALSE){riv.add}else{}), 
               disregard.column = "rivers.label", 
               disregard.elements = riv.remove,
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  Rhein = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "rivers.label", 
               regard.elements = c("Rhein", if(is.na(riv.add)==FALSE){riv.add}else{}), 
               disregard.column = "rivers.label", 
               disregard.elements = riv.remove,
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  Aare = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "rivers.label", 
               regard.elements = c("Aare", if(is.na(riv.add)==FALSE){riv.add}else{}), 
               disregard.column = "rivers.label", 
               disregard.elements = riv.remove,
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  Wutach = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "rivers.label", 
               regard.elements = c("Wutach", if(is.na(riv.add)==FALSE){riv.add}else{}), 
               disregard.column = "rivers.label", 
               disregard.elements = riv.remove,
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  Reuss = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "rivers.label", 
               regard.elements = c("Reuss", if(is.na(riv.add)==FALSE){riv.add}else{}), 
               disregard.column = "rivers.label", 
               disregard.elements = riv.remove,
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  Thur = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "rivers.label", 
               regard.elements = c("Thur", if(is.na(riv.add)==FALSE){riv.add}else{}), 
               disregard.column = "rivers.label", 
               disregard.elements = riv.remove,
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  )))
)


# these are needed, if no additional filtering is set (meaning if these objects are not set manually)
if (exists("riv.add")==F){riv.add = NA}
if (exists("riv.remove")==F){riv.remove = NA}