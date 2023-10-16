# custom expressions, to filter specific terraces
# some old profiles have been projected with "map.terrace" instead of "mapped.terrace"
# if filter expressions do not work, maybe just replace this


ds <- list(
hds = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(dsIds$hds[[1]], if(is.na(hds.add)==FALSE){hds.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = hds.remove,
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
  ))),
tds = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(dsIds$tds[[1]], if(is.na(tds.add)==FALSE){tds.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = tds.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
ht = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(htIds, if(is.na(ht.add)==FALSE){ht.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = ht.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
nt = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(ntIds, if(is.na(nt.add)==FALSE){nt.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = nt.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
odm = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(dsIds$odm[[1]],if(is.na(odm.add)==FALSE){odm.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = odm.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
odg = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(dsIds$odg[[1]], if(is.na(odg.add)==FALSE){odg.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = odg.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
ods = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(dsIds$ods[[1]], if(is.na(ods.add)==FALSE){ods.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = ods.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
lib = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = if(is.na(lib.add[1])==FALSE){lib.add}else{NA}, 
             disregard.column = "mapped.terrace", 
             disregard.elements = lib.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
dem = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = NA,
             regard.elements = NA, 
             disregard.column = NA, 
             disregard.elements = NA, 
             thresh.column = c("slope","distance"), # these were commented out, but I don't know why... If there's trouble, comment out again!
             threshold = c(max.slope,max.distance), # these were commented out, but I don't know why... If there's trouble, comment out again!
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
tdsh = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(tdshNames, if(is.na(tdsh.add)==FALSE){tdsh.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = tdsh.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
))),
tdst = parse(text = quote(expression(
  pmt.filter(data, 
             regard.column = "mapped.terrace", 
             regard.elements = c(tdstNames, if(is.na(tdst.add)==FALSE){tdst.add}else{NULL}), 
             disregard.column = "mapped.terrace", 
             disregard.elements = tdst.remove, 
             thresh.column = c("slope","distance"), 
             threshold = c(max.slope,max.distance), 
             x = "location", 
             y = y.source, 
             z = "distance", 
             drop.unused = FALSE)
)))



)


# these are needed, if no additional filtering is set (meaning if these objects are not set manually)
if (exists("hds.add")==F){hds.add = NA}
if (exists("hds.remove")==F){hds.remove = NA}
if (exists("tds.add")==F){tds.add = NA}
if (exists("tds.remove")==F){tds.remove = NA}
if (exists("odm.add")==F){odm.add = NA}
if (exists("odm.remove")==F){odm.remove = NA}
if (exists("odg.add")==F){odg.add = NA}
if (exists("odg.remove")==F){odg.remove = NA}
if (exists("ods.add")==F){ods.add = NA}
if (exists("ods.remove")==F){ods.remove = NA}
if (exists("lib.add")==F){lib.add = NA}
if (exists("lib.remove")==F){lib.remove = NA}
if (exists("tdsh.add")==F){tdsh.add = NA}
if (exists("tdsh.remove")==F){tdsh.remove = NA}
if (exists("tdst.add")==F){tdst.add = NA}
if (exists("tdst.remove")==F){tdst.remove = NA}
if (exists("ht.add")==F){ht.add = NA}
if (exists("ht.remove")==F){ht.remove = NA}
if (exists("nt.add")==F){nt.add = NA}
if (exists("nt.remove")==F){nt.remove = NA}





