# FLAGG specific color code
styles <- list(
  nt = list(
    name = "01_NT", 
    col = "#98c872",
    col2 = "#dff5cd"),
  ht = list(
    name = "02_HT", 
    col = "#5b8cbb",
    col2 = "#b5def7"),
  tds = list(
    name = "03c_TDS_Mindel_Guenz_1800_780_ka", 
    col = "#907a58",
    col2 = "#dbcd9e"),
  hds = list(
    name = "06a_HDS_Donau_Biber_1800_780_ka", 
    col = "#dd4243",
    col2 = "#f39a96"),
  tdst = list(
    name = "",
    col = "#f6c9de",
    col2 = "#f6c9de"),
  tdsh = list(
    name = "",
    col = "#cbe6b3",
    col2 = "#cbe6b3"
  ),
  loess = list(
    name = "12_Deckschi", 
    col = "grey30",
    col2 = "grey80"),
  modern = list(
    name = "river.buffer450",
    col = "#292c78",
    col2 = "#bec1fc"),
  mindel = list(
    name = "03ba_ODM",
    col = "#ffaf01",
    col2 = "#ffe8b6"),
  guenz = list(
    name = "03bb_ODG",
    col = "#ff0101",
    col2 = "#ffbbbb"),
  donau = list(
    name ="03bc_ODD",
    col = "#ad3a01",
    col2 = "#febfa0"),
  osds = list(# meaningful colors to be set
    name ="03b_OSDS_undiff_1800_780_ka",
    col = c("#ad3a01","#012875"),
    col2 = c("#febfa0","#012875")),
  hs = list(# meaningful colors to be set
    name ="10a_Hoehenschotter",
    col = c("#d7ffc3","#012875"),
    col2 = c("#d7ffc3","#012875")),
  # old FLAGG color code
  flagg.colors = list(biber = "#012875",
                      donau = "#ad3a01",
                      guenz = "#ff0101",
                      mindel = "#ffaf01",
                      riss = "#ffff01",
                      wuerm = "#d7ffc3",
                      modern = "#0000d1",
                      altpleistoPlio = "#398017",
                      ht_Schweiz = "#e6e600",
                      LGMice = "#99f1ff",
                      LGMiceDARK = "#59b1bf", # darker version, better for annotations
                      RISSice = "#4ba8ff",
                      RISSiceDARK = "#0b68bf", # darker version, better for annotations
                      MINDELice = "#544bff",
                      GUENZice = "#934bff"
  )
)




# store filter expressions for on demand processing
f <- list(
  nt = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "map.NAME_KURZ", 
               regard.elements = c("01_NT"), 
               disregard.column = NA, 
               disregard.elements = NA, 
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  ht = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "map.NAME_KURZ", 
               regard.elements = c("02_HT"), 
               disregard.column = NA, 
               disregard.elements = NA, 
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  tds = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "map.NAME_KURZ", 
               regard.elements = c("03c_TDS_Mindel_Guenz_1800_780_ka"), 
               disregard.column = NA, 
               disregard.elements = NA, 
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  hds = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "map.NAME_KURZ", 
               regard.elements = c("06a_HDS_Donau_Biber_1800_780_ka"), 
               disregard.column = NA, 
               disregard.elements = NA, 
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  ats = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "map.NAME_KURZ", 
               regard.elements = c("08_Aelterer_Terrscho_Mid_Plei"), 
               disregard.column = NA, 
               disregard.elements = NA, 
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  smp = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "map.NAME_KURZ", 
               regard.elements = c("01d_Spaet_bis_Mid_Plei"), 
               disregard.column = NA, 
               disregard.elements = NA, 
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  deck = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = "map.NAME_KURZ", 
               regard.elements = c("12_Deckschi"), 
               disregard.column = NA, 
               disregard.elements = NA, 
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
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance),# c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  ))),
  mask = parse(text = quote(expression(
    pmt.filter(data, 
               regard.column = NA, 
               regard.elements = NA, 
               disregard.column = NA, 
               disregard.elements = NA, 
               thresh.column = c("slope","distance"), 
               threshold = c(max.slope,max.distance),# c(max.slope,max.distance), 
               x = "location", 
               y = y.source, 
               z = "distance", 
               drop.unused = FALSE)
  )))
)




