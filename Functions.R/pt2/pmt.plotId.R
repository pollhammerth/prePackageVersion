
# this can be used, to plot terrace ids, for overview

pmt.plotId <- function(filter.expression, col = "white", field = "id"){
  eval(filter.expression) %>% pmt.bin(value="median",mode="id",cth = NA,sth = NA) %>% pmt.plotBin(fill = col, anno = T, column = field)
}
