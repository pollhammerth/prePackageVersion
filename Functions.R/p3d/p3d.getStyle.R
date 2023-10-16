# uses profile data containing one unit (e.g. output of pmt.filter())
# and returns the associated "col" or other desired attribute from styles list
# if the style is not defined or styles is unexisting, the output is NA

p3d.getStyle <- function(data = filtered.data, style = "col", s = NA) {
  if (exists("styles") == FALSE) {return(s);break}
  for (i in 1:length(styles)) {
    if (styles[[i]]$name == unique(data[[map1.infield.outfield[2]]])) {
      s <- styles[[i]][[style]]
      break
    }
  }
  return(s)

}

