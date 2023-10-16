#### pmt.plotBin()
# ideal for plotting pmt.bin() output
# anno = T for plotting polygon ids or content of other column = "". e.g. "s" for standard deviation.
# pcex and tcex for point and text size respectively.
# requires pmt.empty() if add = F is set


pmt.plotBin <- function(data = b, points = T, ppch = 16, border = "black", fill = "white", anno = F, column = "id", add = T, 
                        pcex = 1, tsrt = 66, tcex = 1, x = "x", y = "y", 
                        anno.spacer = c("---- ","--------------- "), ...){
  
  if (add == F) {pmt.empty(...)}
  if (anno == T) {
    text(data[[x]],data[[y]],
         # check if the column exists
    if (isTRUE(names(data)[which(names(data) == column)] == column) == TRUE) {
        paste0(anno.spacer,data[[column]]) # use the contents of a column, if it exists in the data
      } else {
        paste0(anno.spacer,column) # use the argument string, if the column does not exist in the data
      }
    ,adj = 0, srt = tsrt, cex = tcex*0.84)
  }
  if (points == T) {
    points(data[[x]],data[[y]],cex = pcex*1, pch = ppch, col = border)
    points(data[[x]],data[[y]],cex = pcex*0.6, pch = ppch, col = fill)
  }
}




