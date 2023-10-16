# this function can be used to make an expression out of a zoomed plot extent, set with pmt.zoom()
# that way it can be recalled for individual plots

pmt.zoomPaster <- function(e = extent) {

  # create string from current extent
  string <- paste0(
    " <- list(xlim = ",
    paste0("c(",paste0(round(e$xlim,0), collapse = ","),")"),
    ", ",
    "ylim = ",
    paste0("c(",paste0(round(e$ylim,0), collapse = ","),")"),
    ", ",
    "zlim = ",
    paste0("c(",paste0(round(e$zlim,0), collapse = ","),")"),
    ")"
  )
  
  # function output
  cat("\n\n> Paste to your script to restore original extent, or set custom zoom:\n")
  cat("fullExtent", string,"; ")
  cat("extent <- fullExtent\n")
  cat("extent", string, "\n")
  
  
}
