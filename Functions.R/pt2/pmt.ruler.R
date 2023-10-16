

# function pmt.ruler()
# draw a line in an existing profile plot and a linear model will be created from these points.
# line and extrapolated model are plotted as well as the model slope in permil

# this is a quick and dirty function
# uses: 
# pmt.drawLine()
# pmt.model()
# pmt.modelToLine()


pmt.ruler <- function(){

l <- pmt.drawLine()

lines(l, lwd = 2, col = "#0000ff66")

lm.slope <- round(abs(pmt.model(data = l, deg = 1)$coefficients[["x"]])*1000, digits = 1); lm.slope # get slope in permil via linear modelling

m <- l %>% pmt.model(deg = 1)

# pmt.plotModel(m, extrapol = T, conf = F, col = "red")

ml <- m %>% pmt.modelToLine(extrapol = T)

lines(ml, lwd = 1, col = "#ff000066")

text(x = l$x[1], y = l$y[1], labels = paste0("   ",lm.slope, " permil"), srt = 66, adj = 0, cex = 2)

cat(lm.slope)

}



#pmt.ruler()






