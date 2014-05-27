theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
RngMean <- ddply(enzMlt, .(Date, co2, ring, variable), Crt_SmryDF) 
TrtMean <- ddply(RngMean, .(Date, co2, variable), function(x) Crt_SmryDF(x, val = "Mean"))

#################################
# plot each nutrient separately #
#################################
vars <- c("CBH", "BG", "NAG", "AP")

RngFg <- dlply(RngMean, .(variable), PltRnghMean)
fls <- paste("Output//Figs/FACE_Enzyme_Ring_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltCO2Mean)
fls <- paste("Output//Figs/FACE_Enzyme_CO2Trt_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltCO2Mean(TrtMean) +
  facet_wrap(~ variable, ncol = 2, scales= "free_y")
ggsavePP(filename = "output//figs/FACE_Enzyme_CO2Trt", plot = pl, width = 6, height = 4)
