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
# plot all enzymes in one graph #
##################################
# labels for facet_grid
ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltCO2Mean(TrtMean) +
  facet_wrap(~ variable, ncol = 2, scales= "free_y")
ggsavePP(filename = "output//figs/FACE_Enzyme_CO2Trt", plot = pl, width = 6, height = 4)

#######################
# Fig for publication #
#######################
# define graphic background
science_theme <- theme(panel.grid.major = element_line(size = 0.2, color = "grey"), 
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = c(.9, .9), 
                       legend.title = element_blank())


# change labels
vars <- c("CBH", "BG", "NAG", "AP")
df <- TrtMean
df$variable <- factor(df$variable, 
                      levels = c("cello.act", "gluco.act", "nag.act", "phos.act"), 
                      labels = vars)

# create data frame for fig sub labels
df$subLab <- factor(df$variable, labels = LETTERS[1:length(levels(df$variable))])

p <- ggplot(df, aes(x = Date, y = Mean, group = co2))

p2 <- p + geom_line(aes(linetype = co2)) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 15, size = .3,
                position = position_dodge(20)) + 
  geom_point(aes(shape = co2, fill = co2), position = position_dodge(20)) +
  labs(x = "Month", y = expression(Potential~enzyme~activity~(mu*mol~h^"-1"~g^"-1"))) +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
             linetype = "dashed", col = "black") +
  scale_x_date(breaks= date_breaks("2 month"),
               labels = date_format("%b-%y"), 
               limits = c(as.Date("2012-08-15"), as.Date("2013-06-15"))) +
  scale_shape_manual(values = c(24, 21), labels = c("Ambient", expression(eCO[2]))) +
  scale_fill_manual(values = c("black", "white"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Ambient", expression(eCO[2]))) +
  facet_wrap(~ variable, ncol = 2, scales= "free_y") +
  geom_text(aex(x = as.Date("2012-08-15"), y = ))
  science_theme
ggsavePP(filename = "Output//Figs//FACE_Manuscript/FACE_Enzyme", plot = p2, 
         width = 6, height = 5)



# white-black figure
WBFig <- function(data, ylab, facetLab = ylab_label, figTheme = science_theme){
  return(p2)
}



#########################################
# plot regression agianst soil moisture #
#########################################
RegFg <- dlply(enzMlt,. (variable), pltReg) 
fls <- paste("Output//Figs/FACE_Enzyme_RegreMoist_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = RegFg[[x]], width = 6, height = 3))

pl <- pltReg(enzMlt) +
  facet_wrap(~ variable, ncol = 2, scales= "free_y")
ggsavePP(filename = "output//figs/FACE_Enzyme_RgreMoist", plot = pl, width = 6, height = 4)





