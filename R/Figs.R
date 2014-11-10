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
science_theme <- theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(), 
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = c(.9, .91),
#                        legend.text = element_text(size = 2),
                       legend.title = element_blank())

# change labels
vars <- c("CBH", "BG", "NAG", "AP")
df <- TrtMean
df$variable <- factor(df$variable, 
                      levels = c("cello.act", "gluco.act", "nag.act", "phos.act"), 
                      labels = vars)

# create data frame for fig sub labels
subLabDF <- data.frame(xv = as.Date("2012-08-15"),
                       ddply(df, .(variable), summarise, yv = max(Mean + SE)),
                       labels = LETTERS[1:length(levels(df$variable))],
                       co2 = "amb")

# create data frame for stat summary table
## determine ylength
ylengthDF <- ddply(df, .(variable), summarise,
                   ylength = max(Mean + SE) - min(Mean - SE),
                   ymax = max(Mean + SE))

# df for stat table
load("Output//Data//CO2Time_Stat.RData")
statDF <- StatPositionDF(StatRes = Stat_CO2Time, 
                         variable = levels(df$variable), 
                         ytop = c(ylengthDF$ymax[1], 0.46, ylengthDF$ymax[3:4]),
                         ylength = ylengthDF$ylength,
                         gap = .06)

# create a plot
p <- ggplot(df, aes(x = Date, y = Mean, group = co2))

p2 <- p + geom_line(aes(linetype = co2), position = position_dodge(20)) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 15, size = .3,
                position = position_dodge(20)) + 
  geom_point(aes(shape = co2, fill = co2), position = position_dodge(20)) +
  labs(x = "Month", y = expression(Potential~enzyme~activity~(mu*mol~h^"-1"~g^"-1"))) +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
             linetype = "dashed", col = "black") +
  scale_x_date(breaks= date_breaks("2 month"),
               labels = date_format("%b-%y"), 
               limits = c(as.Date("2012-08-15"), as.Date("2013-07-15"))) +
  scale_shape_manual(values = c(24, 21), labels = c("Ambient", expression(eCO[2]))) +
  scale_fill_manual(values = c("black", "white"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Ambient", expression(eCO[2]))) +
  geom_text(aes(x = xv, y = yv * .95, label = labels),
            fontface = "bold",
            data = subLabDF) +
  facet_wrap(~ variable, ncol = 2, scales= "free_y") +
  science_theme +
  geom_text(data = subset(statDF, predictor != ""), 
            aes(x = as.Date("2013-6-1"), y = yval, label = predictor),
            size = 2, hjust = 1, parse = TRUE) +
  # unless remove "" with predictor != "", labels will be messed up due to
  # this empty level
  geom_text(data = statDF, 
            aes(x = as.Date("2013-7-1"), y = yval, label = p), 
            size = 2, parse = TRUE)

ggsavePP(filename = "Output//Figs//FACE_Manuscript/FACE_Enzyme", 
         plot = p2, width = 6, height =5)

#########################################
# plot regression agianst soil moisture #
#########################################
RegFg <- dlply(enzMlt,. (variable), pltReg) 
fls <- paste("Output//Figs/FACE_Enzyme_RegreMoist_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = RegFg[[x]], width = 6, height = 3))

pl <- pltReg(enzMlt) +
  facet_wrap(~ variable, ncol = 2, scales= "free_y")
ggsavePP(filename = "output//figs/FACE_Enzyme_RgreMoist", plot = pl, width = 3, height = 4)





