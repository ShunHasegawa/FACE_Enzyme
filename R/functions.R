##########################
# Create a summary table #
##########################
CreateTable <- function(dataset, fac, ...){
  a <- dataset[c("Date", fac, "value")] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a, date~variable, mean, na.rm = TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) #merge datasets
  mer <- mer[,c(1, order(names(mer)[-grep("date|N", names(mer))])+1, grep("N", names(mer)))] #re-order columns
  mer$date <- as.character(mer$date) # date is turned into character for knitr output 
  return(format(mer, ...))
}

#function which creates excel worksheets
crSheet <- function(sheetname, dataset){
  #create sheet
  sheet <- createSheet(wb, sheetName = sheetname)
  
  #add data to the sheet
  addDataFrame(dataset, sheet, showNA = TRUE, row.names = FALSE, startRow = 2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname, "unit=umol h^(-1) g^(-1)")), sheet, startRow = 1, row.names = FALSE, col.names = FALSE)
}

############################
# make a summary dataframe #
############################
Crt_SmryDF <- function(data, val = "value"){
  x <- data[ ,val]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N  <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

####################
# plot mean and se #
####################

PltMean <- function(data, ...){
  
  # change variable level labels for plot labelling
  vars <- c("CBH", "BG", "NAG", "AP")
  data$variable <- factor(data$variable, 
                          levels = c("cello.act", "gluco.act", "nag.act", "phos.act"), 
                          labels = vars)
  
  ylabs <- c(expression(CBH~(mu*mol~h^"-1"~g^"-1")), 
             expression(BG~(mu*mol~h^"-1"~g^"-1")),
             expression(NAG~(mu*mol~h^"-1"~g^"-1")),
             expression(AP~(mu*mol~h^"-1"~g^"-1")))
  
  # atop: put the 1st argument on top of the 2nd
  
  # create ylab according to variable
  # when plotting multiple variables at the same time
  if(length(unique(data$variable)) > 1) 
    ylab <- expression(Potential~enzyme~activity~(mu*mol~h^"-1"~g^"-1")) else {
      # only one variable
      for (i in 1:4){
        if(unique(data$variable) == vars[i]) ylab  <- ylabs[[i]]
      }
    }
  
  p <- ggplot(data, aes_string(x = "Date", y = "Mean", ...))
  
  p2 <- p + geom_line(size = 1) + 
    geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", ...), width = 5) + 
    labs(x = "Time", y = ylab) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
               linetype = "dashed", 
               col = "black") +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"), 
                 limits = c(as.Date("2012-08-15"), as.Date("2013-06-15"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1))
}

##################
# Plot ring mean #
##################
PltRnghMean <- function(data){
  # change factor level names for labelling
  p <- PltMean(data, col = "ring", linetype = "co2") +
    scale_color_manual(values = palette(), "Ring", 
                       labels = paste("Ring", c(1:6), sep = "_")) +
    scale_linetype_manual(values = c("dashed", "solid"),
                          expression(CO[2]~trt),
                          labels = c("Ambient", expression(eCO[2])))
  
  return(p)
}

######################
# Plot temp trt mean #
######################
PltCO2Mean <- function(data){
  p <- PltMean(data, col = "co2") +
    scale_color_manual(values = c("blue", "red"), 
                       expression(CO[2]~trt),
                       labels = c("Ambient", expression(eCO[2])))
}

#########################################
# Plot regression line against moisture #
#########################################

# regression and get coefs and predicted values 

regrDF <- function(data){
  # regression and get coefficients and R^2
  rgr<-aov(log(value) ~ I(moisture^(1/3)), data)
  
  regrDF <- data.frame(
    xv = xv <- seq(0.015, 0.2, 0.001),
    yv = exp(coef(rgr)[1] + coef(rgr)[2] * xv^(1/3))
  )
  return(regrDF)
}

coefDF <- function(data){
  # regression and get coefficients and R^2
  rgr<-aov(log(value) ~ I(moisture^(1/3)), data)
  
  # coefficient and sqare-R
  a <- round(coef(rgr)[1], 2)
  b <- round(coef(rgr)[2], 2)
  r <- round(summary.lm(rgr)$r.squared, 2)
  
  Eqlab <- as.character(ifelse(b < 0, paste("y == exp(", a, "+", -b, "*x^(1/3))"), 
                               paste("y == exp(", a, "-", b, "*x^(1/3))")))
  Rlab <- paste("R^2 ==", r)
  
  coefDF <- data.frame(
    labels = c(Eqlab, Rlab),
    Max = max(data$value),
    xv = max(data$moisture)
  )
  return(coefDF)
}

pltReg <- function(data){
  
  # change variable level labels for plot labelling
  vars <- c("CBH", "BG", "NAG", "AP")
  data$variable <- factor(data$variable, 
                          levels = c("cello.act", "gluco.act", "nag.act", "phos.act"), 
                          labels = vars)
  # regression sumamry DF
  regSmDF <- ddply(data, .(variable), regrDF)
  coefSmfDF <- ddply(data, .(variable), coefDF)
  
  # ylables
  ylabs <- c(expression(CBH~(mu*mol~h^"-1"~g^"-1")), 
             expression(BG~(mu*mol~h^"-1"~g^"-1")),
             expression(NAG~(mu*mol~h^"-1"~g^"-1")),
             expression(AP~(mu*mol~h^"-1"~g^"-1")))
  
  # create ylab according to variable
  # when plotting multiple variables at the same time
  if(length(unique(data$variable)) > 1) 
    ylab <- expression(Potential~enzyme~activity~(mu*mol~h^"-1"~g^"-1")) else {
      # only one variable
      for (i in 1:4){
        if(unique(data$variable) == vars[i]) ylab  <- ylabs[[i]]
      }
    }
  
  p <- ggplot(data, aes(x = moisture, y = value))
  p2 <- p + geom_point(aes(col = co2)) +
    scale_color_manual(values = c("blue", "red"), 
                       expression(CO[2]~trt), 
                       labels = c("Ambient", expression(eCO[2]))) +
    geom_line(aes(x = xv, y = yv), data = regSmDF) +
#     geom_text(aes(x = xv, 
#                   y = Max * 0.9, 
#                   label = labels), 
#               parse = TRUE, 
#               hjust = 1,
#               vjust = c(0, 1),
#               size = 3,
#               data = coefSmfDF) +
    labs(x = "Soil moisture", y = ylab)
  return(p2)
}


##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

#########################
# subset and droplevels #
#########################
subsetD <-  function(...){
  droplevels(subset(...))
}