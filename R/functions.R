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
    geom_text(aes(x = xv, 
                  y = Max * 0.9, 
                  label = labels), 
              parse = TRUE, 
              hjust = 1,
              vjust = c(0, 1),
              size = 3,
              data = coefSmfDF) +
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

###########################
# step deletion with lmer #
###########################
stepLmer <- function(model, red.rndm = FALSE, ddf = "Kenward-Roger", ...){
  require(lmerTest)
  update(step(model, reduce.random = red.rndm, ddf = ddf,...)$model, 
         contrasts = NULL)
}
# use "Kenward-Roger" for approximation for denominator degrees of freedom. This
# is the same as the default DF given by Anova(model, test.statistic = "F). The
# default of step gives me a warning message for IEM-NO3 for some reasons (not
# sure why.. so changed it.)

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse OR box-cox
bxplts <- function(value, ofst = 0, data, ...){
  data$y <- data[[value]] + ofst #ofst is added to make y >0
  a <- boxcox(y ~ co2 * time, ..., data = data)
  par(mfrow = c(2, 3))
  boxplot(y ~ co2*time, data, main = "raw")
  boxplot(log(y) ~ co2*time, main = "log", data)
  boxplot(sqrt(y) ~ co2*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ co2*time, main = "power(1/3)", data)
  boxplot(1/y ~ co2*time, main = "inverse", data)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black") 
  boxplot(y^(BCmax) ~ co2*time, 
          main = "", sep = "=", 
          data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), 
        col.main = texcol)
  par(mfrow = c(1,1))
}

# multiple box-cox power plot for different constant values
bxcxplts <- function(value, data, sval, fval, ...){
  data$yval <- data[[value]]
  ranges <- seq(sval, fval, (fval - sval)/9)
  
  # store parameters given from box-cox plot
  par(mfrow = c(5, 2))
  BCmax <- vector()
  for (i in 1:10){
    data$y <- data$yval + ranges[i]
    a <- boxcox(y ~ co2 * time, data = data, ...)
    BCmax[i] <- a$x[a$y == max(a$y)]
  }
  
  # plot box plot with poer given from box-box for 
  # each contstant value
  par(mfrow = c(5, 2))
  par(omi = c(0, 0, 0, 0), mai = c(0.4, 0.4, 0.4, 0))
  sapply(1:10, function(x) {
    boxplot((yval + ranges[x]) ^ BCmax[x] ~ co2 * time, 
            main = "", data = data)
    texcol <- ifelse(BCmax[x] < 0, "red", "black") 
    title(main = paste("constant=", round(ranges[x], 4), 
                       ", boxcox=", round(BCmax[x], 4)),
          col.main = texcol)
  })
  par(mfrow = c(1,1))
}

################################
# Return star based on P value #
################################
FormatPval <- function(Pval) {
  stars <- ifelse(Pval > .1, "ns",
                  ifelse(Pval > .05, ".",
                         ifelse(Pval > .01, "*",
                                ifelse(Pval > .001, "**",
                                       c("***")))))
  
  p <- as.character(ifelse(Pval > .1, round(Pval, 3),
                           ifelse(Pval < .001, "bold('<0.001')", 
                                  # shown with bold font. Note that inside of
                                  # bold needs to be in ''
                                  paste("bold(", round(Pval, 3), ")", sep = "'"))))
  return(data.frame(stars, p))
} 

########################################
# Create summary stat table from anova #
########################################
StatTable <- function(x, variable) { # x is anova result
  df <- data.frame(predictor = c(row.names(x)),
                   rbind(FormatPval(x$Pr)))
  
  # add a row for column name of the table in the fig 
  df <- rbind(df, data.frame(predictor = "", 
                             stars = "italic('P>F')", 
                             p = "italic('P>F')"))
  
  result <- merge(df, data.frame(predictor = c("co2", "time", "co2:time")), all = TRUE)
  
  # replace NA with ns
  result <- within(result, {
    p <- ifelse(is.na(p), "ns", as.character(p)) 
    # ifelse tries to return factor, so use as.character
    stars <- ifelse(is.na(stars), "ns", as.character(stars))
  })
  
  # relabel for plotting
  result$predictor <- factor(result$predictor, 
                             labels = c("", "CO[2]", "Time", "CO[2]*~x~Time"), 
                             levels = c("", "co2", "time", "co2:time"))
  result$variable <- variable
  result <- result[order(result$predictor), ]
  return(result)
}

############################################
# Create df to add a stat table to figures #
############################################
StatPositionDF <- function(StatRes, variable, ytop, ylength, gap = .07){
  d <- data.frame(variable, ytop, gap = gap * ylength) 
  # ytop is y coordinate for the top (i.e. CO2) of the table for each fig 
  # (variable), ylength is the difference of max and min value of the plot (i.e.
  # max(mean+SE) - min(mean-SE)). 0.1 * ylength is used to determine the gap between each row
  # of the table
  
  predictor <- levels(StatRes$predictor)
  
  # create df which contains variable, predictor and y coordinates for the other
  # predictors (i.e. Time, CO2xTime) which is ylength*0.1 (= gap) lower than one above
  d2 <- ddply(d, .(variable),
              function(x){
                data.frame(predictor, 
                           ldply(1:length(predictor), function(z) x$ytop - z * x$gap))
              })
  names(d2)[3] <- "yval"
  
  # mege every thing
  d3 <- merge(d2, StatRes, by = c("variable", "predictor"))
  d3$co2 <- "amb" # co2 column is required for ggplot
  return(d3)
}

##################################################
# compute mean of soil variable for given period #
##################################################
SoilPeriodMean <- function(data, rings, plots, Start, End){
  sDF <- subset(data, Date >= Start & Date <= End & ring == rings & plot == plots)
  colMeans(sDF[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")], na.rm = TRUE)
}

# Apply the above function to data frame and merge
SoilVarPeriMean <- function(data, period, SoilData){ 
  # period = number of days to back from sampling date to get average soil vars
  df <- ddply(data, .(date, ring, plot),
              function(x) SoilPeriodMean(
                data = SoilData, 
                Start = x$date - period,
                End = x$date, 
                rings = x$ring, 
                plot = x$plot))
  merge(data, df, by = c("date", "ring", "plot"))
}

###############################################
# Run lmer for each data frame and return AIC #
###############################################
LmrAicComp <- function(ListDF, formula){
  # lmer test for each data set
  LstLmrs <- llply(ListDF, 
                   function(x) lmer(formula, data = x),
                   .progress = "text")
  names(LstLmrs) <- names(ListDF)
  
  # plot AIC
  aicDF <- ldply(LstLmrs, AIC)
  names(aicDF) <- c("period", "AICs")
  plot(AICs ~ period, data = aicDF, xlab = "N of Days back from sampling")
  
  
  # lmer for the lowest aic
  df <- ListDF[[which(aicDF$AICs == min(aicDF$AICs))]]
  Iml <- lmer(formula, data = df)
  return(list("Initial" = Iml, "Data" = df, "AICdf" = aicDF))
}
