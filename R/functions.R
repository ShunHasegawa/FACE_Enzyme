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
  data$variable <- factor(data$variable, 
                          levels = c("cello.act", "gluco.act", "nag.act", "phos.act"), 
                          labels = c("CBH", "BG", "NAG", "AP"))
  
  ylabs <- c(expression(CBH~(mu*g~h^"-1"~g^"-1")), 
             expression(BG~(mu*g~h^"-1"~g^"-1")),
             expression(NAG~(mu*g~h^"-1"~g^"-1")),
             expression(AP~(mu*g~h^"-1"~g^"-1")))
  
  # atop: put the 1st argument on top of the 2nd
  
  # create ylab according to variable
  # when plotting multiple variables at the same time
  if(length(unique(data$variable)) > 1) 
    ylab <- expression(Potentail~enzyme~activity~(mu*g~h^"-1"~g^"-1")) else {
      # only one variable
      for (i in 1:4){
        if(unique(data$variable) == levels(data$variable)[i]) ylab  <- ylabs[[i]]
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
                 labels = date_format("%b-%y")) +
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
  
  # add asterisk on NH graphs at co3 treatments
  if(!any(unique(data$variable) == "nh")) p else{
    newDF <- subset(data, time %in% c(3, 7) & variable == "nh") # the times and variable where "*" is placed
    ant_pos <- ddply(newDF, .(date, variable), summarise, Mean = max(Mean + SE)) #y position of "*"
    ant_pos$lab <- "*"
    ant_pos$temp <- factor("amb", levels=c("amb", "elve")) 
    # the original data frame uses "temp", so it needs to have "temp" as well in ggplot2
    # but it doesn't really do anything    
    p +  geom_text(data = ant_pos, aes(x =date, y = Mean, label= lab), col = "black", vjust = 0)
  }
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
