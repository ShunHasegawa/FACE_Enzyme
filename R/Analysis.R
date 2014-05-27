rm(list = ls(all = TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(scales)
library(xtable)


source("R/functions.R")

###########
# Process #
###########
enz <- read.csv("Data//FACE_enzyme.csv", colClasses= c("ring" = "factor", "plot" = "factor"))

# CO2
enz$co2 <- factor(ifelse(enz$ring %in% c(1, 4, 5), "elev", "amb"))

# Date
enz$Date <- as.Date(recode(enz$dates, "'dec12' = '2012-12-11'; 
                                        'jun13' = '2013-06-11' ;
                                        'mar13' = '2013-03-11' ; 
                                        'sep12' = '2012-09-03'"))
# change unit
enz[c("cello.act", "gluco.act", "nag.act", "phos.act")] <- enz[c("cello.act", "gluco.act", "nag.act", "phos.act")]/1000

#plot mean
enzy <- ddply(enz, .(dates, Date, co2, ring, plot), 
              function(x) colMeans(x[c("cello.act", "gluco.act", "nag.act", "phos.act")], na.rm = TRUE))

# soil variable
soil <- read.table("Data//FACE_enzyme_SoilVar.txt", header = T, colClasses = c("ring" = "factor", "plot" = "factor"))

# merge soil variable with plot mean enzyme data
enzy <- merge(enzy, soil, by.x = c("dates", "ring", "plot"), by.y = c("date", "ring", "plot"))

#######################
# Summary excel table #
#######################
source("R/SummaryExlTable.R")

########
# Figs #
########
source("R/Figs.R")
