rm(list = ls(all = TRUE))

source("R/pckg.R")
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

# add block and id
enzy <- within(enzy, {
  block <- recode(ring, "c(1, 2) = 'A'; c(3, 4) = 'B'; c(5, 6) = 'C'")
  id <- factor(ring:plot)
  time <- factor(dates, levels = c("sep12", "dec12", "mar13", "jun13"), 
                 labels = c(1:4))
})

# save
save(enzy, file = "Output//Data/FACE_Enzyme.RData")
load("Output//Data/FACE_Enzyme.RData")

#######################
# Summary excel table #
#######################
source("R/SummaryExlTable.R")

########
# Figs #
########
source("R/Figs.R")

#########
# Stats #
#########
source("R/Stats.R")
