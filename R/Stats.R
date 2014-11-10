#########################################
# Process soil variable data for ANCOVA #
#########################################

# Soil variables (i.g. moisture and temperature) will be used as covariates. I 
# need to deternmine how many days to go back from the sampling dates in order 
# to calculate their means for given period. The number of days to be used will
# be determined by AIC valuse for models with different periods used to obtain
# soil variable averages.

# TDR soil data
load("Data/FACE_TDR_ProbeDF.RData")

# subset soil
TdrSoil <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")

##################################################
# Create mean of soil variable for ginven period #
##################################################

# Using the function, SoilVarPeriMean, create soil variables for given period
# and merge with data. Then run this for different periods and store all the
# resulted data frames in a sigle list

# Actual values
LstDF_SoilVar <- llply(seq(0, 90, 1), 
                       function(x) SoilVarPeriMean(data = subsetD(enzy, time != "1"),
                                                   period = x, SoilData = TdrSoil), 
                       .progress = "text")
names(LstDF_SoilVar) <- seq(0, 90, 1)
save(LstDF_SoilVar, file =  "Output/Data/LstDF_SoilVar.RData")
load("Output/Data/LstDF_SoilVar.RData")

#######
# CBH #
#######
source("R/Stat_CBH.R")

#######
# NAG #
#######
source("R/Stat_NAG.R")

######
# BG #
######
source("R/Stat_BG.R")

######
# AP #
######
source("R/Stat_AP.R")

#######################
# Summary Stats table #
#######################

################
## CO2 x Time ##
################

# create stat summary table for LMM with CO2 and time
CO2TimeStatList <- list('CBH' = AnvF_CBH,
                        'BG' = AnvF_BG,
                        'NAG' = AnvF_NAG,
                        'AP' = AnvF_AP)

Stat_CO2Time <- ldply(names(CO2TimeStatList), 
                      function(x) StatTable(CO2TimeStatList[[x]], variable = x))
save(Stat_CO2Time, file = "output//data/CO2Time_Stat.RData")
