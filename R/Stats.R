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
