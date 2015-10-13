##############
# CO2 x Time #
##############
bxplts(value = "cello.act", data = enzy)
  # use log transormation

# The initial model is
Iml <- lmer(log(cello.act) ~ co2 + (1|block) + (1|ring), 
            data = enzy, subset = time == 1)
Anova(Iml)

Iml <- lmer(log(cello.act) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
            data = enzy, subset = time != 1)
Anova(Iml)

# The final model is
Fml <- stepLmer(Iml)
Anova(Fml)
AnvF_CBH <- Anova(Fml, test.statistic = "F")
AnvF_CBH

# model diagnosis
plot(Fml)
# little wedge-shaped
qqnorm(residuals(Fml))
qqline(residuals(Fml))

######################
# CO2 x Moist x Temp #
######################

# Determine how many days to go back from the sampling dates to calculate soil 
# variables-> it's determined and save as
# "Output//Data//FACE_Enzyme_FreshSoilWC_90dTemp.RData"

# m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
#                  formula = formula(log(cello.act) ~ co2 * (log(Moist) + Temp_Mean) + 
#                                      (1|block) + (1|ring) + (1|id)))
# 
# aicDF <- m1$AICdf
# aicDF[which(aicDF$AICs == min(aicDF$AICs)), ]
# 
# # 2-day gives the lowest AIC
# df <- m1$Data
# 
# ## check linearity agains soil variables
# 
# # plot against soil varriable
# scatterplotMatrix(~ log(cello.act) + log(Moist) + Temp_Mean, diag = "boxplot", df)
# 
# # what if I use soil moisture at day of sampling
# M.fmoist <- lmer(log(cello.act) ~ co2 * (log(moisture) + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
# AIC(M.fmoist)
# # this is slightly better than above, so use this
# 
# # what about soil temperature for this moisutre
# m2 <- LmrAicComp(ListDF = LstDF_SoilVar, 
#                  formula = formula(log(cello.act) ~ co2 * (log(moisture) + Temp_Mean) + 
#                                      (1|block) + (1|ring) + (1|id)))
# 
# aicDF2 <- m2$AICdf
# aicDF2[which(aicDF2$AICs == min(aicDF2$AICs)), ]
# # improved so use this period for temperature
# df2 <- m2$Data
# 
# #  soil moisture at the time of sampling best represent enayme activity. given 
# #  that soil moiasuter, 90-day temperature mean showed the smallest AIC for
# #  other enzymes. so use this as soil temperature
# save(df2, file = "Output/Data/FACE_Enzyme_FreshSoilWC_90dTemp.RData")
# 
# 
# # plot for each plot against soil variables
# print(xyplot(log(cello.act) ~ log(moisture) | ring + plot, df2, type = c("r", "p")))
# print(xyplot(log(cello.act) ~ Temp_Mean | ring + plot, df2, type = c("r", "p")))
# looks fine

## Analysis

# The initial model is
Iml_ancv <- lmer(log(cello.act) ~ co2 * (log(moisture) + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = df2)
Anova(Iml_ancv)

# The final models
# Fml_ancv <- stepLmer(Iml_ancv)

# for some reasons stepLmer doesn't work with this so manually remove ns factors
m2 <- lmer(log(cello.act) ~ co2 * log(moisture) + Temp_Mean + 
             (1|block) + (1|ring) + (1|id), data = df2)
m3 <- lmer(log(cello.act) ~ co2 * log(moisture) + 
             (1|block) + (1|ring) + (1|id), data = df2)
anova(Iml_ancv, m2, m3)

Fml_ancv <- m3
Anova(Fml_ancv)
AnvF_ancv_CBH <- Anova(Fml_ancv, test.statistic = "F")
AnvF_ancv_CBH
r2_CBH <- rsquared.glmm(Fml_ancv)

plot(allEffects(Fml_ancv))
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
