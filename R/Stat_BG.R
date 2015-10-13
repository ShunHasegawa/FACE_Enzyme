#############
# gluco.act #
#############
bxplts(value = "gluco.act", data = enzy)
# use log transormation

# The initial model is
Iml <- lmer(log(gluco.act) ~ co2 + (1|block) + (1|ring), 
            data = enzy, subset = time == 1)
Anova(Iml)

Iml <- lmer(log(gluco.act) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
            data = enzy, subset = time != 1)
Anova(Iml)

# The final model is
Fml <- stepLmer(Iml)
Anova(Fml)
AnvF_BG <- Anova(Fml, test.statistic = "F")
AnvF_BG

# model diagnosis
plot(Fml)
qqnorm(residuals(Fml))
qqline(residuals(Fml))

######################
# CO2 x Moist x Temp #
######################

# Determine how many days to go back from the sampling dates to calculate soil 
# variables-> it's determined and save as
# "Output//Data//FACE_Enzyme_FreshSoilWC_90dTemp.RData"
# m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
#                  formula = formula(log(gluco.act) ~ co2 * (log(Moist) + Temp_Mean) + 
#                                      (1|block) + (1|ring) + (1|id)))
# 
# aicDF <- m1$AICdf
# aicDF[which(aicDF$AICs == min(aicDF$AICs)), ]
# 
# # 7-day gives the lowest AIC
# df <- m1$Data
# 
# ## check linearity agains soil variables
# 
# # plot against soil varriable
# scatterplotMatrix(~ log(gluco.act) + log(Moist) + Temp_Mean, diag = "boxplot", df)
# 
# # what if I use soil moisture at day of sampling
# M.fmoist <- lmer(log(gluco.act) ~ co2 * (log(moisture) + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
# AIC(M.fmoist)
# # no difference, so just use this as it's simpler
# 
# # what about soil temperature for this moisutre
# m2 <- LmrAicComp(ListDF = LstDF_SoilVar, 
#                  formula = formula(log(gluco.act) ~ co2 * (log(moisture) + Temp_Mean) + 
#                                      (1|block) + (1|ring) + (1|id)))
# 
# aicDF2 <- m2$AICdf
# aicDF2[which(aicDF2$AICs == min(aicDF2$AICs)), ]
# # improved so use this period for temperature
# df2 <- m2$Data
# 
# 
# # plot for each plot against soil variables
# print(xyplot(log(gluco.act) ~ log(moisture) | ring + plot, df2, type = c("r", "p")))
# print(xyplot(log(gluco.act) ~ Temp_Mean | ring + plot, df2, type = c("r", "p")))
# # looks fine

## Analysis

# The initial model is
Iml_ancv <- lmer(log(gluco.act) ~ co2 * (log(moisture) + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = df2)
Anova(Iml_ancv)

# The final models
# Fml_ancv <- stepLmer(Iml_ancv)

# for some reasons stepLmer doesn't work with this so manually remove ns factors
m2 <- lmer(log(gluco.act) ~ co2 * log(moisture) + Temp_Mean + 
             (1|block) + (1|ring) + (1|id), data = df2)

m3 <- lmer(log(gluco.act) ~ co2 + log(moisture) + Temp_Mean + 
             (1|block) + (1|ring) + (1|id), data = df2)

m4 <- lmer(log(gluco.act) ~ log(moisture) + Temp_Mean + 
             (1|block) + (1|ring) + (1|id), data = df2)
anova(Iml_ancv, m2, m3, m4)

Fml_ancv <- m4
Anova(Fml_ancv)
AnvF_ancv_BG <- Anova(Fml_ancv, test.statistic = "F")
AnvF_ancv_BG
r2_BG <- rsquared.glmm(Fml_ancv)

plot(allEffects(Fml_ancv))
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
