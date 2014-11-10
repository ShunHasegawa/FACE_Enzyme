#######
# NAG #
#######
bxplts(value = "nag.act", data = enzy)
# use sqrt transormation

# The initial model is
Iml <- lmer(sqrt(nag.act) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
            data = enzy)
Anova(Iml)

# The final model is
Fml <- stepLmer(Iml)
Anova(Fml)
AnvF_NAG <- Anova(Fml, test.statistic = "F")
AnvF_NAG

# model diagnosis
plot(Fml)
qqnorm(residuals(Fml))
qqline(residuals(Fml))

######################
# CO2 x Moist x Temp #
######################

# Determine how many days to go back from the sampling dates to calculate soil
# variables
m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
                 formula = formula(log(gluco.act) ~ co2 * (log(Moist) + Temp_Mean) + 
                                     (1|block) + (1|ring) + (1|id)))

aicDF <- m1$AICdf
aicDF[which(aicDF$AICs == min(aicDF$AICs)), ]

# 7-day gives the lowest AIC
df <- m1$Data

## check linearity agains soil variables

# plot against soil varriable
scatterplotMatrix(~ log(gluco.act) + log(Moist) + Temp_Mean, diag = "boxplot", df)

# what if I use soil moisture at day of sampling
M.fmoist <- lmer(log(gluco.act) ~ co2 * (log(moisture) + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
AIC(M.fmoist)
# no difference, so just use this as it's simpler

# plot for each plot against soil variables
print(xyplot(log(gluco.act) ~ log(moisture) | ring + plot, m1$Data, type = c("r", "p")))
print(xyplot(log(gluco.act) ~ Temp_Mean | ring + plot, m1$Data, type = c("r", "p")))
# looks fine

## Analysis

# The initial model is
Iml_ancv <- M.fmoist
Anova(Iml_ancv)

# The final models
# Fml_ancv <- stepLmer(Iml_ancv)

# for some reasons stepLmer doesn't work with this so manually remove ns factors
m2 <- lmer(log(gluco.act) ~ co2 * log(moisture) + Temp_Mean + 
             (1|block) + (1|ring) + (1|id), data = df)
m3 <- lmer(log(gluco.act) ~ co2 + log(moisture) + Temp_Mean + 
             (1|block) + (1|ring) + (1|id), data = df)
m4 <- lmer(log(gluco.act) ~ co2 + log(moisture) + 
             (1|block) + (1|ring) + (1|id), data = df)
m5 <- lmer(log(gluco.act) ~ log(moisture) + 
             (1|block) + (1|ring) + (1|id), data = df)
anova(Iml_ancv, m2, m3, m4, m5)

Fml_ancv <- m5
Anova(Fml_ancv)
AnvF_ancv_BG <- Anova(Fml_ancv, test.statistic = "F")
AnvF_ancv_BG

plot(allEffects(Fml_ancv))
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
