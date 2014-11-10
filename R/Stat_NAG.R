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
                 formula = formula(log(nag.act) ~ co2 * (log(Moist) + Temp_Mean) + 
                                     (1|block) + (1|ring) + (1|id)))

aicDF <- m1$AICdf
aicDF[which(aicDF$AICs == min(aicDF$AICs)), ]

# 4-day gives the lowest AIC
df <- m1$Data

## check linearity agains soil variables

# plot against soil varriable
scatterplotMatrix(~ log(nag.act) + log(Moist) + Temp_Mean, diag = "boxplot", df)

# what if I use soil moisture at day of sampling
M.fmoist <- lmer(log(nag.act) ~ co2 * (log(moisture) + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
AIC(M.fmoist)
# this is a lot better, so use this.

# which period is good use for temperature then
m2 <- LmrAicComp(ListDF = LstDF_SoilVar, 
                 formula = formula(log(nag.act) ~ co2 * (log(moisture) + Temp_Mean) + 
                                     (1|block) + (1|ring) + (1|id)))

aicDF2 <- m2$AICdf
aicDF2[which(aicDF2$AICs == min(aicDF2$AICs)), ]
# slight improvement, so use this
df2 <- m2$Data

# plot for each plot against soil variables
print(xyplot(log(nag.act) ~ log(moisture) | ring + plot, df2, type = c("r", "p")))
print(xyplot(log(nag.act) ~ Temp_Mean | ring + plot, df2, type = c("r", "p")))
# looks fine

## Analysis

# The initial model is
Iml_ancv <- lmer(log(nag.act) ~ co2 * (log(moisture) + Temp_Mean) +  
                   (1|block) + (1|ring) + (1|id), data = df2)

Anova(Iml_ancv)
Anova(Iml_ancv, test.statistic = "F")

# The final models
# Fml_ancv <- stepLmer(Iml_ancv)
# for some reasons stepLmer doesn't work with this so manually remove ns factors
m3 <- lmer(log(nag.act) ~ co2 * log(moisture) + Temp_Mean +  
             (1|block) + (1|ring) + (1|id), data = df2)

m4 <- lmer(log(nag.act) ~ co2 + log(moisture) + Temp_Mean + 
             (1|block) + (1|ring) + (1|id), data = df2)

m5 <- lmer(log(nag.act) ~ log(moisture) + Temp_Mean +
             (1|block) + (1|ring) + (1|id), data = df2)

anova(Iml_ancv, m3, m4, m5)

Fml_ancv <- m5
Anova(Fml_ancv)
AnvF_ancv_NAG <- Anova(Fml_ancv, test.statistic = "F")
AnvF_ancv_NAG

plot(allEffects(Fml_ancv))
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
