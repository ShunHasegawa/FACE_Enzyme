#############
# gluco.act #
#############
bxplts(value = "gluco.act", data = enzy)
# use log transormation

# The initial model is
Iml <- lmer(log(gluco.act) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
            data = enzy)
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
