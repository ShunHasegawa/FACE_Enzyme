######
# AP #
######
bxplts(value = "phos.act", data = enzy)
# use log transormation

# The initial model is
Iml <- lmer(log(phos.act) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
            data = enzy)
Anova(Iml)

# The final model is
Fml <- stepLmer(Iml)
Anova(Fml)
AnvF_AP <- Anova(Fml, test.statistic = "F")
AnvF_AP

# model diagnosis
plot(Fml)
qqnorm(residuals(Fml))
qqline(residuals(Fml))
