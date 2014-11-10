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
