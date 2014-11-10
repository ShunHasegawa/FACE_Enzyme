#############
# cello.act #
#############
bxplts(value = "cello.act", data = enzy)
  # use sqrt transormation

# The initial model is
Iml <- lmer(sqrt(cello.act) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
            data = enzy)
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
