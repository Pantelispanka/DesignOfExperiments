require("AlgDesign")

filament.Design <- gen.factorial(2,3, varNames = c("length", "oscilation", "weight"))

filament.Design$y <- c(674, 3636, 170, 1140, 292, 2000, 90, 360)

filament.MainEffects <- lm(formula = y ~ length + oscillation + weight, data = filament.Design)

filament.Interactions < lm(formula = y ~ length * oscillation + length * weight + oscillation * weight, data = filament.Design)

summary(filament.MainEffects)
anova(filament.MainEffects)
summary(filament.Interactions)
anova(filament.Interactions)
anova(filament.MainEffects, filament.Interactions)
plot(filament.MainEffects)
plot(filament.Interactions)

require("FrF2")
MEPlot(filament.MainEffects)
IAPlot(filament.Interactions)