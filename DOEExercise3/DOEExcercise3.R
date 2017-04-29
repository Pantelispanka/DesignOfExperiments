filament.Design <- gen.factorial(3,3, varNames = c("x1", "x2", "x3"))

filament.Design$y <- c(674, 1414,3636, 338,1022,1568,170,442,1140,370,1198,3184,266,620,1070,118,332,884,292,634,2000,210,428,566,90,220,360)

filament.Model <- lm(formula = y ~ ., data = filament.Design)

filament.Model.squared <- lm(formula = y ~ x1 + x2 + x3 + I(x1^2) + I(x2^2) + I(x3^2) + x1*x2 + x1*x3 + x2*x3, data = filament.Design)

summary(filament.Model)
anova(filament.Model)
summary(filament.Model.squared)
anova(filament.Model.squared)
plot(filament.Model)
plot(filament.Model.squared)