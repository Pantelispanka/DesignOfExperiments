require("AlgDesign")

solder.design <- gen.factorial(2,3, varNames = c("A","B","C"))
solder.design$D <- c(-1,1,1,-1,1,-1,-1,1)

solder.design$AxB <- solder.design$A*solder.design$B
solder.design$AxC <- solder.design$A*solder.design$C
solder.design$BxC <- solder.design$B*solder.design$C

solder.design$y <- c(15,20,4,9,25,29,10,8)

solder.design.model.main <- lm(formula = y ~ A + B + C + D, data = solder.design)



require("FrF2")
MEPlot(solder.design.model.main)