j1 <- c(1,1,-1,-1,0,0,0,0)
j2 <- c(-1,1,-1,1,0,0,0,0)
y<-c(82.5,83.5,84.7,85,84.1,84.5,83.9,84.3)
oxygen.design <- data.frame(j1,j2,y)
j1.uncoded <- c(-22.5,-22.5,-21.5,-21.5,-22,-22,-22,-22)
j2.uncoded <- c(1.1,1.3,1.1,1.3,1.2,1.2,1.2,1.2)
data <- data.frame(j1.uncoded, j2.uncoded, y)
##Model
oxygen.design.model <- lm(y~j1.uncoded + j2.uncoded, data = data)
plot(oxygen.design.model)
##Model with rsm package
oxygen.design.model.test <- rsm(y ~ FO(j1,j2), data = data)
plot(oxygen.design.model.test)
##Variance
prediction.variance <- predict(oxygen.design.model.test,newdata=data.frame(j1=-220,j2=1.2),type="response",interval="prediction",level=0.95)
##Stepest
steepest <- steepest(oxygen.design.model.test, seq(0, 20 , by = 1))

