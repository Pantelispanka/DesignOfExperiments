runs <- c(16,2,10,1,14,8,9,7,4,15,13,3,12,6,5,11)
x1 <- c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
x2 <- c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1)
x3 <- c(-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1)
x4<-c(-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1)
x5<-c(1,-1,-1,1,-1,1,1,-1,-1,1,1,-1,1,-1,-1,1)
y <- c(51.8,56.3,56.8,48.3,62.3,49.8,49.0,46,72.6,49.5,56.8,63.1,64.6,67.8,70.3,49.8)

pharmacy.data <- data.frame(runs,x1,x2,x3,x4,x5,y)

pharmacy.second.order.model <- rsm(y ~ FO(x1,x2,x3,x4,x5)+TWI(x1,x2,x3,x4,x5), data = pharmacy.data)

summary(pharmacy.second.order.model)

pharmacy.second.order.model <- rsm(y ~ SO(x1,x2,x3,x4,x5), data = pharmacy.data)

steepest <- steepest(pharmacy.first.order.model, seq(0, 20 , by = 1))
