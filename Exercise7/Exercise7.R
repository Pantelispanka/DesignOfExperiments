x1<- c(-1,-1,-1,-1,1,1,1,1,-1.682,1.682,0,0,0,0,0,0,0,0,0,0)
x2<- c(-1,-1,1,1,-1,-1,1,1,0,0,-1.682,1.682,0,0,0,0,0,0,0,0)
x3 <- c(-1,1,-1,1,-1,1,-1,1,0,0,0,0,-1.682,1.682, 0,0,0,0,0,0)
y <- c(66,70,78,60,80,70,100,75,100,100,68,63,65,82,113,100,118,88,100,85)

design.second.order <- rsm(y ~ SO(x1,x2,x3), data = design)

summary(design.second.order)

contour(design.second.order, ~ x1+x2+x3, image = TRUE, main="second-order model")
persp(design.second.order,  ~x1+ x2 + x3, zlab = "y", main="second-order model")


design.first.order <- rsm(y ~ FO(x1,x2,x3), data = design)

contour(design.first.order, ~ x1+x2+x3, image = TRUE, main="first-order model")
persp(design.first.order,  ~x1+x2+x3, zlab = "y", main="first-order model")