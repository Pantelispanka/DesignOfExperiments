
#The data
x1 <- c(-1,1,-1,1,-1,1,-1,1,0,0,0,0,0,0,0)
x2<-c(-1,-1,1,1,0,0,0,0,-1,1,-1,1,0,0,0)
x3<-c(0,0,0,0,-1,-1,1,1,-1,-1,1,1,0,0,0)
y<-c(53,58,59,56,64,45,35,60,59,64,53,65,65,59,62)

#The box-behnken design
bbd.design <- data.frame(x1,x2,x3,y)

#The model
bbd.second.order <- rsm(y~ SO(x1,x2,x3), data = bbd.design)

summary(bbd.second.order)

