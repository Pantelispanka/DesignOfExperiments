A <- c(-1,-1,-1,-1,1,1,1,1)
B<-c(-1,-1,1,1,-1,-1,1,1)
C<-c(-1,-1,1,1,1,1,-1,-1)
D <- c(-1,1,-1,1,-1,1,-1,1)
E<-c(1,1,-1,1,1,-1,1,-1)
F <- c(-1,1,1,-1,-1,1,1,-1)
G<-c(-1,1,1,-1,1,-1,-1,1)
lll<-c(2.2,2.5,0.5,2,4,3.1,5,3)
lhh <- c(2.1,0.3,3.1,1.9,4.1,5.2,2.0,2.9)

#Data data.frame
spray.data <- data.frame(A,B,C,D,E,F,G,lll,lhh,hlh,hhl)

meany <- c(mean(2.2,2.1,2.3), mean(2.5,0.3,2.7,0.3), mean(0.5,3.1,0.4,2.8), mean(2.,1.9,1.8,2), mean(4,4.1,4,4.1), mean(3.1,5.2,2,4.1), mean(5,2.9,5.6,3.2), mean(3,2.9,2.9,2.8))
lnsi <- c(log(var(c(2.2,2.1,2.3,2.3))), log(var(c(2.5,0.3,2.7,0.3))), log(var(c(0.5,3.1,0.4,2.8))), log(var(c(2,1.9,1.8,2))), log(var(c(4,4.1,4,4.1))), log(var(c(3.1,5.2,2,4.1))), log(var(c(5,2.9,5.6,3.2))), log(var(c(3,2.9,2.9,2.8))))

#Taguchi lnsi Data.frame
spray.data.tag.design <- data.frame(A,B,C,D,E,F,G,lll,lhh,hlh,hhl,meany,lnsi)

spray.data.models <- rsm(lnsi ~ FO(A,D,E,F,G), data = spray.data.tag.design)
spray.data.models <- rsm(lnsi ~ FO(A,B,E,F,G), data = spray.data.tag.design)
.
.
.
##To deside significance because of not desired degrees of freedom could do QQPlot

##Combuined array
spray.combined.array <- FrF2(32,10,randomize = FALSE)

spray.combined.array$A <- c(-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,)
spray.combined.array$B <- c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1)
spray.combined.array$C <- c(-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1)
spray.combined.array$D <- c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
spray.combined.array$E <- c(-1,1,-1,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1)
spray.combined.array$F <- c(-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1)
spray.combined.array$G <- c(-1,1,1,-1,1,-1,-1,+1,-1,1,1,-1,1,-1,-1,+1,-1,1,1,-1,1,-1,-1,+1,-1,1,1,-1,1,-1,-1,+1)
spray.combined.array$H <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
spray.combined.array$J <- c(-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1)
spray.combined.array$K <- c(-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1)
spray.combined.array$y <- c(-2.2,2.5,0.5,2,4,3.1,5,3,2.1,0.3,3.1,1.9,4.1,5.2,2.9,2.9,2.3,2.7,0.4,1.8,4,2,5.6,2.9,2.3,0.3,2.8,2,4.1,4.1,3.2,2.8)


#The model
combined.model <- lm(y~A+B+C+D+E+F+G+H+J+K+A*H+A*J+A*K+B*H+B*J+B*K+C*H+C*J+C*K+D*H+D*J+D*K+E*H+E*J+E*K+F*H+F*J+F*K+G*H+G*J+G*K, data = spray.combined.array)

summary(combined.model)


require("FrF2")
IAPlot(combined.model)
MEPlot(combined.model)



