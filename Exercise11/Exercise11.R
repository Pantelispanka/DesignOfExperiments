A<- c(1,-1,1,-1,-1,-1,1,1,1,-1,1,-1)
B <- c(1,1,-1,1,-1,-1,-1,1,1,1,-1,-1)
C<- c(-1,1,1,-1,1,-1,-1,-1,1,1,1,-1)
D<-c(1,-1,1,1,-1,1,-1,-1,-1,1,1,-1)
E<-c(1,1,-1,1,1,-1,1,-1,-1,-1,1,-1)
F<-c(1,1,1,-1,1,1,-1,1,-1,-1,-1,-1)
G<-c(-1,1,1,1,-1,1,1,-1,1,-1,-1,-1)
eight<-c(-1,-1,1,1,1,-1,1,1,-1,1,-1,-1)
nine <- c(-1,-1,-1,1,1,1,-1,1,1,-1,1,-1)
ten <- c(1,-1,-1,-1,1,1,1,-1,1,1,-1,-1)
eleven <- c(-1,1,-1,-1,-1,1,1,1,-1,1,1,-1)
y<-c(8.0858,5.683,7.917,5.818,6.607,5.682,6.752,9,6.899,4.652,5.733,4.809)


#The Data
corruption.design <- data.frame(A,B,C,D,E,F,G,eight,nine.ten,eleven,y)

#Taguchi
snrs <- -10* log(y^2/12)
snrl <- -10 * log(1/12*y^2)
snrt <- -10* log((y - mean(y))^2 /11)

#The taguchi design with the tagichi snr values for target low and high
corruption.design.with.snr <- data.frame(A,B,C,D,E,F,G,eight,nine,ten,eleven,y,snrs,snrl,snrt)

##SNRl should be maximized for dispersion to be minimized
##Dispersion model :
dispersion.model <- lm(snrl ~ A+B+C+D+E+F+G+eight+nine+ten+eleven, data = corruption.design.with.snr)



location.model <- lm(y ~ A+B+C+D+E+F+G+eight+nine+ten+eleven, data = corruption.design.with.snr)


