install.packages("fda")
library(fda)
#provide directory of the RData
?setwd
setwd("/Users/cao/Dropbox/Teaching/FDA/SummerCourse2017/R")

# daily temperature in Vancouver
y = CanadianWeather$dailyAv[,'Vancouver','Temperature.C']

# First of all a polynomial basis


X = matrix(1:365,365,10)
for(i in 1:10){ X[,i] = ((X[,i]-183)/50)^(i-1) }

X[,1] = 0.2


for(i in 1:10){
	yhat = X[,1:i]%*%solve(t(X[,1:i])%*%X[,1:i])%*%(t(X[,1:i])%*%y)

	name1 = paste('monomial_basis',i,'.png',sep='')
	name2 = paste('vancfit_monomial',i,'.png',sep='')

	png(name1)
	matplot(150:216,X[150:216,1:i],type='l',lwd=2,lty=1,
		xlab='day',ylab='basis',cex.lab=1.5,cex.axis=1.5)
	dev.off()

	png(name2)
	plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
		main='Vancouver',cex.axis=1.5)
	lines(1:365,yhat,lwd=2,col=4)
	dev.off()
}


X = X[,1:6]
C = solve(t(X)%*%X)%*%(t(X)%*%y)

# Now a fourier basis
?create.fourier.basis
daybasis365 <- create.fourier.basis(c(0, 365), 365)
bvals = eval.basis(1:365,daybasis365)
dim(bvals)

for(i in 1:6){
	
	X = bvals[,1:(2*i+1)]

	yhat = X%*%solve(t(X)%*%X)%*%(t(X)%*%y)

	name1 = paste('fourier_basis',i,'.png',sep='')
	name2 = paste('vancfit_fourier',i,'.png',sep='')

	png(name1)
	matplot(1:365,X[,(2*i):(2*i+1)],type='l',lwd=2,lty=1,
		xlab='day',ylab='basis',cex.lab=1.5,cex.axis=1.5)
	dev.off()

	png(name2)
	plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
		main='Vancouver',cex.axis=1.5)
	lines(1:365,yhat,lwd=2,col=4)
	dev.off()
}


C = solve(t(X)%*%X)%*%(t(X)%*%y)


dvals = eval.basis(1:365,daybasis365,Lfdobj=1)
dim(dvals)

d2vals = eval.basis(1:365,daybasis365,Lfdobj=2)

# Now we want to look at a spline basis

knots = cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))

bbasis = list()

bbasis[[1]] = create.bspline.basis(rangeval=c(0,365),breaks=knots,norder=1)
bbasis[[2]] = create.bspline.basis(rangeval=c(0,365),breaks=knots,norder=2)
bbasis[[3]] = create.bspline.basis(rangeval=c(0,365),breaks=knots,norder=3)
bbasis[[4]] = create.bspline.basis(rangeval=c(0,365),breaks=knots,norder=4)
bbasis[[5]] = create.bspline.basis(rangeval=c(0,365),breaks=knots,norder=5)
bbasis[[6]] = create.bspline.basis(rangeval=c(0,365),breaks=knots,norder=6)
quartz() # open a new graph window in R in a Mac laptop.
par(mfrow=c(1,1))
plot(bbasis[[4]])

for(i in 1:6){
	
	name1 = paste('spline_basis',i,'.png',sep='')
	name2 = paste('vancfit_spline',i,'.png',sep='')

	png(name2)	
 	plot(1:365,y,col=2,ylab='temperature',xlab='day',
		main='vancouver',cex.lab=1.5,cex.axis=1.5)
	abline(v=knots,lty=2,lwd=1)
	lines(Data2fd(y,1:365,bbasis[[i]]),col=4,lwd=2)
	dev.off()

	bvals = eval.basis(1:365,bbasis[[i]])

	png(name1)
	plot(1:365,bvals[,i+1],type='l',lty=1,lwd=2,col=2,
		xlab='day',ylab='basis',cex.lab=1.5,cex.axis=1.5)
	abline(v=knots,lty=2,lwd=1)
	dev.off()
}


