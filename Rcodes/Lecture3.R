library('fda')

y = CanadianWeather$dailyAv[,'Vancouver','Precipitation.mm']

for(i in c(7,13,21,31,51,101,181)){
	
  daybasis365 <- create.fourier.basis(c(0, 365),i)
  bvals = eval.basis(1:365,daybasis365)
  
  # Evaluate the first derivative of the basis functions at 1,2,...,365
  d1bvals = eval.basis(1:365,daybasis365,Lfdobj=1)
  
  # Evaluate the second derivative of the basis functions at 1,2,...,365
  d2bvals = eval.basis(1:365,daybasis365,Lfdobj=2)
  
	X = bvals

	yhat = X%*%solve(t(X)%*%X)%*%(t(X)%*%y)
	# Estimate the second derivative of f(t)
	d2yhat = d2bvals%*%solve(t(X)%*%X)%*%(t(X)%*%y)

	quartz()
	par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
	title = paste(i,' Basis Functions',sep='')
	plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='precipitation',cex.lab=1.5,
		main=title,cex.axis=1.5)
	lines(1:365,yhat,lwd=2,col=4)
	quartz()
	par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
	plot(1:365,d2yhat,col=4,type='l',lwd=2,cex.lab=1.5,cex.axis=1.5,
		ylab = 'D2 precipitation',xlab='day',main=title)
	readline(prompt="Press [enter] to continue")
}


# Now a bias-variance simulation:

knots = cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))
bbasis = create.bspline.basis(rangeval=c(0,365),breaks=knots,norder=6)

tbvals = eval.basis(1:365,bbasis)

yhat = tbvals%*%solve( t(tbvals)%*%tbvals )%*%t(tbvals)%*%y

err = y-yhat

nsimu = 100
SimRes = array(0,c(365,nsimu,50))

for(i in 1:nsimu){
	ty = yhat + err[sample(365)]
	for(j in 1:50){
		tbvals = bvals[,1:(2*j+1)]
		SimRes[,i,j] = tbvals%*%solve( t(tbvals)%*%tbvals )%*%t(tbvals)%*%ty
	}
}

Vars = apply(apply(SimRes,c(1,3),var),2,mean)
Bias = apply( (matrix(yhat,365,50)-apply(SimRes,c(1,3),mean))^2,2,mean)
MSE = apply( (array(yhat,c(365,nsimu,50))-SimRes)^2,3,mean)

quartz() # open a new graph window in R in a Mac laptop.
stats = cbind(Vars,Bias,MSE)
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
matplot(1+2*(2:10),stats[2:10,],type='l',lwd=3,xlab='Number of Basis Functions',ylab='',cex.lab=1.5,cex.axis=1.5)
legend(17.2,0.11,c('Variance','Bias','MSE'),col=c(1,2,3),lty=1:3,lwd=3)


# I'll choose 13 basis functions and look at standard errors

daybasis365 <- create.fourier.basis(c(0, 365),13)
bvals = eval.basis(1:365,daybasis365)
tbvals = bvals
S = tbvals%*%solve( t(tbvals)%*%tbvals )%*%t(tbvals)
yhat = S%*%y
install.packages('psych')
library(psych)
tr(S)
err = (y-yhat)
sig = sum( (y-yhat)^2 )/(365-13)

# Variance-covariance matrix of fhat
Sig = sig*S%*%t(S)

# 2 * standard error of fitted curve
off = 2*sqrt(diag(Sig))

quartz()
plot(1:365,y,col=2,xlab='day',ylab='precipitation',cex.lab=1.5,cex.axis=1.5,main='Vancouver')
lines(1:365,yhat,col=4,lwd=2)
lines(1:365,yhat+off,col=4,lwd=2,lty=2)
lines(1:365,yhat-off,col=4,lwd=2,lty=2)

