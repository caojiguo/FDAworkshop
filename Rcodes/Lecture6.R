library(fda)
#provide directory of the RData
setwd("/Users/cao/Dropbox/Teaching/FDA/SummerCourse2018/R")

daybasis365 <- create.fourier.basis(c(0, 365), 365)

# define the roughness penalty on the fitted function
# Lfdobj=int2Lfd(2) is to use the second derivative to 
# define the rought penalty

fdParobj = fdPar(daybasis365,Lfdobj=int2Lfd(2),lambda=1e4)
y = CanadianWeather$dailyAv[,'Vancouver','Temperature.C']

# Do the penalized spline smoothing
# This function returns a fd object
precfd = smooth.basis(1:365,y,fdParobj)

quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
# plot the data
plot(1:365,y,col=2,xlab='day',ylab='precipitation',main='Vancouver',
	cex.lab=1.5,cex.axis=1.5)
# plot the fitted curve
lines(precfd$fd,lwd=2,col=4)

# basis matrix
bvals = eval.basis(1:365,daybasis365)
# chat = y2cMap%*% y
# infMat is the smoothing matrix
infMat = bvals%*%precfd$y2cMap
dim(infMat)
matplot(infMat[,c(20,180,300)],type='l',ylab='influence',xlab='day',
	main='Observations 20, 180, 300',cex.lab=1.5,cex.axis=1.5,lwd=2)

# define the harmonic acceleration differential operator
# L = D^3 f(t) + 0 * D^2 f(t) + w^2 * D^f(t) + 0 * f(t)
# c(0,(2*pi/365)^2,0) is the vector of coefficients to 
# the lower derivatives
harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))

# evaluate of the fitted curve under the harmonic acceleration differential operator
harmfdvals = eval.fd(1:365,precfd$fd,harmLfd)

# The y-axis is Lf(t)
plot(1:365,harmfdvals,type='l',lwd=2,col=4,xlab='day',ylab='Harmonic Acceleration',
	main ='Vancouver',cex.lab=1.5,cex.axis=1.5)


# Show how gcv varies with the values of lambda;

lambdas = exp(seq(-1,22,by=1))

gcvs = rep(0,length(lambdas))
dfs = rep(0,length(lambdas))
ocvs = rep(0,length(lambdas))
errs = rep(0,length(lambdas)) # SSE

for(i in 1:length(lambdas)){
	fdParobj = fdPar(daybasis365,Lfdobj=int2Lfd(2),lambda=lambdas[i])
	precfd = smooth.basis(1:365,y,fdParobj)

	dfs[i] = precfd$df
	gcvs[i] = precfd$gcv
	errs[i] = precfd$SSE

	# this is the smoothing matrix
	hatmat = bvals%*%precfd$y2cMap

	ocvs[i] = mean( (y-eval.fd(1:365,precfd$fd))^2/(1-diag(hatmat))^2 )


	quartz()
	par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
	title = paste('ln(lambda) = ',log(lambdas[i]),sep='')
	plot(1:365,y,col=2,xlab='day',ylab='Precipitation',main=title,
		cex.lab=1.5,cex.axis=1.5)
	lines(precfd$fd,lwd=2,col=4)
	readline(prompt="Press [enter] to continue")
}


par(mfrow=c(2,2))
l = log(lambdas)
plot(l,dfs,type='l',col=2,xlab='log lambda',ylab='df',cex.lab=1.5,cex.axis=1.5)
plot(l,errs,type='l',col=2,xlab='log lambda',ylab='SSE',cex.lab=1.5,cex.axis=1.5)
plot(l,ocvs,type='l',col=2,xlab='log lambda',ylab='OCV',cex.lab=1.5,cex.axis=1.5)
plot(l,gcvs,type='l',col=2,xlab='log lambda',ylab='GCV',cex.lab=1.5,cex.axis=1.5)

gcvs
i = 10
fdParobj = fdPar(daybasis365,Lfdobj=int2Lfd(2),lambda=lambdas[i])
precfd = smooth.basis(1:365,y,fdParobj)

par(mfrow=c(1,1))
plot(1:365,y,col=2,xlab='day',ylab='precipitation',cex.lab=1.5,cex.axis=1.5,
	main="Vancouver")
lines(precfd$fd,col=4,lwd=2)


# fitted value
yhat = eval.fd(1:365,precfd$fd)
# estimate sigma
ny = length(y)
sig2 = precfd$SSE/(ny-precfd$df)
### Now a couple of probes

y2cMap = precfd$y2cMap

dfvals = eval.fd(1:365,precfd$fd,1)
dbvals = eval.basis(1:365,daybasis365,Lfdobj=1)



dfvar = diag(dbvals%*%y2cMap%*%t(y2cMap)%*%t(dbvals))*sig2

quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(1:365,dfvals,type='l',col=4,lwd=2,xlab='day',ylab='D precipitation',
	cex.lab=1.5,cex.axis=1.5,ylim=c(-0.2,0.2))
lines(1:365,dfvals+2*sqrt(dfvar),lty=2,lwd=2,col=4)
lines(1:365,dfvals-2*sqrt(dfvar),lty=2,lwd=2,col=4)
abline(h=0,col=2)







