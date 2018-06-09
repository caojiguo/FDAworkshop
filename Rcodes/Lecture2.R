install.packages("fda")
library(fda)
#provide directory of the RData
?setwd
setwd("/Users/cao/Dropbox/Teaching/FDA/SummerCourse2018/R")

# daily temperature in Vancouver
y = CanadianWeather$dailyAv[,'Vancouver','Temperature.C']
quartz() # To open a new graph windows in Mac computer
# windows() if you have a windows computer
mar.default <- c(5,4,4,2) + 0.1
# sets the bottom, left, top and right margins respectively of 
# the plot region in number of lines of text.
par(mar = c(8, 8, 4, 2))
plot(1:365,y,col=2,xlab='day',ylab='temperature',main='Vancouver',cex.axis=2,cex.lab=3,lwd=3,font = 3)
?plot
# First of all a polynomial basis


X = matrix(1:365,365,3)
for(i in 1:3){ X[,i] = (X[,i])^(i-1) }



for(i in 1:3){
	
	name1 = paste('monomial_basis',i,'.png',sep='')
	name2 = paste('vancfit_monomial',i,'.png',sep='')

	png(name1)
	matplot(1:365,X[,1:i],type='l',lwd=2,lty=1,
		xlab='day',ylab='basis',cex.lab=1.5,cex.axis=1.5)
	dev.off()

	yhat = X[,1:i]%*%solve(t(X[,1:i])%*%X[,1:i])%*%(t(X[,1:i])%*%y)
	
	png(name2)
	plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
		main='Vancouver',cex.axis=1.5)
	lines(1:365,yhat,lwd=2,col=4)
	dev.off()
}

# Now a fourier basis
?create.fourier.basis
# the number of fourier basis functions has to be odd;
daybasis365 <- create.fourier.basis(c(1, 365), nbasis = 5)
quartz() # open a new graph window in R in a Mac laptop.
par(mfrow=c(1,1))
plot(daybasis365,lwd=4)

Phi = eval.basis(1:365,daybasis365) # basis matrix
dim(Phi)

chat = solve(t(Phi)%*%Phi)%*%(t(Phi)%*%y)
chat
yhat = Phi%*%chat

quartz()
par(mar = c(8, 8, 4, 2))
plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
     main='Vancouver',cex.axis=1.5)
lines(1:365,yhat,lwd=2,col=4)

# We can evaluate the fitted curve at any time points in [0,365]
tstar = 3.3

phistar = eval.basis(tstar,daybasis365) # evaluate the basis functions at tstar
phistar

ystar = phistar%*%chat # the value of the fitted curve at tstar
ystar

# Now we want to look at a spline basis

knots = cumsum(c(1,31,28,31,30,31,30,31,31,30,31,30,30))
knots
bbasis1 = create.bspline.basis(rangeval=c(1,365),breaks=knots,norder=4)
length(knots) # the number of knots
norder = 4 # the order of polynomial functions = degree +1
length(knots)-2+norder # the number of basis functions
quartz() # open a new graph window in R in a Mac laptop.
par(mfrow=c(1,1))
plot(bbasis1,lwd=4)



Phi = eval.basis(1:365,bbasis1) # basis matrix
dim(Phi)

chat = solve(t(Phi)%*%Phi)%*%(t(Phi)%*%y)
chat
yhat = Phi%*%chat

quartz()
par(mar = c(8, 8, 4, 2))
plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
     main='Vancouver',cex.axis=1.5)
lines(1:365,yhat,lwd=2,col=4)

# We can evaluate the fitted curve at any time points in [0,365]
tstar = 3.3

phistar = eval.basis(tstar,bbasis1) # evaluate the basis functions at tstar
phistar

ystar = phistar%*%chat # the value of the fitted curve at tstar
ystar

# By-product - Estimate derivatives
dPhi = eval.basis(1:365,bbasis1,Lfdobj=1) # basis matrix
dim(dPhi)

dyhat = dPhi%*%chat
quartz()
par(mar = c(8, 8, 4, 2))
plot(1:365,dyhat,type='l',lwd=2,col=2,cex=1.5,xlab='Day',ylab='Derivative of Temperature',cex.lab=1.5,
     main='Vancouver',cex.axis=1.5)
dev.off()


# Try basis functions with different orders
for(i in 1:6){
  
  knots = cumsum(c(1,31,28,31,30,31,30,31,31,30,31,30,30))
  bbasis1 = create.bspline.basis(rangeval=c(1,365),breaks=knots,norder=i)
  name1 = paste('spline_basis',i,'.pdf',sep='')
  pdf(name1)
  par(mfrow=c(1,1))
  plot(bbasis1,lwd=4)
  dev.off()
  
  Phi = eval.basis(1:365,bbasis1) # basis matrix
  chat = solve(t(Phi)%*%Phi)%*%(t(Phi)%*%y)
  yhat = Phi%*%chat
  
  name2 = paste('vancfit_spline',i,'.pdf',sep='')
  
  pdf(name2)	
  par(mar = c(8, 8, 4, 2))
  plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
       main='Vancouver',cex.axis=1.5)
  lines(1:365,yhat,lwd=2,col=4)
  dev.off()
}





