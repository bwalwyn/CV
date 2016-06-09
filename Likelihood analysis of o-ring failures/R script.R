#ben walwyn
#likelihood assignment

library(stats4)
data<-rbind(c(6, 0, 66,  50,  1),
	c(6, 1, 70,  50,  2),
	c(6, 0, 69,  50,  3),
	c(6, 0, 68,  50,  4),
	c(6, 0, 67,  50,  5),
	c(6, 0, 72,  50,  6),
	c(6, 0, 73, 100,  7),
	c(6, 0, 70, 100,  8),
	c(6, 1, 57, 200,  9),
	c(6, 1, 63, 200, 10),
	c(6, 1, 70, 200, 11),
	c(6, 0, 78, 200, 12),
	c(6, 0, 67, 200, 13),
	c(6, 2, 53, 200, 14),
	c(6, 0, 67, 200, 15),
	c(6, 0, 75, 200, 16),
	c(6, 0, 70, 200, 17),
	c(6, 0, 81, 200, 18),
	c(6, 0, 76, 200, 19),
	c(6, 0, 79, 200, 20),
	c(6, 2, 75, 200, 21),
	c(6, 0, 76, 200, 22),
	c(6, 1, 58, 200, 23)
	)
	
col.names<-c('offset','number_distress','temp','psi','flight.no')	
colnames(data)<-col.names
m=6
ndistress<-data[,'number_distress']
temp<- data[,'temp']
psi<- data[,'psi']

#-----------------1.)----------------------
#Exploratory plots

#Temperature plots
plot(data[,'temp'],data[,'number_distress'],xlab='Temp.',ylab='Number of distresses',main='Scatter of distress by temperature')
boxplot(data[,'temp']~data[,'number_distress'],ylab='Temp.',xlab='Number of distresses',main='Boxplots of distresses by temperature')
#Psi plots
psi50<-sum(subset(data,data[,'psi']==50)[,'number_distress'])/6
psi100<-sum(subset(data,data[,'psi']==100)[,'number_distress'])/2
psi200<-sum(subset(data,data[,'psi']==200)[,'number_distress'])/15
barplot(c(psi50,psi100,psi200),main='Mean number of distress',names.arg=c('50','100',"200"),xlab='psi')
boxplot(data[,'number_distress']~data[,'psi'],xlab='psi',main='Distress for levels of pressure')

#-----------------2.)----------------------
#Write down the likilihood

pi.f=function(b0,b1) {
  exp(b0+b1*temp)/(1+exp(b0+b1*temp))
}

binomial.ll <- function(b0,b1) { 
  sum(dbinom(ndistress,m ,pi.f(b0,b1), log = TRUE))
}

binomial.ll<-function(b0,b1) {
	sum(ndistress*(b0+b1*temp)-6*log(1+exp(b0+b1*temp)))
}

#-----------------3.)----------------------
#Plot log-likelihood

#creating matrix to plot 2D surface
b0s <- seq(-2000,2000, length=100)
b1s <- seq(-30,30, length=100)
pp <- expand.grid(x = b0s, y = b1s)
z <- numeric(length(pp$x))
for(i in 1:length(pp$x))
{ z[i] <- binomial.ll(pp$x[i], pp$y[i])          
}
Z <- matrix(z,nrow=100)

#Plot Likelihood for two varying parameters
image(b0s, b1s, Z, col = heat.colors(30), 
      xlab = expression(paste(beta[0])), ylab = expression(paste(beta[1])))

contour(b0s, b1s, Z, add = F, nlevels = 30, ylab = expression(paste(beta[1])), 
        xlab = expression(paste(beta[0])))
points(bin.glm$coef[1],bin.glm$coef[2],pch=15)
image(b0s, b1s, Z, col=heat.colors(30), 
      xlab=expression(beta[0]), ylab=expression(beta[1]), 
      main=expression(paste('logL(', beta[0], ',', beta[1],';x,y)')))
points(bin.glm$coef[1],bin.glm$coef[2],pch=15)
persp(x=b0s,y=b1s, z=Z,zlim=range(c(-100000,1000),na.rm=T), theta =240, phi =25,
      border=NA,shade=0.6,xlab=expression(beta[0]),ylab=expression(beta[1]),zlab='L')

#-----------------4.)----------------------
#Find MLE using the glm function

bin.glm<-glm(cbind(ndistress,6-ndistress)~temp, family=binomial(link='logit'))
bin.glm$coefficients
J <- vcov(bin.glm)
J
summary(bin.glm,correlation=T)
confint(bin.glm)

#-----------------5.)----------------------
#Probability of failure at 31

#likelihood of phi, lambda
binomial.ll2 <- function(theta){
  sum((ndistress)*(log(theta[1]/(1-theta[1]))-theta[2]*(31-temp)) - 6*log(1+exp(log(theta[1]/(1-theta[1])) -theta[2]*(31-temp))) 
         +(log(factorial(m)/(factorial(m-ndistress)*factorial(ndistress)))))
}

#Plots for phi and lambda
phis <- seq(0,1,length=100)
lambdas <- seq(-30,30,length=100)
pp2 <- expand.grid(x = phis, y = lambdas)
z2 <- numeric(length(pp2$x))
for(i in 1:length(pp2$x))
{ z2[i] <- binomial.ll2(c(pp2$x[i], pp2$y[i]))          
}
Z2 <- matrix(z2,nrow=100)

contour(phis, lambdas, Z2, add = F, nlevels = 30, 
       	 	xlab = expression(paste(phi)), ylab = expression(paste(lambda)))
points(phi.mle$par[1],phi.mle$par[2],pch=15)
image(phis, lambdas, Z2, col=heat.colors(30), 
      xlab=expression(phi), ylab=expression(lambda), 
      main=expression(paste('logL(', phi, ',', lambda,';x,y)')))
points(phi.mle$par[1],phi.mle$par[2],pch=15)
persp(x=phis,y=lambdas, z=Z2,zlim=range(c(-100000,1000),na.rm=T), theta =50, 
      phi =25,border=NA,shade=1,xlab=expression(phi),ylab=expression(lambda),zlab='L')

#MLE opitmization
phi.mle <- optim(c(.5,-3),binomial.ll2,control=list(fnscale=-1))
phi.mle

#-----------------6.)----------------------
#Taylor expansion: partial derivatives and variance of transformed parameter

delta0 <- function(b0,b1){
  exp(b0+b1*31)/((1+exp(b0+b1*31))^2)
}
g.delta <- c(delta0(bin.glm$coef[1],bin.glm$coef[2]),31*delta0(bin.glm$coef[1],bin.glm$coef[2]))
phi.var <- t(g.delta)%*%J%*%g.delta

#a.)
phi.l <- phi.mle$par[1]-1.959964*sqrt(phi.var)
phi.u <- phi.mle$par[1]+1.959964*sqrt(phi.var)
phi.CI <- cbind(phi.l,phi.u)
colnames(phi.CI)=c('2.5%','97.5%')
phi.var
phi.CI

#b.)
phi.pred<-predict.glm(bin.glm, data.frame(temp=c(31)),type='response',se.fit=T)
phi.pred.confint<-c(phi.pred$fit-1.959964*phi.pred$se.fit, phi.pred$fit+1.959964*phi.pred$se.fit)
phi.pred$fit
phi.pred.confint

#-----------------7.)----------------------
#a.)
psi.model <- glm(cbind(ndistress,6-ndistress)~temp+psi, family=binomial(link='logit'))
temp.model <-glm(cbind(ndistress,6-ndistress)~temp, family=binomial(link='logit'))
anova(temp.model,psi.model,test='Chisq')
#b.)
anova(temp.model,test="Chisq")

#---------------- 8 and 9.)----------------------

binomial.ll3 <- function(phi,lambda){
  sum((ndistress)*(log(phi/(1-phi))-lambda*(31-temp)) - 6*log(1+exp(log(phi/(1-phi)) -lambda*(31-temp))) 
         +(log(factorial(m)/(factorial(m-ndistress)*factorial(ndistress)))))
}

k <- 500
prof.lik <- numeric(k)
phi.seq <- seq(0, 1, length = k)
for(i in 1:k) {
  phi <- phi.seq[i]
  out <- optimize(binomial.ll3,interval=c(-10,10), phi=phi, maximum=T)
  lambda.est <- out$maximum
  prof.lik[i] <- binomial.ll3(phi, lambda.est)
}

#MLE check
nbinomial.ll <- function(phi,lambda){ - binomial.ll3(phi,lambda)}
phi.lik.prof<-profile(mle(nbinomial.ll,start=list(phi=0.5,lambda=-2))
                      
f <- function(phi, maxloglik){
  out <- optimize(binomial.ll3, interval = c(-10,10), maximum = T, phi=phi)
  f <- 2 * (maxloglik - out$objective) - qchisq(.95, 1) 
  return(f)
}

LL <- uniroot(f, c(0.0001, phi.mle$par[1]), maxloglik = binomial.ll2(phi.mle$par))$root
UL <- uniroot(f, c(phi.mle$par[1], 0.99999), maxloglik = binomial.ll2(phi.mle$par))$root
c(LL, UL)


#Plot profiles and check to see if likelihood is quadratic
plot(phi.seq, prof.lik,type='l')
plot(phi.seq, exp(prof.lik)/max(exp(prof.lik[2:(k-1)])),type='l',ylab='L',xlab=expression(phi))
abline(h = exp(-nbinomial.ll(phi.mle$par[1], phi.mle$par[2]) - qchisq(.95, 1) / 2)/max(exp(prof.lik[2:(k-1)])), lty = 2)
abline(v=c(LL, UL))

