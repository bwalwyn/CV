#--- Mean BMI error bar plot ---#

#--- Mean BMI error bar plot ---#
means<- read.csv("national_means.csv",header=T)
upper<-means[,'mean']+1.96*means[,'se']
upper
lower<-means[,'mean']-1.96*means[,'se']
lower

means<-cbind(means,upper,lower)
year<-means[1:5,'year']
#male
par(mfrow=c(1,2))
par(oma=c(1,1,0,0))

plot(year, means[1:5,'mean'],
     ylim=c(min(lower[1:5])-0.1,max(upper[1:5])+0.1),
     pch=19, xlab="", ylab=""
)
arrows(year, lower[1:5], year, upper[1:5], length=0.05, angle=90, code=3)
mtext('Male',side=3,line=2)
#female
plot(year, means[6:10,'mean'],
     ylim=c(min(lower[6:10])-0.1,max(upper[6:10])+0.1),
     pch=19, xlab="", ylab=""
)
arrows(year, lower[6:10], year, upper[6:10], length=0.05, angle=90, code=3)
mtext('Female',side=3,line=2)
mtext('year',side=1,line=0,outer=T)
mtext('mean BMI',side=2,outer=T)
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))

#Linear future plot
x2=seq(0:20)-1
x=x2+19988
year<-means[1:5,'year']
#male
par(mfrow=c(1,2))
plot(year, means[1:5,'mean'],
     ylim=c(min(lower[1:5])-0.1,max(upper[1:5])+1), xlim=c(1998,2018),
     pch=19, xlab="Year", ylab="mean BMI")
mtext('Male',side=3,line=2)
polygon(c(x,rev(x)),c(lower[1]+(x2)*0.04507,rev(upper[1]+x2*0.1078))
        ,col='gray70',border=NA)
lines(x, means[1,'mean']+x2*0.07629)
points(year, means[1:5,'mean'],pch=19)


#female
plot(year, means[6:10,'mean'],
     ylim=c(min(lower[6:10])-0.1,max(upper[6:10])+1),
     pch=19, xlab="Year", ylab="mean BMI"
)
polygon(c(x,rev(x)),c(lower[6]+x2*0.07882,rev(upper[6]+x2*0.1659))
        ,col='gray70',border=NA)
lines(x, means[6,'mean']+x2*0.12)
points(year, means[6:10,'mean'],pch=19)
mtext('Female',side=3,line=2)
par(mfrow=c(1,1))
#---Overweight---#

par(mfrow=c(2,2))
overweight <- read.table("nat_overweight.txt")
sex<-c('f','m')
year<-c(rep(1998,2),rep(2003,2),rep(2008,2),rep(2010,2),rep(2012,2))
overweight<-cbind(overweight,rep(sex,5),year)
colnames(overweight)<-c('prev','se','upper','lower','sex','year')
f<-c(1,3,5,7,9)
m<-c(2,4,6,8,10)

#male plot
plot(year[m], overweight[m,'prev'],
     ylim=c(min(overweight[m,'lower'])-0.1,max(overweight[m,'upper'])+0.1),
     pch=19, xlab="Year", ylab="Overweight prevalence"
)
arrows(year[m], overweight[m,'lower'], year[m], overweight[m,'upper'], length=0.05, angle=90, code=3)
mtext('Male',side=3,line=2)
#female plot
plot(year[f], overweight[f,'prev'],
     ylim=c(min(overweight[f,'lower'])-0.1,max(overweight[f,'upper'])+0.1),
     pch=19, xlab="Year", ylab="Overweight prevalence"
)
arrows(year[f], overweight[f,'lower'], year[f], overweight[f,'upper'], length=0.05, angle=90, code=3)
mtext('Female',side=3,line=2)
#---Obese---#

obese <- read.table("nat_obese.txt")
sex<-c('f','m')
year<-c(rep(1998,2),rep(2003,2),rep(2008,2),rep(2010,2),rep(2012,2))
obese<-cbind(obese,rep(sex,5),year)
colnames(obese)<-c('prev','se','upper','lower','sex','year')
f<-c(1,3,5,7,9)
m<-c(2,4,6,8,10)

#male plot
plot(year[m], obese[m,'prev'],
     ylim=c(min(obese[m,'lower'])-0.1,max(obese[m,'upper'])+0.1),
     pch=19, xlab="Year", ylab="Obesity prevalence"
)
arrows(year[m], obese[m,'lower'], year[m], obese[m,'upper'], length=0.05, angle=90, code=3)

#female plot
plot(year[f], obese[f,'prev'],
     ylim=c(min(obese[f,'lower'])-0.1,max(obese[f,'upper'])+0.1),
     pch=19, xlab="Year", ylab="Obesity prevalence"
)
arrows(year[f], obese[f,'lower'], year[f], obese[f,'upper'], length=0.05, angle=90, code=3)
par(mfrow=c(1,1))

#--- Prevalence shaded plots ---#
par(mfrow=c(1,2))
#male
plot(year[m],obese[m,'prev'],ylim=c(0,1),type='l',ylab='Prevalence',xlab='Year')
polygon(c(year[m],2012,1998),c(overweight[m,'prev'],1,1),col='gray90')
polygon(c(year[m],rev(year[m])),c(obese[m,'prev'],rev(overweight[m,'prev'])),col='gray70')
polygon(c(year[m],2012,1998),c(obese[m,'prev'],0,0),col='gray50')
legend("topleft",c('Not Overweight','Overweight','Obese'), fill=c('gray90','gray70','gray50'),bty='n')
mtext("Male",side=3,line=1)

#female
plot(year[f],obese[f,'prev'],ylim=c(0,1),type='l',ylab='',xlab='Year')
polygon(c(year[f],2012,1998),c(overweight[f,'prev'],1,1),col='gray90')
polygon(c(year[f],rev(year[f])),c(obese[f,'prev'],rev(overweight[f,'prev'])),col='gray70')
polygon(c(year[f],2012,1998),c(obese[f,'prev'],0,0),col='gray50')
mtext("Female",side=3,line=1)
par(mfrow=c(1,1))

#--- Age plots for population groups (2012) ---#

#National

age_nat<-read.csv('age2012_nat.csv',header=T)
age=c(20,30,40,50,60,70)

plot(age,age_nat[1:6,'mean'],type='b',xlab='Age Category',
     ylab='mean BMI',ylim=c(21,33),xaxt='n')
axis(1, at=age, labels=c('15-24','25-34','35-44','45-54','55-64','65+'))
lines(age,age_nat[7:12,'mean'],type='b',pch=2)
legend("topright",c('male','female'),pch=c(1,2),bty='n')

#By race

age_by_pop<-read.csv("age2012.csv")
age_male<- age_by_pop[1:24,]
age_female<- age_by_pop[25:48,]

african<- c(1,5,9,13,17,21)
coloured<-african+1
a.i<-african+2
w<-african+3
age=c(20,30,40,50,60,70)

par(mfrow=c(1,2))
par(oma=c(1,1,0,0))
plot(age,age_male[w,'mean'],pch=0,ylim=c(22,33),ann=F,xaxt='n',type="n")
axis(1, at=age, labels=c('15-24','25-34','35-44','45-54','55-64','65+'))
#male
smoothingSpline1 = smooth.spline(age,age_male[african,'mean'], spar=0.4)
lines(smoothingSpline1,lty=1)
smoothingSpline2 = smooth.spline(age,age_male[coloured,'mean'], spar=0.4)
lines(smoothingSpline2,lty=2)
smoothingSpline3 = smooth.spline(age,age_male[a.i,'mean'], spar=0.4)
lines(smoothingSpline3,lty=3)
smoothingSpline4 = smooth.spline(age,age_male[w,'mean'], spar=0.4)
lines(smoothingSpline4,lty=4)
legend("topleft", c('African','Coloured','Asian/Indian','White')
       ,lty=c(1,2,3,4),bty='n')
mtext('Male',side=3,line=2)

#female
plot(age,age_female[w,'mean'],pch=0,ann=F,xaxt='n',type="n")
axis(1, at=age, labels=c('15-24','25-34','35-44','45-54','55-64','65+'))
smoothingSpline1 = smooth.spline(age,age_female[african,'mean'], spar=0.4)
lines(smoothingSpline1,lty=1)
smoothingSpline2 = smooth.spline(age,age_female[coloured,'mean'], spar=0.4)
lines(smoothingSpline2,lty=2)
smoothingSpline3 = smooth.spline(age,age_female[a.i,'mean'], spar=0.4)
lines(smoothingSpline3,lty=3)
smoothingSpline4 = smooth.spline(age,age_female[w,'mean'], spar=0.4)
lines(smoothingSpline4,lty=4)
mtext('Female',side=3,line=2)
mtext('Age Category',side=1,line=0,outer=T)
mtext('mean BMI',side=2,outer=T)
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
