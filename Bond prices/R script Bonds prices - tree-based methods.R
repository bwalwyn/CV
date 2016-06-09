#Ben Walwyn
#Analytics project R
#04/09/14

#----------------------Part 1------------------------#
setwd("F:/Analytics")
install.packages("tree")
install.packages("MASS")
install.packages("randomForest")
install.packages("doParallel")
install.packages("foreach")
install.packages("ipred")
install.packages("gbm")
install.packages("DAAG")
library(DAAG)
library(gbm)
library(tree)
  library(MASS)
library(randomForest)
library(doParallel)
library(foreach)
library(ipred)
library(stats)
options(scipen=999)

bond <- read.csv("bond25.csv",header=T)
newdata<-data.frame(bond$trade_price,bond$trade_price_last1,bond$trade_price_last2,
                    bond$trade_price_last3,bond$trade_price_last4,bond$trade_price_last5,
                    bond$trade_price_last6,bond$trade_price_last7,bond$trade_price_last8,
                    bond$trade_price_last9,bond$trade_price_last10)


#--- Exploratory analysis ---#

par(mfrow=c(1,2))
#variables
median(bond$trade_price)
length(bond)
#histogram of response
hist(bond$trade_price,breaks=75,col="lightblue",freq=F,xlab='Trade Price',main='Histogram of trade price')
#trade series
plot(bond$trade_price,type='l',xlab='Trade number',ylab='Trade Price',main='Series of trade prices',col='black')
abline(a=quantile(bond$trade_price,0.025),b=0,col='red',lty=2)
abline(a=quantile(bond$trade_price,0.975),b=0,col='red',lty=2)
#correlation with prevuious trades
cor(newdata,bond$trade_price)
#plotting explanatory variables
par(mfrow=c(2,2))
par("oma"=c(0,0,2,0))
plot(bond$trade_price_last1,bond$trade_price)#strong relationship
plot(bond$trade_size,bond$trade_price)
plot(bond$current_coupon,bond$trade_price)#spreads out
plot(bond$time_to_maturity,bond$trade_price)#an extremely long term bond
mtext("Plots of relationships to trade price",outer=T,side=3,cex=1.5)
par(mfrow=c(1,1))
par("oma"=c(0,0,0,0))
#boxplots of relationships
par(mfrow=c(1,2))
par("oma"=c(0,0,2,0))
boxplot(bond$trade_price~as.factor(bond$is_callable),xlab='is_callable',ylab='trade_price')
boxplot(bond$trade_price~as.factor(bond$trade_type_last10),xlab='trade_type')
mtext('Boxplots of relationship of trade price to factor variables',outer=T,side=3,cex=1.5)
par(mfrow=c(1,1))
par("oma"=c(0,0,0,0))

### (1) Regression tree

full_tree <- tree(bond$trade_price~., data=bond,control=tree.control(nrow(bond),mindev=0))
summary(full_tree)
plot(full_tree)

stop_tree <- tree(bond$trade_price~., data=bond, control=tree.control(nrow(bond),mindev=0.004)) #defaults
summary(stop_tree)
plot(stop_tree)
text(stop_tree,cex=0.6)

#--- Cross-val on full tree
cross_val<-cv.tree(full_tree,K=100)
#plot the cross validation error against no. of terminal nodes and the tuning parameter
plot(cross_val$size,round(cross_val$dev),type='b',xlab="Number of terminal nodes",ylab="CV error",xlim=c(0,20),cex.lab=0.8,col="blue",cex.axis=0.8)
alpha<-round(cross_val$k)
axis(3, at=cross_val$size, lab=alpha, cex.axis=0.8)
mtext(expression(alpha), side=3,line=2, col="blue")
mtext(expression(bold("Regression tree cross-validation")),3,3,)

#optimum number of terminal nodes, minimzing overfitting
abline(v=9,col="red",lty=2)
#the selected value for the tuning parameter
best_alpha<-cross_val$k[cross_val$size==9]

#--- Tree Pruning
tree_prune <- prune.tree(full_tree, k=best_alpha+0.1)
#Interpretation
plot(tree_prune)                                    
text(tree_prune, cex=0.6,col="darkgreen")
mtext(expression(paste("Plot of regression tree tuned to ", alpha, "=4420")),3,line=2,col="blue")
summary(tree_prune)


### (2) Bagging

#default #trees is 500 

B <- 500 #bag samples and trees
core <- 5 #cores to be used for parallel

#Perform bagging in parallel
cl <- makeCluster(core)
registerDoParallel(cl)
getDoParWorkers()

t_bagpar<-system.time({ bag_dopar<-foreach(i=1:core, .combine="combine", .packages="randomForest") %dopar% {
    randomForest(bond$trade_price~., data=bond, mtry=57, ntree=B/core, importance=TRUE)
  }
})

mse_parallel <- mean((bag_dopar$predicted-bond$trade_price)^2)

stopCluster(cl)

#Bagging without parallel
t_bag<-system.time({ bag<-randomForest(bond$trade_price~., data=bond, mtry=57, ntree=500, importance=TRUE, na.action=na.exclude)})


### (3) Random forests

#default number of variables randomized in each permutation (deafult=19)
t_for20<-system.time({ forest_mdef<-randomForest(bond$trade_price~., data=bond, ntree=500, importance=TRUE, na.action=na.exclude)})
t_for5<-system.time({ forest_m5<-randomForest(bond$trade_price~., data=bond, mtry=5, ntree=500, importance=TRUE, na.action=na.exclude)})
t_for30<-system.time({ forest_m30<-randomForest(bond$trade_price~., data=bond, mtry=30, ntree=500, importance=TRUE, na.action=na.exclude)})

#parallel
cl2 <- makeCluster(core)
registerDoParallel(cl2)

t_for30par<-system.time({ forest_dopar<-foreach(i=1:core, .combine="combine", .packages="randomForest") %dopar% {
  randomForest(bond$trade_price~., data=bond,mtry=30, ntree=B/core, importance=TRUE)
}
})

stopCluster(cl2)

attributes(forest_dopar)

### (4) Boosting

t_boostd1<-system.time({boost_d1_l1=gbm(bond$trade_price~., data=bond, distribution='gaussian',n.trees=1000, interaction.depth=1, shrinkage=0.1, cv.folds=10, bag.fraction=1, n.minobsinnode=10, n.core=7)})
system.time({boost_d1_l2=gbm(bond$trade_price~., data=bond, distribution='gaussian',n.trees=1000, interaction.depth=1, shrinkage=0.01, cv.folds=10, bag.fraction=1, n.minobsinnode=10, n.core=7)})
system.time({boost_d1_l3=gbm(bond$trade_price~., data=bond, distribution='gaussian',n.trees=1000, interaction.depth=1, shrinkage=0.001, cv.folds=10, bag.fraction=1, n.minobsinnode=10, n.core=7)})
t_boostd2<-system.time({boost_d2_l1=gbm(bond$trade_price~., data=bond, distribution='gaussian',n.trees=1000, interaction.depth=2, shrinkage=0.1, cv.folds=10, bag.fraction=1, n.minobsinnode=10, n.core=7)})
system.time({boost_d2_l2=gbm(bond$trade_price~., data=bond, distribution='gaussian',n.trees=1000, interaction.depth=2, shrinkage=0.01, cv.folds=10, bag.fraction=1, n.minobsinnode=10, n.core=7)})
system.time({boost_d2_l3=gbm(bond$trade_price~., data=bond, distribution='gaussian',n.trees=1000, interaction.depth=2, shrinkage=0.001, cv.folds=10, bag.fraction=1, n.minobsinnode=10, n.core=7)})


#--- Tuning parameter selection ---#

#pruning -> alpha selected best using graph

#bag: how many trees to fit

#forests
#comparison plot for m
plot(forest_m30,col="darkgreen",main="Errors: Bagging and Random Forests")
lines(forest_mdef$mse,col="blue")
lines(forest_m5$mse,col='red')
lines(bag$mse,col='purple')
legend("topright",c("m=5","m=20 (default)","m=30",'m=57 (bag)'),lty=c(1,1,1,1),col=c("red","blue","darkgreen",'purple'))

forest_mdef$mse[500] #fastest reduction in error
forest_m5$mse[500]
forest_m30$mse[500] #best but slower 
bag$mse[500]

#boost
min(boost_d2_l1$cv.error)
par(mfrow=c(1,2))
par("oma"=c(1,1,2,0))
plot(boost_d1_l1$cv.error, type="l", ylab="CV error",ann=FALSE,lwd=2)
lines(boost_d1_l2$cv.error,col="blue",lwd=2)
lines(boost_d1_l3$cv.error,col="red",lwd=2)
abline(cve,0,lty=2,col="darkgreen")
mtext("d=1", side=3,line=0)

plot(boost_d2_l1$cv.error, type="l",ann=FALSE,lwd=2)
abline(cve,0,lty=2,col="darkgreen")
text(800, cve+5, "regression tree") 
lines(boost_d2_l2$cv.error,col="blue",lwd=2)
lines(boost_d2_l3$cv.error,col="red",lwd=2)
legend('topright', 
       legend=c(expression(lambda~"= 0.1"),expression(lambda~"= 0.01"),expression(lambda~"= 0.001")), 
       col=c("black","blue", "red"), 
       lwd=2,
       bty='n')
mtext("d=2", side=3,line=0)
mtext("Boosted Regression Tree", outer = TRUE,side=3, cex = 1.5)
mtext("Number of trees", outer = TRUE,side=1, cex = 1)
mtext("CV error", outer = TRUE,side=2, cex = 1)
par("oma"=c(0,0,0,0))
par(mfrow=c(1,1))

#at the endpoints there d=2 and lambda =0.1 is the best. 
#Possibly more trees need to be fitted

#--- Comparison of methods ---#



#OOB mse plot
#single tree(dashed line on cv error),bagging,random forest,boost

plot(bag,col="blue",main="Comparison of methods")
lines(forest_mdef$mse, col="darkgreen")
lines(boost_d2_l1$cv.error,col="red")
abline(v=which(boost_d2_l1$cv.error==min(boost_d2_l1$cv.error)),col="black",lty=2)
legend("topright", legend=c("Bagging", "Random Forest","Boosting"), col=c("blue", "darkgreen","red"), lwd=2,bty='n')

#Actual vs. predicted
#names
par(mfrow=c(2,2))
par("oma"=c(0,0,2,0))
plot(bond$trade_price,predict(tree_prune))
abline(1,1,col='red')
plot(bond$trade_price,predict(bag))
abline(1,1,col='red')
plot(bond$trade_price,predict(forest_mdef))
abline(1,1,col='red')
plot(bond$trade_price,predict(boost_d2_l1,n.trees=500))
abline(1,1,col='red')
mtext("Actual vs. predicted",side=3,outer=T)
par("oma"=c(0,0,0,0))
par(mfrow=c(1,1))
#boosted prediction expected to get close on the whole data set as the following tree is grown on the residuals, thereby reducing them.

#--- Interpretation ---#

#Variable Importance and partial plots

#bag
par("oma"=c(0,4,0,0))
colblue <- colorRampPalette(c("lightblue", "blue"))
barplot(sort(bag$importance[,2])[50:57],offset=200,cex.names=0.6,horiz=T,col=colblue(8),las=1,xpd=F,main="Bag Variable Importance",xlab="Node Imp")

par("oma"=c(0,0,2,0))
par(mfrow=c(1,2))
partialPlot(bag,bond,trade_price_last1,cex.main=0.8)
partialPlot(bag,bond,curve_based_price,cex.main=0.8)

partialPlot(bag,bond,trade_price_last10)

#random forest
colgreen <- colorRampPalette(c("green", "darkgreen"))
barplot(sort(forest_m30$importance[,2])[50:57],offset=200,cex.names=0.6,horiz=T,col=colgreen(8),las=1,xpd=F,main="RF Variable Importance",xlab="Node Imp")


partialPlot(forest_m30,bond,trade_price_last1,cex.main=0.8)
partialPlot(forest_m30,bond,curve_based_price,cex.main=0.8)
partialPlot(forest_m30,bond,trade_price_last10)

#boosting
colred <- colorRampPalette(c("orange", "red"))
barplot(sort(summary(boost_d2_l1)[1:8,2]),names.arg=summary(boost_d2_l1)[8:1,1],cex.names=0.6,las=1,horiz=T,col=colred(8),main="Boosted Variable Importance",xlab="Relative Importance")


plot(boost_d2_l1,i=c("trade_price_last1",'curve_based_price'))
plot(boost_d2_l1,i=c("trade_price_last1"))
plot(boost_d2_l1,i=c("curve_based_price"))
mtext(expression(bold('Partial plots for boosting')),side=3,outer=T)
par("oma"=c(0,0,0,0))
par(mfrow=c(1,1))
#most important variable are k asat traded prices and the j last curved prices, indicates an autoregressive model is suitable

#--- Parallel Computing and Time components ---#

#t's

#--- (b) Linear Model ---#


cl <- makeCluster(7)
registerDoParallel(cl)

ms<-foreach(i=1:10,.combine='c', .packages="DAAG") %dopar% {
  fit <- lm(bond.trade_price~.,data=newdata[,1:(i+1)])
  cv<-cv.lm(fit, df=newdata[,1:(i+1)],printit=T,plotit=F,m=10)
  attributes(cv)$ms
}
     
stopCluster(cl)

plot(seq(1:10),unlist(ms),type='b',main="Plot of CV error against autoregressive order",xlab='Order',ylab='cv.error',col='brown')
unlist(ms)

