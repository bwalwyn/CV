setwd("F:/Analytics")
bond <- read.csv("bond25.csv",header=T)

library(DAAG)
library(doParallel)

newdata<-data.frame(bond$trade_price,bond$trade_price_last1,bond$trade_price_last2,
                    bond$trade_price_last3,bond$trade_price_last4,bond$trade_price_last5,
                    bond$trade_price_last6,bond$trade_price_last7,bond$trade_price_last8,
                    bond$trade_price_last9,bond$trade_price_last10)

cl <- makeCluster(7)
registerDoParallel(cl)

ms<-foreach(i=1:10, .packages="DAAG") %dopar% {
  fit <- lm(bond.trade_price~.,data=newdata[,1:(i+1)])
  cv<-cv.lm(fit, df=newdata[,1:(i+1)],printit=T,plotit=F,m=10)
  attributes(cv)$ms
}

stopCluster(cl)


#Shuffle data
newdata<-bond[sample(nrow(bond)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(bond)),breaks=10,labels=FALSE)

pred <- matrix()
#Perform 10 fold cross validation
foreach(i = 1:10) %dopar% {
  #Segment data into fold 
  fold_index <- which(folds==1,arr.ind=TRUE)
  test_data <- newdata[fold_index, ]
  train_data <- newdata[-fold_index, ]
  #fit train data to linear model
  fit <- lm(trade_price~trade_price_last1,data=train_data)
  p<-predict.lm(fit,test_data$trade_pricelast1)
}
p