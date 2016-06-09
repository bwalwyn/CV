library(RevoScaleR)
options(scipen=999)

save.image("D:/Analytics/bigtree.RData")

#create xdf
ad <- rxImport(inData="advert25.csv", outFile='ad.xdf')
#import created xdf
ad <- RxXdfData(file="ad.xdf")

#Recode
recode<-rxFactors(inData=ad,
                  factorInfo=c('Label','C1','C2','C3','C4','C5','C6','C7'),
                  sortLevels = T,
                  outFile='recodedAd.xdf',
                  overwrite=T  
                  )

recode <- RxXdfData(file="recodedAd.xdf")

# --- Summary statistics --- #

#Exploration
adI <- rxGetInfo(recode, getVarInfo=TRUE, numRows=6)
adI
head(recode)
nrow<-nrow(recode)

###'Label' - analysing the response
rxSummary(Label~., data=recode)
lab.tab <- rxCrossTabs(~Label,data=recode)
lab.prop <- unlist(lab.tab$counts)/nrow(recode)
lab.prop
rxHistogram(~Label, data=recode,main='Click-through counts')

###Predictors

#Integers
cor.matrix <- rxCor(~Label+I1+I2+I3+I4+I5+I6+I7+I8+I9+I10+I11+I12+I13, data=recode)
cor.matrix[2,3:15]


#99 quantiles
q99<-cbind(
  rxQuantile('I1',data=recode,probs=0.99), #numeric
  rxQuantile('I2',data=recode,probs=0.99),
  rxQuantile('I3',data=recode,probs=0.99),
  rxQuantile('I4',data=recode,probs=0.99),
  rxQuantile('I5',data=recode,probs=0.99),
  rxQuantile('I6',data=recode,probs=0.99),
  rxQuantile('I7',data=recode,probs=0.99),
  rxQuantile('I8',data=recode,probs=0.99),
  rxQuantile('I9',data=recode,probs=0.99),
  rxQuantile('I10',data=recode,probs=0.99),
  rxQuantile('I11',data=recode,probs=0.99),
  rxQuantile('I12',data=recode,probs=0.99),
  rxQuantile('I13',data=recode,probs=0.99)
)

#scaled means by click/no-click
scaled.means<-cbind(
  means.I1<-rxSummary(~Label:I1, data=recode)$categorical[[1]][,3]/q99[,1],
  means.I2<-rxSummary(~Label:I2, data=recode)$categorical[[1]][,3]/q99[,2],
  means.I3<-rxSummary(~Label:I3, data=recode)$categorical[[1]][,3]/q99[,3],
  means.I4<-rxSummary(~Label:I4, data=recode)$categorical[[1]][,3]/q99[,4],
  means.I5<-rxSummary(~Label:I5, data=recode)$categorical[[1]][,3]/q99[,5],
  means.I6<-rxSummary(~Label:I6, data=recode)$categorical[[1]][,3]/q99[,6],
  means.I7<-rxSummary(~Label:I7, data=recode)$categorical[[1]][,3]/q99[,7],
  means.I8<-rxSummary(~Label:I8, data=recode)$categorical[[1]][,3]/q99[,8],
  means.I9<-rxSummary(~Label:I9, data=recode)$categorical[[1]][,3]/q99[,9],
  means.I10<-rxSummary(~Label:I10, data=recode)$categorical[[1]][,3]/q99[,10],
  means.I11<-rxSummary(~Label:I11, data=recode)$categorical[[1]][,3]/q99[,11],
  means.I12<-rxSummary(~Label:I12, data=recode)$categorical[[1]][,3]/q99[,12],
  means.I13<-rxSummary(~Label:I13, data=recode)$categorical[[1]][,3]/q99[,13]
)
par(mfrow=c(1,2))
#correlation plot
plot(seq(1:13),cor.matrix[2,3:15],type='b',xlab='I<x>',ylab='Correlation',cex.main=0.8)
abline(a=0,b=0,col='red',lty=2)
#plot scaled means
plot(seq(1:13),-(scaled.means[1,]-scaled.means[2,]),type='b',xlab='I<x>',
     ylab='Scaled Mean Differential')
abline(a=0,b=0,col='red',lty=2)

#Only plot relevant histograms 
rxHistogram(~I5|Label, data=recode,startVal=0,endVal=q99[,5]) # more
rxHistogram(~I6|Label, data=recode,startVal=0,endVal=q99[,6]) # more
rxHistogram(~I7|Label, data=recode,startVal=0,endVal=q99[,7]) # more
rxHistogram(~I10|Label, data=recode,startVal=0,endVal=q99[,10]) # interesting
rxHistogram(~I11|Label, data=recode,startVal=0,endVal=q99[,11]) #more non clicks for 0
rxHistogram(~I13|Label, data=recode,startVal=0,endVal=q99[,13]) # =0 more clicks

sum.I5<-rxSummary(~Label:I5,data=recode)
sum.I10<-rxSummary(~Label:F(I10), data=recode)
sum.I10


#Categorical
tab.c2$counts[[1]]
tab.c2.scaled

par(mfrow=c(3,3))
tab.c1<-rxCrossTabs(~Label:C1, data=recode)
tab.c1.scaled<-rbind(tab.c1$counts[[1]][1,]/sum(tab.c1$counts[[1]][1,]),tab.c1$counts[[1]][2,]/sum(tab.c1$counts[[1]][2,]))
plot(seq(1:22),tab.c1.scaled[1,],type='b',col='darkgreen',xlab='C1',ylab='Density')
lines(seq(1:22),tab.c1.scaled[2,],col='red',lty=2)
which.c1<-which(tab.c1.scaled[1,]>0.1, arr.ind=TRUE)
text(which.c1,subset(tab.c1.scaled[1,],tab.c1.scaled[1,]>0.1)+0.015,labels=colnames(tab.c1.scaled)[which.c1])

tab.c2<-rxCrossTabs(~Label:C2, data=recode)
tab.c2.scaled<-rbind(tab.c2$counts[[1]][1,]/sum(tab.c2$counts[[1]][1,]),tab.c2$counts[[1]][2,]/sum(tab.c2$counts[[1]][2,]))
plot(seq(1:3),tab.c2.scaled[1,],type='b',col='darkgreen',xlab='c2',ylab='Density')
lines(seq(1:3),tab.c2.scaled[2,],col='red',lty=2)
which.c2<-which(tab.c2.scaled[1,]>0.1, arr.ind=TRUE)
text(which.c2,subset(tab.c2.scaled[1,],tab.c2.scaled[1,]>0.1)+0.015,labels=colnames(tab.c2.scaled)[which.c2])

tab.c3<-rxCrossTabs(~Label:C3, data=recode)
tab.c3.scaled<-rbind(tab.c3$counts[[1]][1,]/sum(tab.c3$counts[[1]][1,]),tab.c3$counts[[1]][2,]/sum(tab.c3$counts[[1]][2,]))
plot(seq(1:27),tab.c3.scaled[1,],type='b',col='darkgreen',xlab='c3',ylab='Density')
lines(seq(1:27),tab.c3.scaled[2,],col='red',lty=2)
which.c3<-which(tab.c3.scaled[1,]>0.1, arr.ind=TRUE)
text(which.c3,subset(tab.c3.scaled[1,],tab.c3.scaled[1,]>0.1)+0.015,labels=colnames(tab.c3.scaled)[which.c3])

tab.c4<-rxCrossTabs(~Label:C4, data=recode)
tab.c4.scaled<-rbind(tab.c4$counts[[1]][1,]/sum(tab.c4$counts[[1]][1,]),tab.c4$counts[[1]][2,]/sum(tab.c4$counts[[1]][2,]))
plot(seq(1:10),tab.c4.scaled[1,],type='b',col='darkgreen',xlab='c4',ylab='Density')
lines(seq(1:10),tab.c4.scaled[2,],col='red',lty=2)
which.c4<-which(tab.c4.scaled[1,]>0.1, arr.ind=TRUE)
text(which.c4,subset(tab.c4.scaled[1,],tab.c4.scaled[1,]>0.1)+0.03,labels=colnames(tab.c4.scaled)[which.c4])

tab.c5<-rxCrossTabs(~Label:C5, data=recode)
tab.c5.scaled<-rbind(tab.c5$counts[[1]][1,]/sum(tab.c5$counts[[1]][1,]),tab.c5$counts[[1]][2,]/sum(tab.c5$counts[[1]][2,]))
plot(seq(1:3),tab.c5.scaled[1,],type='b',col='darkgreen',xlab='c5',ylab='Density',ylim=c(0.3,0.5))
lines(seq(1:3),tab.c5.scaled[2,],col='red',lty=2)
which.c5<-which(tab.c5.scaled[1,]>0.1, arr.ind=TRUE)
text(which.c5,subset(tab.c5.scaled[1,],tab.c5.scaled[1,]>0.1)+0.015,labels=colnames(tab.c5.scaled)[which.c5])

tab.c6<-rxCrossTabs(~Label:C6, data=recode)
tab.c6.scaled<-rbind(tab.c6$counts[[1]][1,]/sum(tab.c6$counts[[1]][1,]),tab.c6$counts[[1]][2,]/sum(tab.c6$counts[[1]][2,]))
plot(seq(1:17),tab.c6.scaled[1,],type='b',col='darkgreen',xlab='c6',ylab='Density')
lines(seq(1:17),tab.c6.scaled[2,],col='red',lty=2)
which.c6<-which(tab.c6.scaled[1,]>0.1, arr.ind=TRUE)
text(which.c6,subset(tab.c6.scaled[1,],tab.c6.scaled[1,]>0.1)+0.015,labels=colnames(tab.c6.scaled)[which.c6])

tab.c7<-rxCrossTabs(~Label:C7, data=recode)
tab.c7.scaled<-rbind(tab.c7$counts[[1]][1,]/sum(tab.c7$counts[[1]][1,]),tab.c7$counts[[1]][2,]/sum(tab.c7$counts[[1]][2,]))
plot(seq(1:15),tab.c7.scaled[1,],type='b',col='darkgreen',xlab='c7',ylab='Density')
lines(seq(1:15),tab.c7.scaled[2,],col='red',lty=2)
which.c7<-which(tab.c7.scaled[1,]>0.1, arr.ind=TRUE)
text(which.c7,subset(tab.c7.scaled[1,],tab.c7.scaled[1,]>0.1)-0.015,labels=colnames(tab.c7.scaled)[which.c7])
par('oma'=c(0,0,2,0))
mtext(expression(bold('Relative click counts for categorical factors')),side=3,line=0,outer=T,cex=1.2)
par(mfrow=c(1,1))
par('oma'=c(0,0,0,0))

cube.c1 <- rxCube(~Label:C4,data=recode)
rxLinePlot(formula=Counts~Label|C4,data=rxResultsDF(cube.c1))

# --- Classification Tree --- #

adClassTree <- rxDTree(Label ~ I1+I2+I3+I4+I5+I6+I7+I8+I9+I10+I11+I12+I13+C1+C2+C3+C4+C5+C6+C7, 
                       data=recode, 
                       method="class", 
                       xVal=10,
                       cp=0,
                       maxDepth=10,
                       parms=list(loss=c(0,3,1,0))
                      )

adClassTree.noloss <- rxDTree(Label ~ I1+I2+I3+I4+I5+I6+I7+I8+I9+I10+I11+I12+I13+C1+C2+C3+C4+C5+C6+C7, 
                       data=recode, 
                       method="class", 
                       xVal=10,
                       cp=0,
                       maxDepth=10
)

#Cross-validation plots
par(mfrow=c(1,1))
cptable.noloss <- adClassTree.noloss$cptable
plot(cptable.noloss[,'nsplit'],cptable.noloss[,'xerror'],type='l',xlab='Number of splits',ylab='CV error')

par(mfrow=c(1,2))
par('oma'=c(1,1,0,0))
cptable <- adClassTree$cptable
plot(cptable[,'nsplit'],cptable[,'xerror'],type='l',xlab=,ylab='CV error',ann=F)
abline(v=c(22),col='red',lty=2)
plot(cptable[1:20,'nsplit'],cptable[1:20,'xerror'],type='b',xlab='Number of splits',ann=FALSE)
abline(v=c(1,3,22),col='red',lty=2)
mtext('Number of splits',outer=T,side=1)
mtext('CV error',outer=T,side=2)
par('oma'=c(0,0,0,0))
par(mfrow=c(1,1))

#Tree pruning
cp.no<-22
adClassPruned <- prune(adClassTree,cp=cptable[which(cptable[,'nsplit']==cp.no, arr.ind=TRUE),'CP'])
adClassPruned
adClassPrune.noloss <- prune(adClassTree.noloss,cp=cptable.noloss[which(cptable[,'nsplit']==55, arr.ind=TRUE),'CP'])
adClassPrune.noloss

# Predictions:
tree.pred <- rxPredict(adClassPruned, data=recode, outData="predictions.xdf", type="class", writeModelVars=TRUE, overwrite=TRUE)
tree.pred.noloss <- rxPredict(adClassPrune.noloss, data=recode,outData="predictions_noloss.xdf", type="class", writeModelVars=TRUE, overwrite=TRUE)

#variable name for prediciton Label_pred
rxGetInfo(tree.pred, getVarInfo=TRUE)
rxGetInfo(tree.pred.noloss, getVarInfo=TRUE)
#Percentages
miss.class <- rxCrossTabs(~Label:Label_Pred, data=tree.pred)
miss.class.mat<- miss.class$counts[[1]]
miss.class.mat/rowSums(miss.class.mat)

miss.class.noloss <- rxCrossTabs(~Label:Label_Pred, data=tree.pred.noloss)
miss.class.mat.noloss<- miss.class.noloss$counts[[1]]
miss.class.mat.noloss/rowSums(miss.class.mat.noloss)

#Tree plot for loss
plot(rxAddInheritance(adClassPruned))
text(rxAddInheritance(adClassPruned), cex=0.3, pretty=0)

library(RevoTreeView)
plot(createTreeView(adClassPruned))

  ###Variable importance 
colblue <- colorRampPalette(c('cyan', "blue"))
var.imp <- adClassPruned$variable.importance
barplot(sort(var.imp[which(var.imp>3000)]),horiz=T,las=1,xpd=F,col=colblue(10),
        main='Variable Importance',xlab='Reduction in Gini impurity',ylab='Variable Name')

#--- Random Forest ---#

adRf <- rxDForest(Label ~ I1+I2+I3+I4+I5+I6+I7+I8+I9+I10+I11+I12+I13+C1+C2+C3+C4+C5+C6+C7,
                  data=recode, 
                  outFile="adRandomForest.xdf", 
                  method="class", 
                  mTry=5, 
                  nTree = 500,
                  parms=list(loss=c(0,3,1,0)),
                  importance=TRUE,
)

OOB.predrf <- adRf$confusion
rf.confusion <- OOB.predrf[,1:2]/rowSums(OOB.predrf[,1:2])
rf.confusion
#Plots
plot(adRf,main='')
rxVarImpPlot(adRf)

colgreen <- colorRampPalette(c("green", "darkgreen"))
barplot(sort(rxVarImpPlot(adRf)[1:20])[11:20],horiz=T,las=1,xpd=F,col=colgreen(10),
        main='Variable Importance',xlab='Reduction in Gini impurity',ylab='Variable Name')

# Predictions:
tree.predrf <- rxPredict(adRf, data=recode, outData="predictionsRF.xdf", 
                         type="class", writeModelVars=TRUE, overwrite=TRUE)
rxGetInfo(tree.predrf, getVarInfo=TRUE)

miss.classrf <- rxCrossTabs(~Label:Label_Pred, data=tree.predrf)
miss.classrf.mat/rowSums(miss.classrf.mat)
