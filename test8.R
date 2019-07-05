rm(list=ls())
gc()
setwd('D:/mal2016/bin/32/TCGA')
library('randomForest')
library(InformationValue)
library(pROC)
library(ROSE)
library(DMwR)
library(stringr)
library(sampling)
species_abundance<-data.frame(read.table('D:/mal2016/bin/32/TCGA/minus_delta3.txt',sep='\t'))
tumor<-species_abundance[c(1:191),]
health<-species_abundance[c(192:425),]

importance<- matrix(0,250,4)
sepsis.pred.scores<- list()
sepsis.pred.labels<- list()
auc<-rep(0,1000)
for(bs in 1:1000)    {
  ##bs=1
  
  ind1<- sample(nrow(tumor),7/10*nrow(tumor))
  train_data1 = tumor[ind1,]
  test_data1 = tumor[-ind1,]
  #index2<- sample(x=2,size=nrow(health),replace=TRUE,prob=c(0.7,0.3))
  #train_data2 = tumor[index2==1,]
  #test_data2 = tumor[index2==2,]
  ind2<- sample(nrow(health),7/10*nrow(health))
  train_data2 = health[ind2,]
  test_data2 = health[-ind2,]

  trainning_data<-rbind(train_data1,train_data2) %ÐÐºÏ²¢
  testing_data<-rbind(test_data1,test_data2)
  
  
  trainlabel=rep(1,nrow(trainning_data))
  trainlabel[1:nrow(train_data1)]=0
  trainlabel=as.factor(trainlabel)
  
  
  testlabel=rep(1,nrow(testing_data))
  testlabel[1:nrow(test_data1)]=0
  testlabel=as.factor(testlabel)
  
  rf <- randomForest(trainlabel ~., data = trainning_data,ntree =10000,importance=TRUE,proximity=TRUE)
  rf.importance<- data.frame(rf$importance)
  predict.scores<-predict(rf,newdata=testing_data,type="prob")   
  sepsis.pred.scores<-append(sepsis.pred.scores,list(predict.scores[,2]))
  sepsis.pred.labels<-append(sepsis.pred.labels,list(testlabel))
  auc[bs]<-(roc(testlabel,predict.scores[,2]))$auc
  cat(bs,auc[bs],'\n')
}
print(mean(auc))


