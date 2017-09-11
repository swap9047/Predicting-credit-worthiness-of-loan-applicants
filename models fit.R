###LDA



num_var<-colnames(up_train)[sapply(up_train,is.numeric)]
temp<-rbind(up_train,test)
num_var
temp<-temp[,-c(1,23,25)]

levels(temp$purpose) <- make.names(levels(factor(temp$purpose)))
levels(temp$emp_length) <- make.names(levels(factor(temp$emp_length)))
levels(temp$joint_status) <- make.names(levels(factor(temp$joint_status)))
levels(temp$loan_status) <- make.names(levels(factor(temp$loan_status)))
levels(temp$verification_status) <- make.names(levels(factor(temp$verification_status)))
levels(temp$grade) <- make.names(levels(factor(temp$grade)))
levels(temp$addr_state) <- make.names(levels(factor(temp$addr_state)))


m <- model.matrix( 
  ~., 
  data = temp[,-1] 
)

colnames(m)[1]<-'Intercept'
m<-scale(m)
m[,1]<-1
colnames(m)<-gsub(' ','',fixed = T,colnames(m))

m[,24]<-ifelse(m[,24]>0,1,0)###0 is default
unique(m[,24])

train_m<-m[1:nrow(up_train),]
test_m<-m[-c(1:nrow(up_train)),]


n <- colnames(train_m)
f_lda <- as.formula(paste("loan_statusFully.Paid ~", paste(n[!n %in% c("loan_statusFully.Paid","Intercept","home_ownershipRENT")], collapse = " + ")))

lda1<-lda(f_lda,data=data.frame(train_m[,-21]))
pred_lda<-predict(lda1,data.frame(train_m))
head(pred_lda$posterior,20)
table('prediction'=pred_lda$class,'truth'=train_m[,24])
pred_lda_test<-predict(lda1,data.frame(test_m))
p<-table('prediction'=pred_lda_test$class,'truth'=test_m[,24])
sum(diag(p))/sum(p)
p<-table('prediction'=pred_lda$class,'truth'=train_m[,24])
sum(diag(p))/sum(p)
###QDA

qda1<-qda(f_lda,data=data.frame(train_m[,-21]))
pred_qda<-predict(qda1,data.frame(train_m))
table('prediction'=pred_qda$class,'truth'=train_m[,24])
pred_qda_test<-predict(qda1,data.frame(test_m))
p<-table('prediction'=pred_lda_test$class,'truth'=test_m[,24])
p
sum(diag(p))/sum(p)

####RF
err<-NULL
cverr<-NULL

trainerr<-NULL
testerr<-NULL

mtr<-2
ntr<-2
i<-1
mtry1<-NULL
ntree1<-NULL
k<-1

for(mtr in c(5,8,11,15,18,21,27,30))
{for (ntr in c( 300,500,800))
{ 
  
  model<-randomForest(loan_status~.-member_id-application_type-joint_status,data=up_train,mtry=mtr,ntree=ntr,importance=T)
  trainerr[k]<-mean(model$err.rate[,1])  
  pred_test<-predict(model,newdata=test)
  p<-table(pred_test,test[,'loan_status'])  
  testerr[k]<-sum(diag(p))/sum(p)
  mtry1[k]<-mtr
  ntree1[k]<-ntr
  print(paste(k,'rf done'))
  k<-k+1
}
}


save.image("output_rf.RData")

up_train$loan_status<-factor(up_train$loan_status,levels(up_train$loan_status)[c(2,1)])
model_rf_optimal<-randomForest(loan_status~.-member_id-application_type-joint_status,data=up_train,mtry=5,ntree=800,importance=T,keep.inbag=T)

is.na(up_train)
levels(up_train$loan_status)


trainerr<-mean(model$err.rate[,1])  
pred_test<-predict(model,newdata=test)
pred_test2<-predict(model,newdata=test,type='prob')
p<-table(pred_test,test[,'loan_status'])  
testacc<-sum(diag(p))/sum(p)

pred_rf_train<-model$votes
summary(up_train$loan_status)
####partial plot
ff = forestFloor(
  rf.fit = model_rf_optimal,       # mandatory
  X = up_train[,!colnames(up_train) %in% ('loan_status')],              # mandatory
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = FALSE  # takes no effect here when rfo$type="regression"
)


#plot partial functions of most important variables first
plot(ff,                       # forestFloor object
     plot_seq = 1:2,           # optional sequence of features to plot
     orderByImportance=TRUE    # if TRUE index sequence by importance, else by X column  
)
?fcol
Col=fcol(ff,orderByImportance=FALSE) #create color gradient see help(fcol)
plot(ff,col=Col,plot_GOF=TRUE, plot_seq = c(1,2,5)) 

rf.partial.ci(data_rf, train[,-1], "TOTNGELSPH", "BTUNGWTH", lci = 0.25, uci = 0.75, delta = FALSE)
??rf.partial.ci
varImpPlot(model,n.var=20)
partialPlot(model,up_train,x.var='credit_length_weeks',rug=T)
partialPlot(model,up_train,x.var='fico',rug=T)
partialPlot(model,up_train,x.var='credit_length_weeks',rug=T)
partialPlot(model,up_train,x.var='credit_length_weeks',rug=T)
partialPlot(model,up_train,x.var='credit_length_weeks',rug=T)
partialPlot(model,up_train,x.var='credit_length_weeks',rug=T)

####BART
options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_1<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE)
save.image("output_bart1.RData")

options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_2<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE,num_tree=250)
save.image("output_bart2.RData")

options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_3<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE,num_tree=100)
save.image("output_bart3.RData")


options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_4<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE,num_tree=130)
save.image("output_bart4.RData")





#SVM Parallel

library(ROCR)

pred<-prediction(df_train$rf_mtr5_ntr500,df_train$train)
perf<-performance(pred,"tpr","fpr")
plot(perf)

model_svm1 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=1, gamma=0.1, probability=TRUE)
svm.pred.train <- predict(model_svm1, up_train[,-c(1,23,25,13)])
svm.pred.train1 <- predict(model_svm1, up_train[,-c(1,23,25,13)],type='probability')
svm.pred.test  <- predict(model_svm1, test[,-c(1,23,25,13)])
save.image("output_svm1.RData")
ptrain1<-table(true=up_train$loan_status, pred=svm.pred.train)
acc1=sum(diag(ptrain1))/sum(ptrain1)
ptest1<-table(true=test$loan_status, pred=svm.pred.test)
acctest1=sum(diag(ptest1))/sum(ptest1)

model_svm2 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=10, gamma=1, probability=TRUE)
svm.pred.train2 <- predict(model_svm2, up_train[,-c(1,23,25,13)])
svm.pred.test2  <- predict(model_svm2, test[,-c(1,23,25,13)])
ptrain2<-table(true=up_train$loan_status, pred=svm.pred.train2)
acc2=sum(diag(ptrain2))/sum(ptrain2)
ptest2<-table(true=test$loan_status, pred=svm.pred.test2)
acctest2=sum(diag(ptest2))/sum(ptest2)
save.image("output_svm2.RData")


model_svm3 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=10, gamma=2, probability=TRUE)
svm.pred.train3 <- predict(model_svm3, up_train[,-c(1,23,25,13)])
svm.pred.test3  <- predict(model_svm3, test[,-c(1,23,25,13)])
ptrain3<-table(true=up_train$loan_status, pred=svm.pred.train3)
acc3=sum(diag(ptrain3))/sum(ptrain3)
ptest3<-table(true=test$loan_status, pred=svm.pred.test3)
acctest3=sum(diag(ptest3))/sum(ptest3)
save.image("output_svm3.RData")


model_svm4 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=100, gamma=2, probability=TRUE)
svm.pred.train4 <- predict(model_svm4, up_train[,-c(1,23,25,13)])
svm.pred.test4  <- predict(model_svm4, test[,-c(1,23,25,13)])
ptrain4<-table(true=up_train$loan_status, pred=svm.pred.train4)
acc4=sum(diag(ptrain4))/sum(ptrain4)
ptest4<-table(true=test$loan_status, pred=svm.pred.test4)
acctest4=sum(diag(ptest4))/sum(ptest4)
save.image("output_svm4.RData")


####logistic

library(glmnet)


x <- model.matrix(loan_status ~ 0 + . , data=up_train[,-c(1,23,25)])
model_logistic1 = cv.glmnet(x,y=factor(up_train$loan_status),
                            family='binomial',alpha=0,type.measure = 'class')
model_logistic2 = cv.glmnet(x,y=factor(up_train$loan_status),
                            family='binomial',alpha=0.5,type.measure = 'class')
model_logistic3 = cv.glmnet(x,y=factor(up_train$loan_status),
                            family='binomial',alpha=1,type.measure = 'class')
par(mfrow=c(1,3))
plot(model_logistic1);plot(model_logistic2);plot(model_logistic3)

pred_train_2<-predict(model_logistic2,newx=x,s=model_logistic1$lambda.min,type =  "response",alpha=0.5)
table(up_train$loan_status,pred_train_2<0.5)
pred_train_22<-predict(model_logistic2,newx=x,s=model_logistic1$lambda.min,type =  "class",alpha=0.5)### Default is predicted by model if probability is less than 0.5


newX=model.matrix(loan_status ~ 0 + . , data=test[,-c(1,23,25)])
pred1<-predict(model_logistic1,newx=newX,s=model_logistic1$lambda.min,type =  "response",alpha=0)
pred11<-predict(model_logistic1,newx=newX,s=model_logistic1$lambda.min,type =  "class",alpha=0)### Default is predicted by model if probability is less than 0.5

p1<-table(test$loan_status,pred11)
p1
sum(diag(p1))/sum(p1)
p1<-table(up_train$loan_status,predict(model_logistic1,newx=x,s=model_logistic1$lambda.min,type =  "class"))
p1
sum(diag(p1))/sum(p1)

pred2<-predict(model_logistic2,newx=newX,s=model_logistic2$lambda.min,type='response',alpha=0.5)
pred22<-predict(model_logistic2,newx=newX,s=model_logistic2$lambda.min,type='class',alpha=0.5)
p2<-table(test$loan_status,pred22)
p2
sum(diag(p))/sum(p)

pred3<-predict(model_logistic3,newx=newX,s=model_logistic3$lambda.min,alpha=1)
p3<-table(test$loan_status,pred3>0.5)
p3


###Neural Net

library(neuralnet)
num_var<-colnames(up_train)[sapply(up_train,is.numeric)]
temp<-rbind(up_train,test)
num_var
temp<-temp[,-c(1,23,25)]

levels(temp$purpose) <- make.names(levels(factor(temp$purpose)))
levels(temp$emp_length) <- make.names(levels(factor(temp$emp_length)))
levels(temp$joint_status) <- make.names(levels(factor(temp$joint_status)))
levels(temp$loan_status) <- make.names(levels(factor(temp$loan_status)))
levels(temp$verification_status) <- make.names(levels(factor(temp$verification_status)))
levels(temp$grade) <- make.names(levels(factor(temp$grade)))
levels(temp$addr_state) <- make.names(levels(factor(temp$addr_state)))


m <- model.matrix( 
  ~., 
  data = temp[,-1] 
)

colnames(m)[1]<-'Intercept'
m<-scale(m)
m[,1]<-1
colnames(m)<-gsub(' ','',fixed = T,colnames(m))

m[,24]<-ifelse(m[,24]>0,0,1)###1 is default
unique(m[,24])

train_m<-m[1:nrow(up_train),]
test_m<-m[-c(1:nrow(up_train)),]


n <- colnames(train_m)
f <- as.formula(paste("loan_statusFully.Paid ~", paste(n[!n %in% "loan_statusFully.Paid"], collapse = " + ")))
nn <- neuralnet(f,data=train_m,hidden=c(5,3),linear.output=T)


###Bagging
model1<-bagging(loan_status~.,data=up_train,nbagg = 100)
pred_train1 <-predict(model1,newdata=up_train)
pred_test1<-predict(model1,newdata=test)
tabletrain1<-table(pred_train1,up_train[,'loan_status'])
tabletest1<-table(pred_test1,test[,'loan_status'])
acc<-sum(diag(tabletrain1))/sum(tabletrain1)
acctest<-sum(diag(tabletest1))/sum(tabletest1)
save.image("output_bagging001.RData")


model2<-bagging(loan_status~.,data=up_train,nbagg = 500)
pred_train2 <-predict(model2,newdata=up_train)
pred_test2<-predict(model2,newdata=test)
tabletrain2<-table(pred_train2,up_train[,'loan_status'])
tabletest2<-table(pred_test2,test[,'loan_status'])
acc<-sum(diag(tabletrain2))/sum(tabletrain2)
acctest<-sum(diag(tabletest2))/sum(tabletest2)
save.image("output_bagging002.RData")


model3<-bagging(loan_status~.,data=up_train,nbagg = 700)
pred_train3 <-predict(model3,newdata=up_train)
pred_test3<-predict(model3,newdata=test)
tabletrain3<-table(pred_train3,up_train[,'loan_status'])
tabletest3<-table(pred_test3,test[,'loan_status'])
acc<-sum(diag(tabletrain3))/sum(tabletrain3)
acctest<-sum(diag(tabletest3))/sum(tabletest3)
save.image("output_bagging003.RData")


model4<-bagging(loan_status~.,data=up_train,nbagg = 1000)
pred_train4 <-predict(model4,newdata=up_train)
pred_test4<-predict(model4,newdata=test)
tabletrain4<-table(pred_train4,up_train[,'loan_status'])
tabletest4<-table(pred_test4,test[,'loan_status'])
acc<-sum(diag(tabletrain4))/sum(tabletrain4)
acctest<-sum(diag(tabletest4))/sum(tabletest4)
save.image("output_bagging004.RData")


###rpart

library(e1071)
library(rpart)
library(fancy)
rpart_tune<-tune.rpart(as.factor(loan_status)~.,minsplit=seq(10,30,60,100),cp=c(seq(0.01,0.2,0.04),0.4,0.75,1),maxdepth =2:5
                       ,data=up_train[,-c(1,23,25)])

rpart_model<-rpart(loan_status~.,data=up_train[,-c(1,23,25)],control=rpart.control(cp=0.01,maxdepth=4,minsplit=10))
fancyRpartPlot(rpart_model)
pred_rpart<-predict(rpart_model,newdata = up_train)
pred_rpart_test<-predict(rpart_model,newdata = test)
rm(list=setdiff(ls(), c("pred_rpart","pred_rpart_test")))

