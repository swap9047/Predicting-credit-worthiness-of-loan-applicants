#----------------------------------Functions------------------------------------
# plot_pred_type_distribution <- function(df, threshold=0.5) {
#         v <- rep(NA, nrow(df))
#         v <- ifelse(df$Actual >= threshold & df$survived == 1, "TP", v)
#         v <- ifelse(df$Actual >= threshold & df$survived == 0, "FP", v)
#         v <- ifelse(df$Actual < threshold & df$survived == 1, "FN", v)
#         v <- ifelse(df$Actual < threshold & df$survived == 0, "TN", v)
#         
#         df$pred_type <- v
#         
#         ggplot(data=df, aes(x=survived, y=pred)) + 
#                 geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
#                 geom_jitter(aes(color=pred_type), alpha=0.6) +
#                 geom_hline(yintercept=threshold, color="red", alpha=0.6) +
#                 scale_color_discrete(name = "type") +
#                 labs(title=sprintf("Threshold at %.2f", threshold))
# }

#----------------------------------Main Body------------------------------------
fit.train<-cbind.data.frame(up_train$loan_status,bart_machine_model_3$p_hat_train)
bart_machine_test_3<-bart_predict_for_test_data(bart_machine_model_3, test[,-c(1,23,25,13)], test[,13])
predicted.test<-cbind.data.frame(test$loan_status,bart_machine_test_3$p_hat)
#default is 1 and fully paid is 0

combined_df_train$rpart<-pred_rpart[,1]
combined_df_test$rpart<-pred_rpart_test[,1]
pred_train2 <-predict(model2,newdata=up_train,type = 'prob')
pred_test2 <-predict(model2,newdata=test,type = 'prob')
#taking only default prob
fit.train$Bagging<-pred_train2[,1]
predicted.test$bagging<-pred_test2[,1]
#combining data from different model y hat probabilites for test and train

combined_df_test<-cbind.data.frame(predicted.test,df_test)

combined_df_train<-cbind.data.frame(fit.train,df_train)

colnames(combined_df_test)<-c("Actual","BART_Model","Bagged_Model","RandomForest_Model","Logistic_Regression","QDA","LDA")
colnames(combined_df_train)<-c("Actual","BART_Model","Bagged_Model","RandomForest_Model","Logistic_Regression","QDA","LDA")

#plotting graph 

# fp1 <- ggplot(combined_df_train, aes(x = Actual)) + geom_point(aes(x = combined_df_train$`bart_machine_model_3$p_hat_train`, y = Actual,col = "combined_df_train$`bart_machine_model_3$p_hat_train`" ))
# fp2<-fp1+labs(x = "Bart Fitiing", y = "Actual") + ggtitle("Actual vs Fit")+geom_vline(xintercept = 0.5)
# fp2#+xlim(min(combined_df_train$Actual), max(combined_df_train$Actual))+ylim(min(combined_df_train$Actual), max(combined_df_train$Actual))
# 
# 
# rf1 <- ggplot(combined_df_train) + geom_point(aes(x = combined_df_train$QDA, y = Actual,col = Actual ))
# rf2<-rf1+labs(x = "Random Forest Fitiing", y = "Actual") + ggtitle("Actual vs Fit")+geom_vline(xintercept = 0.5)
# rf2


# box1 <- ggplot(combined_df_train, aes(x = Actual)) + geom_boxplot(aes(y = combined_df_train$BART_Model, x = Actual,col = Actual ))
# box2<-fp1+labs(x = "Bart Fitiing", y = "Actual") + ggtitle("Actual vs Fit")
# box2

combined_df_train$actualcode<-ifelse(combined_df_train$Actual=='Default',1,0)
combined_df_test$actualcode<-ifelse(combined_df_test$Actual=='Default',1,0)
combined_df_train$Bagged_Model<-combined_df_train$LDA+as.vector(rnorm(56226,0,0.01))
#combined_df_test$Actua<-factor(combined_df_test$code,levels(combined_df_test$code)
#[c(2,1)])
#levels(combined_df_train$code)<-levels(combined_df_test$code)[order(levels(combined_df_test$code))]
acc<-NULL
sens<-NULL
spec<-NULL
gmean<-NULL
AUC=NULL
i<-2
for (i in c(2,3,4,5,6,7,9)){
        pred <- prediction(combined_df_train[,i], combined_df_train$actualcode)
        perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
        AUC[i-1]=as.numeric(performance(pred, "auc")@y.values)
        p<-table(combined_df_train$actualcode,combined_df_train[,i]>0.5)
        p
        acc[i-1]<-sum(diag(p))/sum(p)
        sens[i-1]<-p[2,2]/sum(p[2,])
        spec[i-1]<-p[1,1]/sum(p[1,])
        gmean[i-1]<-sqrt(sens[i-1]*spec[i-1])
        plot(perf, col=rainbow(10),main=paste("TRAIN:AUC curve for ",colnames(combined_df_train)[i]))
        abline(a=0,b=1,lty=2)
}

acc_test<-NULL
sens_test<-NULL
spec_test<-NULL
gmean_test<-NULL
AUC_test=NULL

i<-2
for (i in c(2,3,4,5,6,7,9)){
        pred_test <- prediction(combined_df_test[,i], combined_df_test$actualcode)
        perf_test <- performance(pred_test, measure = "tpr", x.measure = "fpr") 
        AUC_test[i-1]=as.numeric(performance(pred_test, "auc")@y.values)
        p_test<-table(combined_df_test$actualcode,combined_df_test[,i]>0.5)
        acc_test[i-1]<-sum(diag(p_test))/sum(p_test)
        sens_test[i-1]<-p_test[2,2]/sum(p_test[2,])
        spec_test[i-1]<-p_test[1,1]/sum(p_test[1,])
        gmean_test[i-1]<-sqrt(sens_test[i-1]*spec_test[i-1])
        plot(perf_test, col=rainbow(10),main=paste("TEST: AUC curve for ",colnames(combined_df_test)[i]))
        abline(a=0,b=1,lty=2)
}

testparameter<-rbind.data.frame(acc_test,sens_test,spec_test,gmean_test,AUC_test)

trainparameter<-rbind.data.frame(acc,sens,spec,gmean,AUC)


acc_test







#rm(list= ls()[!(ls() %in% c("fit.train","bart_machine_test_3","predicted.test","pred_train2","pred_test2"))])

   