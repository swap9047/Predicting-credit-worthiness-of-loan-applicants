#------------------------- Rdata Cleaning -------------------------------------
rm("loan","loan2","loan3","tmp","tmp0","tmp1","tmp2","tmp3","tmp4","tmp5","tmp6","tmp7","tmp8","tmp9",bagtreemodel,cartmodel,corpus,prune_bag)
rm("dtm_emp_title")

######## Packages ########

library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(e1071)
library(xgboost)
library(stringr)
library(lubridate)
library(tm)
library(SnowballC)
library(rms)
library(glmnet)
library(pROC)
library(doMC)
library(kernlab)
library(MASS)
library(reshape)
library(missForest)
library(caTools)
library(psych)
library(ipred)
library(dplyr)
library(corrplot)

loan<-read.csv(file="LC_dataset.csv" ,header=T,stringsAsFactors = F,sep=',')
str(loan)

## counting instances of missing values and finding columns with appropriate NA
count_NA<-sapply(loan[loan$loan_status!="Current",],function(x)(sum(is.na(x))))
write.csv(count_NA,file="missing_count.csv")
count_NA1 <- count_NA<0.05 * nrow(loan)
count_NA
col_keep<-as.data.frame(count_NA1[count_NA1==1])
col_keep<-rownames(col_keep)
nrow(loan[loan$loan_status!="Current",])
loan2<-loan[col_keep]

numcol<-sapply(loan2,function(x)(is.numeric(x)))
num_col<-as.data.frame(numcol[numcol==1])
num_col<-rownames(num_col)
num_col
str(loan2)
loan3<-loan2[complete.cases(loan2),]
colnames(loan3)
loan3<-loan3[,-49]
numcol<-sapply(loan3,function(x)(is.numeric(x)))
num_col<-as.data.frame(numcol[numcol==1])
num_col<-rownames(num_col)
cor_plot<-corrplot(cor(loan3[num_col]),method="number")
table(loan3$policy_code)
count_NA

var.in <- names(loan)[!names(loan)%in% c('verification_status_joint','total_bal_il','acc_now_delinq','acc_open_past_24mths','acc_open_past_24mths','all_util','avg_cur_bal','bc_open_to_buy','bc_util','chargeoff_within_12_mths','collection_recovery_fee','collections_12_mths_ex_med','delinq_amnt','dti_joint','fico_range_high','fico_range_low','funded_amnt','funded_amnt_inv','id','il_util','initial_list_status','inq_fi','inq_last_12m','last_credit_pull_d','last_fico_range_high','last_fico_range_low','last_pymnt_amnt','last_pymnt_d','max_bal_bc','mo_sin_old_il_acct','mo_sin_old_rev_tl_op','mo_sin_rcnt_rev_tl_op','mo_sin_rcnt_tl','mort_acc','mths_since_last_delinq','mths_since_last_major_derog','mths_since_last_record','mths_since_rcnt_il','mths_since_recent_bc','mths_since_recent_bc_dlq','mths_since_recent_inq','mths_since_recent_revol_delinq','next_pymnt_d','num_accts_ever_120_pd','num_actv_bc_tl','num_actv_rev_tl','num_bc_sats','num_bc_tl','num_il_tl','num_op_rev_tl','num_rev_accts','num_rev_tl_bal_gt_0','num_sats','num_tl_120dpd_2m','num_tl_30dpd','num_tl_90g_dpd_24m','num_tl_op_past_12m','open_acc_6m','open_il_12m','open_il_24m','open_il_6m','open_rv_12m','open_rv_24m','out_prncp_inv','pct_tl_nvr_dlq','percent_bc_gt_75','policy_code','pub_rec_bankruptcies','recoveries','tax_liens','title','tot_coll_amt','tot_cur_bal','tot_hi_cred_lim','total_bal_ex_mort','total_bc_limit','total_cu_tl','total_il_high_credit_limit','total_pymnt_inv','total_rec_int','total_rec_late_fee','total_rec_prncp','total_rev_hi_lim','url')]
loan_col_cl <- loan[,var.in]
loan_col_cl$joint_status<-ifelse(is.na(loan_col_cl$annual_inc_joint),0,1)  ##introducing joint status column
loan_col_cl$annual_inc_joint<-ifelse(is.na(loan_col_cl$annual_inc_joint),loan_col_cl$annual_inc,loan_col_cl$annual_inc_joint)   ##updating annual_inc_joint column
sapply(loan_col_cl,function(x)(sum(is.na(x))))

loan_clean<-loan_col_cl[complete.cases(loan_col_cl),]

data <- loan_clean
summary(data$int_rate)
data$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = data$int_rate)))
data$issue_y = as.numeric(sapply( data$issue_d ,function(x){str_split(x,"-")[[1]][2]})) # Extracting year

## Importing macro economic data

poverty_new <- read.csv("Poverty2.csv", header = T)
poverty_new$Median_Income[poverty_new$issue_y==2016]<-NA
poverty_new$Poverty[poverty_new$issue_y==2016]<-NA

poverty.imp<-missForest(poverty_new[,-4])
poverty_new$value[poverty_new$issue_y==2016]<-poverty.imp$ximp$value
Income.imp<-missForest(poverty_new[,-5])
poverty_new$Median_Income<-Income.imp$ximp$Median_Income
poverty_new$Poverty<-poverty.imp$ximp$Poverty
poverty_new <-poverty_new[,-2]

loan_clean_macro <- data


colnames(loan_clean_macro)
loan_clean_macro <- merge(loan_clean_macro, poverty_new, by = c("addr_state", "issue_y"), all.x = T)
colnames(loan_clean_macro)

sum(is.na(loan_clean_macro))
sapply(loan_clean_macro,function(x)(sum(is.na(x))))
str(loan_clean_macro)
df <- loan_clean_macro[is.na(loan_clean_macro$Median_Income),c(1,2)]
unique(loan_clean_macro$emp_title)

## Removing rows with loan_status which are not relevant to our analysis
loan_clean_macro$loan_status <- gsub("Does not meet the credit policy. Status:Fully Paid", "Fully Paid", loan_clean_macro$loan_status)
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "Current"),]
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "Late (16-30 days)"),]
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "Issued"),]
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "In Grace Period"),]
loan_clean_macro$loan_status = ifelse(str_detect(loan_clean_macro$loan_status,"Paid"),loan_clean_macro$loan_status,"Default")
unique(loan_clean_macro$loan_status)                           
table(loan_clean_macro$loan_status)
loan_clean_macro$addr_state <- as.factor(loan_clean_macro$addr_state)
str(loan_clean_macro)
sum(is.na(loan_clean_macro))

#--------------------------Data Re-structure-----------------------------------

loan_clean_macro$loan_status <- gsub("Fully Paid", 0, loan_clean_macro$loan_status)
loan_clean_macro$loan_status <- gsub("Default", 1, loan_clean_macro$loan_status)
loan_clean_macro$loan_status<-as.numeric(loan_clean_macro$loan_status)

loan_clean_macro$verification_status <- gsub("Not Verified", 0, loan_clean_macro$verification_status)
loan_clean_macro$verification_status <- gsub("Source Verified", 1, loan_clean_macro$verification_status)
loan_clean_macro$verification_status <- gsub("Verified", 2, loan_clean_macro$verification_status)
loan_clean_macro$verification_status<-as.numeric(loan_clean_macro$verification_status)

loan_clean_macro$zip_code <- gsub("xx",0, loan_clean_macro$zip_code)
loan_clean_macro$zip_code<- as.numeric(loan_clean_macro$zip_code)

loan_clean_macro$revol_util <- data$revol_util
loan_clean_macro$revol_util <- gsub("%",0, loan_clean_macro$revol_util)
head(loan_clean_macro$revol_util)
loan_clean_macro$revol_util<-as.numeric(loan_clean_macro$revol_util)
loan_clean_macro <- loan_clean_macro[complete.cases(loan_clean_macro),]
sum(is.na(loan_clean_macro))
colnames(loan_clean_macro)

##loan_clean_macro<-loan_clean_macro[,-33]# 33 is prediction

loan_clean_macro$term<-gsub(" months",' ', loan_clean_macro$term)
loan_clean_macro$term<-as.numeric(loan_clean_macro$term)
loan_clean_macro$home_ownership<-as.factor(loan_clean_macro$home_ownership)
unique(loan_clean_macro$emp_length)

##Changing predictors as factors
colnames(loan_clean_macro)
fact_col<-c("addr_state","grade" ,"joint_status" )

loan_clean_macro[,fact_col]<-lapply(loan_clean_macro[,fact_col],function(x)(as.factor(x)))

##Breaking emp_length to buckets
loan_clean_macro <- data

loan_clean_macro$emp_length<-ifelse(loan_clean_macro$emp_length %in% c("< 1 year",'1 year','2 years'),'0_2years',loan_clean_macro$emp_length)
loan_clean_macro$emp_length<-ifelse(loan_clean_macro$emp_length %in% c("3 years",'4 years','5 years'),'3_5years',loan_clean_macro$emp_length)
loan_clean_macro$emp_length<-ifelse(loan_clean_macro$emp_length %in% c("6 years",'7 years','8 years','9 years'),'6_9years',loan_clean_macro$emp_length)
loan_clean_macro1<-loan_clean_macro[(loan_clean_macro$emp_length!='n/a'),]

##Getting credit length in weeks and converting date variables as date
#loan_clean_macro1$issue_d<-gsub('-','\\',loan_clean_macro1$issue_d)
loan_clean_macro1$issue_d<-paste0(loan_clean_macro1$issue_d,'-01')
loan_clean_macro1$issue_d<-as.Date(toupper(loan_clean_macro1$issue_d),format='%b-%Y-%d')
#class(loan_clean_macro1$issue_d)
loan_clean_macro1$earliest_cr_line<-paste0(loan_clean_macro1$earliest_cr_line,'-01')
loan_clean_macro1$earliest_cr_line<-as.Date(toupper(loan_clean_macro1$earliest_cr_line),format='%b-%Y-%d')
##head(loan_clean_macro1$earliest_cr_line)
loan_clean_macro1$credit_length_weeks<-difftime(loan_clean_macro1$issue_d,loan_clean_macro1$earliest_cr_line,units='weeks')
#head(loan_clean_macro1$credit_length_weeks)
#head(loan_clean_macro1$issue_d)

colnames(loan_clean_macro1)
fact_col<-c("term","emp_length" ,"purpose" )

loan_clean_macro1[,fact_col]<-lapply(loan_clean_macro1[,fact_col],function(x)(as.factor(x)))
drop_col<-c("emp_title" ,"issue_d" ,"pymnt_plan" ,"desc","zip_code")

model_data1<-loan_clean_macro1[,!colnames(loan_clean_macro1)%in% drop_col]
model_data <- model_data1
model_data$loan_status <- as.factor(model_data$loan_status)
model_data$verification_status <- as.factor(model_data$verification_status)
model_data$credit_length_weeks <- as.numeric(model_data$credit_length_weeks)
str(model_data)
model_data$grade <- as.factor(model_data$grade)
model_data$sub_grade <- substr(model_data$sub_grade,2,2) 
head(model_data$sub_grade)
model_data$sub_grade <- as.numeric(model_data$sub_grade) 
str(model_data)
model_data$revol_util <- gsub("%",0, model_data$revol_util)
head(model_data$revol_util)
model_data$revol_util<-as.numeric(model_data$revol_util)
str(model_data)
sum(is.na(model_data))
model_data <- model_data[complete.cases(model_data),]
model_data$home_ownership <- as.factor(model_data$home_ownership)
model_data$application_type <- as.factor(model_data$application_type)
model_data$earliest_cr_line <- NULL
sum(is.na(model_data))
str(model_data)
model_data$X <- NULL
str(model_data)
model_data$annual_inc <- NULL

save(list=ls(all=T),file='Model_Data_v3_base.RData')
rm(list=setdiff(ls(), "model_data"))
sum(is.na(model_data))

## Splitting Data in two parts

model_data_pre2011 <- model_data[model_data$issue_y < 2011,]
unique(model_data_pre2011$issue_y)

model_data_post2011 <- model_data[model_data$issue_y >= 2011,]
unique(model_data_post2011$issue_y)

#--------------------------- Taking 5% of the entire dataset----------------------#

set.seed(101)

## select training indices preserving class distribution
in.train <- createDataPartition(model_data$loan_status, p=0.05, list=FALSE)
summary(factor(model_data$loan_status))/length(model_data$loan_status)
train <- model_data[in.train,]; summary(train$loan_status)/length(train$loan_status)
test <- model_data[-in.train,]; summary(test$loan_status)/length(test$loan_status)


set.seed(101)
in.train_pre <- createDataPartition(model_data_pre2011$loan_status, p=0.2, list=FALSE)
summary(factor(model_data_pre2011$loan_status))/length(model_data_pre2011$loan_status)
train_pre <- model_data_pre2011[in.train_pre,]; summary(train_pre$loan_status)/length(train_pre$loan_status)
test_pre <- model_data_pre2011[-in.train_pre,]; summary(test_pre$loan_status)/length(test_pre$loan_status)

set.seed(101)
in.train_post <- createDataPartition(model_data_post2011$loan_status, p=0.05, list=FALSE)
summary(factor(model_data_post2011$loan_status))/length(model_data_post2011$loan_status)
train_post <- model_data_post2011[in.train_post,]; summary(train_post$loan_status)/length(train_post$loan_status)
test_post <- model_data_post2011[-in.train_post,]; summary(test_post$loan_status)/length(test_post$loan_status)

######## EDA ########

#### EDA 1 ####
slices <- loan_clean$loan_status
freq_slices <- table(slices)
piepercent<- round(100*freq_slices/sum(freq_slices), 1)
pie(freq_slices, labels = piepercent, col = c("red","green"))
legend("topright", c("Default","Fully Paid"), cex = 0.8,
       fill = c("red","green"))

count <- table(loan_clean$loan_status)
barplot(count, col = c("red", "dark green"),legend = rownames(count))

#### EDA 2 ####
data <- 
  
  tmp = data %>% group_by(loan_status) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(data)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=loan_status,y=ncount,fill=loan_status)) + geom_bar(stat="identity") +
  geom_text(aes(label=ncount_p),vjust = 2)

#### EDA 3 ####

data$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = data$int_rate)))

data$issue_y = as.numeric(sapply( data$issue_d ,function(x){str_split(x,"-")[[1]][2]})) # Extracting year

displayInterestByGrade <- function(dt){
  g1 = dt %>% filter(loan_status == "Default") %>% group_by(grade) %>% summarise(default_count = n())
  g2 = dt %>% group_by(grade) %>% summarise(count = n(),int_rate=mean(int_rate))
  g2 %>% left_join(g1) %>% mutate(default_rate = 100*default_count/count) ## %>% select(grade,count,default_count,int_rate,default_rate)
}

#Storing data split by grade across different years
tmp0 = displayInterestByGrade(data %>% filter(issue_y==2007))
tmp0$year <- 2007
tmp1 = displayInterestByGrade(data %>% filter(issue_y==2008))
tmp1$year <- 2008
tmp2 = displayInterestByGrade(data %>% filter(issue_y==2009))
tmp2$year <- 2009
tmp3 = displayInterestByGrade(data %>% filter(issue_y==2010))
tmp3$year <- 2010
tmp4 = displayInterestByGrade(data %>% filter(issue_y==2011))
tmp4$year <- 2011
tmp5 = displayInterestByGrade(data %>% filter(issue_y==2012))
tmp5$year <- 2012
tmp6 = displayInterestByGrade(data %>% filter(issue_y==2013))
tmp6$year <- 2013
tmp7 = displayInterestByGrade(data %>% filter(issue_y==2014))
tmp7$year <- 2014
tmp8 = displayInterestByGrade(data %>% filter(issue_y==2015))
tmp8$year <- 2015
tmp9 = displayInterestByGrade(data %>% filter(issue_y==2016))
tmp9$year <- 2016

tmp = rbind(tmp0,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9) ## Combining all the individual year-wise data into one

ggplot(tmp, aes(x=grade, y=default_rate,fill=as.factor(year))) + geom_bar(stat="identity",position="dodge") + ggtitle("Default Rate(%)")


#### EDA 4 ####

ggplot(tmp, aes(x=grade, y=int_rate,fill=as.factor(year))) + geom_bar(stat="identity",position="dodge") + ggtitle("Interest Rate(%)")

## We plan to discuss with professor how much of the past data should be included in our analysis. Since in time series kind of models it is advised to use recent data rather than past data

#### EDA 5 ####

all_roi = sum((data %>% filter(issue_y==2007, loan_status == "Fully Paid"))$total_pymnt)/sum((data %>% filter(issue_y==2007, loan_status == "Fully Paid"))$loan_amnt) - 1
all_roi

data$prediction = "Fully Paid"
createPerformanceTable <- function(dt){
  
  dt_pick = dt %>% filter(prediction == "Fully Paid")
  all_roi = sum(dt_pick$total_pymnt)/sum(dt_pick$loan_amnt) - 1
  
  temp_table = data.frame(grade=character(0),roi=numeric(0),percent_pick=numeric(0))
  for(g in c("A","B","C","D","E","F","G")){
    data_pick_grade = dt_pick %>% filter(grade==g)
    if(nrow(data_pick_grade)==0){
      temp_table = rbind(temp_table,data.frame(grade=g,roi=0,percent_pick=0))
    }
    else
    {
      data_grade = dt %>% filter(grade==g)
      roi = sum(data_pick_grade$total_pymnt)/sum(data_pick_grade$loan_amnt) - 1
      temp_table = rbind(temp_table,data.frame(grade=g,roi=roi,percent_pick=100 * nrow(data_pick_grade)/nrow(data_grade)))
    }
  }
  
  temp_table = rbind(temp_table,data.frame(grade="ALL",roi=all_roi,percent_pick=100 * nrow(dt_pick)/nrow(dt) ))
  
  return(temp_table)
}

baseline_table <- list()
table <- as.data.frame()
for (i in 2007:2016){
  
  table <-  createPerformanceTable(data %>% filter(issue_y==i))
  baseline_table [i-2006] <- table[8,2]
  
}

years <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
baseline_table$years <- years
baseline_table[[7]]
roi_table <- vector()
for(i in 1:10){
  roi_table[i] <- baseline_table[[i]]
}
roi_table
roi_table <- cbind(roi_table, years)
roi_table

colnames(roi_table) <- c("ROI", "Year")
colnames(roi_table)

## Graph to be plotted

#### EDA 6 ####

##head(data[,c("title","emp_title")])

corpus <-Corpus(VectorSource(data$emp_title))
corpus <- tm_map(corpus, tolower) # lower case
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english')) # from library
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, PlainTextDocument)
dtm_emp_title<-DocumentTermMatrix(corpus,control = list(stemming = stemDocument))
dtm_emp_title<-removeSparseTerms(dtm_emp_title, 0.99)
dtm_emp_title<-as.data.frame(as.matrix(dtm_emp_title))
rownames(dtm_emp_title) <- NULL
names(dtm_emp_title)<-paste(names(dtm_emp_title),sep=" ","emp_title")
names(dtm_emp_title)

#-----------------------------------#Functions #--------------------------------

RMSE_calc <- function(obj,newData, response){
  #rmse for regression
  if(class(newData[,response])!="factor"){
    predicted_val <- predict(obj, newData[,-which(names(newData) %in% response)])
    RMSE <-mean((newData[,response] - predicted_val)^2)^0.5
  }else{ #misclassification error for classification
    predicted_val <- predict(obj, newData[,-which(names(newData) %in% response)],type = "class")
    table_test <- table(predicted_val, newData[,response])
    #in this case RMSE is the misclassification error
    RMSE <- 1 - sum(diag(table_test))/sum(table_test)
  }
  return(RMSE)
}

# Second function is R-sq function in which input parameter are dataset, model and response
R2 <- function(dataset,response,model) {
  pred=predict(model,newdata = dataset)
  SSE<-sum((pred-dataset[,response])^2)
  SST<-sum((dataset[,response]-rep(mean(dataset[,response]),nrow(dataset)))^2)
  Rsq=1-(SSE/SST)
  return(Rsq)
}

# third is final function which return model parameters using above 2 functions
# Model Stat return us RMSE of train, RMSE of test and R-sq value 
Model_stat<- function(obj,response,modelname,traindata=train,testdata=test){
  RMSE_train<-RMSE_calc(obj,traindata,response)
  RMSE_test<-RMSE_calc(obj,testdata,response)
  Rsq<-R2(traindata,response,model = obj)
  out<-c(modelname,RMSE_train,RMSE_test,Rsq)
  return(out)
}







#-------------------------- Scalling & PCA--------------------------------------#

numcol<-sapply(train,function(x)(is.numeric(x)))
head(model_data$grade)
colnames(train)[numcol]
train_pca[, colnames(train)[numcol]] <- lapply(train[, colnames(train)[numcol]] ,function(x)(scale(x)))
train_pca <- scale(as.matrix(train [, colnames(train)[numcol]]))

train_pca <- as.data.frame(train_pca)
head(train_pca)

train_pca1 <- principal(train_pca
                        , nfactors = 18     
                        , rotate = "none"  
                        , scores = T       
)

train_pca1$loadings

##                PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8   PC9  PC10  PC11  PC12  PC13  PC14  PC15  PC16  PC17  PC18
##SS loadings    3.108 1.792 1.517 1.472 1.135 1.092 1.026 1.012 0.970 0.943 0.828 0.730 0.720 0.703 0.582 0.302 0.066 0.001
##Proportion Var 0.173 0.100 0.084 0.082 0.063 0.061 0.057 0.056 0.054 0.052 0.046 0.041 0.040 0.039 0.032 0.017 0.004 0.000
##Cumulative Var 0.173 0.272 0.357 0.438 0.501 0.562 0.619 0.675 0.729 0.782 0.828 0.868 0.908 0.947 0.979 0.996 1.000 1.000




# #---------------- Dataset splitting -------------#--------------#-----------#-------
# 
# 
# # We have removed the column 'Member_ID' because it would not add value to our model. We have not removed it from our dataset 
# #because that is the only unique identifier in our dataset and we would like to keep it for quality control purposes
# 
# # Splitting Main dataset in two parts
# set.seed(101)
# sample = sample.split(model_data[,-3], SplitRatio = .85)
# train = subset(model_data[,-3], sample == TRUE)
# test = subset(model_data[,-3], sample == FALSE)
# 
# # # Splitting Model-2 dataset in two parts
# # set.seed(101)
# # colnames(model_data2)
# # sample = sample.split(model_data[,-c(3,20,21)], SplitRatio = .85)
# # train = subset(model_data[,-c(3,20,21)], sample == TRUE)
# # test = subset(model_data[,-c(3,20,21)], sample == FALSE)
# 
# # Splitting Pre2011 dataset in two parts
# set.seed(101)
# colnames(model_data_pre2011)
# sample = sample.split(model_data_pre2011[,-3], SplitRatio = .85)
# train_pre = subset(model_data_pre2011[,-3], sample == TRUE)
# test_pre = subset(model_data_pre2011[,-3], sample == FALSE)
# 
# # Splitting Post2011 dataset in two parts
# set.seed(101)
# colnames(model_data_post2011)
# sample = sample.split(model_data_post2011[,-3], SplitRatio = .85)
# train_post = subset(model_data_post2011[,-3], sample == TRUE)
# test_post = subset(model_data_post2011[,-3], sample == FALSE)
# 

#-----------------------Fitting Models and Transformation-----------------------


