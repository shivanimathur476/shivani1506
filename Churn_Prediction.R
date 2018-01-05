#------Reading csv file--------#
#------------------------------#
Churn_data=read.csv("C:/Users/shiva/Documents/Business Analytics/Churn_Train.csv")
#----------DATA PREPROCESSING-----------#
#-----Finding out missing values in the data frame-----#
colMeans(is.na(Churn_data))
rowMeans(is.na(Churn_data))
#------Removing NA values from the Data-------#
Churn_data=Churn_data[complete.cases(Churn_data),]
View(Churn_data)
#-------convert negatives to positives in vmail & account length-------#
Churn_data$number_vmail_messages <- abs(Churn_data$number_vmail_messages)
Churn_data$account_length <- abs(Churn_data$account_length)
#---------check correlation of variables in corrplot--------#
corrplot(cor(Churn_data[,1:17]))
#---------check near zero variance variables----------------#
nzv <- nearZeroVar(Churn_data)
nzv
#-----------check variable importance----------------------#
library(ISLR)
library(rpart)
model1 <- rpart(Churn_data$churn ~., data = Churn_data, method = 'anova')
model2 <- rpart(Churn_data$churn~., data = Churn_data)
importance <- varImp(model2, scale=FALSE)
importance
#------------------Model Building------------------------------------------#
#-----------splitting the data into 70% training and 30% test set----------#
set.seed(1234)
library(caret)
intrain <- createDataPartition(y=Churn_data$churn, p=0.7, list=FALSE)
Training_ChurnData <- Churn_data[intrain,]
Test_ChurnData <- Churn_data[-intrain,]
#-----------------logistic model for training data(all variables)-----------------#
ModelLR=glm(churn~.,family='binomial',data = Training_ChurnData)
summary(ModelLR)
ModelTrain_LR <- glm(churn ~ total_day_charge +total_eve_minutes + total_eve_charge +international_plan +total_intl_calls + total_intl_charge +number_customer_service_calls+ number_vmail_messages+total_day_minutes+state,family = "binomial",  data = Training_ChurnData)
summary(ModelTrain_LR)
#------prediction on training data(all variables)---------------#
predicted_value <- predict(ModelLR, newdata = Training_ChurnData, type = 'response')
roc(Training_ChurnData$churn, predicted_value)
#--------------confusion matrix for training data---------------#
head(Training_ChurnData$churn)
head(predicted_value)
predicted_value <- as.factor(predicted_value >0.7)
levels(predicted_value)=list(no='FALSE',yes='TRUE' )
table(Predicted=predicted_value, True=Training_ChurnData$churn)
##----------logistic model for test data------------
ModelTest_LR <- glm(churn ~total_day_charge +total_eve_minutes + total_eve_charge +international_plan +total_intl_calls +total_intl_charge +number_customer_service_calls+number_vmail_messages+total_day_minutes+state, family = "binomial",data = Test_ChurnData)
summary(ModelTest_LR)
#--------------prediction on test data-----------------------#
predicted_value2 <- predict(ModelTest_LR, newdata = Test_ChurnData, type = 'response')
roc(Test_ChurnData$churn, predicted_value2)
#------------Adding average call column & averageCharge in Training  Data------------------#
Training_ChurnData$AvgCallMin=(Training_ChurnData$total_day_minutes+Training_ChurnData$total_eve_minutes+Training_ChurnData$total_night_minutes+Training_ChurnData$total_intl_minutes)/4
View(Training_ChurnData)
Training_ChurnData$AvgCharge=(Training_ChurnData$total_day_charge + Training_ChurnData$total_eve_charge+Training_ChurnData$total_night_charge+Training_ChurnData$total_intl_charge)/4
#----------------Model building using added column-------------------#
ModelTrain_LR <- glm(churn ~ total_day_charge +total_eve_minutes + total_eve_charge +international_plan +total_intl_calls + total_intl_charge +number_customer_service_calls+ number_vmail_messages+total_day_minutes+state + AvgCharge +AvgCallMin,family = "binomial",  data = Training_ChurnData)
predicted_value <- predict(ModelLR, newdata = Training_ChurnData, type = 'response')
roc(Training_ChurnData$churn, predicted_value)
#-----------------Prediction on test data-------------------#
ModelTestData<- glm(churn ~ total_day_charge +total_eve_minutes + total_eve_charge +international_plan +total_intl_calls + total_intl_charge +number_customer_service_calls+ number_vmail_messages+total_day_minutes+state + AvgCharge,family = "binomial",  data = Test_ChurnData)
predicted_value_t <- predict(ModelLR, newdata = Test_ChurnData, type = 'response')
roc(Test_ChurnData$churn,predicted_value_t,type='response')
#-----------------------------------------------#
#------Decision Tree Modelling------------------#
library(rattle)
Model_DTree= rpart(churn ~ ., data=Training_ChurnData,method ='anova',control=rpart.control(minsplit = 30))
plot(Model_DTree)
text(Model_DTree)
fancyRpartPlot(Model_DTree)
rsq.rpart(Model_DTree)
# ------------variables considered during logistic regression-------------------#
Model_DTree= rpart(churn ~total_day_charge +total_eve_minutes + total_eve_charge +international_plan +total_intl_calls + total_intl_charge +number_customer_service_calls+ number_vmail_messages+total_day_minutes+state , data=Training_project_data,method ='anova',control=rpart.control(minsplit = 50))
fancyRpartPlot(Model_DTree)
rsq.rpart(Model_DTree)
#-----------------prediction of AUC on Training data---------------#
library(pROC)
library(rpart)
Model_DTree1=rpart(churn ~. ,data = Training_ChurnData, method = 'class',control=rpart.control(minsplit = 50))
predict_DT1=predict(Model_DTree1,newdata=Training_ChurnData,type = 'prob')
roc(Training_ChurnData$churn,predict_DT1[,2])
Model_DTree= rpart(churn ~total_day_charge +total_eve_minutes + total_eve_charge +international_plan +total_intl_calls + total_intl_charge +number_customer_service_calls+ number_vmail_messages+voice_mail_plan+total_day_minutes+state , data=Training_project_data,method ='class',control=rpart.control(minsplit = 50))
plot(Model_DTree)
text(Model_DTree)
predict_DT=predict(Model_DTree,newdata=Training_ChurnData,type = 'prob')
roc(Training_ChurnData$churn,predict_DT[,2])
#-------------------prediction of AUC on Test Data---------------------#
#------------------Adding column AvgCharge on Test data---------------------------------#
Model_DTree_Test=rpart(churn~., data = Test_ChurnData,method = 'anova',control = rpart.control(minsplit = 50))
Test_ChurnData$AvgCharge=(Test_ChurnData$total_day_charge + Test_ChurnData$total_eve_charge+Test_ChurnData$total_night_charge+Test_ChurnData$total_intl_charge)/4
Model_DTree_Test= rpart(churn ~total_day_charge +total_eve_minutes + total_eve_charge +international_plan +total_intl_calls + total_intl_charge +number_customer_service_calls+ number_vmail_messages+total_day_minutes+state +AvgCharge , data=Test_ChurnData,method ='class',control=rpart.control(minsplit = 50))
fancyRpartPlot(Model_DTree_Test)
rsq.rpart(Model_DTree_Test)
#----------- prediction of AUC on Test Data----------#
predict_DT_test1=predict(Model_DTree_Test,newdata=Test_ChurnData,type = 'prob')
roc(Test_ChurnData$churn,predict_DT_test1[,2])
#----------------prediction of AUC on Test Data---------------#
Model_DTree_Test_A= rpart(churn~., data=Test_ChurnData,method ='class',control=rpart.control(minsplit = 50))
predict_DT_test=predict(Model_DTree_Test_A,newdata=Test_ChurnData,type = 'prob')
roc(Test_ChurnData$churn,predict_DT_test[,2])

#----------Reading Csv file of Week 3 tournament----------#
Week3TestData=read.csv("C:/Users/shiva/Documents/Business Analytics/Tournament_Week3_Test.csv")
#Adding Column AvgCharge on Week 3 Test data
Week3TestData$AvgCharge=(Week3TestData$total_day_charge + Week3TestData$total_eve_charge+Week3TestData$total_night_charge+Week3TestData$total_intl_charge)/4
# Adding a column called PredictionProb in Week 3 data according to model built
Week3TestData$PredictionProb=predict(Model_DTree,Week3TestData,type="prob")
View(Week3TestData)


#Reading Csv file of Final League
FinalLeague=read.csv("C:/Users/shiva/Documents/Business Analytics/FinalLeague_Test.csv")
#Adding Column AvgCharge on Final League Test data
FinalLeague$AvgCharge=(FinalLeague$total_day_charge + FinalLeague$total_eve_charge+FinalLeague$total_night_charge+FinalLeague$total_intl_charge)/4
# Adding a column called PredictionProb in Week 3 data according to model built
FinalLeague$PredictionProb=predict(Model_DTree,FinalLeague,type="prob")
View(FinalLeague)
