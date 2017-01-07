read.csv("pml-training.csv",na.strings=c("","NA"))->training
!is.na(training[1,])->cols
training_data = training[,cols]
training_data = training_data[,-c(1,3,4,5,6,7)]

read.csv("pml-testing.csv",na.strings=c("","NA"))->testing
testing_data = testing[,cols]
testing_data = testing_data[,-c(1,3,4,5,6,7)]


set.seed(33833)
preProc<-preProcess(training_data[,-54],method="pca",thresh=0.9)
preProc
trainPC<-predict(preProc,training_data)
trainPC$classe <- training_data$classe
testPC<-predict(preProc,testing_data[,-54])


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
fitControl <- trainControl(method = "cv",
                           number = 10,
                           allowParallel = TRUE)

fit <- train(classe~.,method = "rf",data = training_data,trControl = fitControl)
confusionMatrix.train(fit)
test_pred <- predict(fit,newdata=testing_data)
fit1 <- train(classe~.,method = "rf",data = trainPC,trControl = fitControl)
stopCluster(cluster)
registerDoSEQ()
fit1
fit1$resample
confusionMatrix.train(fit1)

cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
fit2 <- train(classe~.,method = "gbm",data = trainPC,trControl = fitControl)
fit2
confusionMatrix.train(fit2)

pred1 <- predict(fit1)
pred2 <- predict(fit2)
df <- data.frame(pred1,pred2,classe = trainPC$classe)
combined <- train(classe~.,method = "rf",data = df)
confusionMatrix.train(combined)
confusionMatrix(training_data$classe,predict(combined,trainPC))
test_pred1 <- predict(fit1,testPC)
test_pred2 <- predict(fit2,testPC)
test_df <- data.frame(pred1=test_pred1,pred2=test_pred2)
test_pred_combined <- predict(combined,newdata=test_df)
confusionMatrix(testing_data$classe,predict(combined,testPC))
confusionMatrix(testing_data$classe,predict(fit1,testPC))
confusionMatrix(training_data$classe,predict(combined,df))

training[training$user_name=="carlitos",] -> carlitos
featureplot<-featurePlot(x = carlitos[, 2:5], y = carlitos$classe, plot = "pairs",auto.key = list(columns = 3))

