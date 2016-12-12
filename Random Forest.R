library(randomForest)

rf <- randomForest(as.factor(results$sentiment)~., data = train[,2:6], mtry = 2,importance=TRUE,ntree=100)
rf_p <- predict(rf, results,type="class")
accuracy

accuracy<-sum(test$sentiment==rf_p)/length(rf_p)
plot(rf)
plot(margin(rf))

confusion_matrix_rf <-ftable(test$sentiment,rf_p)

summary(rf_p)

#roc
# Prediction function
ROCRpred = prediction(predict(rf,type="prob")[,2],train$sentiment)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))




#k cross validation
k=5 # 5 cross validation

n= floor(nrow(results)/k)

acc_rf = rep(NA,k)

for(i in 1:k){
  r1 = ((i-1)*n)+1
  r2 = i*n
  range = r1:r2
  train = results[-range,]
  test = results[range,]
  random_forest = randomForest(as.factor(results1$sentiment)~vNeg+neg+pos+vPos, data = train[,2:6], importance=TRUE,ntree=5 )
  pred_forest = predict(random_forest, test, type = "class")
  acc_rf[i] = as.numeric(sum(results$sentiment==rf_p)/length(rf_p))
}
confusionMatrix(results[,6],rf_p)

acc_rf

