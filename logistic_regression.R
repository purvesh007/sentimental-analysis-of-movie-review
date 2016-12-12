#*************************************
# logistic regression
#*************************************
results$sentiment <- as.integer(results$sentiment)
model_lm <-lm(sentiment~., data=train[,2:6])
pred_lr <- predict(model_lm,test,type = "response")
summary(model_lm)
summary(pred_lr)
# make a prediction for each X
predictedY <- predict(model_lm, test[,-1], type = "response")
plot(predictedY)
sum(test$sentiment==predictedY)/length(predictedY)

rmse <- mean((results$sentiment - predictedY)^2)
print(rmse)
rmse

summary(pred_lr)

# k 10 cross validation
k=10

n= floor(nrow(results)/k)

acc_lr = rep(NA,k)

for(i in 1:k){
  r1 = ((i-1)*n)+1
  r2 = i*n
  range = r1:r2
  train = results[-range,]
  test = results[range,]
  fit = lm(as.factor(results1$sentiment)~vNeg+neg+pos+vPos, data = results[,2:6], family = binomial() )
  testpred = predict(fit, test, type = "response")
  #acc_lr[i] = as.numeric(sum(results$sentiment==testpred)/length(testpred))
  rmse[i] <- mean((results$sentiment - testpred)^2)
}


