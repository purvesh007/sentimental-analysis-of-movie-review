
#*************************************
# svm
#*************************************

s_model <- svm(sentiment~., scale = F, data = train[,2:6])
pred_svm <- predict(s_model,test[,2:6])

error<-results$sentiment - pred_svm

rmse_s <- mean((results$sentiment - pred_svm)^2)


plot(cmdscale(dist(results1[,-6])),
     col = as.integer(results1[,5]),
     pch = c("o","+")[1:150 %in% s_1$index + 1])


x<- result_n[,2:5]
y <- results1[,5]
s_1 <- svm(x,y)

print(s_1)
summary(s_1)


pred <- predict(s_1, x, decision.values = TRUE)
attr(pred, "decision.values")[1:5,]


pred_s <- predict(s_1,x)
pred_s <-fitted(s_1)

table(pred_s,y)
