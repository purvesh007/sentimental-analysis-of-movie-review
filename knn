install.packages('party')
install.packages('ROCR')
library(ROCR)

result_n <- as.data.frame(lapply(results[,c(2,3,4,5)], normalize))
#converting attribute value into numeric()
result_n$vNeg <- as.numeric(results$vNeg)
result_n$neg <- as.numeric(results$neg)
result_n$pos <- as.numeric(results$pos)
result_n$vPos <- as.numeric(results$vPos)
result_n <- result_n[,1:5]

#dividing into test and train data
result_n_train <- result_n[1:7000, 2:5]
result_n_test <- result_n[7001:10632, 2:5]
result_n_train_target <- results[1:7000,6]
result_n_test_target <- results[7001:10632,6]
require(class)

# for value of k i have taken sqrt(nrow) it's rule of thumb good to have value of k =odd number because knntakes majority vote so of if our k is odd, we don't have tie

m1 <- knn(train = result_n_train, test = result_n_test, cl = result_n_train_target, k = 103) 
con_matrix_knn <- table(result_n_test_target, m1)


confusionMatrix(result_n_test_target,m1)


