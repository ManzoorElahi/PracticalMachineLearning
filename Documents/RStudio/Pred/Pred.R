library(caret)
library(randomForest)
library(knitr)

training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
set.seed(19)
na_test = sapply(training, function(x) {sum(is.na(x))})
bad_columns = names(na_test[na_test==19216])
training = training[, !names(training) %in% bad_columns]
cvCtrl <- trainControl(method = "cv", number = 2, allowParallel = FALSE, verboseIter = TRUE)
m1 = train(classe ~., method="rf", data=training, trControl = cvCtrl)

pred1 <- predict(m1,newdata=testing)

pred1

answers <- pred1

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

