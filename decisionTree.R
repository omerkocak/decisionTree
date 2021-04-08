## DECISION TREE

library(tree)

dataset <- read.table("wheat_types.txt",header = TRUE,sep = ";")
View(dataset)
dataset$type <- as.factor(dataset$type) # numeric -> categorical

datasetTree <- tree(formula = type ~ ., data=dataset , split="deviance")
datasetTree

summary(datasetTree)
misclass.tree(datasetTree)

plot(datasetTree,  type = "uniform")
text(datasetTree)

set.seed(579462)
subSet <- sample(1:nrow(dataset),size=nrow(dataset)*0.75) # %75 sample
subSet


datasetTree2 <- tree(type ~ ., data=dataset , subset = subSet, split="deviance")
datasetTree2

plot(datasetTree2,  type = "uniform")
text(datasetTree2)

#predicting train values
train_predict <- table(predict(datasetTree2, dataset[subSet, ] , type = "class"), dataset[subSet, "type"])
rownames(train_predict) <- paste("Actual", rownames(train_predict), sep = ":")
colnames(train_predict) <- paste("Predicted", colnames(train_predict), sep = ":")
print(train_predict)

#predicting test values
test_predict <- table(predict(datasetTree2, dataset[-subSet, ] , type = "class"), dataset[-subSet, "type"])
rownames(test_predict) <- paste("Actual", rownames(test_predict), sep = ":")
colnames(test_predict) <- paste("Predicted", colnames(test_predict), sep = ":")
print(test_predict)

# 100 iteration cross validation
dt_acc <- numeric()
set.seed(1815850)
treeList <- list()
for(i in 1:100){
  sub <- sample(1:nrow(dataset), size=nrow(dataset)*0.75)
  fit <- tree(type ~ ., data = dataset, subset = sub)
  test_predict <- table(predict(fit, dataset[-sub, ], type = "class"), dataset[-sub, "type"])
  dt_acc <- c(dt_acc, sum(diag(test_predict)) / sum(test_predict))
  treeList[[i]] <- fit 
}

#plotting best tree
plot(treeList[[which(dt_acc==max(dt_acc))]],type = "uniform")
text(datasetTree2)

plot(dt_acc, type="o", col="blue", xlab="Samples", ylab="Truth Rate")
title(main="wheat_types' iterations ", col.main="blue", font.main=4)

mean(dt_acc)