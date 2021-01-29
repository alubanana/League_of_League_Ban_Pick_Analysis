#BA2
library(leaps) 
library(glmnet)
library(dplyr)
library(tree) 
library(randomForest)
# PREDICTING 
setwd("C:/Users/Achernar/Desktop/Project")
team_merge<-read.csv("team_merge.csv")
team_merge$b_result = as.factor(team_merge$b_result)
x <- model.matrix(b_result~.-gameid,team_merge)
x <- x[,-1]
y <- as.factor(team_merge$b_result) 
set.seed(123)
train <- sample(1:nrow(team_merge),0.75*nrow(team_merge))
test <- -train
## LASSO Logistic
grid <- 10^(-2:10) 
cv.out <- cv.glmnet(x[train,],y[train],alpha=1,lambda=grid,
                    family="binomial",nfolds=10)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=bestlam,
                    family="binomial")
pred <- predict(lasso.mod, x[test,],type="class");
mean(pred==y[test])#0.9880728
coef(lasso.mod)
## Decision Tree
tree1 <- tree(b_result~., data = team_merge[train,])
tree_pred =  predict(tree1, team_merge[test,],type="class")
mean(tree_pred==team_merge$b_result[test])#0.9817954 
## random forest
set.seed(1)
rf1 <- randomForest(b_result~.,data=team_merge[train,],mtry=6,importance=TRUE)
importance(rf1)
varImpPlot(rf1)
rf_pred <- predict(rf1,newdata=team_merge[test,])
mean(rf_pred==team_merge$b_result[test])#0.9905838 

set.seed(2)
rf1v2 <- randomForest(b_result~.-r_towers-r_inhibitors-b_towers-b_inhibitors-r_earnedgold-
                      b_earnedgold-r_totalgold-b_totalgold,data=team_merge[train,],mtry=6,importance=TRUE)
importance(rf1v2)
varImpPlot(rf1v2)
rf_pred1v2 <- predict(rf1v2,newdata=team_merge[test,])
mean(rf_pred1v2==team_merge$b_result[test])

champion_cluster20<-read.csv("cluster_done20.csv")
champion_cluster20 = champion_cluster20[,-1]
champion_cluster20$result = as.factor(champion_cluster20$result)
#champion_cluster10[sapply(champion_cluster10, is.numeric)]=lapply(champion_cluster10[sapply(champion_cluster10, is.numeric)], as.factor)
x <- model.matrix(result~.,champion_cluster20)
x <- x[,-1]
y <- as.factor(champion_cluster20$result) 
set.seed(123)
train <- sample(1:nrow(champion_cluster20),0.75*nrow(champion_cluster20))
test <- -train

#randomforest
set.seed(123)
rf20 <- randomForest(result~.,data=champion_cluster20,subset=train,ntree=25,importance=TRUE)
rf_pred20 <- predict(rf20,newdata=champion_cluster20[test,])
varImpPlot(rf20)
mean(rf_pred20==champion_cluster20$result[test])  # 0.5417451


##logstic with Lasso penalty
grid <- 10^(-2:10) 
cv.out2 <- cv.glmnet(x[train,],y[train],alpha=1,lambda=grid,
                    family="binomial",nfolds=10)
bestlam2 <- cv.out$lambda.min
lasso.mod2 <- glmnet(x[train,], y[train], alpha=1, lambda=bestlam,
                    family="binomial")
pred <- predict(lasso.mod2, x[test,],type="class");
mean(pred==y[test]) #0.5373509
