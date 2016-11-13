rm(list=ls())
data_train = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\train.csv", stringsAsFactors = F)
data_test = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\test.csv", stringsAsFactors =F )
data_submit = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\sample_submission.csv", stringsAsFactors =F )
NROW(data_train)
NCOL(data_train)
data_train_trim = data_train[,c(-1)]
data_train_trim = na.omit(data_train)
trn_Index = sample(1:nrow(data_train_trim),500,FALSE,NULL)
dtr = data_train_trim
#dcv = data_train_trim[-trn_Index,]
library(tree)
attach(dtr)
str(dtr)
dtr$y = str_detect(dtr$species , "Eucalyptus_Glaucescens")
names(dtr)
dtr =  dtr[,c(-2)]
names(dtr)
modeltr = tree(as.factor(dtr$y)~. -id ,dtr)
summary(modeltr)
modeltr
pruned = cv.tree(modeltr , FUN = prune.misclass)
which.min(pruned$dev)
pruned$size[which.min(pruned$dev)]
prunedTree = prune.misclass(modeltr ,  best = pruned$size[which.min(pruned$dev)])
plot(prunedTree)
predictTree = predict(prunedTree,data_test )
