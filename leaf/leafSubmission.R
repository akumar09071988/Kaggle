rm(list=ls())
data_train = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\train.csv", stringsAsFactors = F)
data_test = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\test.csv", stringsAsFactors =F )
data_submit = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\sample_submission.csv", stringsAsFactors =F )
speices = names(data_submit)[2:100]

for (x in speices){
  dtr = data_train
  dtr = na.omit(data_train)
  print(x)
  dtr$y = str_detect(dtr$species , x)
  dtr =  dtr[,c(-2)]
  #print(names(dtr))
  modeltr = tree(as.factor(dtr$y)~.-id ,dtr)
  pruned = cv.tree(modeltr , FUN = prune.misclass)
  print(pruned$size[which.min(pruned$dev)])
  if (pruned$size[which.min(pruned$dev)] == 1){
    predictTree = predict(modeltr,data_test )
    trueProb = predictTree[,2]
    data_submit[,eval(x)] = trueProb
  } else {
    prunedTree = prune.misclass(modeltr ,  best = pruned$size[which.min(pruned$dev)])
    predictTree = predict(prunedTree,data_test )
    trueProb = predictTree[,2]
    data_submit[,eval(x)] = trueProb
  }
  
}