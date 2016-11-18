rm(list=ls())
library(randomForest)
data_train = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\train.csv", stringsAsFactors = F)
data_test = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\test.csv", stringsAsFactors =F )
data_submit = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\sample_submission.csv", stringsAsFactors =F )
speices = names(data_submit)[2:100]
for (x in speices){
  dtr = data_train
  dtr = na.omit(data_train)
  #print(x)
  dtr$y = str_detect(dtr$species , x)
  dtr =  dtr[,c(-2)]
  #print(names(dtr))
  rmodel = randomForest(as.factor(dtr$y)~. -id ,mtry = 12, data = dtr)
  rpredict = predict(rmodel,newdata = data_test, type="prob")
  trueProb = rpredict[,2]
  data_submit[,eval(x)] = trueProb
}
write.csv(data_submit, "C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\leaf\\submission_randomForest.csv", row.names = F, quote = F)