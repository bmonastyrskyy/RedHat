# Author: Bohdan Monoastyrskyy
# Date: 2016-08-27
# Descrition: Kaggle competition
#   RedHat data 
# Task: make prediction of the potential business value of the customers 
#       based on their behavioral features and their on-line activities

# The goal of the script explore/clean data.

# load user-defined library
source("Utils.R")

# load libraries
load.lib("h2o")
#load.lib("data.table")

# init h2o cloud
h2o.init(
  nthreads=2, max_mem_size="6G")
h2o.removeAll() # clean 

# read data
# there are two files with data people.csv and act_train.csv
ppl <- h2o.importFile(path=normalizePath("data/people.csv"))

# change the classes of the columns: string to factors
for (i in c(1,2,3,4,6:12)){
  ppl[,i] <- as.factor(ppl[,i])
}
# change the columns classes: from string to boolean
#for (i in c(13:40)){
#  ppl[,i]<-as.logical(ppl[,i])
#}

# remove Date
#ppl[,5]<- as.Date(ppl[,5])
ppl <- ppl[,-5]
summary(ppl)
colnames(ppl)

for ( i in 1:length(ppl)){
  print(summary(ppl[,i]))
}

# read training data
tr <- h2o.importFile(path=normalizePath("data/act_train.csv"))
#tr <- read.big.table("data/act_train.csv", sep=',',  header=TRUE)

#tr[,3] <-as.Date(tr[,3])
for (i  in c(1, 4:(length(tr)))){
  tr[,i] <- as.factor(tr[,i])
}

# remove date and activity-id
tr <- tr[,c(-2,-3)]
summary(tr)
colnames(tr) <- sapply(colnames(tr), function(x){paste('p.', x, sep='')})
tr[,1] <- as.factor(tr[,1])
colnames(tr)[1] <- 'people_id'

# 
tr1 <- as.data.frame(tr)
tr2 <- as.data.frame(tr)
tr1 <- as.h2o(tr1[is.na(tr1[,3]),-11:-3])
tr2 <- as.h2o(tr2[!is.na(tr2[,3]),-12])



data1 <- h2o.merge(tr1, ppl, by="people_id")
splits <- h2o.splitFrame(data = data1, ratios = c(0.7, .15), seed = 148)
train1 <- splits[[1]]
valid1 <- splits[[2]]
test1 <- splits[[3]]

summary(train1)

# train and validate a grid of RFs
nb_params <- list( laplace = c(0, 0.0001, 0.01, 0.1, 1.0, 5.0, 10.0))

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 10)
predictors1 <- setdiff(names(train1), list('people_id','p.outcome'))

nb_grid1 <- h2o.grid("naivebayes", 
                     x = predictors1, 
                     y = c('p.outcome'), 
                     grid_id = "nb_grid1", 
                     training_frame = train1, 
                     validation_frame = valid1,
                     seed = 148, 
                     hyper_params = nb_params, 
                     search_criteria = search_criteria)

nb_gridperf1 <- h2o.getGrid(grid_id = "nb_grid1",
                            sort_by = "logloss",
                            decreasing = FALSE)
print(nb_gridperf1)

best_nb1 <- h2o.getModel(nb_gridperf1@model_ids[[1]])

#check resuluts for test1
h2o.performance(best_nb1, newdata = test1)

data2 <- h2o.merge(tr2, ppl, by="people_id")
splits2 <- h2o.splitFrame(data = data2, ratios = c(0.7, .15), seed = 148)
train2 <- splits2[[1]]
valid2 <- splits2[[2]]
test2 <- splits2[[3]]

summary(train2)
predictors2 <- setdiff(names(train2), list('people_id','p.outcome'))

nb_grid2 <- h2o.grid("naivebayes", 
                     x = predictors2,
                     y = c('p.outcome'), 
                     grid_id = "nb_grid2", 
                     training_frame = train2, 
                     validation_frame = valid2,
                     seed = 148, 
                     hyper_params = nb_params, 
                     search_criteria = search_criteria)

nb_gridperf2 <- h2o.getGrid(grid_id = "nb_grid2",
                            sort_by = "logloss",
                            decreasing = FALSE)
print(nb_gridperf2)

best_nb2 <- h2o.getModel(nb_gridperf2@model_ids[[1]])







ts <- h2o.importFile(path=normalizePath("data/act_test.csv"))

for (i  in 4:(length(ts))){
  ts[,i] <- as.factor(ts[,i])
}

# remove  Date
ts<-ts[,-3]
ts[,1] <-as.factor(ts[,1])
colnames(ts) <- sapply(colnames(ts), function(x){paste('p.', x, sep='')})
colnames(ts)[1] <- 'people_id'
colnames(ts)[2] <- 'activity_id'

ts1 <- as.data.frame(ts)
ts2 <- as.data.frame(ts)
ts1 <- h2o.merge(as.h2o(ts1[is.na(ts1$p.char_1),-12:-4]), ppl, by=c("people_id"), all.x = TRUE)
ts2 <- h2o.merge(as.h2o(ts2[!is.na(ts2$p.char_1),-13]), ppl, by=c("people_id"), all.x=TRUE)


pred1 <- h2o.predict(object = best_nb1, newdata = ts1)
pred2 <- h2o.predict(object = best_nb2, newdata = ts2)
res <- h2o.rbind(h2o.cbind(ts1[,c("activity_id")], pred1[,3]), h2o.cbind(ts2[,c("activity_id")], pred2[,3]))
colnames(res)<-c("activity_id","outcome")
res <- as.data.frame(res)
write.table(res, "mypred/nb.csv",  row.names=FALSE, sep=",", quote=FALSE)

