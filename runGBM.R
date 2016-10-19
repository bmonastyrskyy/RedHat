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


# remove Date
ppl <- ppl[,-5]
summary(ppl)
colnames(ppl)

for ( i in 1:length(ppl)){
  print(summary(ppl[,i]))
}

# read training data
tr <- h2o.importFile(path=normalizePath("data/act_train.csv"))
# convert into factors
for (i  in c(1, 4:(length(tr)))){
  tr[,i] <- as.factor(tr[,i])
}

# remove date and activity-id
tr <- tr[,c(-2,-3)]
summary(tr)
colnames(tr) <- sapply(colnames(tr), function(x){paste('p.', x, sep='')})

colnames(tr)[1] <- 'people_id'
data <- h2o.merge(tr, ppl, by="people_id")
splits <- h2o.splitFrame(data = data, ratios = c(0.8, .1), seed = 148)
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# train and validate a grid of GBMs
gbm_params <- list(learn_rate = seq(0.01, 0.1, 0.01),
                   max_depth = seq(2,10,1),
                   sample_rate = seq(0.5, 1.0, 0.1),
                   col_sample_rate = seq(0.1, 1.0, 0.1))
search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 10)
gbm_grid <- h2o.grid("gbm", 
                     x = c(2:12,14:(length(colnames(train)))), 
                     y=13, 
                     grid_id = "gbm_grid", 
                     training_frame = train, 
                     validation_frame = valid,
                     ntrees = 200, 
                     seed = 148, 
                     hyper_params = gbm_params, 
                     search_criteria = search_criteria)

gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid",
                            sort_by = "auc",
                            decreasing = TRUE)
print(gbm_gridperf)
gbm_best <- h2o.getModel(gbm_gridperf@model_ids[[1]])

ts <- h2o.importFile(path=normalizePath("data/act_test.csv"))

for (i  in 4:(length(ts)-1)){
  ts[,i] <- as.factor(ts[,i])
}

# remove  Date
ts<-ts[,-3]
ts[,1] <-as.factor(ts[,1])
colnames(ts) <- sapply(colnames(ts), function(x){paste('p.', x, sep='')})
colnames(ts)[1] <- 'people_id'
colnames(ts)[2] <- 'activity_id'

Test <- h2o.merge(ppl, ts, by="people_id")

pred <- h2o.predict(object = gbm_best, newdata = Test)
res <- h2o.cbind(Test["activity_id"], pred)
res <- as.data.frame(res)
res_prob <- res[,c(1,4)]
res_bin <- res[,c(1,2)]
colnames(res_bin)<-c("activity_id","outcome")
colnames(res_prob)<-c("activity_id","outcome")
write.table(res_bin, "mypred/gbm_bin.csv",  row.names=FALSE, sep=",", quote=FALSE)
write.table(res_prob, "mypred/gbm_prob.csv",  row.names=FALSE, sep=",", quote=FALSE)


