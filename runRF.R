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
load.lib("data.table")

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
for (i  in c(1, 4:(length(tr)-1))){
  tr[,i] <- as.factor(tr[,i])
}

# remove date and activity-id
tr <- tr[,c(-2,-3)]
summary(tr)
colnames(tr) <- sapply(colnames(tr), function(x){paste('p.', x, sep='')})
tr[,1] <- as.factor(tr[,1])
colnames(tr)[1] <- 'people_id'
train <- h2o.merge(tr, ppl, by="people_id")

summary(train)

Train <- h2o.assign(train,"train.hex")

rf <- h2o.randomForest(training_frame = Train, 
                       x=c(2:12,14:(length(colnames(Train))-1)), 
                       y=13,
                       ntrees = 200,
                       stopping_rounds = 2,
                       seed=12345)


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

pred <- h2o.predict(object = rf, newdata = Test)
res <- h2o.cbind(Test["activity_id"], pred)
colnames(res)<-c("activity_id","outcome")
res <- as.data.frame(res)
write.table(res, "mypred/rf1.csv",  row.names=FALSE, sep=",", quote=FALSE)

