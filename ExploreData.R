# Author: Bohdan Monoastyrskyy
# Date: 2016-08-27
# Descrition: Kaggle competition
#   RedHat data 
# Task: make prediction of the potential business value of the customers 
#       based on their behavioral features and their on-line activities

# The goal of the script explore/clean data.

# load user-defined library
source("Utils.R")

# read data
# there are two files with data people.csv and act_train.csv
ppl <- read.big.table("data/people.csv", sep=',', header=TRUE)
# summary
summary(ppl)
# change the classes of the columns: string to factors
for (i in c(2,3,4,6:12)){
  ppl[,i] <- as.factor(ppl[,i])
}
# change the columns classes: from string to boolean
for (i in c(13:40)){
  ppl[,i]<-as.logical(ppl[,i])
}

ppl[,5]<- as.Date(ppl[,5])
summary(ppl)

for ( i in 1:length(ppl)){
  print(summary(ppl[,i]))
}

# read training data
tr <- read.big.table("data/act_train.csv", sep=',',  header=TRUE)
tr[,3] <-as.Date(tr[,3])
for (i  in 4:(length(tr)-1)){
  tr[,i] <- as.factor(tr[,i])
}
summary(tr)
