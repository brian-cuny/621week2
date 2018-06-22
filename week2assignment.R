library(tidyverse)

#1
data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week2\\classification-output-data.csv')

#2
table(actual = data$class, predicted = data$scored.class)

#3
Accuracy <- function(data, actual, predicted){
  table <- table(actual = unlist(data[, actual]), predicted = unlist(data[, predicted]))
  (table[1] + table[4]) / sum(table)
}

#4
Error.Rate <- function(data, actual, predicted){
  table <- table(actual = unlist(data[, actual]), predicted = unlist(data[, predicted]))
  (table[2] + table[3]) / sum(table)
}

#5
Precision <- function(data, actual, predicted){
  table <- table(actual = unlist(data[, actual]), predicted = unlist(data[, predicted]))
  table[4] / (table[4] + table[3])
}

#6
Sensitivity <- function(data, actual, predicted){
  table <- table(actual = unlist(data[, actual]), predicted = unlist(data[, predicted]))
  table[4] / (table[4] + table[2])
}

#7
Specificity <- function(data, actual, predicted){
  table <- table(actual = unlist(data[, actual]), predicted = unlist(data[, predicted]))
  table[1] / (table[1] + table[3])
}

#8
F1.Score <- function(data, actual, predicted){
  prec <- Precision(data, actual, predicted)
  sens <- Sensitivity(data, actual, predicted)
  (2 * prec * sens) / (prec + sens)
}

#9
#Prove: 0 <= 2*a*b / (a + b) <= 1
#Use: if 0 < a < 1 and 0 < b < 1 then a*b < a
#Then: 0 <= 2*a*b / (a + b) < a / (a + b) <= 1
#Then: b = 0 -> 1, max value and b = inf -> 0, min value

#10 - FIX
library(ROCR)
library(Bolstad)

ROC <- function(data, actual, predicted.prob){
  ROCRPred <- prediction(data[, predicted.prob], data[, actual])
  ROCRPref <- performance(ROCRPred, 'tpr', 'fpr')
  plot <- plot(ROCRPref, colorize=TRUE, print.cutoffs.at = seq(0.1, by=0.1))
  auc <- sintegral(unlist(ROCRPref@x.values), unlist(ROCRPref@y.values), n.pts=100)$cdf$y
  return(list(plot, auc))
}

ROC(data, 9, 11)

#11
Accuracy(data, 9, 10)
Error.Rate(data, 9, 10)
Precision(data, 9, 10)
Sensitivity(data, 9, 10)
Specificity(data, 9, 10)
F1.Score(data, 9, 10)
ROC(data, 9, 11)







