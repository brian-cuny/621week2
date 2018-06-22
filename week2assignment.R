library(tidyverse)

#1
data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week2\\classification-output-data.csv') %>%
  mutate(class = factor(class) %>% relevel(ref='1'),
         scored.class = factor(scored.class) %>% relevel('1'))

#2
table <- table(predicted = data$scored.class, actual = data$class)
table
#3
Accuracy <- function(data, actual, predicted){
  table <- table(predicted = unlist(data[, predicted]), actual = unlist(data[, actual]))
  (table[1] + table[4]) / sum(table)
}

#4
Error.Rate <- function(data, actual, predicted){
  table <- table(predicted = unlist(data[, predicted]), actual = unlist(data[, actual]))
  (table[2] + table[3]) / sum(table)
}

#5
Precision <- function(data, actual, predicted){
  table <- table(predicted = unlist(data[, predicted]), actual = unlist(data[, actual]))
  table[1] / (table[1] + table[3])
}

#6
Sensitivity <- function(data, actual, predicted){
  table <- table(predicted = unlist(data[, predicted]), actual = unlist(data[, actual]))
  table[1] / (table[1] + table[2])
}

#7
Specificity <- function(data, actual, predicted){
  table <- table(predicted = unlist(data[, predicted]), actual = unlist(data[, actual]))
  table[4] / (table[4] + table[3])
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
library(Bolstad)

ROC <- function(data, actual, predicted.prob){
  sens <- numeric(100)
  spec <- numeric(100)
  for(i in seq(0, 100, 1)){
    data$myPredict <- data[, predicted.prob] <= (i / 100)
    sens[i] <- Sensitivity(data, actual, 12)
    spec[i] <- 1 - Specificity(data, actual, 12)
  }
  plot <- ggplot(data_frame(spec=spec, sens=sens), aes(spec, sens)) +
    geom_point() +
    geom_abline(slope=1, intercept=0, color='red') +
    geom_line() +
    labs(x = 'False Positive Rate',
         y = 'True Positive Rate')
  auc <- sintegral(spec, sens, n.pts=1)$cdf$y
  return(list(plot, auc))
}


#11
Accuracy(data, 9, 10)
Error.Rate(data, 9, 10)
Precision(data, 9, 10)
Sensitivity(data, 9, 10)
Specificity(data, 9, 10)
F1.Score(data, 9, 10)
ROC(data, 9, 11)

#12
library(caret)
caret::confusionMatrix(table, mode='everything')
caret::sensitivity(table)
caret::specificity(table)


#13
library(ROCR)
ROCRPred <- prediction(data[, predicted.prob], data[, actual])
ROCRPref <- performance(ROCRPred, 'tpr', 'fpr')
plot(ROCRPref, colorize=TRUE, print.cutoffs.at = seq(0.1, by=0.1))
sintegral(unlist(ROCRPref@x.values), unlist(ROCRPref@y.values), n.pts=1)$cdf$y





