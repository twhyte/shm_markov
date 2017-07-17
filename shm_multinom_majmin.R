###
# Effects for interruptions under majority/minority govt
###

library(effects)
library(nnet)
library(BSDA)
library(dplyr)

data <-readRDS("data1926maj.Robject")
data <- data[complete.cases(data$state_next),]
data$state_next2 <- relevel(data$state_next, ref = "Gb")
data$numparl = as.numeric(data$parlsess)

### split by majority/minority

datalist = list()

print("Majority/minority comparison by type (99%)")
for (i in c(0,1)) {
  dat <- filter(data, majority==i)
  fit <- multinom(state_next2 ~ state + numparl, data=dat)
  dses <- data.frame(state=c("Gb", "Gm", "I", "O", "S"))
  fit.eff.99 <- Effect("state", fit, confidence.level=.99)
  fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
  row.names(fit.eff.pre.99) <- c("Gb", "Gm", "I", "O", "S")
  cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state <- row.names(cis)
  cis$majority <- i
  print(cis)
}

print("Majority/minority comparison by type (95%)")

for (i in c(0,1)) {
  dat <- filter(data, majority==i)
  fit <- multinom(state_next2 ~ state + numparl, data=dat)
  dses <- data.frame(state=c("Gb", "Gm", "I", "O", "S"))
  fit.eff.99 <- Effect("state", fit, confidence.level=.95)
  fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
  row.names(fit.eff.pre.99) <- c("Gb", "Gm", "I", "O", "S")
  cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state <- row.names(cis)
  cis$majority <- i
  print(cis)
}

### the collapsed rate comparison

data_recode <- data %>%
  mutate(state_recode = ifelse(state=='Gb' | state=="Gm" | state=="O","N",ifelse(state=="I", "I", "S"))) %>%
  mutate(state_next_recode = ifelse(state_next=='Gb' | state_next=="Gm" | state_next=="O","N",ifelse(state_next=="I", "I", "S"))) %>%
  mutate(state_recode = as.factor(state_recode)) %>%
  mutate(state_next_recode = as.factor(state_next_recode))

data_recode$state_next_recode <- relevel(data_recode$state_next_recode, ref = "N")

print("Majority/minority comparison collapsed (99%)")
for (i in c(0,1)) {
  dat <- filter(data_recode, majority==i)
  fit <- multinom(state_next_recode ~ state_recode + numparl, data=dat)
  dses <- data.frame(state_recode=c("I", "N", "S"))
  fit.eff.99 <- Effect("state_recode", fit, confidence.level=.99)
  fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
  row.names(fit.eff.pre.99) <- c("I", "N", "S")
  cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state_recode <- row.names(cis)
  cis$majority <- i
  print(cis)
}

print("Majority/minority comparison collapsed (95%)")
for (i in c(0,1)) {
  dat <- filter(data_recode, majority==i)
  fit <- multinom(state_next_recode ~ state_recode + numparl, data=dat)
  dses <- data.frame(state_recode=c("I", "N", "S"))
  fit.eff.99 <- Effect("state_recode", fit, confidence.level=.95)
  fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
  row.names(fit.eff.pre.99) <- c("I", "N", "S")
  cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state_recode <- row.names(cis)
  cis$majority <- i
  print(cis)
}
