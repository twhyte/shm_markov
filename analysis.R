#####
# interruptions_analysis.R
# tanya.whyte@mail.utoronto.ca
# Reproduction of overall results from Oh, oh! Modeling Interruptions in the Canadian House of Commons, 1926-2015
#####

library(effects)
library(nnet)
library(BSDA)
library(dplyr)
#library(tikzDevice)
library(strucchange)
library(scales)
library(lme4)
library(ggplot2)

set.seed(315)

data <-readRDS("data.Robject")

data$state_next <- relevel(data$state_next, ref = "Gb") # Reference category is government backbencher

### Plot interruption rates of the 3 speaker types, grouped by Parliament Session (Figure 3)

datalist = list()

for (i in levels(data$parlsess)) {
  dat <- filter(data, parlsess==i)
  fit <- multinom(state_next ~ state, data=dat, trace=FALSE)
  dses <- data.frame(state=c("Gb", "Gm", "I", "O"))
  fiteff <- Effect("state", fit, confidence.level=.999)
  fiteffprob <- data.frame(fiteff$prob, fiteff$lower.prob, fiteff$upper.prob)
  row.names(fiteffprob) <- c("Gb", "Gm", "I", "O")
  cis <- fiteffprob[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state <- row.names(cis)
  cis$parlsess <- i
  cis$majority <- dat[1,]$majority
  datalist[[i]] <- cis
}

big_data <- dplyr::bind_rows(datalist)
big_data <- filter(big_data, state!="I")
big_data$majority <- as.factor(big_data$majority)
lab <- unique(big_data$parlsess)
lab2 <- sapply(lab, function(l){paste(substr(l,1,2), substr(l,3,3), sep="-")})

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p <- ggplot(big_data, aes(x=parlsess, y=prob.I, colour=state, group=state, shape=majority)) +
  geom_errorbar(aes(ymin=L.prob.I, ymax=U.prob.I), width=.1) +
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=6))+
  labs(x="Parliament-Session",y="Probability of Transition State -> I")+
  scale_x_discrete("Parliament-Session", breaks=waiver(), labels=lab2)+
  scale_colour_manual(name="State",
                      labels=c("Gb (Backbencher)", "Gm (Minister)", "O (Opposition)"),
                      breaks=c("Gb", "Gm", "O"),
                      values=palette)+
  scale_shape_discrete(name="Parliament Status", labels=c("Minority", "Majority"))

print(p)

### Interruption rates of the 3 speaker types

fit <- multinom(state_next ~ state, data=data, maxit=500, trace=FALSE)
dses <- data.frame(state=c("Gb", "Gm", "I", "O"))

fiteff <- Effect("state", fit, confidence.level=.999)
fiteffprob <- data.frame(fiteff$prob, fiteff$lower.prob, fiteff$upper.prob)
row.names(fiteffprob) <- c("Gb", "Gm", "I", "O")

cis <- fiteffprob[,c("L.prob.I","prob.I",'U.prob.I')]
cis$state <- row.names(cis)
print("Interruption rates of the 3 speaker types")
print(cis)

### Overall probability of interruption (collapsed MP type)
### (N = notinterruption, I = interruption)

data_recode <- data %>%
  mutate(state_recode = ifelse(state=='Gb' | state=="Gm" | state=="O","N","I")) %>%
  mutate(state_next_recode = ifelse(state_next=='Gb' | state_next=="Gm" | state_next=="O","N","I")) %>%
  mutate(state_recode = as.factor(state_recode)) %>%
  mutate(state_next_recode = as.factor(state_next_recode))

data_recode$state_next_recode <- relevel(data_recode$state_next_recode, ref = "N") ### ?

fit <- glm(state_next_recode ~ state_recode, family = binomial, data=data_recode)

fiteffallprob <- as.data.frame(Effect("state_recode", fit, confidence.level=.999))
cisall <- fiteffallprob[,c("lower", "fit", "upper")]
row.names(cisall) <- c("I", "N")
print("Overall probability of interruption")
print(cisall)

### Plot overall interruption rates (for any speaker type) grouped by Parliament-Session

datalist = list()

for (i in levels(data_recode$parlsess)) {
  dat <- filter(data_recode, parlsess==i)
  fit <- glm(state_next_recode ~ state_recode, family = binomial, data=dat, trace=FALSE)
  fiteffprob <- as.data.frame(Effect("state_recode", fit, confidence.level=.999))
  #fiteffprob <- data.frame(fiteff$prob, fitefflower.prob, fiteff95$upper.prob)
  row.names(fiteffprob) <- c("I", "N")
  cis <- fiteffprob[,c("lower", "fit", "upper")]
  cis$state_recode <- row.names(cis)
  cis$parlsess <- i
  datalist[[i]] <- cis
}

big_data2 <- dplyr::bind_rows(datalist)
big_data2 <- filter(big_data2, state_recode!="I")

### Calculate breakpoints here; vlines for calculated breakpoints are manually added in the figure (Figure 2) below

b <- breakpoints(fit ~ as.numeric(parlsess), data=big_data2, h=6)
print(confint(b, level=0.99))

q <- ggplot(big_data2, aes(x=parlsess, y=fit, group=state_recode)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_vline(xintercept=which(big_data2$parlsess == '272'), linetype=4)+
  geom_vline(xintercept=which(big_data2$parlsess == '311'), linetype=4)+
  geom_vline(xintercept=which(big_data2$parlsess == '342'), linetype=4)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=6))+
  labs(x="Parliament-Session",y="Probability of Transition State -> I")+
  scale_x_discrete("Parliament-Session", breaks=waiver(), labels=lab2)

print(q)

### Analyse the "tv" structural break (31-1) in more detail

data$ps_num <- as.numeric(data$parlsess)
data$ps_tv <- ifelse(data$ps_num < 58 , 'notv', ifelse(data$ps_num > 63, 'tv', 'interval')) # see breakpoint confints
data$ps_tv <- as.factor(data$ps_tv)

data_recode <- data %>%
  mutate(state_recode = ifelse(state=='Gb' | state=="Gm" | state=="O","N",ifelse(state=="I", "I", "S"))) %>%
  mutate(state_next_recode = ifelse(state_next=='Gb' | state_next=="Gm" | state_next=="O","N",ifelse(state_next=="I", "I", "S"))) %>%
  mutate(state_recode = as.factor(state_recode)) %>%
  mutate(numparl=as.numeric(parlsess)) %>%
  mutate(state_next_recode = as.factor(state_next_recode))

data_recode$state_next_recode <- relevel(data_recode$state_next_recode, ref = "N")

datalist = list()

for (i in levels(data_recode$ps_tv)) {
  dat <- filter(data_recode, ps_tv==i)
  fit <- glm(state_next_recode ~ state_recode, family = binomial, data=dat, trace=FALSE)
  fiteffprob <- as.data.frame(Effect("state_recode", fit, confidence.level=.999))
  row.names(fiteffprob) <- c("I", "N")
  cis <- fiteffprob[,c("lower", "fit", "upper")]
  cis$state_recode <- row.names(cis)
  cis$parlsess <- i
  datalist[[i]] <- cis
}

big_data3 <- dplyr::bind_rows(datalist)
big_data3 <- filter(big_data3, state_recode!="I")

print("Overall interruption rate before and after tv")
print(big_data3)

### TV breakpoint before-and-after interruption rates, by MP type

datalist = list()

for (i in levels(data$ps_tv)) {
  dat <- filter(data, ps_tv==i)
  fit <- multinom(state_next ~ state, data=dat, trace=FALSE)
  fiteff <- Effect("state", fit, confidence.level=.999)
  fiteffprob <- data.frame(fiteff$prob, fiteff$lower.prob, fiteff$upper.prob)
  row.names(fiteffprob) <- c("Gb", "Gm", "I", "O")
  cis <- fiteffprob[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state <- row.names(cis)
  cis$ps_tv <- i
  datalist[[i]] <- cis
}

big_data4 <- dplyr::bind_rows(datalist)

print("Interruption rate for 4 speaker types before and after tv")
print(big_data4)

### Comparison of majority and minority govenments for 3 MP types

datalist = list()

print("Comparison of majority and minority govenments for 3 MP types")

# Calculate for every parliament, then compare parliament means across groups

for (i in levels(data$parlnum)) {
  dat <- filter(data, parlnum==i)
  fit <- multinom(state_next ~ state, data=dat, trace=FALSE)
  dses <- data.frame(state=c("Gb", "Gm", "I", "O"))
  fiteff <- Effect("state", fit, confidence.level=.999)
  fiteffprob <- data.frame(fiteff$prob, fiteff$lower.prob, fiteff$upper.prob)
  row.names(fiteffprob) <- c("Gb", "Gm", "I", "O")
  cis <- fiteffprob[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state <- row.names(cis)
  cis$parlnum <- i
  cis$majority <- dat[1,]$majority
  datalist[[i]] <- cis
}

big_data5 <- dplyr::bind_rows(datalist)
print("Gb")
print(t.test(prob.I ~ majority, data = filter(big_data5, state=='Gb'))$p.value)
print("Gm")
print(t.test(prob.I ~ majority, data = filter(big_data5, state=='Gm'))$p.value)
print("O")
print(t.test(prob.I ~ majority, data = filter(big_data5, state=='O'))$p.value)

### comparison of majority/minority difference overall

datalist = list()

print("Majority/minority overall comparison (grouped by parliament)")
for (i in levels(data$parlnum)) {
  dat <- filter(data_recode, parlnum==i)
  fit <- glm(state_next_recode ~ state_recode, family = binomial, data=dat, trace=FALSE)
  fiteffprob <- as.data.frame(Effect("state_recode", fit, confidence.level=.999))
  row.names(fiteffprob) <- c("I", "N")
  cis <- fiteffprob[,c("lower", "fit", "upper")]
  cis$state_recode <- row.names(cis)
  cis$parlnum <- i
  cis$majority <- dat[1,]$majority
  datalist[[i]] <- cis
}

big_data6 <- dplyr::bind_rows(datalist)
big_data6 <- filter(big_data6, state_recode=='N')
print(t.test(prob.I ~ majority, data = big_data5)$p.value)
