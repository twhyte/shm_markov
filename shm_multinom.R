###
# Plots and effects for overall interruption data analysis
###

library(effects)
library(nnet)
library(BSDA)
library(dplyr)
library(tikzDevice)
library(strucchange)
library(scales)
library(lme4)

data <-readRDS("data1926maj.Robject")
data <- data[complete.cases(data$state_next),]
data$state_next2 <- relevel(data$state_next, ref = "Gb")
data$numparl = as.numeric(data$parlsess)

### 5 speaker types, split by parliament-session

datalist = list()

for (i in levels(data$parlsess)) {
  dat <- filter(data, parlsess==i)
  fit <- multinom(state_next2 ~ state, data=dat)
  dses <- data.frame(state=c("Gb", "Gm", "I", "O", "S"))
  fit.eff.99 <- Effect("state", fit, confidence.level=.99)
  fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
  row.names(fit.eff.pre.99) <- c("Gb", "Gm", "I", "O", "S")
  cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state <- row.names(cis)
  cis$parlsess <- i
  cis$majority <- dat[1,]$majority
  datalist[[i]] <- cis
}

big_data <- dplyr::bind_rows(datalist)
big_data <- filter(big_data, state!="S")
big_data <- filter(big_data, state!="I")
big_data$majority <- as.factor(big_data$majority)
lab <- unique(big_data$parlsess)
lab2 <- sapply(lab, function(l){paste(substr(l,1,2), substr(l,3,3), sep="-")})

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

tikz("mainplot.tex",
      width = 8.5, height = 6)

ggplot(big_data, aes(x=parlsess, y=prob.I, colour=state, group=state, shape=majority)) +
  geom_errorbar(aes(ymin=L.prob.I, ymax=U.prob.I), width=.1) +
  geom_point()+
  geom_smooth(method="lm", se=F)+
  #ggtitle("Probability of Interruption by Speaker Type, 16th to 41st Parliament")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=6))+
  labs(x="Parliament-Session",y="Probability of Transition State -> I")+
  scale_x_discrete("Parliament-Session", breaks=waiver(), labels=lab2)+
  scale_colour_manual(name="State",
                        labels=c("Gb (Backbencher)", "Gm (Minister)", "O (Opposition)"),
                        breaks=c("Gb", "Gm", "O"),
                        values=cbPalette)+
  scale_shape_discrete(name="Parliament Status", labels=c("Minority", "Majority"))

dev.off()

### Overall probabilities, split by 5 speaker types

fit <- multinom(state_next2 ~ state + numparl, data=data)
dses <- data.frame(state=c("Gb", "Gm", "I", "O", "S"))

fit.eff.99 <- Effect("state", fit, confidence.level=.99)
fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
row.names(fit.eff.pre.99) <- c("Gb", "Gm", "I", "O", "S")

cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
cis$state <- row.names(cis)
print("Values for overall pattern")
print(cis)

### Overall probabilities, collapsed by MP type

data_recode <- data %>%
  mutate(state_recode = ifelse(state=='Gb' | state=="Gm" | state=="O","N",ifelse(state=="I", "I", "S"))) %>%
  mutate(state_next_recode = ifelse(state_next=='Gb' | state_next=="Gm" | state_next=="O","N",ifelse(state_next=="I", "I", "S"))) %>%
  mutate(state_recode = as.factor(state_recode)) %>%
  mutate(state_next_recode = as.factor(state_next_recode))

data_recode$state_next_recode <- relevel(data_recode$state_next_recode, ref = "N") ### ?

fit <- multinom(state_next_recode ~ state_recode + numparl, data=data_recode)

# dses <- data.frame(state_recode=c("I", "N", "S"))
# pre <- predict(fit, newdata=dses, "probs", se=TRUE)
# row.names(pre) <- c("I", "N", "S")

fit.eff.99 <- Effect("state_recode", fit, confidence.level=.99)
print(fit.eff.99)
fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
row.names(fit.eff.pre.99) <- c("I", "N", "S")

cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
cis$state_recode <- row.names(cis)
print("Values for collapsed pattern 99 interval")
print(cis)

### Plot of collapsed MP over Parliament-Session

datalist = list()

for (i in levels(data_recode$parlsess)) {
  dat <- filter(data_recode, parlsess==i)
  if ((table(dat$state_next_recode)[3][["S"]])!=0){
  fit <- multinom(state_next_recode ~ state_recode, data=dat)
  dses <- data.frame(state_recode=c("I", "N", "S"))
  fit.eff.95 <- Effect("state_recode", fit, confidence.level=.95)
  fit.eff.pre.95 <- data.frame(fit.eff.95$prob, fit.eff.95$lower.prob, fit.eff.95$upper.prob)
  row.names(fit.eff.pre.95) <- c("I", "N", "S")
  cis <- fit.eff.pre.95[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state_recode <- row.names(cis)
  cis$parlsess <- i
  datalist[[i]] <- cis
  }else{next}
}

coll_data <- dplyr::bind_rows(datalist)
coll_data <- filter(coll_data, state_recode!="S")
coll_data <- filter(coll_data, state_recode!="I")

b <- breakpoints(prob.I ~ as.numeric(parlsess), data=coll_data, h=5)

tikz("trendplot.tex",
     width = 8.5, height = 6)

ggplot(coll_data, aes(x=parlsess, y=prob.I, group=state_recode)) +
  geom_errorbar(aes(ymin=L.prob.I, ymax=U.prob.I), width=.1) +
  geom_point()+
  #geom_smooth(method="lm",se=F)+ # instead, manually draw this line based on separate calc of fixed effect
  geom_abline(slope=0.0004508, intercept=0.0284048,colour="#E69F00")+
  geom_vline(xintercept=which(coll_data$parlsess == '263'), linetype=4)+
  geom_vline(xintercept=which(coll_data$parlsess == '311'), linetype=4)+
  geom_vline(xintercept=which(coll_data$parlsess == '342'), linetype=4)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=6))+
  labs(x="Parliament-Session",y="Probability of Transition State -> I")+
  scale_x_discrete("Parliament-Session", breaks=waiver(), labels=lab2)

dev.off()

### structural breakpoint TV analysis

data$ps_num <- as.numeric(data$parlsess)
data$ps_tv <- ifelse(data$ps_num < 57 , 'notv', ifelse(data$ps_num > 63, 'tv', 'interval'))
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
  if ((table(dat$state_next_recode)[3][["S"]])!=0){
  fit <- multinom(state_next_recode ~ state_recode, data=dat)
  dses <- data.frame(state_recode=c("I", "N", "S"))
  fit.eff.99 <- Effect("state_recode", fit, confidence.level=.99)
  fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
  row.names(fit.eff.pre.99) <- c("I", "N", "S")
  cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state_recode <- row.names(cis)
  cis$ps_tv <- i
  datalist[[i]] <- cis
  }else{next}
}

tv_data <- dplyr::bind_rows(datalist)
tv_data <- filter(tv_data, state_recode!="S")
tv_data <- filter(tv_data, state_recode!="I")

print(tv_data)

### TV effect, split by type

datalist = list()

for (i in levels(data$ps_tv)) {
  dat <- filter(data, ps_tv==i)
  fit <- multinom(state_next2 ~ state + numparl, data=dat)
  dses <- data.frame(state=c("Gb", "Gm", "I", "O", "S"))
  fit.eff.99 <- Effect("state", fit, confidence.level=.99)
  fit.eff.pre.99 <- data.frame(fit.eff.99$prob, fit.eff.99$lower.prob, fit.eff.99$upper.prob)
  row.names(fit.eff.pre.99) <- c("Gb", "Gm", "I", "O", "S")
  cis <- fit.eff.pre.99[,c("L.prob.I","prob.I",'U.prob.I')]
  cis$state <- row.names(cis)
  cis$ps_tv <- i
  datalist[[i]] <- cis
}

tv_split_data <- dplyr::bind_rows(datalist)

print(tv_split_data)

### TEST: binomial glmm

# data$state_next_recode <- ifelse(data$state_next=="I", 1, 0)
# data$numparl <- as.numeric(data$parlsess)
# 
# reff_glmm <- glmer(state_next_recode ~ state + (-1 + state|parlsess), data=data, family='binomial', verbose=T)
# plain_glm <- glm(state_next_recode ~ state, data=data, family='binomial', verbose=T)
# fixed_time_glm <- glm(state_next_recode ~ state + parlsess, data=data, family='binomial', verbose=T)
# 
# saveRDS(reff_glmm, "reff_glmm.Robject")
# saveRDS(plain_glm, "plain_glm.Robject")
# saveRDS(fixed_time_glm, "fixed_time_glm.Robject")
# 
# print("Fixed vs. plain")
# print(pchisq(2*(logLik(fixed_time_glm)-logLik(plain_glm)),
#        df=1,lower.tail=FALSE)/2)
# 
# print("Random vs. plain")
# print(pchisq(2*(logLik(reff_glmm)-logLik(plain_glm)),
#              df=1,lower.tail=FALSE)/2)
# 
# print("Random vs. fixed")
# print(pchisq(2*(logLik(reff_glmm)-logLik(fixed_time_glm)),
#              df=1,lower.tail=FALSE)/2)

### TEST: split by individual day

# datalist = list()
# data$speechdate <- data$speechdate + 1
# dates <- unique(data$speechdate)
#
# fu <- function(i){dat <- filter(data, speechdate==i)
#                 fit <- multinom(state_next2 ~ state, data=dat)
#                 dses <- data.frame(state=c("Gb", "Gm", "I", "O", "S"))
#                 fit.eff.95 <- Effect("state", fit, confidence.level=.95)
#                 fit.eff.pre.95 <- data.frame(fit.eff.95$prob, fit.eff.95$lower.prob, fit.eff.95$upper.prob)
#
#                 row.names(fit.eff.pre.95) <- c("Gb", "Gm", "I", "O", "S")
#                 cis <- fit.eff.pre.95[,c("L.prob.I","prob.I",'U.prob.I')]
#                 cis$state <- row.names(cis)
#                 cis$speechdate<- i
#                 datalist[[i]] <<- cis # add it to your list))
#                 }
# for (i in dates) {
#   tryCatch(fu(i), finally=next)
# }
#
# big_data <- dplyr::bind_rows(datalist)
#
# big_data <- filter(big_data, state!="S")
# big_data <- filter(big_data, state!="I")
#
# p <- ggplot(big_data, aes(x=speechdate, y=prob.I, colour=state)) +
#   #geom_errorbar(aes(ymin=L.prob.I, ymax=U.prob.I), width=.1) +
#   #geom_point()
#   geom_smooth(se=F)
#
# print(p)

