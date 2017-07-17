###
# Plots and effects for interruptions (MPs coded by gender)
###

library(effects)
library(nnet)
library(BSDA)
library(dplyr)
library(ggplot2)
library(tikzDevice)
library(gtable)

data <-readRDS("data1926gender.Robject")
data <- data[complete.cases(data$state_next),]
data$state_next2 <- relevel(data$state_next, ref = "M")
data$numparl = as.numeric(data$parlsess)

### overall

fit <- multinom(state_next2 ~ state + numparl, data=data)

fit.eff.95 <- Effect("state", fit, confidence.level=.99)
fit.eff.pre.95 <- data.frame(fit.eff.95$prob, fit.eff.95$lower.prob, fit.eff.95$upper.prob)
row.names(fit.eff.pre.95) <- c("M", "F", "I")
cis <- fit.eff.pre.95[,c("L.prob.I","prob.I",'U.prob.I')]
cis$state <- row.names(cis)
print(cis)

### split by parliament (not parl-sess---not enough data for this)

datalist = list()
nlist = list()

for (i in levels(data$parlnum)) {
  dat <- filter(data, parlnum==i)
  if ((table(dat$state_next)[1][["F"]])!=0){
  fit <- multinom(state_next2 ~ state, data=dat)
  fit.eff.95 <- Effect("state", fit, confidence.level=.95)
  fit.eff.pre.95 <- data.frame(fit.eff.95$prob, fit.eff.95$lower.prob, fit.eff.95$upper.prob)
  row.names(fit.eff.pre.95) <- c("M", "F", "I")
  if("L.prob.I" %in% colnames(fit.eff.pre.95)){
    cis <- fit.eff.pre.95[,c("L.prob.I","prob.I",'U.prob.I')]
    row.names(cis) <- c("M", "F", "I")
    cis$state <- row.names(cis)
    cis$parlnum <- i
    cis$n <- (table(dat$state_next)[1][["F"]])
    datalist[[i]] <- cis
  }else{
    next
  }}else{next}

}

big_data <- dplyr::bind_rows(datalist)
big_data <- filter(big_data, state!="I")

big_data$parlnum <- factor(big_data$parlnum, levels=16:41)
parl <- 16:41

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

tikz("gender.tex",
     width = 8.5, height = 6)

p <- ggplot(big_data, aes(x=parlnum, y=prob.I, colour=state, group=state)) +
        geom_errorbar(aes(ymin=L.prob.I, ymax=U.prob.I), width=.1) +
        geom_point()+
        #ggtitle("Count of Female MPs and Probability of Interruption by Speaker Gender, 16th to 41st Parliament")+
        labs(x="Parliament",y="Transition Probability (State -> I)")+
        scale_x_discrete("Parliament",drop=F)+
        scale_colour_manual(name="State",
                        labels=c("Female", "Male"),
                        breaks=c("F", "M"),
                        values=cbPalette)+
        theme(axis.text.x = element_text(angle=0, vjust=0.5, size=12),
              legend.position=c(0.92,0.8))

# Simple data for female MP representation in House
#mps <- data.frame(parl, c(1,1,2,2,1,1,4,2,5,5,6,4,1,5,10,10,16,29,40,54,63,63,65,67,79,80))
mps <- data.frame(parl, c(1/245,1/245,2/245,2/245,1/245,1/262,4/265,2/265,5/265,5/265,6/265,4/265,1/265,
                          5/264,10/264,10/264,16/282,29/282,40/282,54/295,63/295,63/301,65/301,67/308,79/308,80/308))

names(mps)<-c("parlnum","mps")
mps$mps <- as.numeric(mps$mps)

tikz("genderplot.tex", width = 8.5, height = 6)

r <- ggplot(mps, aes(x=parlnum, y=[mps])) +
  geom_point(stat="identity")+
  geom_line()+
  scale_x_discrete("Parliament", breaks=parl)+
  scale_y_continuous("Proportion of Female MPs in House")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(3,-5.5,4,3), "mm"))

gA <- ggplotGrob(p)
gB <- ggplotGrob(r)
grid::grid.newpage()
grid::grid.draw(rbind(gB, gA))

dev.off()

