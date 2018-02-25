#####
# interruptions_data.R
# tanya.whyte@mail.utoronto.ca
# Processes Lipad database into list of speaker transition events
#####

### Imports

library("data.table")
library("dplyr")
library("stringr")
library("RPostgreSQL")
library("DataCombine")

### Query PostgreSQL Lipad database 
### (Development version includes additional error correction and dummies for government and majority status)

m <- dbDriver("PostgreSQL")
con <- dbConnect(m, host="192.168.1.1", user="xxxxx", password="xxxxx", dbname="dilipad_dev_qp")
rs <- dbSendQuery(con, "select basepk,speechdate,pid,parlnum,sessnum,speakername,speechtext,maintopic,majority,speakeroldname,gender,speakerposition,government from dilipadsite_basehansard where speechdate > '1926-12-31' and speechdate < '2015-11-30'")
complete <- FALSE
df <- fetch(rs, n=-1)
while (!complete) {
  complete <- dbHasCompleted(rs)
}

data <- as.data.frame.matrix(df)
dbClearResult(rs)
dbDisconnect(con)
rm(df)
gc()

print("Loaded data.")

### Some additional processing is required for using the SQL import

if (complete){
  ### Convert posix 'speechdate' to an origin where 0 = start of dataset; 
  ### convert 'government' and 'majority' back to 0/1 numeric
  
  offset <- abs(min(data$speechdate))
  data <- data %>%
    mutate(speechdate = speechdate + offset) %>%
    mutate(government = as.numeric(government)) %>%
    mutate(majority = as.numeric(majority))
    
}

### Remove blank speeches, topics, and stagedirections

data <- data %>%
  filter(!is.na(speechtext)) %>%
  filter(speechtext != "") %>%
  filter(speakerposition != "topic" | speakerposition=="" | is.na(speakerposition)) %>%
  filter(speakerposition != "subtopic" | speakerposition=="" | is.na(speakerposition)) %>%
  filter(speakerposition != "stagedirection" | speakerposition=="" | is.na(speakerposition)) %>%
  arrange(basepk)

### Create parlsess variable and dummy variables for Speaker, Minister and Interruption

data$speakeroldname <- tolower(data$speakeroldname)
data$speakername <- tolower(data$speakername)
data$speakerposition <- tolower(data$speakerposition)
data$speechtext <- tolower(data$speechtext)

data <- data %>%
  mutate(speaker=as.numeric(str_detect(speakeroldname, "speaker") | str_detect(speakerposition, "speaker"))) %>%
  mutate(inter=as.numeric(str_detect(speakername, "member|members") | str_detect(speakeroldname, "member|members"))) %>%
  mutate(minister=as.numeric(str_detect(speakeroldname, "minister") | str_detect(speakerposition, "minister") & government==1)) %>%
  mutate(speaker = ifelse(is.na(speaker),0,speaker)) %>%
  mutate(inter = ifelse(is.na(inter),0,inter)) %>%
  mutate(minister = ifelse(is.na(minister),0,minister)) %>%
  mutate(parlsess = as.numeric(paste(parlnum, sessnum, sep = "")))

data$parlsess <- factor(data$parlsess)
  
### Filter out divisions and responses to motions etc.

data <- data %>%
  filter(!(inter==1 & (str_detect(speechtext, "agreed.") | str_detect(speechtext, "motion") |
             str_detect(speechtext, "yes.") | str_detect(speechtext, "no.") |
             str_detect(speechtext, "nay.") | str_detect(speechtext, "division") |
             str_detect(speechtext, "question.") | str_detect(speechtext, "yea."))))

### Create "state" variable containing current speaker state (I[Interruption], 
#                                                     S[Speaker], 
#                                                     Gm[Government Minister], 
#                                                     Gb[Government Backbencher], 
#                                                     O[Opposition Member])

data$state[data$government==0] <- "O"
data$state[data$inter==1] <- "I"
data$state[data$speaker==1] <- "S"
data$state[data$minister==1] <- "Gm"
data$state[data$minister==0 & data$government==1] <- "Gb"
data$state <- factor(data$state)

### Create response variable state_next of next speaker state
### Final speech of each day (and thus of each parliament) is dropped via keepInvalid=FALSE, since it has no response

data <- slide(data, 'state', GroupVar = "speechdate", NewVar="state_next", slideBy = 1, keepInvalid = FALSE)
data$state_next <- factor(data$state_next, labels=levels(data$state))

### Convert speechdate and parlnum to factors (may be needed for estimation)

# data$speechdate <- factor(data$speechdate) # comment me out for numeric speechdate
data$parlnum <- factor(data$parlnum)

### Remove events that start or end with Speaker
### (We want to remove the Speaker but don't want to artificially inflate interruption events)

data <- data %>%
  filter(state != "S") %>%
  filter(state_next != "S")

data <- data[,c("speechdate", "parlnum", "state", "state_next", "parlsess", "majority")]
data <- data[complete.cases(data$state_next),]
data$state <- droplevels(data$state)
data$state_next <- droplevels(data$state_next)

### Save completed full table

print("Saving data.")

saveRDS(data,"data.Robject")