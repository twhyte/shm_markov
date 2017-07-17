#####
# shm_markov_data
# Processes lipad db or csv files into list of speaker transition events
#####

### Imports

library("data.table")
library("dplyr")
library("stringr")
library("RPostgreSQL")
library("DataCombine")

### Use the .csv Lipad dataset files (dev version with additional data included)

# csvdir <- ("E:/Dissertation/Dropbox/Dropbox/lipad")

### Load data, but first remove dates with < 10 speeches as per Eggers and Spirling

# filelist <- list.files(path=file.path(csvdir), recursive=TRUE, full.name=TRUE)
# 
# freadwithremove <- function(f) {
#   debate <- fread(f, sep=",", 
#             encoding='UTF-8', na.strings="",
#             blank.lines.skip=TRUE,
#             select=c("basepk","speechdate", "pid", "parlnum", "sessnum", "speechtext", speakername", maintopic", "speakeroldname", "gender", "speakerposition", "government"))
#   nrows <- nrow(debate)
#   if (nrows < 10){
#     return()
#   } else {
#     return(debate)
#   }
# }

### Alternatively, connect directly to PostgreSQL database (dev version with additional data included)
### Date: newer than '1926-12-31'


m <- dbDriver("PostgreSQL")
con <- dbConnect(m, host="xxx.xxx.xxx.xxx", user="", password="", dbname="dilipad_dev")
rs <- dbSendQuery(con, "select basepk,speechdate,pid,parlnum,sessnum,speakername,speechtext,maintopic,speakeroldname,gender,speakerposition,government from dilipadsite_basehansard where speechdate > '1926-12-31' and speechdate < '2015-11-30'")
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
  ### Convert posix 'speechdate' to origin at start of dataset; convert 'government' back to 0/1 numeric
  
  offset <- abs(min(data$speechdate))
  data <- data %>%
    mutate(speechdate = speechdate + offset) %>%
    mutate(government = as.numeric(government))
    
}

### Remove blank speeches, topics, and stagedirections

data <- data %>%
  filter(!is.na(speechtext)) %>%
  filter(speechtext != "") %>%
  filter(speakerposition != "topic" | speakerposition=="" | is.na(speakerposition)) %>%
  filter(speakerposition != "subtopic" | speakerposition=="" | is.na(speakerposition)) %>%
  filter(speakerposition != "stagedirection" | speakerposition=="" | is.na(speakerposition)) %>%
  arrange(basepk)

### Create dummy variables for Speaker, Minister and Interruption

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
  
### Filter out divisions and responses to motions etc.

data <- data %>%
  filter(!(inter==1 & (str_detect(speechtext, "agreed.") | str_detect(speechtext, "motion") |
             str_detect(speechtext, "yes.") | str_detect(speechtext, "no.") |
             str_detect(speechtext, "nay.") | str_detect(speechtext, "division") |
             str_detect(speechtext, "question.") | str_detect(speechtext, "yea."))))

### Create a collapsed Parliament-Session count (potential todo)


### Create variable containing current speaker state (I[Interruption], 
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
data$parlsess <- factor(data$parlsess)

### Create response variable state_next of next speaker state
### Final speech of each day (and thus of each parliament) is "dropped" via keepInvalid=FALSE, since it has no response

data <- slide(data, 'state', GroupVar = "speechdate", NewVar="state_next", slideBy = 1, keepInvalid = FALSE)
data$state_next <- factor(data$state_next, labels=levels(data$state))

### Convert speechdate and parlnum to factors (needed for estimation)

# data$speechdate <- factor(data$speechdate) # comment out for numeric purposes
data$parlnum <- factor(data$parlnum)

data <- data[,c("speechdate", "parlnum", 'state', "state_next", "parlsess")]
data <- data[complete.cases(data$state_next),]

### Save completed full table

print("Saving data.")

saveRDS(data,"data1926parlsess.Robject")