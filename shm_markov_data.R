#####
# shm_markov_data
# tanya.whyte@mail.utoronto.ca
# Processes lipad csv files into list of speaker transition events
#####

### Imports

library("data.table")
library("dplyr")
library("stringr")
library("RPostgreSQL")

### Random seed

set.seed(451)

### Use the .csv Lipad dataset files (includes government variable from experimental database)

# csvdir <- ("E:/Dissertation/Dropbox/Dropbox/lipad")

### Load data, but first remove dates with < 10 speeches as per Eggers and Spirling

# filelist <- list.files(path=file.path(csvdir), recursive=TRUE, full.name=TRUE)
# 
# freadwithremove <- function(f) {
#   debate <- fread(f, sep=",", 
#             encoding='UTF-8', na.strings="",
#             blank.lines.skip=TRUE,
#             select=c("basepk","speechdate", "pid", "parlnum", "sessnum", "speakeroldname", "speakername", "gender", "speechtext", "speakerposition", "government"))
#   nrows <- nrow(debate)
#   if (nrows < 10){
#     return()
#   } else {
#     return(debate)
#   }
# }

### Alternatively, connect directly to PostgreSQL database

m <- dbDriver("PostgreSQL")
con <- dbConnect(m, host="192.168.1.155", user="twhyte", password="coralviper86", dbname="dilipad_dev")
rs <- dbSendQuery(con, "select basepk,speechdate,pid,parlnum,sessnum,speakeroldname,speakername,gender,speechtext,speakerposition,government from dilipadsite_basehansard where speechdate > '1926-12-31' and speechdate < '2016-01-01'")
complete <- FALSE
df <- fetch(rs, n=-1)
while (!complete) {
  complete <- dbHasCompleted(rs)
}

data <- as.data.frame.matrix(df)
dbClearResult(rs)
dbDisconnect(con)

print("Loaded data.")

### Remove blank speeches, topics, and stagedirections

data <- data %>%
  filter(!is.na(speechtext)) %>%
  filter(speechtext != "") %>%
  filter(speakerposition != "topic" | speakerposition=="" | is.na(speakerposition)) %>%
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
  mutate(minister = ifelse(is.na(minister),0,minister))

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

# speakerstate <- function(i, s, m, g) {
#   if(i==1){
#     return("I")
#   }
#   else if(s==1){
#     return("S")
#   }
#   else if(m==1){
#     return("Gm")
#   }
#   else if(g==1 && m==0){
#     return("Gb")
#   }
#   else {
#     return("O")
#   }
# }

data$state[data$inter==1] <- "I"
data$state[data$speaker==1] <- "S"
data$state[data$minister==1] <- "Gm"
data$state[data$minister==0 & data$government==1] <- "Gb"
data$state[data$government==0] <- "O"

data$state <- factor(data$state)



