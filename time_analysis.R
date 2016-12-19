library(dplyr)
library(plyr)
library(AUC)
library(caret)

#=========================================================================
#   New analysis: take into account time dependency and sequence analysis
#=========================================================================

# import data
db=read.csv('OutputTable.csv', stringsAsFactors = F)

# sort submissions by user, problem and submission
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]

# replace NA values with 0
db[is.na(db)]=0

# remove first submissions
db= filter(db, SubmissionNumber>0)

# add forum and video events
db$NVideoAndForum= db$NVideoEvents+db$NForumEvents

# remove cases when there is no video or forum activity between two submissions
db= filter(db,NVideoAndForum>0) 

# make a catgorical vribale, indicating if grade improved
db$improved = factor(ifelse(db$GradeDiff>0 ,'Yes', 'No' ))
table(db$improved)

# data splitting
users = unique(db$UserID)
set.seed(1234)
user_tr_index = sample(length(users), length(users)*0.9)
training_users = users[user_tr_index]
db.train = subset(db, UserID %in% training_users)
db.test = subset(db, !UserID %in% training_users)

# cross validation
ctrl= trainControl(method = 'cv', number=5, summaryFunction=twoClassSummary ,classProbs = TRUE)


# feature set
fs=c('TimeSinceLast','SubmissionNumber', #'NVideoEvents', 'NForumEvents',
     'ProblemID', 'NumberOfSlowPlay', 'NumberOfVideoInteractions', 'NumberVideoWatched',
     'TimeSpentOnForum', 'NumberOfPosts', 'NumberOfThreadViews', 'DurationOfVideoActivity')











