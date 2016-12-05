library(dplyr)
library(plyr) #ddply
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

#------ read features extracted from train set, using your python script
#db=read.csv('OutputTable.csv', stringsAsFactors = F)

# actual dataset with custom features
db = read.csv('OutputTable2.csv', stringsAsFactors = F)

#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]

#--- replace NA values with 0
db[is.na(db)]=0

#----- remove first submissions
db= filter(db, SubmissionNumber>0)

#---- remove cases when there is no video or forum activity between two submissions
db$NVideoAndForum= db$NVideoEvents+db$NForumEvents
db= filter(db,NVideoAndForum>0)  

#----- make a catgorical vribale, indicating if grade improved
db$improved = factor(ifelse(db$GradeDiff>0 ,'Yes', 'No' ))
table(db$improved)

# ----- (Optional) split your training data into train and test set. Use train set to build your classifier and try it on test data to check generalizability. 
set.seed(1234)
tr.index= sample(nrow(db), nrow(db)*0.9)
db.train= db[tr.index,]
db.test = db[-tr.index,]
dim(db.train)
dim(db.test)

# cross validation
ctrl= trainControl(method = 'cv', summaryFunction=twoClassSummary ,classProbs = TRUE)

#============================================
#     Model 1: random forest classifier
#============================================

# set of features (with some custom features)
fs=c('TimeSinceLast','SubmissionNumber', 'NVideoEvents', 'NForumEvents',
     'NumberOfThreadViews', 'DurationOfVideoActivity', 'TimeSinceLast',
     'NumberVideoWatched')
paramGrid <- expand.grid(mtry = c(1,2,3, 4, 5, 6, 7, 8))
model=train(x=db.train[,fs],
            y=db.train$improved,
            method = "rf",
            metric="ROC",
            trControl = ctrl,
            tuneGrid = paramGrid
            #preProc = c("center", "scale"),
            )
print(model);   plot(model)

#============================================
#     Model 2: SVM
#============================================
model=train(x=db.train[,fs],
            y=db.train$improved,
            method = "svmRadial",
            metric="ROC",
            trControl = ctrl,
            preProc = c("center", "scale"))
print(model);   plot(model)