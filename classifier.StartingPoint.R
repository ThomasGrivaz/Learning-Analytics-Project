library(dplyr)
library(plyr) #ddply
#------ read features
db=read.csv('OutputTable.csv', stringsAsFactors = F)

#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]

#--- replace NA values with 0
db[is.na(db)]=0

#----- remove first submissions
db= filter(db,SubmissionNumber>0)

#---- remove cases when there is no video or forum activity between two submissions
db$NVideoAndForum= db$NVideoEvents+db$NForumEvents
db= filter(db,NVideoAndForum>0)  

#----- make a catgorical vribale, indicating if grade improved
db$improved = factor(if_else(db$GradeDiff>0 ,'Yes', 'No' ))
table(db$improved)

#----- visualize features per each category
boxplot(db$TimeSinceLast ~ db$improved , horizontal = T, outline=F)
boxplot(db$NForumEvents ~ db$improved , horizontal = T, outline=F)
boxplot(db$NVideoEvents ~ db$improved , horizontal = T, outline=F)
boxplot(db$NumberOfThreadViews ~ db$improved , horizontal = T, outline=F)


#============ train a classifier to predict 'improved' status =============

# ----- 1. split data into train and test set
set.seed(1234)
tr.index= sample(nrow(db), nrow(db)*0.8)
db.train= db[tr.index,]
db.test = db[-tr.index,]


#-----
# Train a classifier to identify which features are most predictive
# of an increase versus decrease of the grade. Try different methods, 
# model parameters, features set and find the best classifier (highest 'kappa' value on test set.)
# try to achieve a model with kappa > 0.5 
#
library(caret)

# should have near 100% accuracy - predict grade improvement based on numerical grade improvement
rf1 <- train(improved~GradeDiff, data = db.train, method = "rf")
pred1 = predict(rf1, newdata=db.test)
cm1 = confusionMatrix(pred1, db.test$improved)

# test SVM with our custom feature(s)
svm2 <- train(improved~NumberVideoWatched, data = db.train, method = "svmLinear")
pred2 = predict(svm2, newdata=db.test)
cm2 = confusionMatrix(pred2, db.test$improved)
