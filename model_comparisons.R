library(dplyr)
library(plyr)
library(AUC)#ddply
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

#------ read features extracted from train set, using your python script
db=read.csv('OutputTable.csv', stringsAsFactors = F)

# actual dataset with custom features
#db=read.csv('OutputTable2.csv', stringsAsFactors = F)

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
ctrl= trainControl(method = 'cv', number=5, summaryFunction=twoClassSummary ,classProbs = TRUE)

#============================================
#     Model 1: random forest classifier
#============================================

# set of features (with some custom features)
fs=c('TimeSinceLast','SubmissionNumber', 'NVideoEvents', 'NForumEvents',
     'NumberOfThreadViews', 'DurationOfVideoActivity', 'NumberOfThreadCreated',
     'NumberOfUpvotes', 'NumberOfPosts', 'NumberOfComments', 'NumberOfSeekEvents',
     'NumberVideoWatched', 'AverageVideoTimeDiffs')

training_data = db.train[,fs]

# get rid of predictors which have almost zero variance
nearzv = nearZeroVar(training_data, freqCut = 95/5)
training_data = training_data[,-nearzv]

# parameters grid
rfGrid <- expand.grid(mtry = c(1, 2, 3, 4)
                      #maxDepth = c(1,2,5, 10, 20)
                      )
random_forest=train(x=db.train[,fs],
            y=db.train$improved,
            method = "rf",
            metric="ROC",
            trControl = ctrl,
            tuneGrid = rfGrid,
           # tuneLength=5,
            preProc = c("center","scale","pca")
            )
print(random_forest);   plot(random_forest)

preds_rf = predict(random_forest, newdata=db.test[,fs]);
table(preds_rf)

confusionMatrix(db.test$improved, preds_rf)

ROC_curve= roc(preds_rf, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)
#============================================
#     Model 2: SVM
#============================================
svmGrid = expand.grid(sigma = c(.01, .015, 0.2),
                      C = c(0.75, 0.9, 1, 1.1, 1.25))
svm=train(x=db.train[,fs],
            y=db.train$improved,
            method = "svmPoly",
            metric="ROC",
            trControl = ctrl,
            tuneGrid = svmGrid,
            tuneLength= 9,
            preProc = c("center", "scale"))
print(svm);   plot(svm)

preds_svm = predict(svm, newdata=db.test[,fs]);
table(preds_svm)

confusionMatrix(db.test$improved, preds_svm)

ROC_curve= roc(preds_svm, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)
#============================================
#     Model 3: neural network
#============================================
nnGrid = expand.grid(size = c(1,2,3, 5, 10),
                     decay = c(0, 0.1, 0.5, 1))
nn=train(x=db.train[,fs],
          y=db.train$improved,
          method = "nnet",
          metric="ROC",
          trControl = ctrl,
          tuneGrid = nnGrid,
          #tuneLength= 9,
          #preProc = c("center", "scale", "pca")
         )
print(nn);   plot(nn)
test_data = db.test[,fs][,-nearzv]
preds= predict(nn, newdata=test_data);
table(preds)
ROC_curve= roc(preds, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)
#============================================
#     Model 4: gradient boosting classifier
#============================================
gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                        n.trees = c(50,100, 150, 200), 
                        shrinkage = c(0.001, 0.01, 0.1, 0.5),
                        n.minobsinnode = (5,10, 20, 50)
gbm=train(x=db.train[,fs],
         y=db.train$improved,
        #improved ~ . - improved - Grade - GradeDiff - StudentID - TimeStamp - ProblemID,
        #data = db.train,
         method = "gbm",
         metric="ROC",
         trControl = ctrl,
         tuneGrid = gbmGrid,
         #tuneLength= 9,
         preProc = c("center", "scale", "pca")
         )
print(gbm); plot(gbm)

preds= predict(gbm, newdata=db.test[,fs]);
table(preds)
confusionMatrix(preds, db.test$improved)
ROC_curve= roc(preds, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)