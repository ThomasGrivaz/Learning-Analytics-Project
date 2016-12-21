library(dplyr)
library(plyr)
library(AUC)
library(caret)
#ddply
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

#------ read features extracted from train set, using your python script
db=read.csv('OutputTable.csv', stringsAsFactors = F)

# actual dataset with custom features
#db=read.csv('features.csv', stringsAsFactors = F)

#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]

#--- replace NA values with 0
db[is.na(db)]=0

#----- remove first submissions
db= filter(db, SubmissionNumber>0)

# add the average time between submissions (maybe mor suited for regression)
db = ddply(db, .(UserID), transform, AvgTimeBwSubs = mean(TimeSinceLast))

# add the activity rate (fraction of time there is activity between two submissions)
# also maybe more suited for regression
db$NVideoAndForum= db$NVideoEvents+db$NForumEvents
db = ddply(db, .(UserID), transform, ActivityRate = sum(NVideoAndForum!=0)/length(UserID))

#---- remove cases when there is no video or forum activity between two submissions

db$TimeSinceLastVideo = db$TimeStamp - db$LastVideoEvent
db$TimeSinceLastForum = db$TimeStamp - db$LastForumEvent
db= filter(db,NVideoAndForum>0)  

#----- make a catgorical vribale, indicating if grade improved
db$improved = factor(ifelse(db$GradeDiff>0 ,'Yes', 'No' ))
table(db$improved)

write.csv(db, file='data/data_preprocessed.csv', row.names=FALSE)
# ----- (Optional) split your training data into train and test set. Use train set to build your classifier and try it on test data to check generalizability. 
set.seed(1234)
tr.index= sample(nrow(db), nrow(db)*0.9)
db.train= db[tr.index,]
db.test = db[-tr.index,]
dim(db.train)
dim(db.test)

# I think this is wrong to split data like this. Since observations are highly correlated
# and not independent at all, grade improvement will depend on previous grade and user
# for example. Thus we split data as such: we randomly sample 90% of users (but we keep
# the ordering of problemID and submissionNumber for one user) for training data and the
# remaing 10 % will go to test data
users = unique(db$UserID)
set.seed(1234)
user_tr_index = sample(length(users), length(users)*0.9)
training_users = users[user_tr_index]
db.train = subset(db, UserID %in% training_users)
db.test = subset(db, !UserID %in% training_users)
# cross validation
ctrl= trainControl(method = 'cv', number=5, summaryFunction=twoClassSummary ,classProbs = TRUE)
ctrl = trainControl(method='repeatedcv', number=5, repeats=3, summaryFunction=twoClassSummary ,classProbs = TRUE)

## Exploratory data analysis

db_no_improvement = filter(db, improved == 'No')
db_improvement = filter(db, improved == 'Yes')

t.test(db_improvement$TimeSinceLast, db_no_improvement$TimeSinceLast)
ks.test(db_improvement$ActivityRate, db_no_improvement$ActivityRate)

#============================================
#     Model 1: random forest classifier
#============================================

# set of features (with some custom features)
fs=c('TimeSinceLast', 
     'SubmissionNumber',
     'ProblemID',
     'NumberOfSlowPlay',
     'RewatchingScore',
     'DurationOfVideoActivity',
     'NumberOfVideoInteractions',
     'NumberVideoWatched',
     'NVideoEvents',
     'TimeSinceLastVideo',
     'TimeSinceLastForum',
     'AvgTimeBwSubs',
     'TimeSpentOnForum',
     'NumberOfPosts',
     'NumberOfThreadViews',
     'AverageVideoTimeDiffs',
     'NForumEvents',
     'ActivityRate')

db.train = subset(db.train, select=-c(UserID, Grade, GradeDiff, improved))
db.test = subset(db.test, select=-c(UserID, Grade, GradeDiff, improved))

# correlation matrix (to find redundant features)
correlationMatrix = cor(db.train[,fs])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.8, verbose=TRUE)
highlyCorrelated_cols = colnames(db.train[,fs][highlyCorrelated])

training_data = db.train[,fs]
test_data = db.test[,fs]
training_data = training_data[,-highlyCorrelated]
test_data = test_data[,-highlyCorrelated]

# get rid of predictors which have almost zero variance
nearzv = nearZeroVar(db.train[,fs], freqCut = 95/5, uniqueCut=5)
nearzv_cols = colnames(training_data[nearzv])
training_data = training_data[,-nearzv]
test_data = test_data[,-nearzv]


# parameters grid
rfGrid <- expand.grid(mtry = c(1:5)
                      #maxDepth = c(1,2,5, 10, 20)
                      )
random_forest=train(x=training_data,
            y=db.train$improved,
            method = "rf",
            metric="ROC",
            trControl = ctrl,
            tuneGrid = rfGrid,
            #preProc = c("center", "scale")
            preProc = c("range")
            )
print(random_forest);   plot(random_forest)

importance = varImp(random_forest, scale=FALSE)
plot(importance)

preds_rf = predict(random_forest, newdata=test_data);#db.test[,fs]);
#preds_rf = predict(random_forest, newdata=db_temp_test)
table(preds_rf)

confusionMatrix(preds_rf, db.test$improved)

ROC_curve= roc(preds_rf, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)
#============================================
#     Model 2: SVM
#============================================
svmGrid = expand.grid(sigma = c(1e-1, 0.5, 1, 2, 10, 50),
                      C = c(0.0001, 0.001, 0.01, 0.1, 0.5, 1, 5))
#svmGrid = expand.grid(degree=c(1,2,3,4,5,7,10),
 #                     scale=1,
  #                    C=c(0.1, 0.75, 0.9, 1, 1.1, 1.25))
svm=train(x=training_data,
            y=db.train$improved,
            method = "svmRadial",
            metric="ROC",
            trControl = ctrl,
            tuneGrid = svmGrid,
            tuneLength = 9,
            preProc = c("center", "scale")
            #preProc=c('pca')
          )
print(svm);   plot(svm)
preds_svm = predict(svm, newdata=test_data);
table(preds_svm)

confusionMatrix(preds_svm, db.test$improved)

ROC_curve= roc(preds_svm, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)
#============================================
#     Model 3: neural network
#============================================
nnGrid = expand.grid(size = c(1,2,3, 5, 10, 50, 100),
                     decay = c(0, 0.1, 0.5, 1, 2))
nn=train(x=training_data,
          y=db.train$improved,
          method = "nnet",
          metric="ROC",
          trControl = ctrl,
          tuneGrid = nnGrid,
          #tuneLength= 9,
          preProc = c("range")#, "scale")
         )
print(nn);   plot(nn)
preds= predict(nn, newdata=test_data);
table(preds)
ROC_curve= roc(preds, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)
#============================================
#     Model 4: gradient boosting classifier
#============================================
gbmGrid <-  expand.grid(interaction.depth = 1, 
                        n.trees = c(50,100, 150, 200, 250, 300), 
                        shrinkage = c(0.001, 0.01, 0.1, 0.5),
                        n.minobsinnode = c(5,10,15, 20)
                        )
gbm=train(x=training_data,
          y= db.train$improved,
         method = "gbm",
         metric="ROC",
         trControl = ctrl,
         tuneGrid = gbmGrid,
         tuneLength= 9,
         preProc = c("scale")
         )
print(gbm); plot(gbm)

preds= predict(gbm, newdata=test_data);
table(preds)
confusionMatrix(preds, db.test$improved)
ROC_curve= roc(preds, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)

knn = train(x=training_data,
            y=db.train$improved,
            metric="ROC",
            method='knn',
            tuneGrid = expand.grid(k=c(1,2,3,4,5,7)),
            trControl=ctrl,
            preProc = c("center", "scale")
            )

print(knn);   plot(knn)

importance = varImp(knn, scale=FALSE)
print(importance)

preds_knn = predict(knn, newdata=test_data);#db.test[,fs]);
#preds_rf = predict(random_forest, newdata=db_temp_test)
table(preds_knn)

confusionMatrix(preds_knn, db.test$improved)

ROC_curve= roc(preds_knn, db.test$improved);  auc(ROC_curve)
plot(ROC_curve)

