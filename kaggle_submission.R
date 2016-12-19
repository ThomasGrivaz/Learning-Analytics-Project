model = random_forest

testDb=read.csv('OutputTable_test.csv', stringsAsFactors = F)
testDb$Grade=NULL; testDb$GradeDiff=NULL;
testDb[is.na(testDb)]=0
testDb = ddply(testDb, .(UserID), transform, AvgTimeBwSubs = mean(TimeSinceLast))
testDb$NVideoAndForum= testDb$NVideoEvents+testDb$NForumEvents
testDb = ddply(testDb, .(UserID), transform, ActivityRate = sum(NVideoAndForum!=0)/length(UserID))
testDb$TimeSinceLastVideo = testDb$TimeStamp - testDb$LastVideoEvent
testDb$TimeSinceLastForum = testDb$TimeStamp - testDb$LastForumEvent

#---- use trained model to predict progress for test data
preds= predict(model, newdata=testDb[,fs]);

#======================================================================== 
#         step 2.1: prepare submission file for kaggle
#======================================================================== 

cl.Results=testDb[,c('ProblemID', 'UserID', 'SubmissionNumber')]
cl.Results$improved=preds
levels(cl.Results$improved)=c(0,1) # 
cl.Results$uniqRowID= paste0(cl.Results$UserID,'_', cl.Results$ProblemID,'_', cl.Results$SubmissionNumber)
cl.Results=cl.Results[,c('uniqRowID','improved')]
table(cl.Results$improved)

#----- keep only rows which are listed in classifier_templtae.csv file
#----- this excludes first submissions and cases with no forum and video event in between two submissions
classifier_template= read.csv('data/classifier_template.csv', stringsAsFactors = F)
kaggleSubmission=merge(classifier_template,cl.Results )
write.csv(kaggleSubmission,file='data/classifier_results.csv', row.names = F)


#------- submit the resulting file (classifier_results.csv) to kaggle 
#------- report AUC in private score in your report


