#Author: Sindhu S Chandran
#Towards submission for Getting and Cleaning Data week 4 assignment

#set working directory
setwd("./UCI HAR Dataset/train")
getwd()

#Add libraries used
library(dplyr)
library(qdapTools)

#read training data
#read subject file
subject <- read.table("subject_train.txt",header=FALSE)
#inspect subject data
str(subject)
unique(subject)
#read feature variables
measurements <- read.table("X_train.txt",header=FALSE)
#inspect feature info
str(measurements)
#read activity labels
labels <- read.table("y_train.txt",header=FALSE)
#inspect labels info
str(labels)
unique(labels)
#combine subject, measurement features and activity labels to single data frame
mergedtrain1 <-cbind(measurements,subject)
str(mergedtrain1)
mergedtrain2 <- cbind(mergedtrain1,labels)
str(mergedtrain2)
#repeat for test data
setwd("../test")
getwd()
#read test data
#read subject file
subject <- read.table("subject_test.txt",header=FALSE)
#inspect subject data
str(subject)
unique(subject)
#read feature variables
measurements <- read.table("X_test.txt",header=FALSE)
#inspect feature info
str(measurements)
#read activity labels
labels <- read.table("y_test.txt",header=FALSE)
#inspect labels info
str(labels)
unique(labels)
#combine subject, measurement features and activity labels to single data frame
mergedtest1 <-cbind(measurements,subject)
str(mergedtest1)
mergedtest2 <- cbind(mergedtest1,labels)
str(mergedtest2)


#now combine train and test data 
#Q1. Merges the training and the test sets to create one data set.
dim(mergedtrain2)
dim(mergedtest2)
completeData <- rbind(mergedtrain2,mergedtest2)
str(completeData)

#Q2. Extracts only the measurements on the mean and standard deviation for each measurement.
#column 1 to 561 = feature variables
#column 562 = subject
#column 563 = activity label
setwd("../")
#read feature variable names
featureNames <- read.table("features.txt",header = FALSE)
str(featureNames)
dim(featureNames)
names(completeData)[562] <- paste("Subject")
names(completeData)[563] <- paste("ActivityLabel")
names(completeData)[1:561] <- paste(featureNames[,2])
str(completeData)

#select only mean and std deviation by looking for mean
i <- grep("mean", names(completeData))
names(completeData)[i]
j <- grep("std", names(completeData))
names(completeData)[j]
names(completeData)[j]
selectedCols <- c(names(completeData)[i],names(completeData)[j])
length(selectedCols)
selectedCols

selectedData <- completeData[,c(which(names(completeData) %in% selectedCols),562,563)]
str(selectedData)

#Q3. Uses descriptive activity names to name the activities in the data set
activityNames <- read.table("activity_labels.txt",header = FALSE)
activityNames

str(activityNames)

selectedData$activityName <- lookup(selectedData$ActivityLabel, activityNames[, 1:2])
head(selectedData,20)
selectedData <- selectedData[,-81]


#Q4. Appropriately labels the data set with descriptive variable names.

names(selectedData) [1:6]<- c("tBodyAccelerationMeanX","tBodyAccelerationMeanY","tBodyAccelerationMeanZ","tBodyAccelerationStdX","tBodyAccelerationStdY","tBodyAccelerationStdZ")
names(selectedData) [7:12]<- c("tGravityAccelerationMeanX","tGravityAccelerationMeanY","tGravityAccelerationMeanZ","tGravityAccelerationStdX","tGravityAccelerationStdY","tGravityAccelerationStdZ")
names(selectedData) [13:18]<- c("tBodyAccJerkMeanX","tBodyAccJerkMeanY","tBodyAccJerkMeanZ","tBodyAccJerkStdX","tBodyAccJerkStdY","tBodyAccJerkStdZ")
names(selectedData) [19:24]<- c("tBodyGyroMeanX","tBodyGyroMeanY","tBodyGyroMeanZ","tBodyGyroStdX","tBodyGyroStdY","tBodyGyroStdZ")
names(selectedData) [25:30]<- c("tBodyGyroJerkMeanX","tBodyGyroJerkMeanY","tBodyGyroJerkMeanZ","tBodyGyroJerkStdX","tBodyGyroJerkStdY","tBodyGyroJerkStdZ")
names(selectedData) [31:36]<- c("tBodAccMagMean","tBodyAccMagStd","tGravityAccMagMean","tGravityAccMagStd","tBodyAccJerkMagMean","tBodyAccJerkMagStd")
names(selectedData) [37:40]<- c("tBodGyroMagMean","tBodyGyroMagStd","tBodyGyroJerkMagMean","tBodyGyroJerkMagStd")

names(selectedData) [41:46]<- c("fBodyAccelerationMeanX","fBodyAccelerationMeanY","fBodyAccelerationMeanZ","fBodyAccelerationStdX","fBodyAccelerationStdY","fBodyAccelerationStdZ")
names(selectedData)[47:49] <- c("fBodyAccelerationMeanFreqX","fBodyAccelerationMeanFreqY","fBodyAccelerationMeanFreqZ")
names(selectedData) [50:55]<- c("fBodyAccJerkMeanX","fBodyAccJerkMeanY","fBodyAccJerkMeanZ","fBodyAccJerkStdX","fBodyAccJerkStdY","fBodyAccJerkStdZ")
names(selectedData)[56:58] <- c("fBodyAccJerkMeanFreqX","fBodyAccJerkMeanFreqY","fBodyAccJerkMeanFreqZ")
names(selectedData) [59:64]<- c("fBodyGyroMeanX","fBodyGyroMeanY","fBodyGyroMeanZ","fBodyGyroStdX","fBodyGyroStdY","fBodyGyroStdZ")
names(selectedData)[65:67] <- c("fBodyGyroMeanFreqX","fBodyGyroMeanFreqY","fBodyGyroMeanFreqZ")
names(selectedData)[68:70] <- c("fBodyAccMagMean","fBodyAccMagStd","fBodyAccMagMeanFreq")
names(selectedData)[71:73] <- c("fBodyAccJerkMagMean","fBodyAccJerkMagStd","fBodyAccJerkMagMeanFreq")
names(selectedData)[74:76] <- c("fBodyGyroMagMean","fBodyGyroMagStd","fBodyGyroMagMeanFreq")
names(selectedData)[77:79] <- c("fBodyGyroJerkMagMean","fBodyGyroJerkMagStd","fBodyGyroJerkMagMeanFreq")
names(selectedData)[80:81] <- c("SubjectId","ActivityName")
str(selectedData)

#Q5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
aggdata <-aggregate(selectedData[1:79], by=list(selectedData$ActivityName,selectedData$SubjectId),FUN=mean, na.rm=TRUE)

head(selectedData,2)
str(aggdata)
head(aggdata,2)

names(aggdata)[1:2] <- c("ActivityName","SubjectId")

setwd("../")
getwd()
#write to file
write.table(aggdata, file = "tidy_data.txt", row.names = FALSE)




