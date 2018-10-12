#loading the necessarily package
library(dplyr)
#loading the data zip,if no such zip download and create a new dir.
dataurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
datadir<-"UCI HAR Dataset"
datafile<-"UCI HAR Dataset.zip"
if(!(datadir %in% list.files())){
  download.file(dataurl,datafile)
  unzip(datafile)
}
#read sets
features<-read.table("UCI HAR Dataset/features.txt")
actlabel<-read.table("UCI HAR Dataset/activity_labels.txt")

testset<-read.table("UCI HAR Dataset/test/X_test.txt")
testlabel<-read.table("UCI HAR Dataset/test/y_test.txt")
testsubject<-read.table("UCI HAR Dataset/test/subject_test.txt")

trainset<-read.table("UCI HAR Dataset/train/X_train.txt")
trainlabel<-read.table("UCI HAR Dataset/train/y_train.txt")
trainsubject<-read.table("UCI HAR Dataset/train/subject_train.txt")
#Step 1 Merges the training and the test sets to create one data set
allset<-rbind(cbind(testsubject,testset,testlabel),cbind(trainsubject,trainset,trainlabel))
names(allset)<-c("No",as.character(features[,2]),"activity")
#Note:clean the used data can save the RAM 
rm(testset,testlabel,testsubject,trainsubject,trainlabel,trainset)

#Step 2 Extracts only the measurements on the mean and standard deviation for each measurement
keep<- grepl("No|activity|mean|std",names(allset))
allset<-allset[keep]
#Step 3 Uses descriptive activity names to name the activities in the data set
allset<-mutate(allset,Activity=actlabel[allset$activity,2])
allset<-select(allset,-activity)
## other way:
## humanActivity$activity <- factor(humanActivity$activity, 
## levels = activities[, 1], labels = activities[, 2])
#Step 4 Appropriately label the data set with descriptive variable names
allsetname<-names(allset)
allsetname <- gsub("^f", "frequencyDomain", allsetname)
allsetname <- gsub("^t", "timeDomain", allsetname)
allsetname <- gsub("Acc", "Accelerometer", allsetname)
allsetname <- gsub("Gyro", "Gyroscope", allsetname)
allsetname <- gsub("Mag", "Magnitude", allsetname)
allsetname <- gsub("Freq", "Frequency",allsetname)
allsetname <- gsub("mean", "Mean", allsetname)
allsetname <- gsub("std", "StandardDeviation", allsetname)
allsetname <- gsub("\\(\\)(-*)", "",allsetname)
allsetname <- gsub("BodyBody", "Body", allsetname)
names(allset)<-allsetname
#Step 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
allsetmean<- allset %>% group_by(No,Activity) %>% summarise_all(funs(mean))
#create a tidy_data.txt 
write.table(allsetmean,"tidy_data.txt",row.names = F,quote = FALSE)