
#First we read metadata

feat <- read.table("features.txt")

actvt <- read.table("activity_labels.txt", header = FALSE)

#Then we load in the training data

feattrn <- read.table("train/X_train.txt", header = FALSE)

acttrn <- read.table("train/y_train.txt", header = FALSE)

subtrn <- read.table("train/subject_train.txt", header = FALSE)

#Next we load in the test data

feattst <- read.table("test/X_test.txt", header = FALSE)

acttst <- read.table("test/y_test.txt", header = FALSE)

subtst <- read.table("test/subject_test.txt", header = FALSE)

#The next step is to merge the data

features <- rbind(feattrn, feattst)

activity <- rbind(acttrn, acttst)

subject <- rbind(subtrn, subtst)

colnames(features) <- t(feat[2])

colnames(activity) <- "Activity"

colnames(subject) <- "Subject"

alldata <- cbind(features,activity,subject)

#The next step is to extract the mean and standaard deviation

meanstdmeas <- grep(".*Mean.*|.*Std.*", names(alldata), ignore.case=TRUE)

neccols <- c(meanstdmeas, 562, 563)

extract <- alldata[,neccols]

#Next we describe the activities

extract$Activity <- as.character(extract$Activity) 
for (i in 1:6){ extract$Activity[extract$Activity == i] <- as.character(actvt[i,2])}

extract$Activity <- as.factor(extract$Activity)

#Applying proper labels

names(extract)<-gsub("tBody", "time body ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("Acc", "accelerometer ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("-std()", "- std", names(extract), ignore.case = TRUE)

names(extract)<-gsub("tGravity", "time gravity ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("-mad()", "- mad", names(extract), ignore.case = TRUE)

names(extract)<-gsub("-max()", "- max", names(extract), ignore.case = TRUE)

names(extract)<-gsub("Jerk()", "jerk ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("BodyBody", "body ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("fBody", "freq body ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("bandsEnergy()", " energy ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("gravity", "gravity ", names(extract), ignore.case = TRUE)

names(extract)<-gsub("-meanFreq()", "- mean frequency", names(extract), ignore.case = TRUE)

names(extract)<-gsub("-Mean()", "- mean", names(extract), ignore.case = TRUE)

names(extract)<-gsub("Mag", "magnitude", names(extract), ignore.case = TRUE)

names(extract)<-gsub("angle", " angle", names(extract), ignore.case = TRUE)

names(extract)<-gsub("Gyro", "gyroscope ", names(extract), ignore.case = TRUE)

#Finally we create the tidy dataset

extract$Subject <- as.factor(extract$Subject)

extract <- data.table(extract)

tidydataset <- aggregate(. ~Subject + Activity, extract, mean)

tidydataset <- tidydataset[order(tidydataset$Subject,tidydataset$Activity),]

write.table(tidydataset, file = "tidydataset.txt", row.names = FALSE)
