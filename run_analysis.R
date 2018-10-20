
# Getting and Cleaning Data Course Project Getting and Cleaning Data Course Project
###################################################################################
# Link to description of data from site: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# Data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##################################################################################################
# string variables for the downloaded file

fileName <- "UCIdata.zip"

url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir <- "UCI HAR Dataset"

# File download verification. If file does not exist, download to working directory.

if(!file.exists(fileName)){

        download.file(url,fileName, mode = "wb") 

}

# File unzip verification. If the directory does not exist, unzip the downloaded file.

if(!file.exists(dir)){

	unzip("UCIdata.zip", files = NULL, exdir=".")

}

## Read the data

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("UCI HAR Dataset/test/X_test.txt")

X_train <- read.table("UCI HAR Dataset/train/X_train.txt")

y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

features <- read.table("UCI HAR Dataset/features.txt")  


## Analysis of data

# Merge the training and the test sets to create one data set.

dataSet <- rbind(X_train,X_test)


# Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a vector of only mean and std, use the vector to subset.

MeanStdOnly <- grep("mean()|std()", features[, 2]) 

dataSet <- dataSet[,MeanStdOnly]


# Label the dataset with descriptive activity names.

# Create a vector of "Clean" feature names by getting rid of "()" apply to the dataSet to rename labels.

CleanFeatureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})

names(dataSet) <- CleanFeatureNames[MeanStdOnly]


# Descriptive Lables after combining test and train set for subject data and activity data

subject <- rbind(subject_train, subject_test)

names(subject) <- 'subject'

activity <- rbind(y_train, y_test)

names(activity) <- 'activity'


# Create the final dataset by combining  mean and std deviation only for each measurement

dataSet <- cbind(subject,activity, dataSet)


# 3. Uses descriptive activity names to name the activities in the data set

# group the activity column of dataSet, re-name lable of levels with activity_levels, and apply it to dataSet.

act_group <- factor(dataSet$activity)

levels(act_group) <- activity_labels[,2]

dataSet$activity <- act_group


# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# check if reshape2 package is installed

if (!"reshape2" %in% installed.packages()) {

	install.packages("reshape2")

}

library("reshape2")


# write the tidy data to the working directory as "tidy_data.txt"

baseData <- melt(dataSet,(id.vars=c("subject","activity")))

secondDataSet <- dcast(baseData, subject + activity ~ variable, mean)

names(secondDataSet)[-c(1:2)] <- paste("[mean of]" , names(secondDataSet)[-c(1:2)] )

write.table(secondDataSet, "tidy_data.txt", sep = ",", row.name=FALSE)
