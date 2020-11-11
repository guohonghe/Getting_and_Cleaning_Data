# Getting and Cleaning Data Course Project

# Create folder to store data sets for project
if(!file.exists("project_answer")){dir.create("project_answer")}

# Input test data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", head=FALSE, sep="")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", head=FALSE, sep="")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", head=FALSE, sep="")

# Input train data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", head=FALSE, sep="")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", head=FALSE, sep="")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", head=FALSE, sep="")

# Input feature.txt and activity_labels.txt. 
features <- read.table("./UCI HAR Dataset/features.txt", head=FALSE, sep="")
activity <- read.table("./UCI HAR Dataset/activity_labels.txt", head=FALSE, sep="")

# Assign variable names
colnames(X_test) <- t(features[,2])
colnames(X_train) <- t(features[,2])
colnames(y_test) <- "activity_labels"
colnames(y_train) <- "activity_labels"
colnames(subject_test) <- "subjects"
colnames(subject_train) <- "subjects"

# Mark activities in y_test and y_train
for (i in 1:6) {
    for (j in 1:length(y_test[,1])) {
        if (y_test[j,1]==activity[i,1]) {
            y_test[j,1] <- activity[i,2]
        }
    }
    for (j in 1:length(y_train[,1])) {
        if (y_train[j,1]==activity[i,1]) {
            y_train[j,1] <- activity[i,2]
        }
    }
}

# Adding subjects and activity columns to both test and training datasets
X_test <-cbind(subject_test, y_test, X_test)
X_train <-cbind(subject_train, y_train, X_train)

# Q1: Merge the training and the test sets to one data set
X_data <- rbind(X_train, X_test)  # --> Q1 answer
write.csv(X_data, "./project_answer/Q5_1_3_4.csv")

# Q2: Extracts only the measurements on the mean and standard deviation for 
# each measurement. Here the meanfreq() is included. 
X_mean_std <-subset(X_data, select=(grepl("mean()", names(X_data))|
                        grepl("std()", names(X_data))))
X_mean_std <-cbind(X_data[,1:2], X_mean_std)    # --> Q2 answer
write.csv(X_mean_std, "./project_answer/Q5_2.csv")

# Q3: Uses descriptive activity names to name the activities in the data set
# This has been down previously. The whole data set (including test and train)
# are in X_data 
X_data  # --> Q3 answer

# Q4: Appropriately labels the data set with descriptive variable names
# This has been down previously. The whole data set (including test and train)
# are in X_data 
X_data  # --> Q4 answer

# Q5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject

# Subset the whole data set for variables including mean()
X_mean <-subset(X_data, select=(grepl("mean()", names(X_data))))
X_mean <-cbind(X_data[,1:2], X_mean) 

# Merge all column variables by average (using rowMeans())
X_mean$average <- rowMeans(X_mean[,3:48], na.rm=TRUE)
X_mean <- data.frame(X_mean[,1:2], X_mean$average)

# Group them by each activity and subject then averaging records
library(dplyr)
X_mean_sub_activity <- X_mean %>%           # Q5 answer
    group_by(subjects, activity_labels) %>%
    summarise(mean_average=mean(X_mean.average),n=n()) 
write.csv(X_mean_sub_activity, "./project_answer/Q5_5.csv")
