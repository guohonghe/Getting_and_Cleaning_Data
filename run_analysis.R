# Getting and Cleaning Data Course Project

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
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", head=FALSE, sep="")

# Assign variable names
colnames(X_test) <- t(features[,2])
colnames(X_train) <- t(features[,2])
colnames(y_test) <- "activities"
colnames(y_train) <- "activities"
colnames(subject_test) <- "subjects"
colnames(subject_train) <- "subjects"

# Q1: Merge the training and the test sets to one data set
X_all <- rbind(X_train, X_test)  # --> Q1 answer
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test)

# Q2: Extracts only the measurements on the mean and standard deviation for 
# each measurement. Here the "meanfreq" is included but angle("xxxMean",xxx) isn't. 
tidyData <-subset(X_all, select=(grepl("mean", names(X_all))|
                        grepl("std", names(X_all)))) # --> Q2 answer

# Q3: Uses descriptive activity names to name the activities in the data set
# Mark activities in y_all data set
y_all$activities <- activity_labels[y_all$activities, 2]

# Adding "subjects" and "activities" columns to tidyData
tidyData <-cbind(subject_all, y_all, tidyData)      # --> Q3 answer
varNames_org <-names(tidyData)

# Q4: Appropriately labels the data set with descriptive variable names
names(tidyData)<-gsub("^t", "time", names(tidyData))    # change ^t to ^time
names(tidyData)<-gsub("^f", "freq", names(tidyData))    # change ^f to ^freq
names(tidyData)<-gsub("[-|(|)]", "", names(tidyData))   # delete all special characters
names(tidyData)<-gsub("meanX", "X.mean", names(tidyData)) # move the "mean" to the end
names(tidyData)<-gsub("meanY", "Y.mean", names(tidyData))
names(tidyData)<-gsub("meanZ", "Z.mean", names(tidyData))
names(tidyData)<-gsub("stdX", "X.std", names(tidyData)) # move the "std" to the end
names(tidyData)<-gsub("stdY", "Y.std", names(tidyData))
names(tidyData)<-gsub("stdZ", "Z.std", names(tidyData))
names(tidyData)<-gsub("MagmeanFreq", "MagWeighted.mean", names(tidyData)) # change "meanFreq" to "Weighted.mean"
names(tidyData)<-gsub("Magmean", "Mag.mean", names(tidyData)) # move the "mean" to the end
names(tidyData)<-gsub("Magstd", "Mag.std", names(tidyData))   # move the "std" to the end
names(tidyData)<-gsub("meanFreqX", "WeightedX.mean", names(tidyData)) # change "meanFreqX" to "WeightedX.mean"
names(tidyData)<-gsub("meanFreqY", "WeightedY.mean", names(tidyData))
names(tidyData)<-gsub("meanFreqZ", "WeightedZ.mean", names(tidyData))
# tidyData --> Q4 answer
varNames_new <-names(tidyData)
write.table(cbind(varNames_org, varNames_new), "./varNames.txt", quote=FALSE,
            sep="\t-->  ", row.name=FALSE)

# Q5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject
library(dplyr)
tidyData <- tidyData %>%           
    group_by(subjects, activities) %>%
    summarise_all(funs(mean))       # tideData --> Q5 answer
write.table(tidyData, "./tidyData.txt", quote=FALSE, sep=" ", row.name=FALSE)