
##1.Merges the training and the test sets to create one data set.
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

test_data <- cbind(x_test, y_test)
train_data <- cbind(x_train, y_train)
data_merge <- rbind(test_data, train_data)



##2.Extracts only the measurements on the mean and standard deviation for each measurement.
features <-read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")
extract1 <- features[grep("mean()", features$V2, fixed = TRUE), ]
extract2 <- features[grep("std()", features$V2, fixed = TRUE), ]
extract_number <- sort(c(extract1$V1, extract2$V1))
meannstd <- data_merge[, extract_number]



##3.Uses descriptive activity names to name the activities in the data set
activity <- data_merge[, 562]
activity1 <- sub("1", "WALKING", activity)
activity2 <- sub("2", "WALKING_UPSTAIRS", activity1)
activity3 <- sub("3", "WALKING_DOWNSTAIRS", activity2)
activity4 <- sub("4", "SITTING", activity3)
activity5 <- sub("5", "STANDING", activity4)
activity6 <- sub("6", "LAYING", activity5)
meannstd_new <- cbind(meannstd, activity6)



##4.Appropriately labels the data set with descriptive variable names.
select_features <- as.character(features[extract_number, 2])
variable_names <- c(select_features, "activity")
colnames(meannstd_new) <- variable_names



##5.From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject.
subject <- rbind(subject_test, subject_train)
data <- cbind(meannstd_new, subject)
colnames(data)[68] <- "subject"

#install.packages("reshape2")
library(reshape2)

avg_data <- dcast(melt(data, c("activity", "subject")), activity + subject ~ variable, mean)
new_col_names <- lapply(select_features, function(x) paste("avg-",x, sep = ""))
colnames(avg_data)[3:68] <- new_col_names


## Export data as a txt file. 
write.table(avg_data, file = "./Average.txt", row.names = FALSE)

