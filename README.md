##The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

##The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## For each record, it is provided with: 1) A tidy clean data named "Average.csv"; 2) A script file called "run_analysis.R"; 3) A codebook file called "Codebook.txt" explains briefly the data; and 4) the README file. 

## Below is a brief explanation of how the script write for each step. 

##1.Merges the training and the test sets to create one data set.
##load the following six txt files into R. 
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

## Creating test data by column combining x_test and y_test.
test_data <- cbind(x_test, y_test)
## Creating train data by column combining x_train and y_train. 
train_data <- cbind(x_train, y_train)
## Merge test data and train data together using row combining.
data_merge <- rbind(test_data, train_data)



##2.Extracts only the measurements on the mean and standard deviation for each measurement.
## Read features data into R and name it features. 
features <-read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")
## Extract measurement of mean by using "grep" function and save the result in variable "extract1".
## Note that you should add the argument "fixed = TRUE" or else other measurements that contain "mean" will also be selected by mistake. 
extract1 <- features[grep("mean", features$V2, fixed = TRUE),]
## Extract measurement of std by using "grep" function and save the result in variable "extract2".
extract2 <- features[grep("std", features$V2, fixed = TRUE),]
## Combine these two extracted results together and sort it by increasing order, save the result in "extract_number". 
extract_number <- sort(c(extract1$V1, extract2$V1))
## Use "extract_number" to subset columns of measurements of mean and std, save it in variable "meannstd" 
meannstd <- data_merge[, extract_number]



##3.Uses descriptive activity names to name the activities in the data set
## Subset last column of data_merge created in step1, the result is activity column.
activity <- data_merge[, 562]
## Use "sub" function to substitute number of 1-6 with their corresponding activities, repeat the process 6 times. Each time the result is saved in a new variable. 
activity1 <- sub("1", "WALKING", activity)
activity2 <- sub("2", "WALKING_UPSTAIRS", activity1)
activity3 <- sub("3", "WALKING_DOWNSTAIRS", activity2)
activity4 <- sub("4", "SITTING", activity3)
activity5 <- sub("5", "STANDING", activity4)
activity6 <- sub("6", "LAYING", activity5)
## "activity6" created above is the exact column used to  combine to the dataframe "meannstd" which created in step2. Result is saved in "meannstd_new"
meannstd_new <- cbind(meannstd, activity6)



##4.Appropriately labels the data set with descriptive variable names.
## For the dataframe "feature", we only want selective elements of the second column, which contain means and std. Note that subset result is factor, we have to convert it to character, and the result is saved in "select_features" 
select_features <- as.character(features[extract_number, 2])
## creat a vector of variable names which is comprised of two parts: "select_features" and "activity". Note that activity will be the variable name of the last column which will be done in the next step.
variable_names <- c(select_features, "activity")
## Use "colnames" to name all columns of "meannstd_new"
colnames(meannstd_new) <- variable_names



##5.From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject.
## Creat a subject variable by combining both subject_test and subject_train. 
subject <- rbind(subject_test, subject_train)
## Add the column of subject to dataframe "meannstd_new"
data <- cbind(meannstd_new, subject)
## At this time, if you check names of the new dataframe named "data", you see that last column is named "V1", so change its name to "subject".
colnames(data)[68] <- "subject"
## Now data is ready to proceed to step5.

## Install packages of "reshape2" if you haven't done so. 
install.packages("reshape2")
## Load packages of reshape2. 
library(reshape2)
## Use "melt" function to reshape data into a narrow form and then apply function "dcast" to produce a table of mean value of each variable grouped by "activity" and "subject".
avg_data <- dcast(melt(data, c("activity", "subject")), activity + subject ~ variable, mean)
## Since value is mean now, you should also change variable name by adding "avg" before old variable names to reflect the change. 
new_col_names <- lapply(select_features, function(x) paste("avg-",x, sep = ""))
## Apply new variable name to columns except the first two. 
colnames(avg_data)[3:68] <- new_col_names

## Finally, export data as a txt file with the name "Average". 
write.table(avg_data, file = "./Average.txt", row.names = FALSE)


