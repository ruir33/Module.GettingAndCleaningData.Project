# Lets position on the working dir 
# [If required, uncomment the next line, after set it appropriate to your config]
# setwd("C:/Users/Rui/Documents/mod3.project")

# Lets Load required packages
library(reshape2)

##
## STEP 1. Merges the training and the test sets to create one data set.
##

# Lets get all info into data frames
subject_train <- read.table("./data/train/subject_train.txt")
subject_test <- read.table("./data/test/subject_test.txt")
trainData <- read.table("./data/train/X_train.txt")
testData <- read.table("./data/test/X_test.txt")
trainLabel <- read.table("./data/train/y_train.txt")
testLabel <- read.table("./data/test/y_test.txt")

# Lets add column name for subject files
names(subject_train) <- "subjectID"
names(subject_test) <- "subjectID"

# Lets add column names for measurement files
features <- read.table("./data/features.txt")
names(trainData) <- features$V2
names(testData) <- features$V2

# Lets add column name for label files
names(trainLabel) <- "activity"
names(testLabel) <- "activity"

# Lets join files into one dataset
train <- cbind(subject_train, trainLabel, trainData)
test <- cbind(subject_test, testLabel, testData)
joinData <- rbind(train, test)


##
## STEP 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
##

# Lets check which columns contain "mean()" or "std()"
meanStdCols <- grepl("mean\\(\\)", names(joinData)) |
    grepl("std\\(\\)", names(joinData))

# Lets make sure that we keep the subjectID and activity columns
meanStdCols[1:2] <- TRUE

# Lets remove unnecessary columns
joinData <- joinData[, meanStdCols]


##
## Step 3.  Uses descriptive activity names to name the activities in the data set 
##

# Lets get those activity names
activity <- read.table("./data/activity_labels.txt")

# Lets put them lower case (more simple to read) ...
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))


##
## Step 4.  Appropriately labels the data set with descriptive variable names. 
##

joinData$activity <- factor(joinData$activity, labels=activity[,2])


##
## Step 5.  From the data set in step 4, creates a second, independent tidy data
##          set with the average of each variable for each activity and each subject

# Lets create the tidy data set
melted <- melt(joinData, id=c("subjectID","activity"))
tidy <- dcast(melted, subjectID+activity ~ variable, mean)

# Lets write the tidy data set to a file
write.table(tidy, file = "tidyData.txt", row.name=FALSE)

#---------------  That's all !  Carpe Diem !  ---------------------#