#This is a project assignment for 'Getting and Cleaning Data' course for Week3
# This reads the data from train and test data sets and features data file
# and prepares a tidy data set for means and standard deviations for all the measurments
# Finally it generates a tidyDataSet.txt file.

# Read the Training and Testing Data Sets
trainSet = read.csv("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
trainSet[,562] = read.csv("./UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
trainSet[,563] = read.csv("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

testSet = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testSet[,562] = read.csv("./UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testSet[,563] = read.csv("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

actLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Read the features and change them to nice readable format
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# Merging the trainSet and testSet
allData = rbind(trainSet, testSet)

# Need columns with Mean and Std (standard deviation)
colsNeeded <- grep(".*Mean.*|.*Std.*", features[,2])
# narrow to the features needed
features <- features[colsNeeded,]
# Add the last two columns too
colsNeeded <- c(colsNeeded, 562, 563)
# And remove the unwanted columns from allData
# Extract all the rows with Mean and Std columns and 562, and 563 columns
allData <- allData[,colsNeeded]
# Define the column names for the last two columns and Make everything to lower
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

currentActivity = 1
for (currentActivityLabel in actLabels$V2) {
  allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
  currentActivity <- currentActivity + 1
}

allData$activity <- as.factor(allData$activity)
allData$subject <- as.factor(allData$subject)

tidySet = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)
# Remove the subject and activity column, since a mean of those has no use
tidySet[,90] = NULL
tidySet[,89] = NULL

# Write to a file with row.name = FALSE
write.table(tidySet, "tidyDataSet.txt", sep="\t", row.name=FALSE)
