## Set of code to read, combine and make a tidy data set from UCI HAR

# PLEASE NOTE: I included code to download the data and move among working directories,
# as opposed to assuming the user already had the data. 

# Download data set (zipped archive)
#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#if(!file.exists("./data")){dir.create("./data")}
#download.file(fileUrl, destfile="./data/UCI_data.zip",method="curl")

# Unzip files from archive
unzip("./data/UCI_data.zip",overwrite=TRUE,exdir="./data")

# Set working directory for easier file access
setwd("./data/UCI HAR Dataset")

# Import all tables into R objects for manipulation

xtest <- read.table("./test/X_test.txt")
ytest <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

xtrain <- read.table("./train/X_train.txt")
ytrain <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

# Require dplyr; throw error if not available
require("dplyr")

# Merge activity values and activity labels in both y tables, left join to test tables on code #
test_labels <- merge(ytest, activity_labels, by.x = "V1", by.y = "V1", all=TRUE)
train_labels <- merge(ytrain, activity_labels, by.x = "V1", by.y = "V1", all=TRUE)

# Add column labels to test and train x tables by taking the second column of Features table
column_names <- features$V2
names(xtest) <- column_names
names(xtrain) <- column_names

# Create a list of the variable indexes for mean and standard deviation
means_sds <- means_sds <- c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503:504, 513, 516:517, 526, 529:530, 539, 542:543, 552)

# Create a subset of each table representing only the measurements
# on the mean and standard deviation
## DON'T NEED features_mean_sd <- filter(features, V1 %in% means_sds)
xtest_mean_sd <- xtest[,means_sds]
xtrain_mean_sd <- xtrain[,means_sds]

# Add Activity columns to test and train tables
test_combined <- cbind(Activity = as.factor(test_labels[,2]), xtest_mean_sd)
train_combined <- cbind(Activity = as.factor(train_labels[,2]), xtrain_mean_sd)

# Add Subject columns to test and train tables
test_combined <- cbind(Subject_ID = subject_test$V1, test_combined)
train_combined <- cbind(Subject_ID = subject_train$V1, train_combined)

# Combine both data sets into a single table using rbind (all columns are the same)
combined_tidy_data_set <- rbind(test_combined, train_combined)

## THIS IS STEP 5: Creation of a new, separate tidy data set with the average of each variable
## for each activity and each subject.

# Create a new table that melts activities (1 row of values)
require("plyr")
require(reshape2)
melted_data <- melt(combined_tidy_data_set, id.vars=c("Subject_ID", "Activity"))

# Create a table summarizing melted data by Subject, Activity, and Variable
tidy_means <- summarise_each(subject_activity_groups, funs(mean))
names(tidy_means) <- c("Subject_ID","Activity","Variable","Mean_Measured_Value")

# Writes tidy data set of means to a .txt file in one folder up
setwd("..")
write.table(tidy_means, file="combined_tidy_data_set.txt", row.names = FALSE)