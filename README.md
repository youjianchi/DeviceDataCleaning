DeviceDataCleaning Script Notes
This is a step-by-step explanation of the R scripts used to clean raw data and produce a tidy data set
=====================================================================

# Step 1: merges the training and the test sets

# Read measurement data for the test set
X_test <- read.table("./test/X_test.txt")
# Read activity labels for the test set
Y_test <- read.table("./test/y_test.txt")
# Read subject IDs for the test set
Subject_test <- read.table("./test/subject_test.txt")
# Read measurement data for the training set
X_train <- read.table("./train/X_train.txt")
# Read activity labels for the training set
Y_train <- read.table("./train/y_train.txt")
# Read subject IDs for the training set
Subject_train <- read.table("./train/subject_train.txt")
# Merge subject, activity and measurement of training & test sets
data <- cbind(rbind(Subject_test, Subject_train),
              rbind(Y_test, Y_train),
              rbind(X_test, X_train))
              
=====================================================================

# Step 2: Extract only mean and std measurements

# Read features data, which contains measurement ID and descriptions
features <- read.table("./features.txt")

# Use grep() function to search and return index of rows that contain
# "mean()" and "std()" in variable names
mean_std_index <- c(grep("mean()", features$V2), grep("std()", features$V2))

# Sort the index so that the variables are ranked in ascending order
# to be consistent with the order of variables in "data"
index_sort <- sort(mean_std_index)

# Subset "data" to extract only mean and std measurements, with the
# first column as subject_id, second column as activity_id,
# and the third column and beyond as extracted measurements
mean_std_data <- cbind(data[,1], data[,2], data[,index_sort+2])

=====================================================================

# Step 3: Label the data set with descriptive variables

# Get the description of measurements from "features" table
features_desc <- as.character(features[index_sort,2])

# Assign descriptions to name columns of the dataset
names(mean_std_data) <- c("Subject", "Activity_ID", features_desc)

=====================================================================

# Step 4: Use descriptive activities names to name activities

# Read activity table that contains activity_id and description
Activity <- read.table("./activity_labels.txt")
names(Activity) <- c("Activity_ID", "Activity")

# Use merge function to join activity table to the dataset
merged_data <- merge(Activity, mean_std_data, by.x="Activity_ID", 
                     by.y="Activity_ID", all=TRUE)

=====================================================================

# Step 5: create tidy dataset
# Keep only subject_id, activity name, and mean/std variables from merged data
clean_data <- merged_data[,c(3,2,4:82)]
# Use a for loop to compute the mean value for each of the measurements
# Use aggregate function to compute the mean for the first measurement
dt <- aggregate(clean_data[,3], list(clean_data$Subject, clean_data$Activity), mean)
# Apply a for loop to compute the mean values for other variables and
# combine the results to yield tidy data
i <- 4
for (i in 4:81) {
  dt1 <- aggregate(clean_data[,i], list(clean_data$Subject, clean_data$Activity), mean)
  dt <- cbind(dt, dt1[,3])
  i <- i+1
}
# Rename the varialbes
names(dt) <- names(clean_data)
# Export data
tidy_data <- dt
write.table(tidy_data, "./tidy_data.txt")