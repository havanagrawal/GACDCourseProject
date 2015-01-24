## run_analysis

## The current working directory should contain the test and training folders.

## To prevent floats from being displayed as e+01.
options(scipen=999)

## Note: Reading the following files may take a minute or two.

# Read the input variables
X_test <- read.table("./test/X_test.txt")
X_train <- read.table("./train/X_train.txt")

# Read the output activity
y_test <- read.table("./test/y_test.txt")
y_train <- read.table("./train/y_train.txt")

# Read the subject identifier
sub_test <- read.table("./test/subject_test.txt")
sub_train <- read.table("./train/subject_train.txt")

######################################
## 1. Step 1 of Spec
## Merge the two datasets 
## to create one single dataset
######################################
X_all <- rbind(X_test, X_train)
y_all <- rbind(y_test, y_train)
sub_all <- rbind(sub_test, sub_train)

# Read the feature list
# Effective variable names of the variables in X
features <- read.table("./features.txt")
features <- features[, 2]

######################################
## 2. Step 2 of Spec
## Extract only the measurements
## on the mean and standard deviation
## For each measurement
######################################
## Note that this will not match meanFreq (on purpose)
required_features_indices <- grep(".*(mean|std)[\\(].*", features)
feature_names <- as.character(features[required_features_indices])
x_required <- X_all[, required_features_indices]

######################################
## 3. Step 3 of Spec
## Use descriptive activity names
## to name the activities
## in the dataset
######################################

activitynames <- c("Walking", 
              "WalkingUpstairs", 
              "WalkingDownstairs", 
              "Sitting",
              "Standing",
              "Laying")

# Used subsetting to name the activities
# Directly use the number in the y files as an index
# to the activitynames vector
activities <- factor(activitynames[y_all[, ]])

# Create a data frame with all of the data
# i.e.  * the required measurement variables, 
#       * corresponding activities,
#       * subject identifier
# The ones are used later for aggregation
ones = rep(1, dim(x_required)[1])
complete_data <- cbind(x_required, activities, sub_all, ones)

######################################
## 4. Step 4 of Spec
## Appropriately label the dataset
## With descriptive variable names
######################################

## Naming rules followed:
#     * All lower case letters
#     * Descriptive
#     * Not duplicated
#     * No non-numeric or non-alphabet characters
#       Such as . or _ or - or whitespace

names(complete_data) <- c(feature_names, "activity", "subjectid", "count")

# All lower case
names(complete_data) <- tolower(names(complete_data))
# No -, (, )
names(complete_data) <- gsub("-", "", names(complete_data))
names(complete_data) <- gsub("\\(", "", names(complete_data))
names(complete_data) <- gsub("\\)", "", names(complete_data))

######################################
## 5. Step 5 of Spec
## Create a second, independent tidy data set
## with the average of each variable for 
## each activity and each subject.
######################################

# Use crosstabs for making independent data set
# Saw usage of cbind in example(xtabs)

# a contains the sum of all values for each subject, each activity, and each measurement
# It is a [1:30, 1:6, 1:66] xtabs object
a <- xtabs(as.matrix(cbind(complete_data[, 1:66])) ~ subjectid + activity, data = complete_data)

# Contains the sum of records for each subject and each activity
# For example b[1, 1] contains the number of measurements taken
# For subject no 1
# For activity number 1, i.e. Walking
# b is a [1:30, 1:6] xtabs object
b <- xtabs(count ~ subjectid + activity, data = complete_data)

# I know, for loops are bad and all
# But I couldn't really think of any better way to do this
# Divide the sum of values of measurements of each variable
# By the number of records for the same
# Since sum/freq = mean
for (i in 1:66) {
    a[, , i] = a[, , i] / b
}

# Flatten the table created, for readability
final <- ftable(a)

# Finally write the table to the text file
write.table(as.table(final), 
            "./cleanandtidysummarydata.txt", 
            row.names=FALSE, 
            col.names = c("subjectid", "activity", "feature", "average"))

# And we're done!
# Thank you for your patience for looking through my code
# Suggestions and criticisms are welcome.

# Code for reading data back into R
# For verification
file_path <- "./cleanandtidysummarydata.txt"
data <- read.table(file_path, header = TRUE)
View(data)
