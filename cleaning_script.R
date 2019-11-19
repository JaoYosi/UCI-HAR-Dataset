

#The work gets and prepare tidy data that can be used for later analysis.
#The data is of the Human Activity Recognition Using Smartphones Data Set.

# -------------------------------------------------------------------------- #

#preparing workspace

rm(list = ls())

#getting and loading the required packages

if ("dplyr" %in% row.names(installed.packages()) == FALSE){install.packages("dplyr")}
if ("tidyr" %in% row.names(installed.packages()) == FALSE){install.packages("tidyr")}

library("dplyr"); 
library("tidyr")

file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


if (!dir.exists("data2")){
  dir.create("data2")
}

data_path = "./data2/UCI HAR Dataset"

if (!file.exists(data_path)){
  download.file(file_url, "./data2/UCI_data.zip")
  unzip("./data2/UCI_data.zip", exdir = "./data2")
  
}

#1. Merging the test and train datasets to create one dataset

#reading in train set values
x_train <- read.table(file = "./data2/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table(file = "./data2/UCI HAR Dataset/train/y_train.txt", col.names = c("activity"))
sub_train <- read.table(file = "./data2/UCI HAR Dataset/train/subject_train.txt", col.names = c("subject"))

#combining the columns to make training data
dt_train <- cbind(sub_train, y_train, x_train)

#reading in the test set values
x_test <- read.table(file = "./data2/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table(file= "./data2/UCI HAR Dataset/test/y_test.txt", col.names = c("activity"))
sub_test <- read.table(file= "./data2/UCI HAR Dataset/test/subject_test.txt", col.names = c("subject"))

#combinig the columns to make the training data
dt_test <- cbind(sub_test, y_test, x_test)

#merging test and train data sets
dt_merged <- rbind(dt_train, dt_test)

#loading the feature names
features <- read.table(file = "./data2/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE, col.names = c("id", "name"))
feature_extended <- c("subject", "activity", features$name)

#2. Extracting only measurements on mean and standard deviation
feature_select <- feature_extended[grepl("mean|std|subject|activity", feature_extended) & !grepl("Freq", feature_extended)]
feature_select_index <- grepl("mean|std|subject|activity", feature_extended) & !grepl("Freq", feature_extended)

#filtering the needed variables in merged dataset by the position of the required features
dt_merged_filtered <- dt_merged[, feature_select_index]

#reading in activity names
activity_labels <- read.table(file = "./data2/UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE, col.names = c("id", "name"))

#3. fixing corresponding descriptive activity name to the values in the merged dataset as a variabe called activity name
dt_merged_filtered <- mutate(dt_merged_filtered, activity_name = as.character(factor(dt_merged_filtered$activity, labels = activity_labels$name))) 

#4. cleaning feature names and using descriptive names

feature_select_clean <- gsub("\\(\\)", "", feature_select)
feature_select_clean <- gsub("^t(.*)$", "\\1-timedomain", feature_select_clean)
feature_select_clean <- gsub("^f(.*)$", "\\1-freqdomain", feature_select_clean)
feature_select_clean <- gsub("(Jerk|Gyro)", "-\\1", feature_select_clean)
feature_select_clean <- gsub("Mag", "-Magnitude", feature_select_clean)
feature_select_clean <- gsub("Acc", "-Acceleration", feature_select_clean)
feature_select_clean <- gsub("BodyBody", "Body", feature_select_clean)
feature_select_clean <- tolower(feature_select_clean)

#fixing variable names for the filtered dataset
names(dt_merged_filtered) <- c(feature_select_clean, "activity_name")

#5. creating a second, independent tidy data set with the average of each variable for each activity and each subject.

#dt_tidy <- dt_merged_filtered %>% group_by(subject, activity) %>% summarise_each(funs(mean)) #%>% gather(key = measure, value = mean, -subject, -activity)

dt_tidy <- dt_merged_filtered %>% group_by(subject, activity, activity_name) %>% summarise_each(funs = list(mean))

#saving independent tidied data to a file
write.table(dt_tidy, file = "./data2/tidied_data.txt")


#Clearing the workspace
#rm(list = ls())
