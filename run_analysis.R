library(dplyr)

# Opens a file with read.table without any parameter.
# Then the data frame is wrapped with dplyr
openFile <- function(path) {
    path %>%
    read.table %>%
    tbl_df %>%
    return
}

# Takes subject, features and result and binds them in columns
columnBindFiles <- function(subject, features, result) {
    return(cbind(openFile(subject), openFile(features), openFile(result)))
}

# Removes duplicates from a vector by renaming the values
removeDuplicates <- function(vector) {
    clearVector <- c()
    for(value in vector) {
        if(value %in% clearVector) {
            i <- 1
            newValue <- paste(value, "dup", i, sep="")
            while(newValue %in% clearVector) {
                i <- i + 1
                newValue <- paste(value, "dup", i, sep="")
            }
            clearVector <- c(clearVector, newValue)
        } else {
            clearVector <- c(clearVector, value)
        }
    }
    return(clearVector)
}

# Gets and cleans feature names
getFeatures <- function(filename) {
    features <- collect(openFile(filename))[[2]]
    features <- gsub('-mean', 'Mean', features)
    features <- gsub('-std', 'Std', features)
    features <- gsub('[-()]', '', features)
    return(features)
}

setwd("./UCI HAR Dataset")
train <- columnBindFiles("train/subject_train.txt", "train/X_train.txt", "train/y_train.txt")
test <- columnBindFiles("test/subject_test.txt", "test/X_test.txt", "test/y_test.txt")
dataset <- rbind(train, test)
rm(train)
rm(test)
colnames(dataset) <- c("Subject", getFeatures("features.txt"), "Activity")
colnames(dataset) <- removeDuplicates(colnames(dataset))
dataset <- dataset[,grep(".*Mean.*|.*Std.*|Subject|Activity", colnames(dataset))]
dataset$Activity <- as.factor(dataset$Activity)
dataset$Subject <- as.factor(dataset$Subject)
tidy <- aggregate(dataset, by=list(Activity = dataset$Activity, Subject=dataset$Subject), mean)
# Remove mean of subject and activity because it has no sense
tidy[,90] <- NULL
tidy[,3] <- NULL
activities <- openFile("activity_labels.txt")
tidy[,89] <- merge(tidy, activities, by.x = "Activity", by.y = "V1")["V2"]
rm(activities)
colnames(tidy)[89] <- "Activity"
# Remove column with activity numbers
tidy[,1] <- NULL
write.table(tidy, "tidy.txt", sep="\t")
setwd("..")