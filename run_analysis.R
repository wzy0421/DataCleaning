#Merges the training and the test sets to create one data set.
combine <- function(file_a, file_b) {
  data_a <- read.csv(file_a, header = FALSE, sep = '');
  data_b <- read.csv(file_b, header = FALSE, sep = '');
  return(rbind(data_a, data_b));
}
X <- combine('UCI HAR Dataset/train/X_train.txt','UCI HAR Dataset/test/X_test.txt');
y <- combine('UCI HAR Dataset/train/y_train.txt','UCI HAR Dataset/test/y_test.txt');

#Extracts only the measurements on the mean and standard deviation for each measurement.
feature_names <- read.table('UCI HAR Dataset/features.txt',header = FALSE, sep = '');
is_eligible <- function(name) {
  return(!is.na(grep('mean', name) || grep('std', name)));
}

feature_eligible <- sapply(feature_names[, 2], FUN = is_eligible);
X_cleared <- X[,feature_eligible];

#Uses descriptive activity names to name the activities in the data set
y_labels <- y;
y_labels[,2] <- factor(y[, 1], levels = 1:6, labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"));

#Appropriately labels the data set with descriptive variable names.
names <- feature_names[feature_eligible == TRUE, 2];
colnames(X_cleared) <- names;

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
X_tiny <- aggregate(X_cleared, y_labels, mean);
