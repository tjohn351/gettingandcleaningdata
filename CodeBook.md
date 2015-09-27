This code book describes the data used in the project along with the processing.

Files:

features.txt: Names of the 561 features.
activity_labels.txt: Names and IDs for each of the 6 activities.
X_train.txt: 7352 observations of the 561 features, for 21 of the 30 volunteers.
subject_train.txt: A vector of 7352 integers, denoting the ID of the volunteer related to each of the observations in X_train.txt.
y_train.txt: A vector of 7352 integers, denoting the ID of the activity related to each of the observations in X_train.txt.
X_test.txt: 2947 observations of the 561 features, for 9 of the 30 volunteers.
subject_test.txt: A vector of 2947 integers, denoting the ID of the volunteer related to each of the observations in X_test.txt.
y_test.txt: A vector of 2947 integers, denoting the ID of the activity related to each of the observations in X_test.txt.

Processing Steps:

1. Read data into data frames
2. Remove columns that do not contain mean or std
3. Convert activity column from integer to a factor
4. Create tidy data set
5. Output to txt file