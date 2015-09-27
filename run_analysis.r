step1 <- function(){
  subtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  subtest  <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  xtrain   <- read.table("./UCI HAR Dataset/train/x_train.txt")
  xtest    <- read.table("./UCI HAR Dataset/test/x_test.txt")
  ytrain   <- read.table("./UCI HAR Dataset/train/y_train.txt")
  ytest    <- read.table("./UCI HAR Dataset/test/y_test.txt")
  names(subtrain) <- "subjectID"
  names(subtest)  <- "subjectID"
  fNames <- read.table("./UCI HAR Dataset/features.txt")
  names(xtrain) <- fNames$V2
  names(xtest)  <- fNames$V2
  names(ytrain) <- "activity"
  names(ytest)  <- "activity"
  train <- cbind(subtrain, ytrain, xtrain)
  test  <- cbind(subtest, ytest, xtest)
  comb  <- rbind(train, test)
}

step2 <- function(){
  meanstdcols <- grepl("mean\\(\\)", names(comb)) |
    grepl("std\\(\\)", names(comb))
  meanstdcols[1:2] <- TRUE
  comb <- comb[, meanstdcols]
}

step3and4 <- function(){
  comb$activity <- factor(comb$activity, labels=c("Walking","Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))
}

step5 <- function(){
  library(reshape2)
  melted <- melt(comb, id=c("subjectID","activity"))
  tidy <- dcast(melted, subjectID+activity ~ variable, mean)
  write.table(tidy, "tidy.txt", row.names=FALSE)
}

step1()
step2()
step3and4()
step5()




