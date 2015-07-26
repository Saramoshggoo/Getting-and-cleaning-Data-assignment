# read all test,train and subject data

  xtest <-  read.table("./UCI HAR Dataset/test/x_test.txt",header = FALSE)
  ytest <-  read.table("./UCI HAR Dataset/test/y_test.txt" ,header = FALSE)
  subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt",header = FALSE)


  xtrain <-  read.table("./UCI HAR Dataset/train/x_train.txt",header = FALSE)
  ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt",header = FALSE )
  subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt",header = FALSE)


#combine rows x(test,tarin),y(test,tarin), subject(test,train)



  allxdatas <- rbind (xtest ,xtrain)
  allydatas<- rbind (ytest,ytrain)
  allsubject<-rbind  (subjecttest, subjecttrain)

#_____________________________________________________________________________________________________________________

# 2. -- Extracts only the measurements on the mean and standard deviation for each measurement.


#read feature file
   featurenames   <- read.table("./UCI HAR Dataset/features.txt",header = FALSE)

#search for match each row with std or mean  from second column of featurename 

   meanandstddevfeatures  <- grepl("(-std\\(\\)|-mean\\(\\))",featurenames$V2)

# if meanandstddevfeature is true , the row goes to filterdactivity

   filteredActivity <- allxdatas[,(meanandstddevfeatures == TRUE)]


#______________________________________________________________________________________________________________________
#3. -- Uses descriptive activity names to name the activities in the data set.

# read activity-lables

   activityLabels  <- read.table("./UCI HAR Dataset/activity_labels.txt")

# transform the allLabelSets from integer codes to factors
   activity <- as.factor(allydatas$V1)

# transform the label factors into a vector of human readable activity descriptions
   levels(activity) <- allxdatas$V2

# transform the subject codes to factors, as they will be used as factors later on.
   subject <- as.factor(allsubject$V1)

# bind as a column the allLabels vector to the dataset
   filteredActivity <- cbind(subject,activity,filteredActivity)

#_______________________________________________________________________________________________________________________


#4.--Uses descriptive activity names to name the activities in the data set

# First,  meanandstddevfeatures(this used in part )  is used to captue the 
# names of all the mean and std. dev. features.
     filteredfeatures <- (cbind(featurenames,meanandstddevfeatures)[meanandstddevfeatures==TRUE,])$V2

# Next, a gsub  is used to clean the parenthesese and hyphens, and
# make the name lowercase.  
   cleaner <- function(features) {
       tolower(gsub("(\\(|\\)|\\-)","",features))
   }
# sapply is used to apply
# the function to all desired featurenames. 

    filteredfeatures <- sapply(filteredfeatures,cleaner)

# Finally, add the filteredfeature names to the filteredActivity set. The first column name is
# skipped, since it already has a name, provided in step 3 above.
    names(filteredActivity)[3:ncol(filteredActivity)] <- filteredfeatures

# write the final dataset to a CSV file, and as a text file
    write.csv(filteredActivity,file="dataset.csv")
    write.table(filteredActivity, "dataset.txt", sep="\t")
                     
#________________________________________________________________________________________________________________________                    
  # 5.From the data set in step 4, creates a second, independent tidy data set with
  #the average of each variable for each activity and each subject.
  
  # using the reshape2 library you must to have version r3.2.1, use the melt function to collapse the filteredActivity 
  #dataframe.
  library(reshape2)
  
  # create the molten data set
  molten <- melt(filteredActivity,id.vars=c("subject","activity"))
  
  # cast the molten data set into a collapsed tidy dataset
  tidy <- dcast(molten,subject + activity ~ variable,mean)
  
  # write the dataset to a file
  write.table(tidy, "dataset.txt", sep="\t")
  
