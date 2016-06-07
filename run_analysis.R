## project week 4
## Part 1
 setwd("/Users/Thansen/Desktop/R/week4project/Dataset")

##Reading in the main labels
label <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/activity_labels.txt")
features <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/features.txt")

colnames(label) <- c("activityid", "activity")

##Reading in the test files
setwd("/Users/Thansen/Desktop/R/week4project/Dataset/test")

testx <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/test/X_test.txt")
testy <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/test/y_test.txt")
subject_test <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/test/st.txt")

## Changing the columnanes in the test data sets
colnames(testy) <- c("activityid")
colnames(subject_test) <- c("subject_id")
colnames(testx) <- features[,2]

## Combinding the three files to get one big data file 
combinedtest <- cbind(testy,subject_test,testx)

## doing the same thing for the train dataset

setwd("/Users/Thansen/Desktop/R/week4project/Dataset/train")

trainx <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/train/X_train.txt")
trainy <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/train/y_train.txt")
subject_train <- read.table("/Users/Thansen/Desktop/R/week4project/Dataset/train/subject_train.txt")


## Changing the column names of the train dataset
colnames(trainy) <- c("activityid")
colnames(subject_train) <- c("subject_id")
colnames(trainx) <- features[,2]

## Combinding the three files to get one big data file 
combinedtrain <- cbind(trainy,subject_train,trainx)

## Combinding the train and test dataset

Train_Test_Combi <- rbind(combinedtest,combinedtrain)

## The next thing I want to do, it to change the numbers id's into the correct labelt names, which are in the labels dataset.
## So what the code does is assigning the activity lable to the id - eg. 1 becomes walking, 2 walking_upstairs and so on.
Train_Test_Combi[["activityid"]] <- label[match(Train_Test_Combi[["activityid"]], label[["activityid"]]), "activity"]

## part 2
## extracting the mean() and std() in the columnames from the merged data
mean_extract  <- grep("mean",colnames(Train_Test_Combi), value = TRUE)
std_extract  <- grep("std", colnames(Train_Test_Combi), value = TRUE)

## Combining the extracted files
total_extract  <-  cbind(Train_Test_Combi[,mean_extract], Train_Test_Combi[,std_extract], id_activity = Train_Test_Combi$activityid, subject_id = Train_Test_Combi$subject_id)

## I'm a little bit annoyed that the subject id and training id is the last here, moving them 
col_idx <- grep("id_activity", colnames(total_extract))
total_extract <- total_extract[, c(col_idx, (1:ncol(total_extract))[-col_idx])]
col_idy <- grep("subject_id", colnames(total_extract))
total_extract <- total_extract[, c(col_idy, (1:ncol(total_extract))[-col_idy])]
## Ahh better

## part 3. So when refering to the data set here, I assume it is the total combined one. 
colnames(Train_Test_Combi) <-  gsub("^t","total_", colnames(Train_Test_Combi))
olnames(Train_Test_Combi) <-  gsub("timeime","time", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("Acc","_acceleration_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("Body","Body_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("Gravity","Gravity_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("mean()","Mean_Value_", colnames(Train_Test_Combi)) 
colnames(Train_Test_Combi) <-  gsub("std()","Standard_Deviation", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("/()","", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("mad","Median_Absolute_Deviation_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("max","Largst_Value_in_Array", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("min","Smallest_Value_in_Array", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("energy","Energy_Measure_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("iqr","Interquartile_range_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("entropy","Signal_Entropy_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("arCoeff","Autorregresion_coefficients_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("maxInds","Frequency_Component_with_Largest_Magnitude_Index", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("meanFreq","Weighted:Avarage_of_frequency_Component_", colnames(Train_Test_Combi))
colnames(Train_Test_Combi) <-  gsub("otal","", colnames(Train_Test_Combi))

## part 4
## Finally I'm extracting a cleaner dataset and with the avarage (mean) of every activity
tidydata <- aggregate(. ~ subject_id + activityid, data = Train_Test_Combi, mean) 
## Can also be ordered by the subject_id
tidydata <- tidydata[order(tidydata$subject_id), ] 
