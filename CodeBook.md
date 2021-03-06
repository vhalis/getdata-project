Codebook for Human Activity Recognition Using Smartphones Data Cleaning Project
===============================================================================

|POSITION       |VARIABLE NAME                  |VARIABLE OR EXPLANATION                                        |VALUES
|---------------|-----------------------        |---------------------------------------------------------------|------------
|1              |`SubjectNum`                     |Unique integer identifier for each individual subject.         |[1,30]
|2              |`ActivityType`                   |Unique identifier for each of the 6 different activities.      |WALKING = Data when subject is walking.<br>WALKING_UPSTAIRS = Data when subject is walking up stairs.<br>WALKING_DOWNSTAIRS = Data when subject is walking down stairs.<br>SITTING = Data when subject is sitting.<br>STANDING = Data when subject is standing.<br>LAYING = Data when subject is laying.<br>
|3		|`Mean-tBodyAcc-mean()-X`         |Mean of var `tBodyAcc-mean()-X` (as described in features_info.txt) across all trials of a particular activity.         |[-1,1]          
|4		|`Mean-tBodyAcc-mean()-Y`         |Mean of var `tBodyAcc-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]      
|5		|`Mean-tBodyAcc-mean()-Z`         |Mean of var `tBodyAcc-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1] 
|6		|`Mean-tBodyAcc-std()-X`          |Mean of var `tBodyAcc-std()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|7              |`Mean-tBodyAcc-std()-Y`          |Mean of var `tBodyAcc-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]        
|8              |`Mean-tBodyAcc-std()-Z`          |Mean of var `tBodyAcc-std()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]        
|9              |`Mean-tGravityAcc-mean()-X`      |Mean of var `tGravityAcc-mean()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|10             |`Mean-tGravityAcc-mean()-Y`      |Mean of var `tGravityAcc-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|11             |`Mean-tGravityAcc-mean()-Z`      |Mean of var `tGravityAcc-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|12             |`Mean-tGravityAcc-std()-X`       |Mean of var `tGravityAcc-std()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|13             |`Mean-tGravityAcc-std()-Y`       |Mean of var `tGravityAcc-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|14             |`Mean-tGravityAcc-std()-Z`       |Mean of var `tGravityAcc-std()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|15             |`Mean-tBodyAccJerk-mean()-X`     |Mean of var `tBodyAccJerk-mean()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|16             |`Mean-tBodyAccJerk-mean()-Y`     |Mean of var `tBodyAccJerk-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|17             |`Mean-tBodyAccJerk-mean()-Z`     |Mean of var `tBodyAccJerk-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|18             |`Mean-tBodyAccJerk-std()-X`      |Mean of var `tBodyAccJerk-std()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|19             |`Mean-tBodyAccJerk-std()-Y`      |Mean of var `tBodyAccJerk-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|20             |`Mean-tBodyAccJerk-std()-Z`      |Mean of var `tBodyAccJerk-std()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|21             |`Mean-tBodyGyro-mean()-X`        |Mean of var `tBodyGyro-mean()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|22             |`Mean-tBodyGyro-mean()-Y`        |Mean of var `tBodyGyro-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|23             |`Mean-tBodyGyro-mean()-Z`        |Mean of var `tBodyGyro-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|24             |`Mean-tBodyGyro-std()-X`         |Mean of var `tBodyGyro-std()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|25             |`Mean-tBodyGyro-std()-Y`         |Mean of var `tBodyGyro-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|26             |`Mean-tBodyGyro-std()-Z`         |Mean of var `tBodyGyro-std()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|27             |`Mean-tBodyGyroJerk-mean()-X`    |Mean of var `tBodyGyroJerk-mean()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|28             |`Mean-tBodyGyroJerk-mean()-Y`    |Mean of var `tBodyGyroJerk-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|29             |`Mean-tBodyGyroJerk-mean()-Z`    |Mean of var `tBodyGyroJerk-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|30             |`Mean-tBodyGyroJerk-std()-X`     |Mean of var `tBodyGyroJerk-std()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|31             |`Mean-tBodyGyroJerk-std()-Y`     |Mean of var `tBodyGyroJerk-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|32             |`Mean-tBodyGyroJerk-std()-Z`     |Mean of var `tBodyGyroJerk-std()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|33             |`Mean-tBodyAccMag-mean()`        |Mean of var `tBodyAccMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|34             |`Mean-tBodyAccMag-std()`         |Mean of var `tBodyAccMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|35             |`Mean-tGravityAccMag-mean()`     |Mean of var `tGravityAccMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|36             |`Mean-tGravityAccMag-std()`      |Mean of var `tGravityAccMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|37             |`Mean-tBodyAccJerkMag-mean()`    |Mean of var `tBodyAccJerkMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|38             |`Mean-tBodyAccJerkMag-std()`     |Mean of var `tBodyAccJerkMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|39             |`Mean-tBodyGyroMag-mean()`       |Mean of var `tBodyGyroMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|40             |`Mean-tBodyGyroMag-std()`        |Mean of var `tBodyGyroMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|41             |`Mean-tBodyGyroJerkMag-mean()`   |Mean of var `tBodyGyroJerkMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|42             |`Mean-tBodyGyroJerkMag-std()`    |Mean of var `tBodyGyroJerkMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|43             |`Mean-fBodyAcc-mean()-X`         |Mean of var `fBodyAcc-mean()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|44             |`Mean-fBodyAcc-mean()-Y`         |Mean of var `fBodyAcc-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|45             |`Mean-fBodyAcc-mean()-Z`         |Mean of var `fBodyAcc-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|46             |`Mean-fBodyAcc-std()-X`          |Mean of var `fBodyAcc-std()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|47             |`Mean-fBodyAcc-std()-Y`          |Mean of var `fBodyAcc-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|48             |`Mean-fBodyAcc-std()-Z`          |Mean of var `fBodyAcc-std()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|49             |`Mean-fBodyAcc-meanFreq()-X`     |Mean of var `fBodyAcc-meanFreq()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|50             |`Mean-fBodyAcc-meanFreq()-Y`     |Mean of var `fBodyAcc-meanFreq()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|51             |`Mean-fBodyAcc-meanFreq()-Z`     |Mean of var `fBodyAcc-meanFreq()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|52             |`Mean-fBodyAccJerk-mean()-X`     |Mean of var `fBodyAccJerk-mean()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|53             |`Mean-fBodyAccJerk-mean()-Y`     |Mean of var `fBodyAccJerk-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|54             |`Mean-fBodyAccJerk-mean()-Z`     |Mean of var `fBodyAccJerk-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|55             |`Mean-fBodyAccJerk-std()-X`      |Mean of var `fBodyAccJerk-std()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|56             |`Mean-fBodyAccJerk-std()-Y`      |Mean of var `fBodyAccJerk-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|57             |`Mean-fBodyAccJerk-std()-Z`      |Mean of var `fBodyAccJerk-std()-Z` (as described in features_info.txt) across all trials of a particular activity.       |[-1,1]
|58             |`Mean-fBodyAccJerk-meanFreq()-X` |Mean of var `fBodyAccJerk-meanFreq()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|59             |`Mean-fBodyAccJerk-meanFreq()-Y` |Mean of var `fBodyAccJerk-meanFreq()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|60             |`Mean-fBodyAccJerk-meanFreq()-Z` |Mean of var `fBodyAccJerk-meanFreq()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|61             |`Mean-fBodyGyro-mean()-X`        |Mean of var `fBodyGyro-mean()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|62             |`Mean-fBodyGyro-mean()-Y`        |Mean of var `fBodyGyro-mean()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|63             |`Mean-fBodyGyro-mean()-Z`        |Mean of var `fBodyGyro-mean()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|64             |`Mean-fBodyGyro-std()-X`         |Mean of var `fBodyGyro-std()-X` (as described in features_info.txt) across all trials of a particular activity.       |[-1,1]
|65             |`Mean-fBodyGyro-std()-Y`         |Mean of var `fBodyGyro-std()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|66             |`Mean-fBodyGyro-std()-Z`         |Mean of var `fBodyGyro-std()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|67             |`Mean-fBodyGyro-meanFreq()-X`    |Mean of var `fBodyGyro-meanFreq()-X` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|68             |`Mean-fBodyGyro-meanFreq()-Y`    |Mean of var `fBodyGyro-meanFreq()-Y` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|69             |`Mean-fBodyGyro-meanFreq()-Z`    |Mean of var `fBodyGyro-meanFreq()-Z` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|70             |`Mean-fBodyAccMag-mean()`        |Mean of var `fBodyAccMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|71             |`Mean-fBodyAccMag-std()`         |Mean of var `fBodyAccMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|72             |`Mean-fBodyAccMag-meanFreq()`    |Mean of var `fBodyAccMag-meanFreq()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|73             |`Mean-fBodyBodyAccJerkMag-mean()`|Mean of var `fBodyBodyAccJerkMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|74             |`Mean-fBodyBodyAccJerkMag-std()` |Mean of var `fBodyBodyAccJerkMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|75             |`Mean-fBodyBodyAccJerkMag-meanFreq()`|Mean of var `fBodyBodyAccJerkMag-meanFreq()` (as described in features_info.txt) across all trials of a particular activity.    |[-1,1]
|76             |`Mean-fBodyBodyGyroMag-mean()`   |Mean of var `fBodyBodyGyroMag-mean()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|77             |`Mean-fBodyBodyGyroMag-std()`    |Mean of var `fBodyBodyGyroMag-std()` (as described in features_info.txt) across all trials of a particular activity.        |[-1,1]
|78             |`Mean-fBodyBodyGyroMag-meanFreq()`|Mean of var `fBodyBodyGyroMag-meanFreq()` (as described in features_info.txt) across all trials of a particular activity.       |[-1,1]
|79             |`Mean-fBodyBodyGyroJerkMag-mean()` |Mean of var `fBodyBodyGyroJerkMag-mean()` (as described in features_info.txt) across all trials of a particular activity.      |[-1,1]
|80             |`Mean-fBodyBodyGyroJerkMag-std()` |Mean of var `fBodyBodyGyroJerkMag-std()` (as described in features_info.txt) across all trials of a particular activity.       |[-1,1]
|81             |`Mean-fBodyBodyGyroJerkMag-meanFreq()`|Mean of var `fBodyBodyGyroJerkMag-meanFreq()` (as described in features_info.txt) across all trials of a particular activity.   |[-1,1]
