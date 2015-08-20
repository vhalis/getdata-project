library(plyr)
library(dplyr)
library(lazyeval)

# Read in from test and train data sets, label them, 
# subset them, and then merge them

# Relevant files per dataset
dirs = c("test", "train")
files_per_dir = c("X_", "y_", "subject_")
file_extension = "txt"
# Files applicable to both datasets
activities = "activity_labels"
features = "features"

readData = function(fileName, dir=".", suffixWithDir=FALSE) {
  f = paste(dir, fileName, sep="/")
  if (suffixWithDir) { f = paste0(f, dir) }
  f = paste(f, file_extension, sep=".")
  read.table(f, stringsAsFactors=FALSE)
}

activity_labels = readData(activities)
measurement_labels = readData(features)
mean_and_std_idx = grepl("mean|std", measurement_labels$V2)
mean_and_std_names = measurement_labels$V2[mean_and_std_idx]

readAndCleanDataSet = function(dataSet) {
  base_data = readData(files_per_dir[1], dataSet, suffixWithDir=TRUE)
  mean_and_std_data = base_data[, mean_and_std_idx]
  names(mean_and_std_data) = mean_and_std_names
  
  test_type = readData(files_per_dir[2], dataSet, suffixWithDir=TRUE)
  test_type_strs_col = sapply(test_type$V1,
                              function(x) activity_labels$V2[x])
  subject_nums = readData(files_per_dir[3], dataSet, suffixWithDir=TRUE)
  mutate(mean_and_std_data,
         ActivityType=test_type_strs_col,
         SubjectNum=subject_nums$V1)
}

mergeAllData = function() {
  test_set = readAndCleanDataSet(dirs[1])
  train_set = readAndCleanDataSet(dirs[2])
  merge(test_set, train_set, all=TRUE)
}

summarizeAllData = function(bySubject=TRUE, byActivityType=TRUE) {
  all_data = mergeAllData()
  if (bySubject) { all_data = group_by(all_data, SubjectNum) }
  if (byActivityType) { all_data = group_by(all_data, ActivityType, add=TRUE) }
  means_by_col = sapply(mean_and_std_names,
                        function(x) interp(~mean(var), var=as.name(x)))
  sum_data = summarize_(all_data, .dots=means_by_col)
  # Rename to indicate the averaging
  new_names = sapply(mean_and_std_names,
                     function(x) paste0("Mean-", x))
  names(new_names) = mean_and_std_names
  plyr::rename(sum_data, new_names)
}