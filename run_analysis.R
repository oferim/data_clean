# Script for Data cleaning Project
# Ofer Imanuel 21-SEP-2014

#setwd( "C:/Work/DataClean/UCI_HAR_Dataset" )	#Working directory - specific to my setup

# Retrieve set of 561 features and 6 activities
features = read.table( "features.txt" )
features = features[,2]
activity_labels = read.table( "activity_labels.txt", col.names = c( "ID", "Name" ) )

# Load main data - training and test
a             = read.table( "train/X_train.txt", col.names=features, colClasses=rep( "numeric", 561 ) )
b             = read.table( "test/X_test.txt", col.names=features, colClasses=rep( "numeric", 561 ) )

# Load subject and activity lists for the main data - combine directly into the respective training / testing variables
# Note, that train1_test2 is self explanatory
a["subject" ] = read.table( "train/subject_train.txt" )
a["activity"] = read.table( "train/Y_train.txt" )
a["train1_test2"] = 1
b["subject" ] = read.table( "test/subject_test.txt" )
b["activity"] = read.table( "test/Y_test.txt" )
b["train1_test2"] = 2

# Combine a (training) and b (testing) component to a unified data - result for step #1 (size 10,299 rows x 564 columns)
data = rbind( a, b )

# Get the indices of the features we need: anything with mean, anything with std (that's the same as std() ), and the 3 added columns
mean_features = grep( "mean", features, ignore.case=TRUE )
std_features  = grep( "std" , features, ignore.case=TRUE )
added_features = 562:564
needed_features = sort( c( mean_features, std_features, added_features ) )

# Retrieve the needed columns to give the result for step #2 - (size 10,299 rows x 89 columns)
needed_data = data[ , needed_features ]

# Turn activity column to factors, and give the right names (walking etc) to the values, without spending extra memory
# That gives us step #3
needed_data$activity = factor( needed_data$activity, labels=activity_labels$Name )

# Step #4 was apparently already accomplished (by specifying col.names=features)

# To acconmplish step #5, I will create a data frame with 1 column less than needed_data.
# It will have the 86 columns of the various measures, as well as the subject and the activity.
# We drop the differentiation between test and train. The values are the means.
# There might be a more elegant way to do it, but I am out of time ...
row_set = unique( needed_data[ ,c( "subject", "activity" ) ] )
row_num = nrow( row_set )
col_num = ncol(needed_data) - 3  # The columns we want to calculate average on - i.e, 86

# Ugly - create a data frame with the required size and columns. All the remains is to override the content
tidy_data = needed_data[ 1:row_num, 1:(col_num + 2) ]

for( i in 1:row_num )
{
  cur_subject = row_set[i,"subject"]
  cur_activity = row_set[i, "activity"]
  tidy_data[ i, "subject" ] = cur_subject
  tidy_data[ i, "activity" ] = cur_activity
  cur_data = needed_data[ needed_data$subject==row_set[i, "subject"] & needed_data$activity==row_set[i,"activity"], 1:col_num ]
  for( j in 1:col_num )
  {
     tidy_data[ i, j ] = mean( cur_data[, j] )
  }
}

# At this point tidy_data contains the required result