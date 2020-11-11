# This function is responsible for finding the k nearest neighbors itself
# dataframe - dataframe whose points we want to predict categories for
# train - the training dataframe
# cl_train - the true classification of the training dataframe
# k - number of nearest neighbors to look for
predict_categories <- function(dataframe, train, cl_train, k) {
  # Predicted categories vector for input dataframe which we will return
  predicted_categories = c()
  
  for (i in 1:nrow(dataframe)) { # Does kNN for each data point in dataframe
    dataframe_row = dataframe[i,] # current data point
    
    # This function calculates the distance between the current data point and
    # a single training dataframe point
    calc_row_dists <- function(train_row) {
      compare_mat <- rbind(train_row, dataframe_row)
      compare_dist <- dist(compare_mat, method='manhattan')
      return(compare_dist)
    }
    
    # We calculate all the distances from the current data point to every point
    # in the training dataframe using the apply function
    distances = as.matrix(unname(apply(train, 1, calc_row_dists)))
    
    # Retrieves indices of closest k neighbors
    k_near_nbs_ix = head(sort(distances, index.return = TRUE)$ix, k)
    
    # Determines the label of majority of the closest neighbors
    predicted_category = names(sort(table(cl_train[k_near_nbs_ix]), decreasing=TRUE)[1])
    
    # Adds the predicted category for the current dataframe point to a list that
    # will include all of them
    predicted_categories = c(predicted_categories, predicted_category)
  }
  
  return(predicted_categories)
}

# This parent function that we call to return predicted categories for the test dataset
# and accuracy of the kNN algorithm
# train - the training dataframe
# test - the testing dataframe
# cl_train - the true classification of the training dataframe
# cl_test - the true classification of the training dataframe
# k - number of nearest neighbors to look for (by default, 3)
mykNN <- function(train, test, cl_train, cl_test, k=3) {
  # Here we compute the confusion matrix for our testing dataframe
  test_predicted_categories <- predict_categories(test, train, cl_train, k)
  
  # Here we compute the confusion matrix for our training dataframe
  # and the accuracy of the predicted categories for it with the true categories
  confusion = table(test_predicted_categories, cl_test)
  accuracy = mean(test_predicted_categories == cl_test)
  
  return(list("predicted_categories"=test_predicted_categories,
              "accuracy"=accuracy,"error"=1-accuracy,
              "confusion"=confusion,"k"=k))
}

# Split 70/30 randomly
train <- iris[c(1:25, 51:75, 101:125), 1:4]
test <- iris[c(26:50, 76:100, 126:150), 1:4]
cl_train <- iris[c(1:25, 51:75, 101:125), 5]
cl_test <- iris[c(26:50, 76:100, 126:150), 5]

print(out <- mykNN(train, test, cl_train, cl_test))