library(caret)

process_unit <- function(training, test) {
  arg <- preProcess(training, method ="range")
  
  train_processed <- predict(arg, training)
  test_processed <- predict(arg, test)
  
  list(train_processed, test_processed)
} 

process_normal <- function(training, test) {
  arg <- preProcess(training, method=c("center", "scale"))
  
  train_processed <- predict(arg, training)
  test_processed <- predict(arg, test)
  
  list(train_processed, test_processed)
} 
