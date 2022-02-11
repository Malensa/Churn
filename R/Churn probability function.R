

churnPrediction <- function(data, customerId){
  if (customerId %in% data$CustomerId) {
    return (data[data$CustomerId == customerId ]$ChurnProbability)
  } else{
    warning("Given customer id does not exist")
  }
}

