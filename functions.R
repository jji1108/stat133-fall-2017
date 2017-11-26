#Remove Missing Function
##This function removes all the "NA"s in a vector.
##Input: a numeric vector
##Output: a numeric vector without any "NA"s

remove_missing <- function (x){
  return(x[!is.na(x)])
}


#Get Minimum Function
##This function returns the minimum value of a numeric vector.
##Input: a numeric vector
##Output: the smallest value in that numeric vector

get_minimum <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  sorted <- sort(x)
  sorted[1]
}


#Get Maximum Function
##This function returns the maximum value of a numeric vector.
##Input: a numeric vector
##Output: the greatest value in that numeric vector

get_maximum <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  sorted <- sort(x)
  sorted[length(x)]
}


#Get Range Function
##This function returns the range of the numeric vector.
##Input: the maximum and the minimum of a numeric vector
##Output: the range of this vector

get_range <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return(get_maximum(x) - get_minimum(x))
}


#Get Percentile 10 Function
##This function returns the top 10th percentile of the numeric vector.
##Input: a numeric vector
##Output: the top 10th percentile of that numeric vector

get_percentile10 <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return(quantile(x, 0.1))
}


#Get Percentile 90 Function
##This function returns the 90th percentile of the numeric vector.
##Input: a numeric vector
##Output: the 90th percentile of that numeric vector

get_percentile90 <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return(quantile(x, 0.9))
}


#Get Quartile 1 Function
##This function returns the first quartile of the numeric vector.
##Input: a numeric vector
##Output: the first quartile of this vector

get_quartile1 <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return(quantile(x, 0.25))
}


#Get Quartile 3 Function
##This function returns the third quartile of the numeric vector.
##Input: a numeric vector
##Output: the third quartile of this vector

get_quartile3 <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return(quantile(x, 0.75))
}


#Get Median Function
##This function returns the median of the numeric vector.
##Input: a numeric vector
##Output: the median of this vector

get_median <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  sorted <- sort(x)
  length <- length(x)
  if (length %% 2 == 0) {
    (sorted[length/2] + sorted[(length/2 + 1)])/2
  }else{
    sorted[(length + 1)/2]
  }
}


#Get Average Function
##This function returns the average of the numeric vector
##Input: a numeric vector
##Output: the average of this numeric vector

get_average <- function (x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  sum = 0
  average = 0
  
  for (i in 1:length(x)){
    sum = sum + x[i]
  }
  average = sum/ length(x)
  return(average)
}


#Get Standard Deviation Function
##This function returns the standard deviation of the numeric vector
##Input: a numeric vector
##Output: the standard deviation of this numeric vector
get_stdev <- function(x, na.rm=TRUE){
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  difference = 0
  stdev = 0
  
  for (i in 1:length(x)){
    difference = difference + (x[i]- get_average(x))^2
  }
  stdev = sqrt(difference/ (length(x)-1))
  return(stdev)
}


#Count Missing Function
##This function outputs the number of missing values in the numeric vector
##Input: a numeric vector
##Output: the number of missing values in this vector

count_missing <- function (x){
  return(length(x[x == "NA"]))
}


#Summary Statistics Function
##This function outputs the summary statistics of a numeric vector
##Input: the vector's minimum, 10th percentile, first quartile, median, mean, third quartiel
##90th percentile, maximum, range, standard deviation, number of missing values
##Output: a list of summary statistics

summary_stats <- function(x) {
 summary_of_stats <- as.list(c(get_minimum(x), get_percentile10(x), get_quartile1(x), get_median(x), 
                               get_average(x), get_quartile3(x), get_percentile90(x), 
                               get_maximum(x), get_range(x), get_stdev(x), count_missing(x)))
 names(summary_of_stats) <- c("Minimum","Percentile 10","Quartile 1","Median","Average","Quartile 3","Percentile 90","Maximum","Range","Stdev","Missing")
 return(summary_of_stats)
 }


#Print Statistics Functions
##This function outputs the summary statistics functions in a list
##Input: the vector's minimum, 10th percentile, first quartile, median, mean, third quartiel
##90th percentile, maximum, range, standard deviation, number of missing values
##Output: a list of summary statistics

print_stats <- function (x){
  min <- get_minimum(x)
  percentile10 <- get_percentile10(x)
  quartile1 <- get_quartile1(x)
  median <- get_median(x)
  average <- get_average(x)
  quartile3 <- get_quartile3(x)
  percentile90 <- get_percentile90(x)
  max <- get_maximum(x)
  range <- get_range(x)
  stdev <- get_stdev(x)
  missing <- count_missing(x)
  
  cat("Minimum      : ", min)
  cat("\n")
  cat("Percentile 10: ", percentile10)
  cat("\n")
  cat("Quartile1    : ", quartile1)
  cat("\n")
  cat("Median       : ", median)
  cat("\n")
  cat("Mean         : ", average)
  cat("\n")
  cat("Quartile3    : ", quartile3)
  cat("\n")
  cat("Percentile 90: ", percentile90)
  cat("\n")
  cat("Maximum      : ", max)
  cat("\n")
  cat("Range        : ", range)
  cat("\n")
  cat("Std. dev.    : ", stdev)
  cat("\n")
  cat("Missing      : ", missing)
  cat("\n")
  
}

#Drop Lowest Function
##This function drops the lowest value of a numeric vector
##Input: a numeric vector
##Output: a numeric vector without the lowest value 

drop_lowest <- function (x){
  x <- sort(x)[2:length(x)]
}

#Rescale 100 Function
##This function rescales all values in the numeric vector based on the input minimum and maximum
##Input: a numeric vector, a minimum, and a miximum
##Output: a numeric vector that is rescaled according to the given minimum and maximum

rescale100 <- function (x,a,b) {
  xmin <- a
  xmax <- b
  100 * ((x - xmin)/(xmax-xmin))
}


#Score Homework Function
##This function drops the lowest value in the vector and returns the average of the vector
##Input: a numeric vector and drop function
##Output: a numeric value that represents the average after the lowest homework score is dropped

score_homework <- function(x,drop) {
  if (drop == TRUE) {
    dropped <- drop_lowest(x)
    avg <- 1/length(dropped)* sum(dropped)
  }else{
    avg <- 1/length(x) * sum(x)
  }
  return(avg)
}

#Score Quiz Function
##This function drops the loweest value in the vector and returns the average of the vector
##Input: a numeric vector and drop function
##Output: a numeric value that represents the average after the lowest quiz score is dropped

score_quiz <- function(x,drop){
  if (drop == TRUE){
    dropped <- drop_lowest(x)
    avg <- 1/length(dropped)*sum(dropped)
  }else{
    avg <- 1/length(x)*sum(x)
  }
  return(avg)
}

#Score Lab Function
##This function returns the score of lab depending on the number of lab attendance
##Input: a numeric value
##Output: the corresponding score for that value of attendance

score_lab <- function(x){
  if (x > 10){
    return(100)
  }
  if (x == 10){
    return(80)
  }
  if (x == 9){
    return(60)
  }
  if (x == 8){
    return(40)
  }
  if (x == 7){
    return(20)
  }
  if (x == 6){
    return(0)
  }
  if (x < 6){
    return(0)
  }
}