#This script is for cleaning the raw data to output the clean version of the data

source(file = "C:/Users/Jean Ji/stat133/stat133-hws-fall17/hw04/code/functions.R")
library(dplyr)

#Reading in the csv file
rawscore <- read.csv("https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2017/master/data/rawscores.csv")
sink(file = "C:/Users/Jean Ji/stat133/stat133-hws-fall17/hw04/output/summary-rawscores.txt")
str(rawscore)
sink()

#write a for loop for replacing all NA values wiht zeros
for (i in 1:ncol(rawscore)) { 
  for (j in 1:nrow(rawscore)) {
    if (is.na(rawscore[j,i])) {
      rawscore[j,i] <- 0
  }
 }
}

#rescaling quizzes
quiz1_rescaled <- rescale100(rawscore$QZ1,0,12)
quiz2_rescaled <- rescale100(rawscore$QZ2,0,18)
quiz3_rescaled <- rescale100(rawscore$QZ3,0,20)
quiz4_rescaled <- rescale100(rawscore$QZ4,0,20)

#Rescaling tests and adding variables to the data frame
Test1 <- rescale100(rawscore$EX1,0,80)
Test2 <- rescale100(rawscore$EX2,0,90)
rawscore$Test1 <- Test1
rawscore$Test2 <- Test2

###Adding the variable "Homework" to the data frame
homework <- vector(mode = 'list', length = 334)
for (row in 1:334){
 homework[[row]] <- score_homework(rawscore[row,1:9],drop = TRUE)
}
rawscore$Homework <- as.numeric(unlist(homework))

###Adding the variable "Quiz" to the data frame
quiz <- data.frame(quiz1_rescaled,quiz2_rescaled,quiz3_rescaled,quiz4_rescaled)
quiz_score <- vector(mode = "list", length = 334)
for (row in 1:334){
  quiz_score[[row]] <- score_quiz(quiz[row,1:4],drop = TRUE)
}
rawscore$Quiz <- as.numeric(unlist(quiz_score))

###Adding the variable "Lab" to the dta frame
lab <- vector(mode = 'list', length = 334)
for (row in 1:334){
  lab[[row]] <- score_lab(rawscore[row,10])
}
rawscore$Lab <- as.numeric(unlist(lab))

#Creating an "Overall" variable
Overall <- 0.1*unlist(lab) + 0.3*unlist(homework) + 0.15*unlist(quiz_score) + 0.2*Test1 + 0.25 * Test2
rawscore$Overall <- Overall

#Creating a "Grade" variable
Grade <- vector(mode = "list", length = 334)
for (i in 1:334){
 if (Overall[i] < 50){
  Grade[[i]] <- print("F")
}else if(Overall[i] >= 50 & Overall[i] < 60){
  Grade[[i]] <- print("D")
 }else if(Overall[i] >= 60 & Overall[i] < 70){
  Grade [[i]] <- print('C-')
 }else if (Overall[i] >= 70 & Overall[i] < 77.5){
  Grade [[i]] <- print("C")
}else if(Overall[i] >= 77.5 & Overall[i] < 79.5){
  Grade [[i]] <- print("C+")
 }else if(Overall[i] >= 79.5 & Overall[i] < 82){
  Grade [[i]] <- print("B-")
 }else if(Overall[i] >= 82 & Overall[i] < 86){
  Grade [[i]] <- print("B")
 }else if(Overall[i] >= 86 & Overall[i] < 88){
  Grade [[i]] <- print("B+")
 }else if(Overall[i] >= 88 & Overall[i] < 90){
  Grade [[i]] <- print("A-")
 }else if(Overall[i] >= 90 & Overall[i] < 95){
  Grade [[i]] <- print("A")
 }else {
  Grade [[i]] <- print("A+")
 }
}
rawscore$Grade <- unlist(Grade)

#Creating summary statistics text files for homework, quiz, test1, test2, and overall grades
names <- c("Lab", "Homework", "Quiz", "Test1", "Test2", "Overall")
for (elem in names) {
  sink(file = paste0(elem, "-stats.txt"))
  print(summary_stats(rawscore[, elem]))
  sink()
}

sink(file = "C:/Users/Jean Ji/stat133/stat133-hws-fall17/hw04/output/summary-cleanscores.txt")
str(rawscore)
sink()

write.csv(rawscore,file = "C:/Users/Jean Ji/stat133/stat133-hws-fall17/hw04/data/cleandata/cleanscores.csv")
