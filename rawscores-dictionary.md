---
title: "rawscore-dictionary.md"
author: "Jean Ji"
date: "November 14, 2017"
output: html_document
---

```{r}
sink(file = "C:/Users/Jean Ji/stat133/stat133-hws-fall17/hw04/data/rawdata/rawscores.csv")
read.csv ("https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2017/master/data/rawscores.csv")
sink()

```

##There are 334 entries of data and 16 variables, including scores for Homework 1, Homework 2, Homework 3, Homework 4, Homework 5, Homework 6, Homework 7, Homework 8, Homework 9, lab attendance, scores for Quiz 1, Quiz 2, Quiz 3, Quiz 4, scores for Exercise 1 and Exercise 2.

##At the firsrt glance of the data table, there are some missing values scattered across the different columns and rows of this data frame.

##Some entries of scores have decimal points while some do not.

##Some entries are integers while others are real numbers.

