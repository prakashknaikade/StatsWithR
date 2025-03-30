#################################################################################
#Tests for Categorical Data and cleaning data
#################################################################################


#################################################################################
##  Exercise 1: Cleaning data
#################################################################################

## download the file insomnia.csv from cms
## The made-up dataset insomnia contains data of a survey on 60 students. 
## They are asked two questions: whether they regularly encounter sleep problems
## and what their preferred and most consumed drink is.

## a. Load the libraries stringr, dplyr and tidyr.
library(stringr)
library(tidyr)
library(dplyr)

## b. read in the data
data <- read.csv("/insomnia.csv")
## c. get a summary of the dataset

summary(data)

## d. how many students encounter sleep problems?

#Solution for non binary data (0.5 would be allowed too as "medium" sleep problems)
sleepProblems = 0

for (i in 1:nrow(data)) {
  if(data$sleepProblem[i] > 0){
    sleepProblems = sleepProblems + 1
  }
}

sleepProblems

#Solution for binary data 1 = has 0 = has not sleep problems

sum(data$sleepProblem)


## e. how many different drinks do students name? (transform the variable into a 
## factor first)

data$drink <- as.factor(data$drink)
levels(data$drink)

#8 factor levels but
#3 drinks were mentioned: Coffee, Tea and Water

## f. collapse factor levels which were spelled wrong. Make sure you first handle
## case and whitespace incongruencies, before you fix individual misspellings
## with gsub

lev = levels(data$drink)
lev = str_trim(lev)
lev = str_to_title(lev)

lev = gsub("Tee", "Tea", lev)
lev = gsub("\\bCoffe\\b", "Coffee", lev)

levels(data$drink) = lev

levels(data$drink)

## You realize that most students had multiple exams in the week from Feb 22 to 
## Feb 26. As students had to learn a lot and were possibly worried, they might 
## misjudge or exaggerate their sleep problems as occurring "regularly"
## We want to exclude all data that was collected between and including Feb 15 
## and Feb 26!

#short form
data %>%
  filter(as.Date(data$date) < as.Date("2021-02-15") 
         | as.Date(data$date) > as.Date("2021-02-26"))


## g.  First show how many data points will be concerned, you need to transform
##     the date column to a Date object first!
 
data$date <- as.Date(data$date)

## h. Now filter out this part of the data and assign the result to clean
 
clean = filter(data, data$date < as.Date("2021-02-15") 
       | data$date > as.Date("2021-02-26"))

#################################################################################
### Exercise 2: chi-squared test
#################################################################################

## consider the data set from above. If you had problems performing the
## required cleaning steps, note that you can also do them by hand
## Now consider we want to see whether the preferred drink influences sleep problems

## a. formulate in plain English what the Null hypothesis is in this context

#The preferred drink has no influence on sleep problems

## b. conduct a chi-squared test to test this hypothesis using the function chisq.test()
##    and assign the result to chi

chi = chisq.test(clean$drink, clean$sleepProblem)

## c. the last call produced a warning. To understand why this warning arises, look
##    at observed and expected frequencies of chi. Why do you think it produced the error?

chi$observed
chi$expected
#Not all expected values are greater than 5 (4.8, 2.88, 4.32), 
#thus not all expected values are sufficiently large

## d. What are the expected frequencies? Do we need to look at expected or 
##    observed frequencies?

#Expected frequencies are the values, that we would expect, if there was no 
#impact of drink on sleep problems. So we have the same ratios for people with
#sleeping problems:
#4.8/20 = 2.88/12 = 4.32/18 and the same for the no sleep problems column

#For the sufficiently large numbers we need to look at the expected values
#For the chi square test we need both

## e. a possible solution is to sample more participants. Given that the smallest 
##    admissible value is 5, from which group(s) in terms of preferred drinks do
##    we have to sample more?

#We need to sample more people from all (Coffee, Tea and Water) groups

## f. Assume we don't have the possibility to sample more students. Which test do
##    you have to run instead? How does it work roughly? Perform a suitable test

#We use the fisher's exact test
#We calculate the diffent possible tables for our fixed margins
#We sum up the probability of those tables that are as or more extreme than our 
#original table
#If the sum is less than our alpha we reject the null hypothesis 

fisher.test(clean$drink, clean$sleepProblem)

## g. Lastly, what is the conclusion of your test? What have you learned and what 
##    have you not learned? 

#p value is 0.018, so for a standard alpha of 5% -> 0.018 < 0.05
#Hence, we reject the null hypothesis -> The fav drink has impact on sleeping problems

#I don't know what we haven't learned


#################################################################################
##Exercise 3. Binomial distribution
#################################################################################
## Suppose there are 18 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a. Please calculate the probability of getting exactly 5 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.

dbinom(5, 18, 0.2)
#0.1507

## b. Next please calculate the probability of answering 6 or more questions 
##    correctly by chance.

pbinom(6, 18, 0.2, lower.tail = FALSE)
#0.0512

#################################################################################
##Exercise 4
#################################################################################
##   Consider the data set used in Ex 1&2. How would the experiment have to change
##   in order for you to choose McNemar's test over the ChiSquare test? 
##   What would be the problem of using the normal ChiSquare test in a case where 
##   McNemar's test would be more appropriate?

#If we want to analyse changes, 
#e.g. we will test whether changing preferred drink from coffee to tea and vice versa 
##    influences people sleeping problems. 
##    If observations are dependent on each other, 
##    it's better to use McNemar's test instead of ChiSqare test

#                   Sleeping problems:
#
#                           Tea
#                       Yes     No
#  Coffee    Yes         a      b
#            No          c      d










