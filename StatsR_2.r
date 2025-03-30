###############################################################
# Deriving sampling distribution and confidence intervals
###############################################################

###############################################################
### Exercise 1: Deriving sampling distributions
###############################################################
## In this exercise, we're going to derive sampling distributions for the mean with 
## different sizes.

## a) We will not use data from a normal distribution, but work with the poisson distribution, which is 
### often used for count data. We start by generating a large random sample of a poisson distribution
### with lambda = 1. Use rpois and create a sample of 1000 values, assign them to 'pdata'.
### Please first set a seed of 555 to have comparable results
set.seed(555)

pdata <- rpois(1000, 1)

## b) Take a look at your sample using the table() function and histogram and boxplot. 
table(pdata)

hist(pdata)
boxplot(pdata)


## c) In what ways does this diverge from a normal distribution?
### Name at least 2 differences in reference to the plots and/or table

#The mean should be in the middle at 3 and not 1
#It's not symmetric around the mean

## d) Now, we are going to draw a smaller sample from our generated data set.
### Use the function sample() to create a sample of five instances from pdata.
### assign it to sample5

sample5 <- sample(pdata, 5)

## e) draw another sample of 5 called sample5b

sample5b <- sample(pdata, 5)

## f) calculate the mean of the two samples and store them in the vector means5

means5 <- c(mean(sample5), mean(sample5b))

## g)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 5. However, we don't want to repeat 
###   question e and f 1000 times. Use a for loop to draw 1000 samples of size 5
###   and store the mean of each sample in the vector means5.

for (i in 1:1000) {
  means5[i] = mean(sample(pdata, 5))
}


## h) Repeat the for-loop in question h, but use a sample size of 20. 
##    Assign this to 'means50' instead of 'means5'.
means50 <- c()

for (i in 1:1000) {
  means50[i] = mean(sample(pdata, 20)) #I guess it should be named means20, but I just copy the typo :D
}


## i) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

#means5 contains 1000 means of samples with size 5 and means50 contains 1000
#means of samples with size 20 --> The size of the samples differs

## j) Draw histograms of means5 and means20. Describe in what way they differ

hist(means5)
hist(means50)

#means 5 is skewed to the left, because it has a wider distribution 
#(more extreme means are included like 3)
#Those are not included in means50, because of the larger sample sizes

## k) Why do you observe a skew for means5, but not for means20?

#Explained in j), because of the higher sampling rate the means are closer to the true value

###############################################################
### Exercise 2: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.


## a) What does a confidence interval mean from the perspective of experiment replication?

#The tighter the confidence interval the higher is the replicabilty of the experiment,
#because we have a high chance, that the real mean is in this tight interval

## b) please install and load packages sciplot and lsr

library(sciplot)
library(lsr)

## c) calculate 95% Confidence Intervals for pdata, sample5 and sample5b. You can
##    use the function ciMean()

ciMean(pdata, 0.95) #1
ciMean(sample5, 0.95) #2
ciMean(sample5b, 0.95) #3

## d) Why are these intervals so different?

#Firstly #1 calculates the CI for our normal data and #2 and #3 refer to the CI for the means of samples
#The CI for #3 is smaller than the CI for #2 since the bigger sample size makes it more precise
#So the chance that you have the true mean is higher

## e) Is the true mean contained in the interval?

mean(pdata) #1.027 inside the interval
mean(sample5) #1.8 inside the interval
mean(sample5b) #0.6 inside the interval

## f) In the following, we will check whether the CI behaves as expected.
### What is the true mean in our example?

trueMean = mean(pdata)

## g) Change your for loop from above (means20) to calculate the confidence interval 
### instead of the mean. Then check whether the confidence interval contains the
### true mean and save the result in the variable TrueMeanContained
### Hint: You will need to compare the mean to the lower and the upper bound of the confidence interval
### ciMean(YOURSAMPLE)[1] gives you the lower bound and ciMean(YOURSAMPLE)[2] the upper bound

TrueMeanContained = c()

for (i in 1:1000) {
  lower = ciMean(sample(pdata, 20))[1]
  upper = ciMean(sample(pdata, 20))[2]
  TrueMeanContained[i] =  trueMean >= lower & trueMean <= upper
}

## h) Given your results in TrueMeanContained, you now need to check, whether the interval really contains
### the mean 95% of the time. Does it?

sum(TrueMeanContained)/1000
#result: 0.951 --> 95% CI is true

## i) Confidence intervals are often used in plots. Lets construct a barplot with confidence intervals for
### the dataset chickwts, which contains weight of chickens after being administered different kinds of 
### food for 6 weeks.
### Use the function bargraph.CI to plot weight by feed, using the arguments response and x.factor

str(chickwts)

bargraph.CI(data = chickwts, response = weight, x.factor = feed)

## j) Now additionally set the optional argument ci.fun to ciMean. How does the graph change and why?
### Hint: Look into the documentation of bargraph.CI.

bargraph.CI(data = chickwts, response = weight, x.factor = feed, ci.fun = ciMean)

#The CI range increases with ciMean
#Standard function is mean +/- 1 SE and ciMean calculates the 95% CI interval that's larger


