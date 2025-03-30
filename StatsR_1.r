### Stats with R Exercise

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 7. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## You are required to work together in groups of three students.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## cms discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()


## b) Get help with this function.
help("getwd")

## c) Change your working directory to another directory.
setwd("E:/study_projects/StatisticsWithR")
###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -4 to 4, by 0.1. Assign this to the 
##    variable x.

x <- seq(-4,4, 0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
help("dnorm")
## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x,dnorm(x))
## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.6, and plot a line 
##    instead of the circles.
plot(x, dnorm(x), type = "l", ylim = c(0, 0.6))
## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v = mean(x), lty = 2)
## f) Take a look at the beaver2 dataset. (You can see it by typing "beaver2".) 
##    Then select only the temperature part and store it in a variable "b2temp".
glimpse(beaver2)

b2temp <- beaver2$temp
## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
b_mean <- mean(b2temp)
b_SD <- sd(b2temp)

plot(b2temp, dnorm(b2temp, mean = b_mean, sd = b_SD))


## h) We observe two additional temperature values (38.13 an 36.81). What's the 
##    likelihood that these temperatures (or more extreme ones) respectively 
##    come from the normal distribution from g)?

1 - pnorm(38.13, b_mean, b_SD) #=0.116 that the value is 38.13 or higher
pnorm(36.81, b_mean, b_SD) #=0.0391 that the value is 36.81 or lower

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 36 to 39 using xlim. 
##    Fix the number of breaks to 10 using breaks
##    What do you observe?

sample1 <- rnorm(20, b_mean, b_SD)
sample2 <- rnorm(20, b_mean, b_SD)
sample3 <- rnorm(20, b_mean, b_SD)
sample4 <- rnorm(20, b_mean, b_SD)
sample5 <- rnorm(20, b_mean, b_SD)

hist(sample1, xlim = c(36,39), breaks = 10)
hist(sample2, xlim = c(36,39), breaks = 10)
hist(sample3, xlim = c(36,39), breaks = 10)
hist(sample4, xlim = c(36,39), breaks = 10)
hist(sample5, xlim = c(36,39), breaks = 10)

#Observation: the histograms differ a lot

###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)
data(package = "languageR")

## b) Specifically, we will deal with the dataset 'selfPacedReadingHeid'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from
##    Inspect 'selfPacedReadingHeid'. Look at the head, tail, 
##    and summary. What do head and tail show you?

head(selfPacedReadingHeid) #shows first 6 samples
tail(selfPacedReadingHeid) #shows last 6 samples
summary(selfPacedReadingHeid)

## c) The file contains multiple observations for each participant. Create a 
##   subset only including subject number PP002 and assign it to PP002.
##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?

PP002 <- selfPacedReadingHeid %>%
  subset(Subject == "PP002")
#40 observations and 18 rows

## d) Create a histogram (using hist()) of "RT" (logarithm of reading time) 
##    for PP002

hist(PP002$RT)

## e) Create a kernel density plot for this data using density()

plot(density(PP002$RT))

## f) What is the difference between the two?

#We now have density instead of frequency on the y-axis (The density plot also gives N and the bandwidth)
#The shapes are very similar but the density plot is continuous

## g) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)

#plot(PP002$RT, dnorm(PP002$RT, mean = mean(PP002$RT), sd = sd(PP002$RT)))
#We needed to compare the normal distribution for the mean and sd of our data with the kernel density plot

###############
### Exercise 4: Dataframes and boxplots
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 26 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 18 19 22 17 18 26 17 14 16 16 17 21 23 16 20 21 20 20 15 17 17 18 20 24


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? 

#Ratio scale. We can say "a said the word twice as often as b" and we can order them
#Discrete since there are only whole numbers. No child could say it 22.5 times 

## b) The researcher is also interested in whether story telling is related to 
##    their reading habits. As a proxy, she asked the children, whether they have 
##    a library card. The following line codes that the first 13 observations are
##    from children with library card (Y) and the remaining 13 from children 
##    without (N). What measurements scale does this variable have?
 lib = c(rep("Y",13),rep("N",13))

#Nominal scale 

## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pps', and your 
##    participants should be labeled from 1 to 26

pps <- c(1:26)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.

obs <- c(18, 15, 18, 19, 22, 17, 18, 26, 17, 14, 16, 16, 17, 21, 23, 16, 20, 21, 20, 20, 15, 17, 17, 18, 20, 24)

## e) Create a dataframe including pps, obs and lib. Assign this to 'stories'. 

stories <- data.frame(pps, obs, lib)


## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pps' and 'lib'?

summary(stories)
lapply(stories, class)
#pps = integer, obs = numerric, lib = character


## g) Change the class of 'pps' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)

stories$pps <- as.factor(stories$pps) #we get more information about the levels
stories$lib <- as.factor(stories$lib) #the n and y also gets encoded in 1 and 2

lapply(stories, class)

## h) Create a boxplot of obs for the two lib groups

boxplot(stories$obs ~ stories$lib, xlab = "lib", ylab = "observation")

## i) Are there outliers in one of the lib groups?

#2 outliers in the Y group

## j) Which group shows the larger interquartile range? Which one has the 
##    greater overall range?

#N has the higher IQR range and the greater overall range 

## k) Which group shows a negative or positive skew?

#N group negative screw
#Y group no screw

## l) What is a whisker? Why is the upper whisker of group "Y" so short?

#A whisker is the dashed line that goes up and down from the box, it shows 
#the range of the most extreme point that is still in 1.5 IQR
#It's so short because there is no other point above it that is in the 1.5 IQR range 
#And the outlier is outside the IQR range


## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?

ymean <- mean(filter(stories, lib == "Y")$obs)
ymedian <- median(filter(stories, lib == "Y")$obs)

#mean = 17.92
#median = 17
#They used the median. They are calculated different. 
#The mean gives the avg, but the median returns the value in the middle of our sorted data







