##########################
# t-test and friends
##########################


#####################################################
### 1. Restructuring, plotting, and t tests
#####################################################

library(lsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(languageR)

## We will be working with the dataset lexdec from the package languageR
## In short, this data set contains reaction times from an experiment, where participants had 
## to decide whether something is a word or not (nonword). Only responses for real words are
## included and there were 79 measurements per participant.
## 
## Variables we will be interested in include 
## Subject (code for a participant)
## NativeLanguage (native language of participant)
## RT (log reaction time)
## Sex (of the participant)
## PrevType (whether the preceding word was a real word or a nonword)
## Class (whether the target word was denoting an animal or a plant)


## 1. Create the dataset lexdat, which is a copy of lexdec, but only includes the columns 
##  indicated above

lexdat <- select(lexdec, Subject, NativeLanguage, RT, Sex, PrevType, Class)

## Say you are interested in the influence of the previous word type on lexical decision time.
## Before we start testing, we want to get an impression of the data and create a barplot of 
## the mean by prevType, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI.
## (just execute the next line, as you will need the function in 2.)
se = function(x){sd(x)/sqrt(length(x))}

## 2. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by prevType and get the mean as well as the
##  se of RT. Store the result to summaryByPrevType
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function

summaryByPrevType <- lexdat %>%
  group_by(PrevType) %>%
  summarise(mean_val = mean(RT), se_val = se(RT))

## 3. Describe the resulting data set (summaryByPrevType) in your own words

#For our two partitions (word, non word), we calculated the mean(RT) and the se(RT)

## 4. Now use summaryByPrevType to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)


ggplot(summaryByPrevType, aes(PrevType, mean_val)) +
  geom_bar(fill = "cyan",stat = "identity") +
  geom_errorbar(aes(x = PrevType, 
                   ymin = mean_val - 1.96 * se_val,
                   ymax = mean_val + 1.96 * se_val))

## 5. The barplot always starts at zero, which makes the portion of the graph, we are most 
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes

ggplot(summaryByPrevType, aes(PrevType, mean_val, group = 1)) +
  geom_line(color = "cyan", size = 6) +
  geom_errorbar(aes(x = PrevType, 
                    ymin = mean_val - 1.96 * se_val,
                    ymax = mean_val + 1.96 * se_val))

## 6. Gauging from the plot, does it look like there's an important difference in mean RT 
##  after words compared to nonwords?

#It looks like here is a small, but important difference in RT, 
#since the error bars do not even slightly cross

## 7. Let's go back to the original data frame "lexdat".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT after words vs nonwords.
##  Why can't you compute a t-test on the data as they are now? 
##  Hint: Which assumption is violated?

#Because we have the assumption that the true distribution is normal.
#But we haven't checked, whether our data has a normal distibution.
  
## 8. We need to restructure the data to only one observation (average RT) per subject 
##  and word/nonword condition (PrevType). We will again use group_by and summarize, but
##  this time we have to group by Subject and PrevType, while we only need the mean to be 
##  stored, not the se. Assign the result to bySubj

bySubi <- lexdat %>%
  group_by(Subject, PrevType) %>%
  summarise(mean_val = mean(RT))

## 9. Create histograms of the RT data in bySubj depending on the preceding word 
##  type and display them side by side. Set the binwidth to 0.08

#I created multiple plots, but you only wanted the last one

ggplot(bySubi, aes(Subject , mean_val, fill = PrevType)) +
  geom_histogram(position = "dodge", stat = "identity", binwidth = 0.08)

ggplot(bySubi, aes(Subject , mean_val)) +
  geom_histogram(stat = "identity", binwidth = 0.08) +
  facet_grid(cols = vars(PrevType))

ggplot(bySubi, aes(mean_val)) +
  geom_histogram(binwidth = 0.08) +
  facet_grid(cols = vars(PrevType))


## 10. Display the same data in density plots. 

ggplot(bySubi, aes(mean_val)) +
  geom_density(binwidth = 0.08) +
  facet_grid(cols = vars(PrevType))

## 11. Based on the histograms and the density plots - are these data normally 
##  distributed?

#Not perfectly, but the data looks normally distributed.The right tail is to high in both groups.

## 12. Create boxplots of the mean RT in bySubj by PrevType

ggplot(bySubi, aes(mean_val)) +
  geom_boxplot() +
  facet_grid(cols = vars(PrevType))

## 13. Compute the t-test to compare the mean RT between decisions following on a word
##  vs a nonword using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?

#We need a paired t-test, because they asked the same group twice (the 2 conditions)

word <- subset(bySubi, bySubi$PrevType == "word")
nonword <- subset(bySubi, bySubi$PrevType == "nonword")

t.test(nonword$mean_val, word$mean_val, paired = TRUE)

#p-value: 5.472e-05

## 14. What does the output tell you? What conclusions do you draw?

#If we take alpha = 5%, then 0.0005 < 0.05 thus there is a significant 
#difference between the condition with words and non words

## 15. In addition to the long-format data we've just been working on, you may also 
## encounter data sets in a wide format (this is the format we have been using in 
## class examples.)
## Let's look at a different variable, namely the semantic class (Class) of the target 
## word instead of the type of the previous word. Again, summarize the dataset
## to obtain the mean RT by subject and class and transform the dataset to a 
## wide format. In addition to group_by() and summarize(), you will need the function 
## spread(). Assign the result to wide

wide <- lexdat %>%
  group_by(Subject, Class) %>%
  summarise(mean_val = mean(RT))

wide <- spread(wide, Class, mean_val)

head(wide)

## 16. Compute a t-test on the wide format data - note that for wide-format 
##  data you need to use a different syntax inside t.test()

t.test(wide$animal, wide$plant, paired = TRUE)

# p-value: 0.4226

## 17. What do you conclude from this?

# 0.4226 > 0.05 -> There is no significant difference in RT, between the Groups

## 18. Now let's look at yet another question, namely whether the native language 
##  of the participant influences their reaction time. Check out the variable
##  NativeLanguage. Can you use a t-test to pursue this question and which type
##  of t-test would you use? Can you think of a situation, where a t-test would not 
##  be enough to test for a difference depending on the native language?

#We can make an independent t-test with independent variance to compare the two different groups 
#Short Welch test

#If we have 3+ languages, then we cant use a t-test to compare all of them at once


## 19. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to NativeLanguage and assign it to bySubjNatLang
## Perform the t-test you decided for.

bySubjNatLang <- lexdat %>%
  group_by(Subject, NativeLanguage) %>%
  summarise(mean_val = mean(RT))

engl <- subset(bySubjNatLang, bySubjNatLang$NativeLanguage == "English")
other <- subset(bySubjNatLang, bySubjNatLang$NativeLanguage != "English")

t.test(engl$mean_val, other$mean_val, paired = FALSE, var.equal = FALSE)

#p-value: 0.0357

## 20. What do you conclude?

#0.0357 < 0.05 -> There is a significant difference between people who speak English
#or another language natively

## 21. Compute the effect size using Cohen's D. 

cohensD(engl$mean_val, other$mean_val)

#1.135

## 22.  Which effect size do we get? How do you interpret this result?

#Although these are only "raw" orientations: cohensD > 0.8 -> strong effect 

## 23. Choose an appropriate plot to visualize the difference between group

#Overview for the individual performance
ggplot(bySubjNatLang, aes(Subject, mean_val, fill = NativeLanguage)) +
  geom_histogram(stat = "identity")

#Density overview for both groups
ggplot(bySubjNatLang, aes(mean_val, col = NativeLanguage)) +
  geom_density()

#Boxplot for better comparison between groups
ggplot(bySubjNatLang, aes(mean_val, col = NativeLanguage)) +
  geom_boxplot()

###############
### 2. T-Test
###############
## In this exercise we will try to explore the independent samples t-test 
## and its affect on different samples. 
## We will take the same example discussed in the lecture. A class has two tutors, and we want 
## to find out which tutor is better by comparing the performance of the students in the final 
## exam by tutor group. First set a seed to make sure your results can be reproduced

set.seed(8254)
## 1. Generate 15 samples from a normal distribution with mean 20 and sd 8 and save it in a variable 
##  called "tutor1_grades"

tutor1_grades <- rnorm(15, 20, 8)

## 2. Now we generate our second sample of size 15, this time for tutor 2 and with mean 35 and 
## sd 15

tutor2_grades <- rnorm(15, 35, 15)


## 3. Combine the two samples and store the result into one vector called "score" (it should 
##    first show all scores from tutor1 followed by the scores of tutor2)

score <- c(tutor1_grades, tutor2_grades)
score

## 4. Create a vector called tutor indicating which tutor the score belongs to: it should show 
##   "tutor1" 15 times followed by "tutor2" 15 times

tutor <- as.factor(c(rep("tutor1", 15), rep("tutor2", 15)))

## 5. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.

data_frame <- data.frame(score, tutor)

## 6. Run the independent samples TTest (independentSamplesTTest()) and formulate the findings as discussed 
###  in the lecture. 
##	independentSamplesTTest() also provides the effect size (Cohen's d). How do you interpret the effect size?

independentSamplesTTest(data = data_frame, formula = score~tutor)

#The mean grade in the class of tutor1 is 18.05 (sd = 5.51) while the mean grade of tutor2's class
#is 38.53 (sd = 13.49). An independentSamplesTTest showed, that this ~20 points difference was
#significant (t(18.552) = -5.442, p <0.001, CI_95 = [-28.367, -12.589], d = 1.987), suggesting, that
#a difference in learning outcome has occurred

#The effect size d > 0.8 by far, so it is a strong effect we observed

## 7. Time to play around!
##	repeat the whole experiment you performed above with different sample size, mean and standard deviation  
##	repeat it 3 times changing all the values (sample size, mean, sd) and formulate the findings.  
##	what do you observe when we keep the means and sd same?

#1
tutor1_grades <- rnorm(30, 50, 3)
tutor2_grades <- rnorm(30, 35, 30)

score <- c(tutor1_grades, tutor2_grades)
tutor <- as.factor(c(rep("tutor1", 30), rep("tutor2", 30)))

data_frame <- data.frame(score, tutor)
independentSamplesTTest(data = data_frame, formula = score~tutor)
#The mean grade in the class of tutor1 is 50.787 (sd = 2.989) while the mean grade of tutor2's class
#is 33.371 (sd = 27.325). An independentSamplesTTest showed, that this ~17 points difference was
#significant (t(29.694) = 3.47, p = 0.002, CI_95 = [7.162, 27.669], d = 0.896), suggesting, that
#a difference in learning outcome has occurred


#2
tutor1_grades <- rnorm(5, 20, 3)
tutor2_grades <- rnorm(5, 20, 10)

score <- c(tutor1_grades, tutor2_grades)
tutor <- as.factor(c(rep("tutor1", 5), rep("tutor2", 5)))

data_frame <- data.frame(score, tutor)
independentSamplesTTest(data = data_frame, formula = score~tutor)
#The mean grade in the class of tutor1 is 18.584 (sd = 2.117) while the mean grade of tutor2's class
#is 23.453 (sd = 6.943). An independentSamplesTTest showed, that this ~5 points difference was not
#significant (t(4.737) = -1.5, p = 0.197, CI_95 = [-13.354, 3.617], d = 0.949), suggesting, that
#there is no difference in learning across the tutors.

#3
tutor1_grades <- rnorm(80, 20, 8)
tutor2_grades <- rnorm(80, 35, 15)

score <- c(tutor1_grades, tutor2_grades)
tutor <- as.factor(c(rep("tutor1", 80), rep("tutor2", 80)))

data_frame <- data.frame(score, tutor)
independentSamplesTTest(data = data_frame, formula = score~tutor)
#The mean grade in the class of tutor1 is 19.468 (sd = 8.037) while the mean grade of tutor2's class
#is 34.503 (sd = 13.955). An independentSamplesTTest showed, that this ~5 points difference was
#significant (t(126.211) = -8.351, p <.001, CI_95 = [-18.598, -11.472], d = 1.32), suggesting, that
#a difference in learning outcome has occurred


#When we only increase the sample size, the CI gets smaller, because we can because of the 
#Central limit theorem, but the effect size decreases
