##########################
#Correlation and Regression
##########################


library(languageR)
library(ggplot2)
library(dplyr)

#######################
### Exercise 1: Correlation
#######################

## a) We will again use lexdec from languageR. This week, we will look at the variables 
##  RT, FreqSingular, Trial, Frequency, and Length. Create a dataset called data with just
##  those variables

data <- select(lexdec, RT, FreqSingular, Trial, Frequency, Length)

## b) Take a look at the data frame.

head(data)

## c) Let's say you're interested in whether there is a linear relationship between 
## Frequency and Length 
## Take a look at the relationship between the frequency and length by 
## means of a scatterplot (use the ggplot library for this).

ggplot(data, aes(Frequency, Length)) +
  geom_point() +
  geom_smooth(method = "lm")

## d) Judging from the graph, do you think that word frequency and word length are 
## in any way correlated with one another?

## There is no relationship between the two variables (according to the plot),
##because the values are scattered across the whole space of the plot. 

## e) We are also interested in correlations between RT and all other variables in
## data. Get the correlations between all variables in the data set using cor().
## Tell R to only include complete pairs of observations. Is the correlation between
## Frequency and Length like you expected?

cor(data, use = "pairwise.complete.obs")

#The correlation (-0.429) is much higher than expected from the Scatterplot

## f) What is the range of a correlation, what is the meaning of a correlation of 0,
## 1, and -1 respectively?

## The range of correlation is [-1; 1] where -1 is a strong negative correlation,
## 1 is a strong positive correlation and 0 is no or negligible correlation
## Positive correlation means if we increase a we increase b and 
## negative correlation means if we increase a we decrease b or the other way around


## g) Going back to the correlation matrix obtained above, please describe the correlations
##  between RT and the other variables in words.

## If we increase RT, then FreqSingular, Trial and Frequency decrease, while the
## Length increases

## h) Is the correlation between RT and FreqSingular significant? Use cor.test()

cor.test(data$RT, data$FreqSingular)

#p-value = 0.00000001149 -> Very small value -> we reject the Null hypothesis and
#conclude, that there is a correlation not equal to zero (significant)

## i) Calculate the Spearman rank correlation between RT and FreqSingular. 
## What do you observe?

RT.rank <- rank(data$RT)
FreqSingular.rank <- rank(data$FreqSingular)

cor(FreqSingular.rank, RT.rank)

## -0.2287

## j) To get a better overview, we will create a plot between FreqSingular and 
## the mean RT per FreqSingular level.  
## Use group_by() and summarize() to obtain mean RTs by FreqSingular.
## Make a scatter plot between the two variables of the resulting data set.

summaryData <- data %>%
  group_by(FreqSingular) %>%
  summarise(mean_RT = mean(RT))


ggplot(summaryData, aes(FreqSingular, mean_RT)) +
  geom_point() +
  geom_smooth(method = "lm")

## k) Looking at the graph, why do you think Spearman's rho is better suited than the Pearson 
## correlation to describe the relationship between the two variables?

cor(data$FreqSingular, data$RT, method = "pearson")#-0.139
cor(data$FreqSingular, data$RT, method = "spearman")#like above -0.228

# If we increase FreqSingular, we decrease RT

## The graph indicates no linear relationship (more like exponential function) 
## and Spearman is better on non linear relations

## l) Calculate Kendall's tau for the same relationship. 

cor(data$RT, data$FreqSingular, method = "kendall")

# -0.1559

## m) Is there any reason to prefer this correlation measure in the current context? 
##  In general, in what contexts would you use Kendall's tau?

# In the current example we wouldn't prefer Kendall's tau (Since we have numeric data).
# But if we have ordinal data we should use it.

################################
### Exercise 2: Regression
################################


## a) Read in the data set lexicalDecision2.csv provided on cms and turn the variable Word 
##  into a factor. This data set is similar to the one used above in that it looks at lexical decision 
##  times for different words with the explanatory variables Frequency, Length and SynsetCount.

data <- read.csv("/lexicalDecision2.csv")

data$Word <- as.factor(data$Word)

head(data)

## b) First, we will investigate the relationship between meanRT and Length, which gives the length 
##  of the word in letters. Make a scatter plot of meanRT and Length (as always: ggplot). You can use
##  geom_jitter() to avoid overplotting

ggplot(data, aes(Length, meanRT)) +
  geom_jitter() +
  geom_smooth(method = "lm")

## c) Run a regression model with meanRT and Length and look at the summary.
## General form: 
## "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
## "summary(modelname)"

modelname <- lm(meanRT ~ Length, data = data)
summary(modelname)

## d) Interpret the model from c. What do intercept and the coefficient of Length tell you?

#The intercept is the estimated RTmean for a Length of 0
#The coefficient tells us how the RTmean changes (0.019 -> increases) per Length

## e) What about the model fit: What proportion of the total variance is explained by your model?

# R^2 = 0.1773 of the total variance can be explained by our model

## f) Now let's turn to the relationship between meanRT and Frequency. Run the regression and 
## interpret.

modelname2 <- lm(meanRT ~ Frequency, data = data)
summary(modelname2)
  
## g) Plot meanRT by Frequency and add a regression line to your plot

ggplot(data, aes(Frequency, meanRT)) +
  geom_jitter() +
  geom_smooth(method = "lm")


## h) Redo the plot, but instead of points, plot the Word value.
## Do you think there are any "bad" outliers, i.e. highly influential data points in your data set? 

ggplot(data, aes(Frequency, meanRT)) +
  geom_text(aes(label = Word)) +
  geom_smooth(method = "lm")

# The outlier egplant drags the regression line extremely down

## i) Rerun the model excluding the data point for the word "egplant". Compare the results.

data2 <- filter(data, Word != "egplant")

ggplot(data2, aes(Frequency, meanRT)) +
  geom_text(aes(label = Word)) +
  geom_smooth(method = "lm") +
  xlim(2,8)

# The regression line now is much higher on the left side (Around frequency 2),
# because the outlier is no longer there


## j) Given the difference between the two models and the peculiarities that you observe for this 
## data point, would you exclude this data point from further analysis?

# I would remove this point, since it doesn't make sene. Presenting a word 0 times
# can't measure a reaction time since it wasn't used

###################################
### Exercise 3: Multiple Regression
###################################

## We will use the same data set as in 2. This time, we will look at the effect of Frequency and Length
## on meanRT simultaneously. 

## a) Run a multiple regression model with Frequency and Length as predictors
## General form: 
## "modelname <- lm(outcome ~ predictor1+predictor2+.., data = dataFrame, na.action = an action)"
## "summary(modelname)"

multiModel <- lm(meanRT ~ Frequency + Length, data)
summary((multiModel))

## b) Interpret the model: what do intercept and the 2 coefficients tell you? What about significance?

#If we have 0 Frequency and Length we get a meanRT of 6.45 (Starting point of the regression line)
#The Frequency decreses the slope while the Length increases it

#?Significance?
# Sorry I don't understand what you mean with this. Could you briefly explain please?
  
## c) Compare to the model in 2c (only including Length), has the model fit improved? How about
## the model in 2f (only including Frequency)?

#2c model fit: 0.1773
#2f model fit: 0.2877
#3b model fit: 0.3328
  
#The model fit indeed increased over the tasks
  
## d) Using the model from 3 a: What is the predicted meanRT for the word "giraffe", which has a Frequency 
## of 3.33. Calculate "by hand", i.e. do not use predict() and show your calculation.

#Frequency = 3.33 Length(giraffe) = 7
#Estimated value = 6.452 + 3.33 * -0.0277 + 7 * 0.0108

value <- 6.452 + 3.33 * -0.0277 + 7 * 0.0108
#Our prediction would be 6.435

#Check:
predict(multiModel, data.frame(Frequency = 3.33, Length = 7))
#6.436 -> Nice
