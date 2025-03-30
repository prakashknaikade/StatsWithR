##########################
# ANOVA
##########################


#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(dplyr)
library(car)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

## a) For the further reference please use ?amis. 
## It may take some time to understand the dataset. 

?amis

## b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
## Feel free to make some plots and calculate some statistics in order to understand 
## the data.

data <- amis

## c) All our columns have numeric type. Convert the categorical columns to factors.

data$period <- as.factor(data$period)
levels(data$period) <- c("Before", "Short", "Long")
data$warning <- as.factor(data$warning)
levels(data$warning) <- c("Yes", "No")
data$pair <- as.factor(data$pair)

## d) Plot boxplots for the distribution of `speed` for each of the `period` values 
## (before, immediately after and after some time). Build 2 plots (each containing 3 
## boxplots) side by side depending on the `warning` variable.
## (For all plots here and below please use ggplot)

ggplot(data = data, aes(speed)) +
  geom_boxplot() +
  facet_grid(cols = vars(period), rows = vars(warning))

## e) What can you conclude looking at the plots? What can you say about people's 
## behaviour in different periods: before, immediately after and after some time?

## According to the plot people tended to slow down shortly after the warning sign was erected,
## and after a longer period, it goes back to the fast speed of the before group.
## You could interpret it that way, that the warning sign makes people to drive more safely,
## because there might be roadworks etc., but after they know there is nothing dangerous,
## they will fall back to their old driving habit

## f) What are your ideas about why the data with warning==2 (sites where no sign was 
## erected) was collected?

# Because now we have a baseline to conclude whether the warning sign has an impact
# on the drivers behaviour. We can also say, that the changes in driving speed are not normal,
# since it doesn't occur in the NO group

#######################
### Exercise 2: 1-way ANOVA
#######################

## a) First let's create a new data frame which will be used for all exercise 2.
## For the 1-way ANOVA we will be working with a subset of `amis` using only the 
## data for sites where warning signs were erected, which corresponds to warning==1. 
## Therefore first subset your data to filter out warning==2 and then apply group_by() and summarize() 
## to average "speed" over each "pair" and "period". 
## Assign this new data frame to the variable casted_data.

data <- filter(data, warning == "Yes")
data <- subset(data, select = -c(warning))

casted_data <- data %>%
                group_by(pair, period) %>%
                  summarise(avg_speed = mean(speed))

## b) Build boxplots of the average speed depending on "period".

ggplot(data = casted_data, aes(avg_speed)) +
  geom_boxplot() +
  facet_grid(cols = vars(period))


## c) Looking at the boxplots, is there a difference between the periods?

## The boxplots indicate, that there is a difference between the periods
## Same explanation as above. People that only saw the sign shortly after drove 
## slower, in the long period, they speed up again

## d) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
## speed depending on the period and assign the result to aov1way

aov1way <- aov(avg_speed ~ period, data = casted_data)

summary(aov1way)

## Before we interpret the results, let's check the ANOVA assumptions and whether 
## they are violated or not and why.

## e) Independence assumption
## (Figure out the best way to check this assumption and give a detailed justified 
## answer to whether it is violated or not.)

# We would need to check that there are no repeated measurements. That means that 
# we didn't track the speed of a driver multiple times in the same period. Because 
# they stated in the description that: 
#"Each set of measurements was nominally of the speeds of 100 cars" this assumption
# is true


## f) Normality of residuals
##  First add the residuals to your casted data set, you find them in model$residuals
##  next, make a qqplot (using qqnorm() or geom_qq() ina ggplot) for the residuals and 
##  run the shapiro wilk test.

casted_data$residuals <- aov1way$residuals

ggplot(casted_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()

shapiro.test(casted_data$residuals)

#p = 0.1662

## g) What do you conclude from your results in f?

# Because 0.1662 > 0.05 we accept H0 and say that the residuals are normally distributed

## h) Homogeneity of variance of residuals
##  First, plot the residuals by period (boxplots) to see whether variance differs between groups
##  Next, run Levene's test using the function leveneTest() (from library car) with the same syntax
##  as aov(). It indicates whether the variance is significantly different between groups (= not
##  homogeneous).

ggplot(casted_data, aes(residuals)) +
  geom_boxplot() +
  facet_grid(vars(period))

leveneTest(avg_speed ~ period, data = casted_data)
# p = 0.8383

## i) What do you conclude from your results in h?

# We accept H0 and conclude that Homogeneity is fulfilled

## j) Now we turn to the results. Look at the summary of aov1way

summary(aov1way)

## k) State your conclusion

# Since our p = 0.382 we accept our H0
# -> There is no difference in means between the period groups

## l) Please do pairwise t-tests of the same variables as in d) using pairwise.t.test().

pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method = "none")

## m) Try to use no adjustment for pairwise testing and then the Bonferroni correction.

pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method = "bonferroni")

## n) If the results change  in m, why do they? What does Bonferroni correction do?

## Yes the p values increase, since Bonferroni correction leads to a more conservative
## result. Since our period has 3 levels we multiply all p values by 3 (clamped by 1)

#######################
### Exercise 3: 2-way ANOVA
#######################
## a) Now we want to analyze the influence of 2 categorical variables 
## (period and warning) on the speed.
## So let's turn back to our initial dataset amis (not its subset with warning==1).
## First, we need to average the speed over each `pair`, `warning` and `period
## Cast your data again and assign the results to casted_data2.

casted_data2 <- amis %>%
                    group_by(pair, warning, period) %>%
                    summarise(avg_speed = mean(speed))

## b) State the main difference between the applicability of 1-way and 2-way ANOVA.

## In the 1 way anova we only have 1 independent variable while in the 2-way ANOVA we
## have multiple independent variables and also evaluate the interaction between them

## c) Do you think, we need to include an interaction term in the ANOVA?

## I wouldn't say we need an interaction term in the ANOVA since there should be no
## correlation between period and warning 

## e) Now apply the 2-way ANOVA: please use the function aov() with mean speed as the
## dependent variable, period and warning as predictor (independent) variables and depending on your
## answer in c) either including an interaction term, or not.

aov2way <- aov(avg_speed ~ period * warning, data = casted_data2)
summary(aov2way)

## f) Report the p-values and interpret the results in detail. Properly formulate the findings
##  with regard to the research question!

#period p: 0.183
#warning p: 0.0046
#interaction: 0.823

#We find a significant main effect of warning (F (1) = 8.491,p < .0005) but no effect for
#period (F (1) = 1.8,p = 0.183), and there is no significant
#interaction between the two (F (1) = 0.05,p = 0.823)
