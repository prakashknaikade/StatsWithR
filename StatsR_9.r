####################################################
## Model Families and Logistic Regression
####################################################


##################################################################################
## Exercise 1: Logistic regression
##################################################################################

require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.

summary(TitanicSurvival)

## a) Build a simple logistic regression model that models the probability of survival (binary) based on 
##  sex (categorical) and passengerClass (categorical) without an interaction and store it in mSurv. 
##  You have to use the glm() function and specify the family correctly.

mSurv <- glm(survived ~ sex + passengerClass, family = binomial, data = TitanicSurvival)

## b) Look at the summary. What group does the intercept correspond to?

summary(mSurv)

## c) Were men more likely to survive than women? Is the effect significant?

# men were less likely to survive than women (Estimate -2.515) 
# This effect is significant (z(1308) = -17.1, p < 0.001)

## Should we take df = 1308 (null deviance) or 1305 (residual deviance)?

## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##  Calculate their expected survival on the logit scale (i.e. the scale of the model) either by hand or 
##  using predict() with a new data.frame

## logit = b_0 + b_1 * x_1 + b_2 * x_2 + b_3 * x_3 = 2.1 - 2.52 * men - 0.88 * 2ndClass - 1.72 * 3rdClass

## Rose = 2.1 - 0 - 0 - 0 = 2.1
## Jack = 2.1 - 2.52 - 1.72 = -2.14

## e) Transform your results from d) to the probability scale, using the formula given on the slides. 
##  You can check your calculation by asserting the probabilities lie in the 0-1 range. For whom does 
##  the model predict the higher probability of survival?

## p(survival Rose) = e^2.1 / 1 + e^2.1 = 0.89 = 89%
## p(survival Jack) = e^-2.14 / 1 + e^-2.14 = 0.1 = 10%

##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption and sleep (among others). 
## The data set "coffee.csv" contains data from 10 students, who reported on 10 randomly chosen days of the year: 
##  sleep: how many hours of sleep they had in the previous night
##  mood: how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
##  In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they are in a bad mood.
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat

coffeedat <- read.csv("/coffee.csv")

## b) Plot the number of consumed cups of coffee in three individual scatterplots by sleep, mood, and temperature. 
##  You can use geom_jitter() to get a nicer plot

ggplot(coffeedat, aes(sleep, coffee)) +
  geom_jitter() +
  geom_smooth(method = "lm")

ggplot(coffeedat, aes(mood, coffee)) +
  geom_jitter() +
  geom_smooth(method = "lm")

ggplot(coffeedat, aes(temperature, coffee)) +
  geom_jitter() + 
  geom_smooth(method = "lm")


## c) Can you detect an obvious relationship in any of the plots? Which direction does it have?

## Negative relationship between coffee consumption and hours slept
## Negative relationship between coffee consumption and mood

## (People don't drink much coffee if its cold)

## Correlation does not give us causality. But from knowledge I would assume, that 
## Low sleep -> more coffee
## Low mood/ grumpy -> more coffee

## d) fit a simple linear regression model with all three predictors and store it in linmod

linmod <- lm(coffee ~ sleep + mood + temperature, data = coffeedat)

## e) fit a generalized linear model with the appropriate family (hint: coffee is a count variable) and
##  store it in poimod

poimod <- glm(coffee ~ sleep + mood + temperature, family = poisson, data = coffeedat)

## f) Look at the two summaries, what changed?

summary(linmod)

## only mood has a significant negative effect on coffee consumption
## t(96) = -2,569, p < 0.05 
## the other predictors are not significant

summary(poimod)
## mood has a significant negative effect on coffee consumption
## t(96) = -3.640, p < 0.001
## sleep has a significant negative effect on coffee consumption
## t(96) = -2.789, p < 0.01
## mood has a significant positive effect on coffee consumption
## t(96) = 2.233, p < 0.05

## The significance of the predictor effects increased

## g) In fact, we have repeated measures in our design, so refit the model including a random intercept for
##  subject using glmer() with the correct family specification and store it in mixedpoi

mixedpoi <- glmer(coffee ~ sleep + mood + temperature + (1 |subj), family = poisson, data = coffeedat)

## h) Look at the summary and report what changed in comparison to both linmod and poimod.

summary(mixedpoi)

## Temperature has no longer a significant effect on coffee consumption 
## How do I report this correct, where are the df in summary?

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin

mixedlin <- glmer(coffee ~ sleep + mood + temperature + (1 |subj), family = gaussian, data = coffeedat)

## j) Compare the AIC for all four models. Which one has the best fit?

summary(mixedlin)

## There is no AIC for normal linear models only for general linear models

## poimod: AIC = 510.16
## mixedpoi: AIC = 478.7

## mixedlin can't find AIC

## 478.7 < 510.16 -> mixedpoi fits better

## k) And which model is conceptually the appropriate one? Why?

# Since our DV is a count variable (amount of coffee) I would choose the generalized mixed effect model
# with the poisson family 

## l) Finally, report on the effects of interest in light of our research hypotheses specified above for the 
##  model you chose in k)

summary(mixedpoi)

## mood has a significant negative effect on coffee consumption
## t(96) = -4.014, p < 0.001
## sleep has a significant negative effect on coffee consumption
## t(96) = -4.014, p < 0.01
## mood has a no significant effect on coffee consumption
## t(96) = 1.242, p > 0.2
