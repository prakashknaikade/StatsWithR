
####################################################
# Model Selection, Transformations, Power
####################################################


# The following line of code clears your workspace.

rm(list = ls())


#########################################################################
### Exercise 1  Simplifying random effect structures
#########################################################################

library(lme4)
library(languageR)

##  Using the lexdec data set again, you want to fit the model that tests for effects of Frequency, the type of the 
##  previous Word and the native language of the participant:

m = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)

## a) Unfortunately, the maximal model given above gives a warning that indicates that the model is too complex for the data.
##  In order to get a model that converges without warnings, try to use backwards selection on the random effects. 
##  First exclude the random effect that is least contributing to the model fit and so on (this may require multiple 
##  steps and a large number of fitted models!). Use model comparison to decide which effects
##  can be excluded.
##  You may exclude random effects only, if they don't contribute significantly with alpha set to 0.1

m1 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (PrevType| Word), lexdec, REML=F)
m2 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (NativeLanguage| Word), lexdec, REML=F)
m3 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)
m4 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)

anova(m1, m)
anova(m2, m)
anova(m3, m)
anova(m4, m)

# The worst model fit is for m1 and m2 (Random slopes for Prevtype by Word and NativeLanguage by Word have no 
##effect on the model fit (Pr>Chisq) = 1) --> Since m1 does not converge we choose m2

m = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (NativeLanguage| Word), lexdec, REML=F)
m1 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (1| Word), lexdec, REML=F)
m2 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType|Subject) + (NativeLanguage| Word), lexdec, REML=F)
m3 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(Frequency|Subject) + (NativeLanguage| Word), lexdec, REML=F)

anova(m1, m)
anova(m2, m)
anova(m3, m)

## All random effects are statistically significant to the model fit --> Don't remove further

## b) Comment on your result in a): were you able to produce a suitable model without convergence problems?

# R doesn't give an error message so yes?

## c) Another approach is to simplify the random effect structure by excluding correlations. 
##  Try out whether this would have solved the problem.

m = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (PrevType + NativeLanguage| Word), lexdec, REML=F)
m1 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency||Subject) + (PrevType + NativeLanguage| Word), lexdec, REML=F)
m2 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (PrevType + NativeLanguage|| Word), lexdec, REML=F)

anova(m1, m)
anova(m2, m)

# Both models are significantly different from m and converge. m2 however has the lowest AIC value -> We choose m2

#########################################################################
### Exercise 2  Simulations and power
#########################################################################

## In the following we provide you with code for simulations. The goal of the exercise is for you to try out
## the code and understand what it does.
## Please always execute the code at least 5 times for each subquestion, to see how stable the results are 
##  -- this is necessary because we are sampling the data randomly, so it could be that we sometimes get more or 
##  less "lucky" draws. 


n <- 200 # number of observations to be simulated
predA <- rnorm(n, 80, 20)
predB <- rnorm (n, 30, 30)
interact <- 0.02*(predA*predB) 
error <- rnorm (n, 0, 50)

resp <- 32 + 0.02*predA - 2.4*predB + interact + error

d <- data.frame(predA, predB, resp)

## a) Write down what values you would hope for the model to estimate in the ideal case:
##   i)  intercept= 32
##   ii) predA= 0.02  
##   iii)predB= -2.4
##   iv) predA:predB = 0.02 

m1<- lm(resp~predA*predB, data=d)
summary(m1)  

## b) Can the model recover the original model structure and estimate correct coefficients 
##  for the predictors?

## First try: 
##  i)  intercept= 33.6
##   ii) predA= -0.03  
##   iii)predB= -1.9
##   iv) predA:predB = 0.016 

## Second try: 
##  i)  intercept= 2.6
##   ii) predA= 0.35  
##   iii)predB= -1.69
##   iv) predA:predB = 0.011 

#... I won't list all
# Some predictions were better and some wre not so good, but the overall result was pretty good predicting the model
# Only the second try is pretty bad for the intercept. In general it was good -> Model can recover the coefficients.

## c) What happens if you change the number of subjects? (specify the numbers you tried out)

#10
## We have a larger type 2 error because of the low sample size --> The predictions are really bad and the predictors have
## no significance according to the summary (Because of low power)

#2000
## Because we have a larger sample size, we are really close to the "true" values

## d) What happens if you change the variance of the error term? (specify the numbers you tried out)

#10
# Because we have a smaller variance, our predictions are better

#100 
# Because of the higher variance, our predictions are not as close

## e) What happens if you change the effect sizes?

## The type 2 error rate decreases -> The probability to find a significant effect is higher

##  Next we include the above code into a loop to calculate the power of the experiment 

# number of simulated data sets
sim = 1000
n = 100
# results matrix
results = matrix(nrow=sim, ncol=4)
colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for(i in c(1:sim)){
  predA <- rnorm(n, 80, 20)
  predB <- rnorm (n, 30, 30)
  interact <- 0.02*(predA*predB) 
  error <- rnorm (n, 0, 50)
  resp <- 32 + predA - 2.4*predB + interact + error
  d <- data.frame(predA, predB, resp)
  m1<- lm(resp~predA*predB, data=d)
  # store the resulting p-values in the results matrix
  results[i,] = summary(m1)$coefficients[,4]
}

## f. We use the above code and the results matrix to calculate power. Recall that the power is 
##  the probability of rejecting the Null hypothesis, given a specific effect size.
##  We can approximate this by calculating the proportion of simulated datasets, 
##  where the effect comes out significant, i.e. below 0.05. 
##  Calculate the power based on the simulations for all three effects of interest 
##  (i.e., predA, predB and the interaction) individually.

sign <- 0

for(i in c(1:sim)){
  if(results[i,4] < 0.05){
    sign <- sign + 1
  }
}
proportion <- sign/sim
proportion

#Power(predA) = 0.762
#Power(predB) = 0.885
#Power(interaction) = 0.616

## g. How does power change when you decrease your alpha level to 0.01?

sign <- 0

for(i in c(1:sim)){
  if(results[i,3] < 0.01){
    sign <- sign + 1
  }
}
proportion <- sign/sim
proportion

#Power(predA) = 0.518
#Power(predB) = 0.727
#Power(interaction) = 0.371

#The power decreased for each effect

## h. How does power change, when you decrease the number of participants in each simulated data 
##  set to 80? (alpha-level = 0.05)

sim = 1000
n = 90
# results matrix
results = matrix(nrow=sim, ncol=4)
colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for(i in c(1:sim)){
  predA <- rnorm(n, 80, 20)
  predB <- rnorm (n, 30, 30)
  interact <- 0.02*(predA*predB) 
  error <- rnorm (n, 0, 50)
  resp <- 32 + predA - 2.4*predB + interact + error
  d <- data.frame(predA, predB, resp)
  m1<- lm(resp~predA*predB, data=d)
  # store the resulting p-values in the results matrix
  results[i,] = summary(m1)$coefficients[,4]
}

sign <- 0

for(i in c(1:sim)){
  if(results[i,4] < 0.05){
    sign <- sign + 1
  }
}
proportion <- sign/sim
proportion


#Power(predA) = 0.723
#Power(predB) = 0.851
#Power(interaction) = 0.547

# The power decreased, because of the smaller sample size

