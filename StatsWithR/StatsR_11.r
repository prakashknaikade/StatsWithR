
##########################
# Bayesian statistics 2
##########################


# The following line of code clears your workspace.

rm(list = ls())


########################################
### Exercise 1
########################################


##  We will again be using the lexdec dataset from library languageR. 
##  In sheet 6, we ran a multiple regression model, which we will now repeat as a Bayesian 
##  analysis using the package brms.

## a) Load the dataset lexdec from package languageR and store it in a variable called data

library(languageR)

data <- lexdec

## b) Load the package brms

library(brms)

## c) Fit a (frequentist) linear model of RT including Frequency and PrevType as predictors, store it in lm1

lm1 <- lm(RT ~ Frequency + PrevType, data = data)

## d) Fit the same model as a Bayesian regression using the function brm() and using only defaults (you don't need
##  to specify priors or fitting parameters like chains and iterations). Store it in bm1

bm1 <- brm(RT ~ Frequency + PrevType, data = data)

## e) Look at the summaries of bm1 and lm1

summary(lm1)
summary(bm1)

## f) How do the parameter estimates compare?

## The estimates are pretty similar, the bayesian model rounds the values to 2 decimals:
## Intercept: lm1 -> 6.621, bm1 -> 6.62
## Frequqncy: lm1 -> -0.043 bm1 -> -0.04
## PrevType: lm1 -> -0.065 bm1 -> -0.07

## g) store the posterior samples of b_Frequency in the variable ps_freq. Use the function as_draws_df()

ps_freq <- as_draws_df(bm1)$b_Frequency

## h) Your colleague claims that the effect of frequency has to be smaller (meaning more negative) than -0.03.
##  What is the probability of the frequency effect being more negative than -0.03 given your posterior samples?
##  Do you agree with your colleague?

sum(ps_freq < -0.03)/length(ps_freq)

# p = 0.9975
# The probability of the frequency to be below -0.03 is approx. 100%, so we agree with our colleagues

## i) Derive 95% and 80% credible intervals from ps_freq. Compare to the results above.

quantile(ps_freq, c(0.025, 0.975))
quantile(ps_freq, c(0.1, 0.9))

## It fits our prediction, that the mean effect of frequency is smaller than -0.03, since the CI's both have 
## a maximum lower than -0.03

## j) What is the meaning of a credible interval compared to the confidence interval in the frequentist's approach?

# Confidence interval express the probability, that the interval contains the true value/mean
# Credible intervals express the probability, that the true value falls within this range

## k) Plot the model using the default function, this will give you the posteriors of the model parameters
##   as well as the trace plot, which give you an indication of the convergence of your model. The trace 
##   plot is supposed to look like a "fat hairy caterpillar", i.e. the different chains should not be 
##   separated in any part of the plot and there should not be a general pattern. Is this the case?

plot(bm1)

# Yes, it is the case. The chains don't separate

## l) We want the model to run quicker. Change the settings such that each chain only has 180 iterations with 1/4 of
# them as warmup. Store the result in bm2 and look at summary and trace plots. Use the provided seed to be able to 
# better compare your results (or try a different one, but provide it together with your answer!)
set.seed(1111)

bm2 <- brm(RT ~ Frequency + PrevType, data = data, iter = 180, warmup = 45)

summary(bm2)
plot(bm2)

# The model gives no summary, but an error, that some chains did not converge

## m) Do you think reducing the iterations was a good idea? Give reasons!

## Since we have convergence problems it was a bad idea to reduce the iterations

## n) Another colleague of yours said 2 months ago to you that the effect of frequency is most likely at -0.01 +-0.005
##  Use these numbers for a normal prior of Frequency (with 0.005 as sd). Assign the model to bm3. 

newPrior <- c(prior(normal(-0.01, 0.005)))
bm3 <- brm(RT ~ Frequency + PrevType, data = data, prior = newPrior)


## o) How did the estimate and credible interval of frequency change?

summary(bm3)

# The estimates for intercept (6.53), Frequency (-0.03) and PrevType (-0.02) changed.
# The credible Intervals also got tighter, because of our informative prior.

## p) What class of priors does the above one belong to? 
# Principled prior
