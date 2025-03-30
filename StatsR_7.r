
#############################################################################
## Checking Assumptions underlying ANOVA and linear regression
#############################################################################


## The following line of code clears your workspace.

rm(list = ls())


#############################################################################
### Exercise 1
#############################################################################

#############################################################################
### Please, use ggplot to make plots in all exercises unless specified differently!
#############################################################################


##  We will again be using the lexdec dataset from library languageR. 
##  In sheet 4, we ran a t-test to test for differences in RT after a word vs after a non word.
##  In sheet 5, we looked at correlations between RT and frequency and length. In this sheet we will 
##  combine these analyses, look for interactions and again look at model assumptions
##  and model diagnostics

## a) Load the dataset lexdec from package languageR and store it in a variable called data

library(languageR)

data <- lexdec

## b) Run a simple regression, just including Frequency as predictor and RT as the dependent variable
##  Store it in lm1

lm1 <- lm(RT ~ Frequency, data = data)

## c) Report and explain the effect of Frequency

summary(lm1)

#Frequency has a negative effect on RT (if we increase Freq we decresase the RT)
#Both effects are significant

## d) Make a scatterplot of RT by Frequency, including the regression line

library(ggplot2)

#I used jitter instead of point to better visualize duplicate RT's
ggplot(data, aes(Frequency, RT)) +
  geom_jitter() +
  geom_smooth()

## e) Next, fit a model including Frequency and PrevType as predictors, store it in lm2

lm2 <- lm(RT ~ Frequency + PrevType, data = data)

## f) Report and explain the effects of Frequency and PrevType.

summary(lm2)

# Frequency and PrevType both have a negative effect on the RT
#Both effects are significant

##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
 data$RT_pred = fitted(lm2)

## g) Now, plot the original data (RT by Frequency with different colors for PrevType), but use the 
## fitted values (RT_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.

 ggplot(data, aes(Frequency, RT, color = PrevType)) +
   geom_point() +
   geom_smooth(aes(y = RT_pred) , method = "lm")
 
## h) Run a regression model that includes also the interaction between Frequency and PrevType and store it
##  as lm3

 lm3 <- lm(RT ~ Frequency * PrevType, data = data)
 
## i) Plot the results of the model! (This time no need to specify the pred data set)
  
 ggplot(data, aes(Frequency, RT, color = PrevType)) +
   geom_point() +
   geom_smooth(method = "lm")


## j) Report the results of lm3 and interpret with the help of the graph in i)

 summary(lm3)

# Frequency affects RT negatively. PrevType alone has no statistically significant effect on RT 
# The interaction between Frequency and PrevType has also a negative effect on RT

## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)

 par(mfcol = c(2,3))
 plot(lm3, which = seq(1,6))
 
 
## l) Interpret what you see in k) and possibly suggest further steps

## The points 1194, 1619 and 1187 stand out in every graph e.g. in the cook's
## distance. We should observe these values more precise, since those could
## be outliers, that have huge impact onto our data.
 
## Other things mentioned in m)

## m) So, what assumptions are violated in the model as it is? Consider both your results from l and what you know 
##  about the data set from previous analyses.
 
# It looks like the variance of the residuals is higher for larger fitted values
# -> Homogeneity is violated

# The normal Q-Q plot goes up on the right tail. I don't know if this is still
# okay or too bad -> Normality of residuals would be violated
 
# Bad outliers is violated (1194)
