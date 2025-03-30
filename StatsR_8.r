##########################
# Linear Mixed Effects Models
##########################


library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


#####################################################
### 1. Linear mixed model for chicken growth 
#####################################################

## a) We will first look at the dataset ChickWeight, which is already loaded in base R. Check out 
##  the help page of the data set to understand how the data was collected and look at the summary

help("ChickWeight")

summary(ChickWeight)

## b) Let's plot the data. We will first follow the strategy from sheet 4, i.e. 
##  1. group the data by Diet and Time and use summarise() to get the mean and se (se() as provided below)
##    of weight. Assign resulting data set to aggData
se = function(x){sd(x)/sqrt(length(x))}

aggData <- ChickWeight %>%
              group_by(Diet, Time) %>%
              summarise(g_mean = mean(weight), g_sd = se(weight))

##  2. Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##    Also add errorbars (mean+/-1.96*se)

ggplot(aggData, aes(x = Time, y = g_mean, color = Diet)) +
  geom_line() +
  geom_errorbar(aes(ymin = g_mean + 1.96 * g_sd, ymax = g_mean - 1.96* g_sd))

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##  by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##  instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##  actual data

ggplot(ChickWeight, aes(x = Time, y = weight, color = Chick)) +
  geom_line() +
  facet_grid(vars(Diet))


## d) What do you observe, looking at c?

# It looks like chicken gain weight faster (grow better) with diet 3 and 4 than with 1 and 2 


## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##  looking for an interaction between time after birth and the diet type. Before running the model,
##  specify:
##  1) What fixed effect(s) do you enter into the model?

## We have Time, diet type and their interaction as fixed effects

##  2) what random effect(s) should be included to account for the repeated measures structure of the data?

## We include random intercept for the Chick

##  3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?

## Since Diet type varies between Subject (Chick) and time varies within Subject (Chick) we include 
## random slope for Time

## f) Run the model you specified in e) using lmer() and assign it to chickmod

chickmod <- lmer(weight ~ Diet * Time + (1 + Time| Chick), data = ChickWeight)

## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull

chicknull <- lmer(weight ~ Diet + Time + (1 + Time| Chick), data = ChickWeight)

## h) compare the two models using the anova() function, which performs a likelihood ratio test

anova(chicknull, chickmod)

## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis

## p = 0.001217 -> significant. The interaction between Diet and Time improves the model fit

## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])

##???Sry I have no clue???
## Maybe intercepts and slopes vary much across chicken, so it is important to include the random variables

#####################################################
### 2. Random effect structures 
#####################################################

## a) Let's return to the lexdec data set and suppose, we want to look at effects of the word type of the 
## previously presented word (each subject saw a different randomized sequence) and effects of the complexity
## of the word itself, while taking into account the dependence between data points collected on the same 
## word and from the same subject. 
## Which of the following models has a maximal random effect structure given the experimental design?
## Motivate your choice.

m1 = lmer(RT ~ PrevType+ Complex+ (PrevType|Subject) + (Complex| Word), lexdec)
m2 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType| Word), lexdec)
m3 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType+Complex| Word), lexdec)
m4 = lmer(RT ~ PrevType+ Complex+ (Complex|Subject) + (PrevType| Word), lexdec)
m5 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (1| Word), lexdec)


## PrevType varies between items (One word is in only one group) and within Subjects
## (each subject performs both prevTypes) -> By subject random slope for PrevType

## Complex varies between items (one word has only one complexity) and within subject
## (each subject gets random complex words) -> By subject random slope for complex

##-> lmer(RT~ PrevType + Complex + (1 + PrevType + Complex| Subject) + (1| Word), lexdec)
##-> m5 is close, but none of them has a maximal random effect structure since the random intercept for subject
##  is missing
  
## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##  to their final math grade in school. Performance is measured as the overall score in the final exam.
##  The summer school course has 200 participants, coming from 8 different partner Universities from all
##  over Germany. These 200 participants were randomly split into 10 tutorial groups, where each tutorial 
##  was held by a different tutor.
##  Given the design of your study, what random effects should you add to the model below?
##  Explain!!! If you want to, you can additionally add the random effects into the formula

## We have 200 subjects, coming from 8 schools and 10 tutor groups 

## mathGrade varies between Subjects (every student has only 1 grade)
## mathGrade varies within schools (because a school has multiple grades from the students)
## --> by school random slope
## mathGrade varies within Tutors (because tutors have students with different grades)
## --> by tutor random slope

## lmer(advancedalgebrascore ~ mathGrade + (1|Subject) + (1+mathGrade|school) + (1+mathGrade|tutor), someData)

