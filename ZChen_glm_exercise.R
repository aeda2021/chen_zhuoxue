## Goal of this assignment
# To learn how to run generalized linear models using glm()

## How to complete this assignment
# read through the comments and run the code below
# whenever you see a comment line that begins with Q, discuss that question with your partner (you do not need to write the answer out)
# if the Q question requires you to write code, do so
# when you are finished, push your completed code to your Github repo at  https://github.com/aeda2021/

## Outline
# some glm basics
# poisson regression
# logistic regression

rm(list = ls()) 
# load libraries
library(titanic)
library(tidyverse)
library(ggfortify) # for autoplot
library(MuMIn) # for model selection
library(car)  # for getting variance inflation factor
library(broom) 	# for augment

### some glm basics

## When might you need glm?
# pretty often actually! 
# because ecologists do a lot of counting, and counts are bounded at zero
# counts are often poisson distributed
# for example, this could represent counts of plants in 500 quadrats, where the mean number of plants per quadrat is 3
hist(rpois(500,3)) 
# Q: why isn't this distribution normal? ie, why can't it be normal?
# another way of stating this problem, which again ecologists frequently run into, is that if zero is one of your more frequent data values, your data cannot be normal
# on the other hand, when the mean of the distribution (e.g., the mean number of plants per quadrat) is large enough, the distribution moves to the right away from the Y axis
# in this case the normal assumption might be fine
hist(rpois(500, 20))
# so you just have to look at your data to decide
# the binomial distribution behaves similarly, wrt moving out away from the Y axis when the mean is larger
# Q: write code to check that statement, analogous to what I wrote above
# hint: try using the function rbinom
hist(rbinom(500,3,0.2))
hist(rbinom(500,50,0.2))
# Q: what is plotted on the x axis exactly, in a histogram from rbinom?

## A quick review of GLM COMPONENTS
# the FAMILY is also called the error distribution
# you choose it based on how you expect the residuals of the model to be distributed
# a practical way to choose a 'family' is based on the type of data you collected
# binary data will follow a binomial distribution
# counts often follow a poisson distribution, unless the mean is >15 or so
# Q: type 'family' in the Help pane search bar to see the family options for glm
# families you might use at some point include:
# negative binomial, which is for count data that have too much variability for the poisson
# gamma, which is for positive numbers that aren't integers (ie, that have decimal places)
# the LINEAR PREDICTOR is just the a + bx stuff - the intercept and slope terms that you wrote for your model
# in glm, the linear predictor is related to the outcome variable via the link function
# the LINK FUNCTION
# this the confusing part
# the role of the link function is to move the outcome variable onto a different scale such that it is no longer bounded
# for example, to move it from a range of 0 to 1, to a range from negative infinity to positive infinity
# the link function is not actually transforming the Y data values themselves
# it is only modeling a mathematical transformation of the expected values of Y
# if that distinction is not clear to you, you are not alone, so don't worry about it too much
# in practice, you rarely have to specify the link function
# R will use the default link for the family you choose
# Q: type 'family' in the Help pane search bar to see the default links for each family 
# remember the link function for poisson is the natural log, base e, which is what R means by 'log'


### Poisson Regression
# based on an exercise by Andrew Beckerman and Owen Petchey, Getting Started with R (2017)
# get some data on soay sheep and look at it in spreadsheet mode
soay <- read.csv("soaysheepfitness.csv")
# each row is an individual ewe in a feral population on a Scottish island 
# 'fitness' is lifetime number of offspring
# 'body.size' is mass in kg
# Our GLM question: do bigger females have more offspring?

# First, a data picture
# Q: write code below to plot the data
# hint: you can use geom_smooth to get a loess ie flexible ie somewhat wiggly fit line
# this can be useful in eyeballing datasets to see whether a linear fit makes sense
ggplot(data = soay, aes(body.size, fitness))+
  geom_smooth()



# if you want the fit line to be less wiggly, change its 'span'
# geom_smooth(span = 1, color = red, se = FALSE)
# so, this looks like a case where it would be a mistake to use lm()
# but let's just try it to see what happens

# Second, run model and check model assumptions
# Q: write code below to run the lm model and look at the diagnostics, including a histogram of the residuals

s_mod <- lm(fitness~body.size,data = soay)
plot(s_mod)
hist(s_mod$residuals)
summary(s_mod)


# Q: do these data meet lm assumptions?
# hmm, we have counts with smallish numbers
# maybe a poisson would be a better choice
# important note: looking at the distribution of the outcome variable is NOT the same as looking at the residuals
# that said, it can give you a first-order sense of what you are dealing with
hist(soay$fitness)
mean(soay$fitness)
# Q: why might we be in poisson glm territory here?
# okay, let's do our first glm!
soay_mod1 <- glm(fitness ~ body.size, data = soay, family = poisson)
# bet that was easier than you expected
# remember if you don't specify a link, R uses the default, which is ln(Y) in this case
# look at diagnostic plots 
autoplot(soay_mod1)
# these look pretty good, much better than for the linear model
# note the Y axes are now 'deviance residuals' which is what they are called in glm but why
# WAIT! wasn't the whole point that we don't have normally distributed data?
# and also, based on our data picture, the variance increases with the mean?
# so these plots should NOT look good, and yet they do!
# here's a secret: autoplot handles glm output just fine
# you can evaluate the plots the same way as for lm()
# if the family you chose is a good fit for your data, the autoplot plots will look good
# you can also use AIC to compare your linear model to the poisson model
# this is legitimate so long as you use exactly the same predictor variables and data set
AIC(soay_mod1)
# Q: write code below to get the AIC of the linear model you ran above, and discuss the results with your partner

AIC(s_mod)


# Third, interpret results
# just like for lm, you can get either anova-type output or regression-type output for glm
# remember anova() is messed up wrt the order in which you enter your predictors mattering, but, since we only have one predictor here it's okay
anova(soay_mod1)
# in glm, the residuals you get are called deviance residuals
# they are calculated differently from sum of squares in lm
# but you can interpret them roughly similarly, as in, how much variability in the data is explained by the model
# here, the deviance shows that a bit less than half (37/85) of the variability in ewe fitness is accounted for by ewe body size
# but, where is our p-value???
# don't panic!
# you can get a p-value but you need to tell R what type of test to use to get it
# in general, it's good for glm to calculate the p value based on the chi square distribution, so use that
# note, this does not mean we are doing a chi square test
anova(soay_mod1, test = "Chisq")
# a chi square value of 37 with one df is highly significant
# as we expected, given our data picture
# now we can get more info from the regression-type output
# Q: write code to get this output below
summary(soay_mod1)


# let's step through the output
# first, we get some fairly useless information about deviance residuals
# then, we get the coefficients - intercept and slope
# note the p-values are now associated with z tests, not t tests; thus df aren't relevant
# next, the dispersion parameter: this can be important for some families, but the poisson doesn't have a dispersion parameter, so ignore for now
# next, summaries of the deviance, similar to the anova output
# the null deviance is a measure of all the variation in the data (it's not exactly this but can be interpreted like this)
# the residual deviance is what's left after fitting the model
# thus the variability explained by the model is  1-(residual/null)
# the bigger this proportion, the better the model, and the smaller the p-value
# last, the AIC
# as usual, we can use this to compare among models containing different predictor variables, so long as they are run on the same dataset
# we can also use AIC to compare among models using different families (eg, poisson versus negative binomial), so long as the linear predictor and the dataset is the same
# remember AIC is better for comparing among models than likelihood, because AIC penalizes for additional parameters
# lastly, there is the number of Fisher scoring iterations, but that is R's problem, not ours (it just has to do with how hard R had to work to fit the model)
# now that we are oriented, let's go back and interpret our results wrt our data picture
# intercept is -2.42 lambs, slope is 0.54 lambs per kg of mom
# hmm. intercept seems odd
# but then a 0 kg mom would also be odd
# let's take another look at the graph, this time with expanded axes, to make sure we're not missing something
ggplot(soay, aes(x=body.size, y=fitness)) +
  geom_point() +
  geom_smooth(span = 1,  se = FALSE) +
  xlim(0, 10) +
  ylim(-5, 15)
# oops. our model coefficients don't seem to be in the same universe as our data
# just as a check in case our vision is off today:
# a 5 kg mom should have -2.42 + 0.54 * 5 = 0.28 lambs
# and an 8 kg mom should have -2.42 + 0.54 * 8 = 1.9 lambs
# nope, it's no good
# time to take a break and check twitter
# but when you're back, know this:  R forgot to tell us something !!
# R does NOT back-transform the coefficients for us from 'the scale of the link function'
# so the coefficients refer to the model that was actually fit, which includes the link
# remember in a poisson glm, ln(Y) = a + bx, or equivalently Y = e^(a + bx)
# no wonder our coefficients are small - they are in the exponent!
# so let's try our test calculation again, this time remembering that the model is predicting ln(Y) instead of plain Y
# for an 8 kg mom, ln(Y) = 1.9, or Y = 2.718 ^ 1.9 = 6.7 lambs
# that looks a lot more reasonable
# so then how do you report glm coefficients, if they are on the link scale?
# one way is to use the reported 0.54 slope for body mass, and say that log(fitness) increases by 0.54 with every kg of mom's weight
# that would be reporting the results on the link scale, which again, is ln(Y) = a + bx
# alternatively, you could report the results on the original scale of the data
# this makes more sense to most people, but requires some effort
# first, know that the slope of Y = e^(a + bx) is equal to e^b
# why that should be is not immediately obvious, but a bit of algebra shows that it is true
# thus, the change in Y for one unit change in x = slope = e^b = e^(0.54) = 1.718
# in words, for every 1 kg increase in body mass, the number of lambs increases by a factor of 1.718 times
# wait what - by a FACTOR?  
# don't leave for a study break! I will explain!
# because the slope is now an exponential (namely, e^b), we have a nonlinear relationship between x and y
# thus, the absolute increase in y per unit x is larger for larger values of y
# more specifically: for every 1-unit change in x, y(new) = 1.718 * y(old)
# this is different from what we are used to thinking about with linear fits, where y increases by a constant absolute amount for each unit change in x (e.g., y=2x always changes by 2 per unit change in x, regardless of where you are on the graph)
# okay, now that we mastered the slope, let's return to the intercept
# the intercept would be the number of lambs for a ewe with a body size of 0
# this is exp(-2.42) = 0.089
# no way to reality-check that one really but at least it isn't negative

# Fourth, plot results back onto data picture
# we can use augment to add the model prediction on the original scale of the data (rather than the link scale that the model runs on)
# augment calls this option 'response,' as in, type of prediction wanted is on the scale of the original response variable
soay_mod1_aug = augment(soay_mod1, data=soay, type.predict="response", se_fit=T)  
# if you took a look at that output and wondered why the deviance residuals (remember this is the relevant type for glm) aren't equal to (fitted - observed), that's because they aren't
# instead, deviance residuals are related (somehow) to the relative contribution of this data point to the total deviance
# moving on to plotting the fitted values on the original data scale
ggplot(data = soay_mod1_aug, aes(x = body.size, y = fitness)) + 
  geom_point() + 
  geom_line(aes(x = body.size, y = .fitted)) +
  theme_bw()
# that wasn't difficult!
# unfortunately adding the CIs to the plot is more of a pain
# we need to first get the standard errors on the link scale, and then back-transform them to the original data scale
# a key thing to recognize here is that the se should be symmetrical on the link scale, but when we back transform them they will be asymmetrical on the original data scale
# that's because of the nonlinear relationship between x and y
# we start by getting the se from augment on the link scale
soay_mod1_aug_link = augment(soay_mod1, data=soay, type.predict="link", se_fit=T)  # note we changed type.predict to link here
soay_mod1_aug_link
# then calculate CIs using 1.96*se. this is all still on the link scale
soay_mod1_aug_link <- mutate(soay_mod1_aug_link, ci_lwr = .fitted - (1.96 * .se.fit), ci_upr = .fitted + (1.96 * .se.fit))
soay_mod1_aug_link
# then backtransform the CIs using exp() to get to the response scale
soay_mod1_aug <-mutate(soay_mod1_aug_link, .fitted = exp(.fitted), ci_lwr = exp(ci_lwr), ci_upr = exp(ci_upr))  # notice we are being bold here and overwriting our previous values
soay_mod1_aug
# now we can plot our data, glm fit, and CI all together!
ggplot(data = soay_mod1_aug, aes(x = body.size, y = fitness)) + 
  geom_point() + 
  geom_line(aes(x = body.size, y = .fitted)) +
  theme_bw() +
  geom_ribbon(data = soay_mod1_aug, aes(ymin = ci_lwr, ymax = ci_upr), alpha = 0.1) 

# optional, for those who wonder why we can't just plot the CI based on what R returns for augment on the scale of the response
soay_mod1_aug_response = augment(soay_mod1, data=soay, type.predict="response", se_fit=T) # reminder of what we already did above, to get the fitted line - but now we name the object different so that we don't get totally messed up
soay_mod1_aug_response
# we do get a .se.fit here, so why can't we use it?
# the reason is that, although this is indeed the se on the scale of the response, 1.96 * this value will not be equal to the 95% CI on the scale of the response
# that's because the analysis was done on the scale of the link
# let's plot the (incorrect) CI based on the response scale se anyway, just to see how it looks different
soay_mod1_aug_response <- mutate(soay_mod1_aug_response, ci_lwr = .fitted - (1.96 * .se.fit), ci_upr = .fitted + (1.96 * .se.fit))
soay_mod1_aug_response
ggplot(data = soay_mod1_aug_response, aes(x = body.size, y = fitness)) + 
  geom_point() + 
  geom_line(aes(x = body.size, y = .fitted)) +
  theme_bw() +
  geom_ribbon(data = soay_mod1_aug_response, aes(ymin = ci_lwr, ymax = ci_upr), alpha = 0.1) +
  ggtitle('this is the wrong kind of CI')
# so actually, in this case it doesn't look that different
# which is sort of pedagogically disappointing
# but it definitely can be really different sometimes
# and if you look at the actual values in the tables here, they are slightly different



### Logistic Regression
# logistic regression is what we use when our outcome variable is constrained to 0,1, and thus follows a binomoial distribution
# family for logistic regression is binomial
# link function is the log odds:  log(p/(1-p)), where p is the probability of success in our bernoulli trials
# we are going to return to the titanic dataset that you met in the class on maximum likelihood
# and now step through it more slowly to do a logistic regression analysis
# our logistic regression question is:
# Which factors predict which passengers survived the sinking of the titanic?

## Get the data and take a look at it
data(titanic_train)  # just fyi, the _train part is there because this is a 'training' data set assembled by someone for machine learning purposes; it's irrelevant to what we are doing
# see ?titanic_train for explanations of variables
# we plan to use the following columns:  the outcome Survived,  and three predictors, Pclass, Sex, Fare
titanic <- select(titanic_train, Pclass, Sex, Fare, Survived)
# take a look at the data using the spreadsheet button
# now that we have a dataset that is too big to check visually, we need R's help to check the data
# let's check for NAs first
# remember when R reads in a csv, it will put an NA in any empty cell in a numeric column
# it won't do that for character variables, though, so those are more tedious to check
summary(titanic)  # no NAs, good
# are our variable types what we want them to be?
glimpse(titanic)
#  the Sex column is just character data, so there could be anything in there including "" values - let's check
unique(titanic$Sex)
# looks good; let's make factor data though, since that's how we are going to use it
titanic$Sex <- as.factor(titanic$Sex)
# let's do a few tables, histograms or other checks on the variables we plan to use
# you should always do this sort of thing at the start of an analysis to get a sense of what the data look like, and whether there are outliers, or likely erroneous values
hist(titanic$Fare, breaks = 20)   
# this one has outliers - three people paid $500 for a ticket (all others <$300)
# at least they Survived!



# Q: discuss your findings with your partner


# First, data pictures
# these pictures are now wrt the outcome variable, survival. we will need to do these one at a time
# here is Survived versus Fare
ggplot(titanic, aes(x=Fare, y=Survived)) +
  geom_point() +
  geom_jitter(height=0.05, alpha = 0.1)  # jittering the points and using a lighter fill are essential for viewing large binomial datasets
# hmm, too many data points to eyeball binary data effectively. that can happen sometimes
# now we get to the categorical variables with binary outcomes
# here is one way to look at such data
ggplot(data = titanic,aes(x=Sex,y=Survived, color=Sex))+geom_jitter()
#Q:  what are you actually comparing here?
# btw women survived better because the men put the women in the lifeboats first
# just in case you were wondering
# Q: write code below to look at Pclass in a similar way to the above
ggplot(data = titanic,aes(x=Pclass,y=Survived, color=Pclass))+geom_jitter()
	
# you could also just get the numeric answer as a check on later stats tests
by_sex <- titanic %>%
  group_by(Sex) %>%
  summarize(meanSurv = (sum(Survived)/n()))
by_sex
# Q: explain to your partner what the code above just did
# Q: write code below to summarize the data for Pclass, with respect to survival
by_Pclass <- titanic %>%
  group_by(Pclass) %>%
  summarize(meanSurv = (sum(Survived)/n()))
by_Pclass


# Q: Tell your partner what you expect the results of the logistic regression analysis to be, based on the data pictures
# the data picture stage is also a good time to think about possible relationships among your variables and ask yourself whether you really want to include all of them
# for example, wouldn't we expect Pclass to convey the information in Fare?
ggplot(data = titanic,aes(x=Pclass,y=Fare, color=Pclass))+geom_jitter()
# looks like it kinda does
# more importantly, why do we have an expectation about Fare per se, other than how it acts through Pclass?
# the ship staff doing the evacuation knew what class passengers were in, based on where their cabins were, etc
# but it seems unlikely they knew what fare each had paid
# seems a good time to simplify our model and drop Fare, since we don't really have a hypothesis about it 


## Second, check model assumptions
# the main assumption of logistic regression is that the outcome variable is binary. we got that
# the standard autoplot type stuff is useless for logistic regression
# we should check for correlations among the variables, but that is easiest to do using variance inflation factors (vif) after running the model
titanicMod.full = glm(Survived ~ Sex + Pclass, family = 'binomial', data = titanic)
# reminder that variance inflation factor for each predictor is based on its correlation with all the other predictors in the model
# since we have only two predictors, we expect their vif to be the same
# vif should be below 10 (or some people say below 5)
vif(titanicMod.full)
# we can also check for highly influential values by looking for standardized residuals that are out past 3 or -3
# we can get the standardized residuals with augment, and look at them with a histogram - so back in histogram of residuals territory, but now without worrying about normality!
titanicMod.full_aug = augment(titanicMod.full)
hist(titanicMod.full_aug$.std.resid)  # we use standardized residuals to get them in units of standard deviations; 3 sd is way out there
# the one other thing to know about logistic regression is that it requires a pretty large sample size, but we have that here


## Third, model results and interpretation
# we will use AIC to select the best model
# we already ran the full model which includes Sex + Pclass
# Q: write code to complete and run the models below

# reminder that R syntax for an intercept-only model is ~ 1
titanicMod.null <- glm(Survived ~ 1, family = 'binomial', data = titanic)
titanicMod.sex <- glm(Survived ~ Sex, family = 'binomial', data = titanic)
titanicMod.class <- glm(Survived ~ Pclass, family = 'binomial', data = titanic)
# now the model selection
model.sel(titanicMod.null, titanicMod.sex,titanicMod.class,titanicMod.full)

# here is some interpretation of the AIC table
# it sorts the models we ran in order of AIC (remember with AIC smaller is better)
# "delta" is the deltaAIC value, or change in AIC between that model and the best model, hence the top model has a deltaAIC = 0  
# "weight" is a ratio of relative model likelihoods, usually interpreted as the probability that the given model is actually the best of the set of models you ran
# the parameters are listed in columns (intercept, Sex, Pclass), before the model summary stats  
# if a parameter is in the model it has the estimated coefficient value for continuous predictors, or a "+" for categorical predictors.  Parameters that weren't in that model have a blank space.
# note model.sel() is actually running AICc here instead of AIC
# AICc is a version of AIC that attempts to correct for bias with small sample size and converges to regular AIC as sample size increases. It doesn't really have any downsides that I know of.
# Q: discuss with your partner what you conclude about the results of the model selection

# NOW for the HAIRY part of logistic regression: INTERPRETING YOUR MODEL PARAMETERS
# remember the parameter values R returns are on the scale of the link function, which is the log-odds scale - that is the log of the odds ratio, log(p/(1-p))
# reminder of what you learned in last week's class - some background interpretation
# for an event with a probability of 0.5, the odds ratio is 1, and the log-odds is 0
# thus, an intercept of 0 would mean a 50/50 chance of 'success' (for us, survival - so it actually makes sense to call it success for once!) when factors such as class and sex are not taken into account 
# for the coefficients, a positive slope means that an increase in the variable makes success more likely; negative slope, less likely
# armed with that knowledge let's look at our model output
summary(titanicMod.full)
# what is the intercept?  
# well, it would be the log odds of survival for females (reference level sex=0) who are Pclass=0...  which is sort of nonsense, but let's figure it out anyway
odds = exp(3.2946)	
odds  # survival odds of 27:1
# now let's convert from odds to probability
# odds of 27:1 means that out of 28 women on the titanic, 27 ar expected to survive
# 27/28 = 0.96
# here's the equation for converting from odds to probability
odds / (1 + odds)	# survival probability of 0.96
# the coefficient for Pclass is the change in log odds for each 1 unit increase in Pclass
# for example, for moving from second class to third class
# if we want to predict the value for a specific Pclass, we would need to add its slope*x to the intercept
# we can use that information to make a prediction for females in class 1, instead of the non existent class 0
# females in class 1
odds = exp(3.2946 + 1*(-0.9606))	# remember we do the addition in terms of log odds, since that's how the coefficients are reported, and then convert the answer to odds 
odds # survival odds of 10:1
odds / (1 + odds)	# survival probability of 0.91
# that wasn't so difficult!
# Q: below, calculate the survival odds and probability of survival for females in class 2
odds = exp(3.2946 + 2*(-0.9606))
odds
odds / (1 + odds)
# Q: let's up our game: calculate the survival odds and probability of survival for males in class 3
odds = exp(3.2946-2.6434+ 3*(-0.9606))
odds
odds / (1 + odds)

## Fourth, plot results back onto a data picture
# for logistic regression, people usually show the data and the curved model fit, with the continuous variable on the x and the probability on y
# since we have a categorical variable also (sex), we can do the two fits in different colors by sex
# we can achieve all this with code that is analogous to what we used for poisson regression, above
# once again, the only tricky part is doing our own back transformation of the CI from the link scale, back to the original data scale
titanicMod.full_aug = augment(titanicMod.full, type.predict="link", se_fit=T)
titanicMod.full_aug
# we are working with .fitted which are the link-scale fitted values of the mean (i.e. using the model to predict a log(odds) value for each observation using its observed values for Pclass and Sex)
# and with se.fit which gives the standard error of the fitted values of the mean at the parameter values of each observation
# then calculate CIs using 1.96*se
titanicMod.full_aug <- mutate(titanicMod.full_aug, ci_lwr = .fitted - (1.96 * .se.fit), ci_upr = .fitted + (1.96 * .se.fit))
titanicMod.full_aug
# then backtransform the CIs to get to the probability scale
# first, back transform from log(odds) to odds by exponentiating
titanicMod.full_aug_odds <-mutate(titanicMod.full_aug, fitted_odds = exp(.fitted), ci_lwr_odds = exp(ci_lwr), ci_upr_odds = exp(ci_upr))
titanicMod.full_aug_odds
# second, back transform from odds to probabilities using the odds/(1+odds) conversion
titanicMod.full_aug_probs <-mutate(titanicMod.full_aug_odds, fitted_probs = (fitted_odds)/(1+fitted_odds), ci_lwr_probs = ci_lwr_odds/(1+ci_lwr_odds), ci_upr_probs = ci_upr_odds/(1+ci_upr_odds))
titanicMod.full_aug_probs
# now we can plot!
ggplot(data = titanicMod.full_aug_probs, aes(x = Pclass, y = Survived, color=Sex)) + 
  geom_point() + 
  geom_jitter(alpha = 0.5, width=.2, height=.02) +
  geom_line(aes(x = Pclass, y = fitted_probs, color=Sex)) +
  theme_bw() +
  geom_ribbon(data = titanicMod.full_aug_probs, aes(ymin = ci_lwr_probs, ymax = ci_upr_probs), alpha = 0.1) 

