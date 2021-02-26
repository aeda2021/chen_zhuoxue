### Maximum likelihood
### An exercise for Advanced Ecological Data Analysis
### Dylan Simpson
### Last updated: 02/10/2021

library(pbapply) # This is a cool package that will give you a progress bar for apply functions
library(titanic) # This has data about who lived and died on the Titanic
library(lme4) # Package for fancy linear models
library(MuMIn) # Tools for model comparison and multi-model inference
library(fitdistrplus) # Tools for fitting and comparing observed distributions
library(tidyverse) # A all the tidy packages together
library(lmtest) # likelihood-based statistical tests

### Likelihood: foundations

## To start, we'll explore some code to
 # a) demonstrate how probability and likelihoods are calculated in R, and
 # b) gain some intuition around likelihood and likelihood profiles.

# First, let us return to the candy jar.
# It's a large jar with candies of many colors, some of which are blue, and the candies are randomly distributed.
# We want to know the proportion of candies that are blue but, either because the jar is just too big,
# or we are too lazy, we cannot count all the candies in the jar to find out.
# Instead, we must rely on sampling and statistics. (Dang!)
# So, statistically, we say that
# the probability of drawing a blue candy on a single draw is a Bernoulli process (or trial) with probability p,
# where p is the proportion of candies that are blue.
# We will make a number of draws (a binomial process) and make inference based on our observation.
# We can also think about site occupancy by a particular focal species:
# Out of a (statistical) population of sites, some proportion, p, are occupied
# (if you're familiar with the occupancy literature, this is what is normally denoted as the greek letter psi).
# We cannot visit every site (i.e we cannot census the landscape), so we visit some random sample of sites.
#
# If 20% of sites are occupied, or 20% of candies are blue,
# then finding the species at a single (random) site or drawing a single random candy
# can be considered a Bernoulli trial with p = 0.2.
# (A Bernoulli process/trial is an event with a binary outcome. The probability of one of those outcomes,
# deemed a 'success,' is given a probability p. The probability of the other outcome is 1-p.)
# That is, our observation (y) of species presence/absence would be Bernoulli distributed: y ~ bern(0.2).
# If you look at many Bernoulli trials together - for instance, if we draw 10 candies -
# we call this a 'binomial process.'
# In R, we use the same function for each of these processes.
# To look at a Bernoulli trial, we simply code for a binomial process with a single trial:
rbinom(1, 1, 0.2) # 1 observation of 1 trial - a 0 is a 'failure' and a 1 is a 'success'
bern.draws <- rbinom(1000, 1, 0.2) # 1000 observations of 1 trial
hist(bern.draws)

# If we visit 10 sites, or draw 10 candies, the number of "successes" (i.e. occupied sites or blue candies) is a binomial process.
# Repeated draws of 10 will yield a binomial distribution:
binom.draws <- rbinom(10000, 10, 0.2) # 10000 observations of 10 trials
hist(binom.draws)
# Note that the observed distribution of a binomial looks very different than the Bernoulli.
# A Bernoulli process can have only 2 outcomes,
# while a binomial process can have as one more outcomes than there were trials.
# In this case, the bionomial process involves 10 trials, so there can be 0-10 successes, or 11 outcomes.

# Now, looking at the actual plot of our outcome, we can see that, even when the true probability is 0.2,
# it is not uncommon to get 1 or 3 blue candies on a draw of 10,
# even though the 'expected value' is 2.
#
# This is why, given a particular result, there is uncertainty in parameter estimates.
# To illustrate: given the result of 2 of 10 sites occupied, how likely are different values of site occupancy, p?
# To calculate, recall that the math is the same under probability and likelihood,
# what's different is what is known and what varies.
# To start, how does likelihood vary from p = 0.05 to p = 1?

# Create a vector of potential values of p:
p <- seq(.05, 1, .05)
# And calculate likelihood for each value of p:
# (the function dbinom is a 'probability density function,' which means it calculates the probability
# density of a given distribution at a particular value of its variables and parameters.
# Said another way, given some parameters, it will give you the probaiblity density of a particular observation.
# Here, we ues it to calculate likelihoods, by giving a fixed observation and varying the probability parameter.)
L <- dbinom(2,10,p)
# Plot likelihood against values of p:
plot(p, L)

# As with the histogram above, we see that a value of .1 or .3 is not unreasonable but, graphically,
# 0.2 is the 'maximum likelihood estimate.' (For a binomial, we also know that the MLE is simply k/n: 2/10 = 0.2.)
# In the case of species occupancy, if 2 of our 10 sites were occupied, how confident are we in this MLE of 0.2?

# Calculating confidence intervals on an MLE is related to the likelihood ratio test for nested models.
# We'll get a little deeper into how the likelihood ratio test works later. For now,
# recall that the difference in likelihood between two models, under the null hypothesis of no difference,
# follows a chi-squared distribution.
# In the case of a confidence interval, we compare models with different values of the same parameter.
# For two models that differ by one parameter, the critical chi-square statistic is 3.84 at the 0.05 level.
# Thus, the bounds of the 95% CI are the values of the parameter that, when compared with the MLE,
# register a chi-squared statistic of 1.92 (half of 3.84, because we do this in either direction).
#
# To illustrate:

# First, we'll write a function to calculate the test statistic:
# (If you're unfamiliar with writing functions, don't sweat the details.
# Essentially, we are writing a tiny little program to calculate the likelihood-ratio test statistic.
# The programs inputs are the probability p, number of observations/trials n, and number of successes k)

chi.binom <- function(p, k, n){ # n = draw size/# of trials, k= successes, p = a vector of potential p's
  L0 <- log(dbinom(k, n, k/n)) # MLE = k/n
  L1 <- log(dbinom(k, n, p))
  r <- 2*abs(L0 - L1)
  names(r) <- p # This will help us match test stat values to parameter values
  return(r)
}

# Now we'll calculate the test statistic for values of p in either direction of the MLE.
# In effect, we are performing the likelihood ratio test between the maximum likelihood statistic
# (how likely is the observed outcome of k = 2 if p = 0.2)
# and a variety of alternative values of p
# (how likely is the observed outcome of k = 2 if p actually 0.19, 0.18 ... 0.1, etc.)

# A more fine-grained vector of possible values for p:
p.test <- seq(.01, .99, 0.001)
# Calculate the test statistic across values of p for k=2, n=10:
chi <- chi.binom(p.test, 2, 10)

# Q1: What do you expect a plot of the test statistic as a function of p to look like?
# (Hint: when will the test statistic be the smallest? What would make it get bigger?)

# Q2: a) Plot the test statistic against possible values of p,
#        and add a horizontal line at the critical value of the test statistic.
#        (Hint: the function abline() can add lines to plots)
#     b) Using your plot from (a), estimate the 95% CI for p. How did you get that estimate?

## ~ [ YOU'RE CODE HERE ] ~ ##

# Now find out if you were right:
# For an approximation of the CI, what values above and below 0.2 are closest to 1.92?
# (this code identifies the value of p associated with the test statistic that is closest to 1.92,
# and it does this twice, once on either side of p = 0.2.)
ci.min <- names(which.min(abs(1.92 - chi[as.numeric(names(chi))<0.2])))
ci.max <- names(which.min(abs(1.92 - chi[as.numeric(names(chi))>0.2])))
# Add lines on the graph:
abline(v = ci.min, lty = 2)
abline(v = ci.max, lty = 2)
# Print values (remember these are approximations, limited by the precision of our p.test vector):
ci.min
ci.max

# Going back to our liklihood profile:
plot(p, L, type = 'l')
abline(v = ci.min, lty = 2)
abline(v = ci.max, lty = 2)

# That's a pretty big confidence bound!
# Thinking about our ecological study design of surveying species across a number of sites,
# how can we increase our confidence our estimate?
# ...
#
# Increase sampling! What if we sampled 100 sites, and 20 were occupied?
chi2 <- chi.binom(p.test, 20, 100)
ci.min2 <- names(which.min(abs(1.92 - chi2[as.numeric(names(chi2))<0.2])))
ci.max2 <- names(which.min(abs(1.92 - chi2[as.numeric(names(chi2))>0.2])))

plot(p.test, chi2, type = 'l')
abline(h = 1.92, lty = 2)
abline(v = ci.min2, lty = 2)
abline(v = ci.max2, lty = 2)

# Q3: a) What is the 95% CI?
#     b) Plot the likelihood profile and 95% CIs for the proportion of sites occupied,
#         based on the observation of 20 of 100 sites occupied

L2 <- dbinom(20, 100, p.test)
plot(p.test, L2, type = 'l')
# CIs:
abline(v = .149, lty = 2)
abline(v = .259, lty = 2)

# Before moving on, a few words on maximum likelihood estimation.
# In some cases, the MLE can be derived analytically by taking the derivative,
# setting to 0, and solving for the parameter.
# The binomial case is very convenient because this calculus simplifies to k/n.
# In other cases, particularly complex models like GLMMs, MLEs are derived numerically,
# meaning through algorithms. If we gloss over a lot of details and elegant software solutions,
# we can think of this as similar to how we found the CIs above:
# Take the likelihood across a range of values for each parameter,
# and find the combination of parameter values with the maximum likelihood.
# The software elegance comes in when we have many parameters and we have to explore multidimensional space.


## Model comparison - the likelihood ratio test

# One of the great things about likelihood is it allows us to directly compare competing hypotheses.
# That is, we can formulate our hypotheses as statistical models, and directly ask, "which of these explains my data better?"
#
# The first method we'll examine is the likeihood ratio test. This method is powerful because it takes advantage of
# frequentist statistical theory (the same theory underlying ANOVAs, etc.) and so is rooted in a framework
# most ecologists are familiar with. It is limited, however, because it is only valid for nested models.
# Before we get into using the test, though, let's think about how and why this stuff works.
#
# As the name implies, the likelihood ratio test examines the relative likelihood of two models.
# But, we can't simply look at which model has a higher likelihood (or lower negative log-likelihood...I'm so sorry...).
# Why? Because even models with the same explanatory power (whether that is high or low), can appear to be different by chance.
# For instance, a model with an extra predictor variable might appear to better explain your observations,
# even if that extra variable is, in the real world, unrelated to your outcome variable.
# In fact, extra parameters, even if they have no true explanatory power, will tend to *appear* as if they do.
# So, because of this uncertainty, we use a statistical test to ask if models are more different from each other
# than we would expect, given the number of parameters in the models and assuming there is no true, meaningful difference between them.
#
# To do so, we use the ratio of likelihoods to generate a test statistic (that's right, you haven't escaped them yet!),
# and compare that test statistic to a chi-square distribution.
# To make sense of this, let's take a detour and consider frequentist test distributions.
# Recall that test distributions are the expected distribution of some value under the null hypothesis.
# In ANOVA, the F distribution is the probability distribution of the ratio of mean squares, assuming no true difference among groups.
# If groups are composed of observations randomly drawn from the same (statstical) population,
# we can expect some differences among groups by chance. This is what's described by the F distribution.
# In the likelihood ratio test, we are asking not whether groups are different, but whether models are different.
# Specifically, we are asking if they are more different than we would expect due to chance.
#
# The test stat we rely on here is the chi-square distribution,
# with degrees of freedom equal to the number of parameters that differ between the models.
# This is important because any random variable can appear to explain some variation in another,
# and more variables can explain more variation by chance.

# Before going any further, let's build a sense for what I'm talking about.
# In the space below, generate 10 observations of three random normal variables, y, x1, and x2.
# Then build four nested linear models: one with no predictors, one with x1,
# one with x1 and x2, and one with x1, x2, and their interaction.
#
# Then, look at the summary of the models and look at the multiple (not adjusted!) R^2,
# and use the logLik() function to check out their log-likelihood.
# See how the models get better/have higher likelihoods when they get more complicated?
y <- rnorm(10)
x1 <- rnorm(10)
x2 <- rnorm(10)

fm0 <- lm(y ~ 1)
fm1 <- lm(y ~ x1)
fm2 <- lm(y ~ x1 + x2)
fm3 <- lm(y ~ x1*x2)

# Now check out the R2 as parameters are added (though check the multiple, not the adjusted R2):
summary(fm1)
summary(fm2)
summary(fm3)

# Similarly, look how the likelihood increases:
logLik(fm0)
logLik(fm1)
logLik(fm2)
logLik(fm3)

# Even weirder, this is not *just* a probabilistic process.
# Analytically, as we increase the number of parameters, the closer we are able to approximate the data.
# At a low number of parameters, stochasticity rules, but, as the number of parameters increases, so does the variance explained.
# And, when the number of parameters equals the number of observations, the model can perfectly describe the data. Always.

# Check it out:
y = rnorm(100) # 100 random observations
X = replicate(100, rnorm(100)) # 100 measurements of 100 random covariates (unrelated to y)

# Don't sweat the details, but this loop will perform the same task as above -
# ie a series of regressions with an increasing number of variables -
# and record the R^2 for each
r.sq <- sapply(1:100, function(i){
  fm <- lm(y ~ X[,1:i])
  summary(fm)$r.squared
})

plot(r.sq) # x axis = number of predictors, y axis = r squared of the model
r.sq[100] # with 100 # predictors = # observations, r squared is always 1.


# So, now that we have seen why we can't rely on raw likelihood, what *do* we do?
#
# When comparing nested models, the test statistic in the likelihood ratio test is:
# R = 2*[the difference in log-likelihoods].
# Assuming the models are actually equal in their explanatory power,
# that is, the extra  parameters in the more complex model are not actually meaningful,
# this test statistic is chi-square distributed.
#
# To demonstrate how/why/that this works, let's take the exercise just above
# and repeat it over and over. Each time, we'll calculate the test statistic and,
# after many iterations, we can see how those are distributed.


# First, a function to return the test statistic:
# Again, don't sweat the details if you don't understand the code,
# but all we're doing is writing a little program to take two models as input,
# and return the test statistic we just defined above: twice the difference in log likelihoods.
chi.LR <- function(m0, m1){
  L0 <- -logLik(m0)[1]
  L1 <- -logLik(m1)[1]
  abs(2*(L0 - L1))
}

# Now we generate random variables as before and relate them via simple regression.
# We'll see that when we add extra variables to a model, even when they are totally
# unrelated to our outcome variable, the likelihood of the model *will* tend to increase.
# The test statistic, which measures this change in likelihood, is chi-square distributed.

# This script uses an apply() function to do the same operation over and over.
# The operation is everything after "fucntion{"
R <- t(pbsapply(1:10000, function(i){
  # Within each iteration, we define three independent random normal variables,
  y <- rnorm(100)
  x1 <- rnorm(100)
  x2 <- rnorm(100)

  # and relate them in a series increasingly complex linear models.
  fm0 <- glm(y ~ 1)
  fm1 <- glm(y ~ x1)
  fm2 <- glm(y ~ x1+x2)
  fm3 <- glm(y ~ x1*x2)

  # Because each parameter difference corresponds to a degree of freedom in the chi-square distribution,
  # comparing fm0 with each of the other three will correspond to three different chi-square distributions,
  # with increasing degrees of freedom.

  # Save a vector of test statistics, using the function we defined previously:
  # (the names of the results ('df1,' etc.) refer to the degrees of freedom of likelihood ratio)
  test.stats <- c(df1 = chi.LR(fm0, fm1), df2 = chi.LR(fm0, fm2), df3 = chi.LR(fm0, fm3))
  return(test.stats)
}))

# Lets check out what this looks like,
# and how they compare with true chi-square distributions
# First we'll plot a histogram of the observed test statistics,
# then plot on top of that a probability density curve for the associated chi-square distribution.
# (Note, these histograms show the relative, rather than absolute, frequency)
hist(R[,1], main = "1 predictors; 1 df", freq = F)
lines(seq(.1,15,.1), dchisq(seq(.1,15,.1), 1), col = 'red', lwd = 2)

hist(R[,2], main = "2 predictors; 2 df", freq = F)
lines(seq(.1,15,.1), dchisq(seq(.1,15,.1), 2), col = 'red', lwd = 2)

hist(R[,3], main = "3 predictors; 3 df", freq = F)
lines(seq(.1,15,.1), dchisq(seq(.1,15,.1), 3), col = 'red', lwd = 2)

# Damn I love when math works.
#
# And, to reiterate the takeaway here:
# We just modeled noise, and found that adding junk variables can actually increase the likelihood of a model,
# sometimes by a lot. Just by chance.
# So, if you want to statistically test whether one model is better than another,
# the likelihoood ratio test allows you to do that, *for nested models.*


# Q4: a) Desribe the difference between these three distributions
#     b) Not thinking about the underlying mathematics of the chi-square function,
#       but thinking about the likelihood ratio test,
#       why does the test distribution behave as it does with more degrees of freedom?

# Q5: Considering that the likelihood ratio test is only valid for nested models,
#     define the p-value explicitly in the context of the question the likelihood ratio test asks
#     and the null hypothesis being tested.
#     (Hints: what is the definition of a p-value?
#             In a likelihood ratio test, what is being observed? More specifically, what is the test statistic?
#             What is the null hypothesis?)


### Likelihood: applications
  # Beyond parameter estimation, the primary application of likelihood is to compare hypotheses.
  # In statistics, hypotheses take the form of different models.
  # When models are nested, they can be compared in a hypothesis-testing framework via the likelihood ratio test;
  # when models are not nested, they can be compared in an information-theoretic framework by comparing
  # associated information criteria (e.g. AIC).


## Using the likelihood ratio test

# Here's a dataset on passengers of the Titanic. Morbid!
# It includes their age, sex, class, what cabin they were in, whether they had siblings aboard, etc.
data(titanic_train)
# For simplicity, drop entries with missing values:
# (though you could potentially keep entries if you weren't using the column that includes the missing value)
titanic <- drop_na(titanic_train)
glimpse(titanic)

# So, let's try to predict who lives and dies!
# To do so, we'll use a general*ized* linear model (GLM), specifically a logistic regression model, to predict the probability of survival.
# Don't worry about how this works. Next week will actually focus on GLMs, including logistic regression models.
# We're covering likelihood first because maxlimum likelihood is how GLMs are fit.
# For now, know that logistic regression predicts the probability of binary events (e.g. survival, presence/absence, etc.)
# Your model parameters will be returned on the log-odds scale - that is the log of the odds ratio.
# For an event with a probability of 0.5, the odds ratio is 1, and the log-odds is 0.
# Thus, an intercept of 0 means a 50/50 chance.
# For beta coefficients (i.e. slopes), the sign still means the same thing;
# A positive slope means a variable makes an event more likely, and vice versa.
# So, for now, don't worry about how it works. You'll learn more about that soon.

# Complete the 3 models below to create an intercept-only model, a model with Sex as a predictor, and a third with Sex and Age both
# (see ?titanic_train for explanations of variables)
# (note that the only real difference from the lm() function is the added 'family' argument. Keep this mind for a little later on.)

fm0 <- glm(Survived ~ , family = "binomial", data = titanic)
fm1 <- glm(Survived ~ , family = "binomial", data = titanic)
fm2 <- glm(Survived ~ , family = "binomial", data = titanic)

# We can look at the likelihood ratios of these models,
# and test for significance, using the lrtest() function from the lmtest package.
# Again, significance here means that the difference in models is more than we would expect
# due to chance, under the null hypothesis that there is no difference in the models.
# That is, no difference in how well they explain the outcome variable.
# Important! Models are entered in order of complexity (global to null, or vice versa)
lrtest(fm0, fm1, fm2)

# Q6: Interpret this output:
#     a) what does each chi-square statistic and p-value refer to?
#     b) What null hypotheses are being tested?
#     c) Which variable(s) offer(s) significant predictive power?


## Information criteria and multi-model inference

# What if we want to compare non-nested models? For instance, if we wanted to ask,
# what is the best and most parsimonious model to predict survival, given a set of predictors?
# In this case, fit the models and compare AIC
# (there are other information criteria, but we'll use AIC; it is the most common)

# Here, choose three variables you think might have influenced probability of survival aboard the Titatnic,
# and write a set of models that include variables alone and in combination. Also include a null (ie intercept-only) model.
# Think carefully about each model:
# Do each of these deserve to be in a model with each other? Is there reason to think some variables might interact?

## ~ [ YOU'RE CODE HERE ] ~ ##

# Q8: How do you know these models are not nested?


# Compare these models using the model.sel() function from the MuMIn package:
?model.sel


# This function provides the 'second-order' AICc criterion, which is adjusted for bias under small sample sizes,
# as well as likelihoods, effects, 'delta-AIC,' and model weights.
# Model weights are interpreted as the probability that a given model is the best model *of the model set*.
# "Of the model set" cannot be emphasized strongly enough.

# Q9: What is 'delta-AIC' and why is this more relevant than the raw AIC values?

# Q10: Interpret your output - which model is best? Which variables most important?


## Another way we can compare models using likelihood is to assess which error structure
## is best suited to a dataset:

# For ease, we'll generate a fake dataset:
# Don't worry about how it's generated - in fact, it's better you don't understand.
# The point is just to have a mystery distribution.
dat.test <- rpois(100, exp(rnorm(100, 2, .2)))
hist(dat.test)
summary(dat.test)

# Now the question is, if we were trying to model these data, what would be the best distribution?
# We would know from collecting them that they are count data.
# The two possibilities then are Poisson and negative binomial.
# Use the fitdist() function from the fitdistrplus package to test Poisson, and negative binomial.
# (Hint: It will help to save the results to objects, then use summary() to get all the deets.)
#
# Fitdist() will give you parameter estimates (e.g. what is the mean) and variance estimates
# *for those parameters*. Meaning, how confident are we in these parameters?
# It will also give you the likelihood and a couple information criteria.
# To compare, you want a higher likelihood, but a lower information criterion.

## ~ [ YOU'RE CODE HERE ] ~ ##



# Q11: a) Which distribution is the best?
#      b) On what basis are we making this inference? That is, what are really comparing?
#         (Think about the mathematical definition of likelihood, and how it is calculated.)


# This method could also be used, and would probably be *better* used,
# to assess the optimal error structure of a *linear model*, rather than a raw dataset.
# This is because, just as with a general linear model, the assumption of Poisson or neg-binomial regression regards the
# distribution of residuals.

# Lastly, let's think about some ways to abuse our new found power.
# Consider that we are studying the distribution and abundance of some species. Maybe bees.
# There are lots of things that might affect bees, and we want to know what they are.
# It can be tempting to just measure everything you can think of and see what happens.
# Imagine: I went to the field to collect bees, and came back with soil chemistry data, atmospheric
# data, adjacent water quality data, floral data, weather data, socioeconomic data, traffic data, oak-masting data,
# tree DBH and canopy height data, and foot traffic data, and then got data from a colleague on small mammal abundance and diversity.
# Then, I go into R and I code a set of models that include each possible combination of these variables
# to predict bee abundance. I come back to you and say, "I figured it out! I know what governs bee populations!"

# Given the goal of finding a 'best model' (and most parsimonious model), it can be tempting to just keep adding variables,
# and considering all possible combinations of variables. This sort of mindless analysis is called data dredging,
# and has theoretical problems similar to the multiple comparison problem in null hypothesis testing, and the
# approach of "let's just keep collecting data until our results are significant" as a method for determining sample size.
#
# Alternatively, imagine that we first discuss a set of environmental measurements we hypothesize are related to bee abundance,
# based on natural history, existing literature, etc. We develop a set of hypotheses,
# but aren't sure whether they are valid or what the relative importance of each variable is.
# So, as part of a pilot season, we collect bees and measure these variables, and then look at all possible combinations.
# In this case, I would probably examine the distribution of weights, and which variables show up in the top models.
# From this analysis, we choose a subset of variables on which to focus during our primary data collection and analysis.
#
# There is little practical difference between these two situations. The code I used to carry them out might be very similar.
# But, there is a philosophical difference, and a difference in both the questions I am asking and the inference I am making.


# Q12: Describe the difference between these approaches. What is my goal in each scenario?
#      What questions am I asking, and what am I left with at the end?


# Data dredging is controverisal, and rightfully so.
# Sometimes it is clearly awful (as in the first case I described), but it can sometimes have merit (as in the second).
# In my experience, dredging is acceptable as a form of 'hypothesis generation,' typically as part of a pilot project,
# in which you develop a set of specific hypotheses on which to follow up with subsequent data collection.
# That said, it might still raise some eyebrows if not carefully justified. (Or not at all, depending on the reviewer ;)


# A more practical problem with dredging is that the size of the model set increases exponentially with the number of variables,
# and this can lead to poor inference if you don't perform a follow up analysis.
# In particular, when you have many variables that have little or no information,
# your weight of evidence gets more evenly spread out among models.
# This can make choosing a best model or even set of models impossible and makes choosing best variables difficult.
#
# To demonstrate, check out the following simulation.
# What we're going to do is simulate a dataset of some number of environmental variables, some of which will truly affect
# observations of abundance of some focal species, but most of which will not.
# We'll do this twice. Once with independent/orthogonal environmental variables,
# and again with a set of environmental variables that exhibit some covariance.
# Problems get worse when your environmental variables are correlated.
#
# However, because I don't want the point of what we're doing to get lost in the details,
# I've put the code for this in a separate file.
# To bring in the datasets, run the following:

source("data_sim.R") # this runs the code in the 'data_sim.R' file;
                     # this filepath assumes that you put data_sim.R in your working directory.

# In this data simulation, I create two sets of of 12 environmental variables.
# In dat1, they are independent, though may have some correlation structure by chance.
# In dat2, they have a true, underlying correlation structure (which was generated randomly).
# In each dat1 and dat2, the observations, y, represent a Poisson distributed species,
# the abundance of which is affected by 4 of those variables.
#
# Below, we'll explore the consequences of data dredging with each of these datasets.


# Here we fit a global model with all the possible variables:
fm.ind.global <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12,
                     data = dat1, family = 'poisson', na.action = na.fail)
summary(fm.ind.global)

# Take all possible combinations of varibles: (takes about 20 seconds)
fm.ind.dredge <- dredge(fm.ind.global)

fm.ind.dredge

# Q13: Briefly, how might you interpret this? Which variables do you think are most important?

# Having thought about it first, here's which variables have an actual effect:
which(B!=0)

# Notice also that delta-AIC values increase slowly and almost linearly across models, and that model weights are
# very evenly distributed.
#
# Depending on how things shaked out, you may have extra (unimportant) variables in your top model,
# or be missing important variables from your top model.
#
# One way to get some useful information out of this is to look at variable weights.
# That is, the sum of model weights across all the models in which a variable occurs:
sw(fm.ind.dredge)

# Usually the variables with a true effect do outperform the rest, but not always.
# Moreover, few if any variables can be definitively rejected.

# All of this also gets more complicated if your independent variables are correlated.
# In the next dataset, there are 12 environmental variables that have a random but non-0 correlation structure,
# and we repeat the analysis as before. The observations, Y, have the same true parameter coefficients as before.
#
# Because this is all generated randomly, your results may differ from mine and from each other's.
# But, some common things to expect: lower power in the global model, and greater model uncertainty,
# particularly regarding variable weights, as returned by sw( ).


# Here we fit a global model with all the possible variables:
fm.cor.global <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12,
                     data = dat2, family = 'poisson', na.action = na.fail)
summary(fm.cor.global)

fm.cor.dredge <- dredge(fm.cor.global)

fm.cor.dredge

sw(fm.cor.dredge)
# You will probably see that the true variables can no longer be picked out quite so easily.
# (Though results can really vary - dang stochasticity!)

# These problems can be avoided (or at least minimized) by being careful with what models you include in your model set,
# and which variables you include in the same model. For instance, if you include mean annual precipitation and mean summer precipitation,
# you are less likely to find an effect of either, because their high correlation will inflate the variance of both coefficients,
# creating greater uncertainty. Instead, AIC of competing models could tell you whether summer or total annual precipitation
# does a better job. Moreover, if include only a limited model set, with robust and well-justified variables, you are more likely
# to be in a position to make clear, strong inference.

# Multi-model inference is both an analytical technique and a philosophical framework for statistical inference.
# It is grounded in information theory and it seeks not to determine statistical significance, but to quantify information
# and weigh evidence. Proponents of multi-model inference place great emphasis on carefully selecting both variables and models
# to be included in a model set, and eschew methods that rely on arbitrary cut-offs. That said, researchers have gone to
# great lengths to create cut-offs and thresholds, probably because there is comfort in simple yes-no answers,
# especially for researchers raised on p-values and all the ways they are misused in the literature.
# We're out of time for an in depth discussion of multi-model inference here,
# but if you're interested in a more thorough examination, check out the work of Ken Burnham and David Anderson.
