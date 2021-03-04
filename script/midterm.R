### Zhuoxue Chen
### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands


##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join
rm(list = ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
Bee <- read_csv("pinelands_bees.csv")
LC <- read_csv("land_cover.csv")

Bee <- mutate(Bee, site_name=factor(site_name))
LC <- mutate(LC, site_name=factor(site_name))
Bdata <- Bee  %>%
    group_by(site_name) %>%
    summarize(n()) 

Bdata <- left_join(Bdata, LC,key = site_name) 
Bdata <- Bdata%>% 
    rename(site = site_name)
Bdata <- Bdata%>% 
    rename(Bees = "n()")
Bdata <- Bdata%>% 
    rename(Landcover = Nat1600)

## 2 Data picture
# plot the data and figure out what type of model might be best
ggplot(data = Bdata, aes(x = Landcover, y = Bees)) +
    geom_point() +
    theme_bw()
# By looking at the graph, we can roughly see a negative linear relationship

## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose
hist(Bdata$Bees)
hist(Bdata$Landcover)
cor.test(Bdata$Landcover, Bdata$Bees)
summary(Bdata)
# Just an overview of the data. The data is normally distributed and the two variables seem correlated based on the cor test p value (0.029)
# By looking at the data, I think I won't use glm, the mean of number of Bees are way larger than 15, it may not follow poisson distribution
# us lm()
B_mod <- lm(data = Bdata, Bees~Landcover)
plot(B_mod)
hist(B_mod$residuals)

## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why
summary(B_mod)
coef(B_mod)

# The coefficient here is -0.4354, which means as the land cover increase, the bee number decreases.
# the p-value here is 0.0293, which is lower than 0.05, which indicates we are confident to say that 
# there is significant relationship between variables in this linear regression.
# The R squared value here is 0.138, which I think is too low that I cannot say the model is a good fit.
# Based on the result, I don't feel it make ecological sense. What I expected was the more natural land cover,
# the more bee found in site. Based on the plot we made in data picture, I realized the data might be nested in some way.
# Some other factors might also affect the data, like when we go out and catch bees, seasonality might also matter.
# Also, two outliers may also effect the model result.


## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want

ggplot(data = Bdata, aes(x = Landcover, y = Bees)) +
    geom_point() +
    geom_smooth(method = "lm")+
    theme_bw()

## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

# I will use sex or catching method as the random effect. Here in this model, we don't care about the
# sex or the catching methods. But in fact, these two variables might cause differences in data.
# I don't know a lot of bees, but this might be situation that female bees prefer some certain place to go,
# or some places only suitable for certain catching methods.In order to avoid these effects, we need to set
# them as random effect.


### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.


# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)
rm(list = ls())
data <- read_csv("modSel.csv")
summary(data)
# I will use Poisson errors. By looking at the summary of the data, we can see the count data's mean is around 2, which
# is relatively small and if we have a mean lower than 15, we might use poisson errors here.

d_lm <- lm(observedAbundance~meanAnnualTemp,data = data)
d_annualtep <- glm(observedAbundance~meanAnnualTemp,data = data,family = poisson)
AIC(d_lm)
AIC(d_p)

# just run these two model to compare the AIC. We can see the model with poisson error distribution shows better result: smaller
# AIC value


# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines
d_annualtep <- glm(observedAbundance~meanAnnualTemp,data = data,family = poisson)
d_sumtep <- glm(observedAbundance~meanSummerTemp,data = data,family = poisson)
d_annualpre <- glm(observedAbundance~annualPrecipitation,data = data,family = poisson)
d_sumpre <- glm(observedAbundance~summerPrecipitation,data = data,family = poisson)
d_d2e <- glm(observedAbundance~distance2edge,data = data,family = poisson)
d_e <- glm(observedAbundance~totalEdge,data = data,family = poisson)

model.sel(d_annualtep,d_sumtep)
model.sel(d_annualpre,d_sumpre)
model.sel(d_d2e,d_e)

#Based on the result, we will choose summer temperature, summer precipitation and the total edge as the variable.
# Here, I am looking at the lower AICc value to tell if I should pick this variable.


# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

d_null <- glm(observedAbundance~1,data = data,family = poisson)
d_1 <- glm(observedAbundance~meanSummerTemp,data = data,family = poisson)
d_2 <- glm(observedAbundance~meanSummerTemp+summerPrecipitation,data = data,family = poisson)
d_3 <- glm(observedAbundance~meanSummerTemp+totalEdge,data = data,family = poisson)
d_4 <- glm(observedAbundance~meanSummerTemp+summerPrecipitation+totalEdge,data = data,family = poisson)
d_5 <- glm(observedAbundance~summerPrecipitation+totalEdge,data = data,family = poisson)

model.sel(d_null,d_1,d_2,d_3,d_4,d_5)

# I will pick the model d_3, whit total edge and summer temp, it shows the lowest AICc value, which means this model can best
# describe the situation.


# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?
summary(d_3)

# By looking at the result, I can say my hypothesis is supported by the model. Looking at the summary of this model,
# p value here is lower than 0.05 this means we can trust this model with confidence. If we interpret the model details, 
# we can say the species is prefer the warm temperature by looking at the positive coefficient for the summer temperature, but not
# for the precipitation.
# and it seems that this species distribution  is negatively related to the total edge of the environment.So I gonna say our
# hypothesis is reject at this point.






