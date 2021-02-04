## Goal of this assignment
# To review basic statistics in R

## How to complete this assignment
# read through the comments and run the code below
# whenever you see a comment line that begins with Q, discuss that question with your partner (you do not need to write the answer out)
# if the Q question requires you to write code, do so
# when you are finished, push your completed code to your Github repo at  https://github.com/aeda2021/

## Outline
# correlation
# regression
# anova
# two-way anova
# multiple regression
# ancova
# variable standardization
# two exercises

## preliminaries
# clear R's brain
rm(list = ls())  # it's a good idea to put this at the start of all your code
# load libraries
library(datasets)
library(tidyverse)
library(ggfortify) # ggplot needs this to use autoplot for lm
library(car) # for vif, crPlots, levene's test, Anova() using type III SS
library(mosaic) # for contrasts in anova
library(scatterplot3d)  # another alternative for 3D graphics is plot_ly
library(broom) # tidyverse thing that makes it easier to plot model results back onto your data picture
# remember if R says 'there is no package called that,' it just means that you need to install the package - see packages pane, install
# the library() function just loads (ie, for use in this work session) packages that are already installed
# remember an easy way to set your working directory for an R session is
# Files pane, navigate to your desired wd, More, Set as working directory


## CORRELATION
# our correlation question: are car weight and mpg correlated?
# let's step through this analysis in the order recommended in this course
# get data
data(mtcars)
# mtcars data are from 1974 Motor Trend car road tests
# click on mtcars in environment pane to look at the data
# search help pane for mtcars to get more info on the data


# first, data picture
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()

# second, stats test
cor(mtcars$mpg, mtcars$wt, method = "pearson")  
# we will use base R for most stats tests
# the tidyverse is mainly for data manipulation and graphics

# third, check assumptions
# Q: are the data reasonably linear? just look at your data picture
# now check for normality with a qq plot
# remember what you are looking for is whether the points fall roughly in a straight line
qqnorm(mtcars$mpg)
# Q: write code to do the same thing for the other variable below
qqnorm(mtcars$wt)


# Q: are both x and y reasonably normally distributed? 
# Q: can you explain how the wt distribution might differ from a normal distribution, just by looking at the qq plot?
# Q: write code below to check your intuition with a histogram
hist(mtcars$mpg)
hist(mtcars$wt)


# Q: explain how the histogram is showing the same thing as the qq plot
# imo the most useful thing to do is compare your data to what a random sample from a normal distribution would look like
# given the same sample size, mean, and sd 
example_data = rnorm(32, mean(mtcars$wt), sd(mtcars$wt))
hist(example_data)
qqnorm(example_data)
# Q: after running the above code a bunch of times, what do you conclude about mtcars?



# fourth, interpret results
# oops, we forgot to get a p value!
# which is actually good, because we shouldn't be thinking about the results until after checking assumptions anyway
cor.test(mtcars$mpg, mtcars$wt)

# Q: put your conclusion from this analysis in words
# A: The data is normally distributed.


## REGRESSION
# our regression question: does car weight predict mpg?
# Q: how and why is this question phrased differently from the correlation question?
# remember that 'tidy data' means that each column is one variable, and each row is one observation
# Q: are the mtcars data tidy? why or why not? might be easier to look at the Environment / Data spreadsheet to tell


# first, data picture
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()
# now, think about your analysis before doing it (this is like, the best habit EVER !!!!)
# Q: what do you expect the slope to be, roughly?
# Q: the intercept?
# Q: the df for the error?  Why?
# you should also use the data picture to check assumptions informally to decide what model to run
# linearity and homoskedasticity look pretty good, so we can try lm()
# note, you will need to run and save the model in order to get more detailed checks on assumptions
# Q: why is the thing noted above the case?

# second, run stats test
# reminder that R reads the lm function as saying, 'fit a linear model, where we predict mpg is a function of car weight, using variables from the mtcars data frame'
# you'll like yourself better in the future if you get in the practice of saving all model results in a consistent name format
car_mod <- lm (mpg ~ wt, data = mtcars)

# third, check assumptions
# first look at a histogram of the residuals
hist(car_mod$residuals)
# autoplot is the ggplot function that automatically generates assumption-checking pictures for you
# plot() in base R does essentially the same thing
# remember you feed autoplot the model results, in this case car_mod
autoplot(car_mod)
# the default blue lines are distracting and don't mean much, remove them next time
autoplot(car_mod, smooth.colour = NA)
# you may have to run the above twice to get past error messages; not sure what is up with that
# if this is a problem for you just use plot() instead; note that with plot() you have to hit return to get all 4 plots
# Q: for each of the 4 figures, what were you checking for? and what did you find?

# fourth, interpret stats
# summary() gives you the regression-type output including the coefficients
summary(car_mod)

# for more details on the output look under environment / values
# Q: are the slope, intercept, and error df roughly what you expected? why or why not?
# some code that might be helpful here; you can change the axis limits to whatever works best for you
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point() +
        xlim(0,7) +       # when you manually set axis limits, R will not plot any data points that fall outside those limits. just be aware.
        ylim(0,50)        


# fifth, plot the model back onto the data 
# here is how you get the coefficients from the model output
coef(car_mod)
# you can see the same stuff under Environment / Values, car_mod
# below the [1] is just base R's way of extracting the first coefficient
car_mod_intercept = coef(car_mod)[1]
car_mod_slope = coef(car_mod)[2]
# Q: Why might it be a good idea to name the slope and intercept terms like this before plotting them, rather than just typing in the numerical values?
# use geom_abline to plot your model results by using (a) the model intercept and (b) the slope 
# set manual x and y limits to override the defaults and see the actual intercept
ggplot(mtcars, aes(x = wt, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = car_mod_intercept, slope = car_mod_slope) +
        xlim(0,6) + ylim(0,40)
# this is a nice proof of concept that regression is doing what we think it is
# however this picture raises two issues
# first, a better fit might be somewhat nonlinear
# for now, we will just proceed with linear models; we will cover nonlinear models in a later class
# second, geom_abline extends the regression line beyond the data points, which is a bad idea
# next we will use a different method to plot the actual fitted values from the regression model
# augment() creates an augmented version of the model results df that includes the fitted values and 95% CI corresponding to each data point
augmented_car_mod <- augment(car_mod, interval = 'confidence') # need to specify the optional argument interval here, otherwise defaults to not giving CI
glimpse(augmented_car_mod)
# Q: which new columns look like the ones we want? 
# now make the data + model results picture
ggplot(augmented_car_mod, aes(x = wt, y = mpg)) + 
        geom_point() + 
        geom_line(aes(y = .fitted)) +  # augment uses .whatever for all the new columns it creates to avoid confusion with existing variables
        geom_ribbon(aes(ymin =.lower, ymax = .upper),alpha=0.3)  # alpha makes things transparent
# oops it looks like there are data points outside the 95% CI
# Q: how can that be? is this something we expect, or not?
# hint: the different types of CI were covered on slide 100ish in the lecture
# another hint: a confidence interval is like a standard error, whereas a prediction interval is like a standard deviation; which did we plot?
# Q: copy and paste the code above, then alter it to plot the prediction interval instead of the confidence interval
augmented_car_mod <- augment(car_mod, interval = 'prediction') 
glimpse(augmented_car_mod)
ggplot(augmented_car_mod, aes(x = wt, y = mpg)) + 
        geom_point() + 
        geom_line(aes(y = .fitted)) + 
        geom_ribbon(aes(ymin =.lower, ymax = .upper),alpha=0.3)

# there is a third type of confidence interval, which gives the CI for the slope itself
confint(car_mod)
# Q: but back to completing our analysis - what is your conclusion about the answer to our regression question? 
# state your answer in terms of the original variables and their units
# Q: can you interpret your intercept in a meaningful way? 



## ANOVA 
# our anova question: do irises of different species have different sepal widths?
# get some data on iris flower morphology
data(iris)
# take a look at the data any way that works for you

# first, data picture
# Q: write code below to ggplot sepal width by species, using boxplots
boxplot(iris$Sepal.Width ~ iris$Species)



# Q: are there any assumptions of anova that you can roughly check by eye, and how do they look?
# Q: what is your guess about the answer to our anova question? be as specific as possible, eg, rank species in order

# second, run model
# Q: write code below to run an lm model for sepal width by species
# call your output iris_mod
iris_mod <- lm(Sepal.Width ~ Species, data = iris)



# third, check assumptions
plot(iris_mod)
# Q: can you interpret these plots with respect to the assumptions of anova?
# some people do, but to me plot() seems kinda unhelpful for categorical variables
# let's check whether the groups have similar variances, since that is the assumption of anova and plot doesn't do it
# levene's test for homogeneity of variances
# null hypothesis of test is that the variances are not different
leveneTest(iris_mod)
# anova also assumes that the outcome variable is normally distributed within each group
# first get the levels ie species names
# note R puts the species names in quotes. thus you should, too
levels(iris$Species)
# extract all columns of the iris data for only the first species
setosa <- filter(iris, Species == "setosa")
# make a histogram and a qq plot of our outcome variable
hist(setosa$Sepal.Width)
qqnorm(setosa$Sepal.Width)
# Q: write code below to check normality for the next two species

versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")
hist(versicolor$Sepal.Width)
qqnorm(versicolor$Sepal.Width)
hist(virginica$Sepal.Width)
qqnorm(virginica$Sepal.Width)

# Q: what do you conclude about these data meeting the assumptions of anova?
# note that even if normality is not perfect, that is probably okay given the large sample sizes and balanced design (ie, equal number of data points per species)

# fourth, interpret results
summary(iris_mod)
# Q: what does the estimate for the intercept mean? 
# Q: what do the t values mean, for both the estimate (reference level) and the other comparisons? 
# Q: what does the F and its p value tell you?
anova(iris_mod) # remember this is how we get anova-type results out of the model we ran
# Q: why are the df what they are? 
# Q: what do the two MS values tell you?
# the null hypothesis we have just rejected is that all groups come from populations with the same mean
# that is, the F value suggests this is unlikely
# but anova does not tell you which group mean(s) are different from which other(s)
# need a tukey 'honestly significant difference' test for comparing all pairwise means
TukeyHSD(iris_mod, ordered = TRUE)
# Q: interpret your results
# Q: write code below to check your Tukey results (ie, the 'diff' column), against the original data
# you can use group_by and summarize to do this

check <- group_by(iris,Species)
summarize(check,mean_sw = mean(Sepal.Width))

# remember R orders levels, and thus chooses the reference level, alphabetically
# Q: write code to check what the current order is
levels(iris$Species)


# you can reorder the factor levels by giving R a new list (this is a base R way, which a bit easier than the tidyverse way, imo)
# let's put them in ascending order, which looks nice on plots
iris$Species = factor(iris$Species, levels = c("versicolor", "virginica", "setosa"))
levels(iris$Species)
# now your reference level and therefore some of your output will be different, although the F and p value should be the same
iris_mod <- lm (Sepal.Width ~ Species, data = iris)
summary(iris_mod)
# Q:  what components of the summary() output change when you reorder levels, and why?  what components stay the same and why?


# fifth, plot model results back onto the data picture
# the tidyverse function 'augment; doesn't work on anova models, so we need to do this a clunkier way with the base R function 'predict'
# getting the mean for each level and the associated se 
iris_mean_se = predict(iris_mod, se=TRUE, newdata=data.frame(Species=c("versicolor", "virginica", "setosa")))  # the newdata argument is how we tell R to predict for levels, not individual data points
# we need to make a new dataframe to hold this info, because predict doesn't do that automatically
# let's look at the results to check which variable is listed first, etc
iris_mean_se
# the code below sets up 3 columns for species, mean, se, and fills them with the values calculated above
output= data.frame(Species = c("versicolor", "virginica", "setosa"), # being very careful to get the species in the right order here!
                   mean = iris_mean_se[[1]],
                   se = iris_mean_se[[2]])
output
# let's see if base R kept our levels in the right order
levels(output$Species)
# oops! it doesn't even realize there ARE levels
# did R lose the factor designation for Species?
glimpse(output)  #seems so
# need to make Species a factor first, then manually assign its levels in ascending order so that the plot will look good
# this is the base R way which is more straightforward actually
output$Species = factor(output$Species, levels = c("versicolor", "virginica", "setosa"))
levels(output$Species)
# finally, we can plot the means with error bars (+/- 1 SE)
ggplot(output, aes(x = Species, y = mean)) +
        geom_point(size=3) +
        geom_errorbar(data=output, aes(x=Species, ymin=mean-se, ymax=mean+se), width=.25) + # error bars are added as a line that goes through the mean
        ylab("model estimated mean +/- 1 SE") +
        theme_bw()
# or better, since this allows one to 'eyeball' significant differences, plot the means with 95% CIs
ggplot(output, aes(x = Species, y = mean)) +
        geom_point(size=3) +
        geom_errorbar(data=output, aes(x=Species, ymin=mean-1.96*se, ymax=mean+1.96*se), width=.25)+
        ylab("model estimated mean and 95% CI") +
        theme_bw()
# you can check that you got the right numbers by comparing the means in your plots, to means you calculate from the data
# Q: write code to do this below
check <- group_by(iris,Species)
summarize(check,mean_sw = mean(Sepal.Width))


# the traditional stars and letters indicating significance in anova are easier to add in keynote/powerpoint than in R, so we won't do that here
# okay, let's finally finish this analysis!
# Q: what is your conclusion about the answer to our anova question? state it in terms of the biology and also include your key statistical result(s)
# Q: why do the graphical results of the modelfit (ie, the plot you just made above) look so much more significant than the picture you did at the start (the data and boxplots)?



## TWO WAY ANOVA
# our two-way anova question: do flower color and/or size predict the number of insect visits?

# load the data set FlowerColourVisits.csv
flowers <- read_csv("3_FlowerColourVisits.csv")  # substitute your own file path here
# look at the flowers data in the spreadsheet
glimpse(flowers)
# oops, 'size' and 'colour' should be factors, but they aren't coded that way
# write code below to change the variable type to factor for these two variables
flowers$colour = factor(flowers$colour, levels = c("orange", "yellow", "white","red"))
flowers$size = factor(flowers$size, levels = c("small", "medium", "large"))


# full disclosure: the flower sizes are made up for the purposes of making an interaction plot.  all the other data used in class exercises are real
glimpse(flowers)
# it's a good idea to know what your levels are, for a factor variable
# write code below to check the levels

levels(flowers$colour)
levels(flowers$size)

# Q: for each variable, which level will R use as the reference level? 


# first, data pictures
ggplot(flowers, aes(x = size, y = number.of.visits, color = colour)) +
        geom_point() +
        theme_bw()
# ugh! that was a bad idea. the colors are all wrong
# rather than taking the time to correct all the colors manually, let's just plot it the other way
ggplot(flowers, aes(x = colour, y = number.of.visits, color = size)) +
        geom_point() +
        theme_bw()
# Q: Can you eyeball-check any anova assumptions here?
# now that we have two variables, we need to think about potential interactions
# if two variables x1 and x2 interact, that means that the answer to the question 'how does y vary with x1?' depends on the level of variable x2
# and conversely - how y varies with x2 will depend on the level of x1
# thus an important data picture is the interaction plot
# for this we need the mean number of visits for all combinations of flower x colour
# remember piping reads from left to right, and means 'and then'
sum_flowers <- flowers %>%
        group_by(colour, size) %>%   # this line creates the 4x3  possible groups
        summarize(meanV = mean(number.of.visits) )   # this line creates a new variable meanV that is the mean number of visits per group
sum_flowers
# we want to connect the means for flowers of the same size, to see how the number of visits changes with flower color when size is held constant
# to do this, within aes() we define the 'group' that we want lines to connect (here, group = size)
# then geom_line will connect the dots within each group
# we will also make the groups different colors (color = size), so that things are easier to see
ggplot(sum_flowers, aes(x = colour, y = meanV, color = size, group = size)) +
        geom_point() +
        geom_line() +
        theme_bw()
# Q: what do you predict for the answer to our anova question, with respect to main effect of colour, main effect of size, and colour x size interaction?
# Q: explain to your partner what the difference is between two variables being correlated, versus the two variables having an interaction

# second, run model
# we want three predictors: colour, size, and the colour x size interaction
flowers_anova <- lm (number.of.visits ~ colour*size, data = flowers)
# these three ways of specifying our model are equivalent in R: colour + size + colour:size, colour*size, colour + size + colour*size
# we did it the way we did above because of a known bug in leveneTest, which wants the model specified this way

# third, check assumptions
# first, it's a good idea to check how many data points you have in each of your groups
# anova is more robust to violations of assumptions when the design is 'balanced,' ie, has similar sample size per group
table(flowers$size, flowers$colour)
# hmm, some small sample sizes here
# tests of normality and equal variance don't have much meaning for distributions this small
# we'll go ahead and do the tests anyway
# two-way anova makes the assumption that all groups, of which there are 12 here (3x4), are reasonably normal and have similar variances
leveneTest(flowers_anova)
# Q: interpret your levene's test. you will need to figure out what the null hypothesis of a levene's test is, to do so
# let's look at histograms all 12 groups
flowers_groups <- flowers %>%
        group_by(colour, size)
flowers_groups
ggplot(data=flowers_groups, aes(x=number.of.visits)) + 
        geom_histogram(bins = 10) + 
        facet_wrap(colour~size)  # this is how to get facet_wrap to break the plots out by two variables, not just one
# Q: what do you conclude from these histograms?

# fourth, interpret results 
# first though, a Warning! 
# R's default anova() function in rstats, which we have used thus far, should not be used for two-way anova
# the reason is, it uses type I sum of squares, which means that your results can change depending on the order in which you enter your predictor variables in the model
# we will use library(car)'s Anova(), which allows you to specify that you want type III sum of squares, for which the order of variable entry will not matter
Anova(flowers_anova, type=3)
# Q: how similar are the results to what you predicted at the start?
# but - where is our overall F and p value for the whole test ?!
# actually, you usually don't get one in two-way anova
# that's because 2-way anova is usually interpreted differently from a multiple regression model
# the F-test in the regression model is asking whether the full model is significantly better than the intercept-only model  
# this is useful because in regression you are generally interested in finding the most parsimonious model; therefore predictors are evaluated in a cost-benefit context, as in AIC
# in contrast, more usually in anova, the goal is to determine which predictors have levels that are different from each other, rather than building the most parsimonious model
# thus there is an F-test for each factor, but usually not an overall F
# that said, if you wanted a whole model F you could get it by adding up the MS for all the factors and dividing by MSerror (ie, MS residuals)
# this works because variance is additive, ie, you can add it (whereas you can't add sd for instance)
# lastly, remember that when there is an interaction in two-way anova, you shouldn't interpret the main effects separately from the interaction

# fifth, plot model results back onto the data picture
# because this is anova, we can't use tidyverse 'augment'; we have to use base R 'predict' to get the relevant means and se for the model fit
# we define newdata as a df with one row per level of the 2-way anova
# R will recognize this as making one prediction per level (as opposed to one prediction per data point, as for regression)
# also have to create a dataframe to hold the mean and se output that predict returns
flowers_mean_se = predict(flowers_anova, se=T, newdata=data.frame(colour=rep(c("orange", "red", "white", "yellow"),3), size=c(rep("large",4), rep("medium",4),rep("small",4) )) )
# the mean is returned by default (to see it, check flowers_mean_se$fit), se you have to specify with =T (check flowers_mean_se$se.fit)
output= data.frame(
        colour=rep(c("orange", "red", "white", "yellow"),3),
        size=c(rep("large",4), rep("medium",4),rep("small",4) ),
        mean = flowers_mean_se[[1]],
        se = flowers_mean_se[[2]])
output

ggplot(output, aes(x = colour, y = mean, color=size, group=size)) +  # group defines the points that should share a line
        geom_point(size=3) +  # 3 makes the points bigger than usual
        geom_line() +  # draws a line through the points within a group, as specified in aes; ggplot will get the line color right on its own
        geom_errorbar(data=output, aes(x=colour, ymin=mean-se, ymax=mean+se), width=.25) +  # tells R to draw error bars from the mean to one se up or down; width is specifying the horizontal line at the end
        ylab("model estimated mean +/- 1 SE") +  # always say what your error bars represent, as part of ylab!
        theme_bw()

# Q: compare the model fit plot you just did,  to the data plot you did earlier. how do they look different? why?
# Q: state your conclusion from this analysis 




## MULTIPLE REGRESSION
# our multiple regression question: do car weight and horsepower predict mpg?

# get data
data(mtcars)
glimpse(mtcars)

# first, data pictures of the variables separately
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()
# write code below to look at mpg versus hp
# and also wt versus hp, to check whether the predictors are correlated

ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point()
ggplot(mtcars, aes(x = wt, y = hp)) +
        geom_point()

# here is another fun picture activity you can do, wrt multiple regression
# you can plot the second variable against the residuals of the first variable
# this is a bit like what multiple regression is, in fact, doing
# doing it manually lets you see the relative effect of the two variables separately, which should match up with your eventual multiple regression results
# first, re run the univariate regression if it is no longer in R's memory
car_mod <- lm (mpg ~ wt, data = mtcars)
# then add the residuals from this model as a new column
mtcars$car_mod_resid = car_mod$residuals
mtcars
# plot the second predictor (hp) against the residuals from car_mod
ggplot(mtcars, aes(x = hp, y = car_mod_resid)) +
        geom_point()
# hp does explain significant variation even after wt is accounted for - which matches our intuition from the picture
summary(lm (car_mod_resid ~ hp, data = mtcars))



# second, run multiple regression model
car_mod2 <- lm(mpg ~ wt + hp, data=mtcars)

# third, check assumptions
hist(car_mod2$residuals)
plot(car_mod2)
# Q: for each plot, write down what you would conclude
# reminder, the rule of thumb is, Cook's D = 1 indicates a potentially too influential datapoint
# in practice one can just test influence empirically: re run the analysis without a potentially influential data point, and if the overall answer changes, you don't put much faith in your results because they rely on one data point
# for multiple regression, also very important to check for multicollinearity
# first have a look at correlation of the two predictors
cor(mtcars$wt, mtcars$hp)
# the rule of thumb is that anything less than 0.7 is generally considered ok
# we only have two predictors here, so we're done
# but once you have three or more variables, it's better to check the variance inflation factor (VIF) for each predictor
# the VIF is based on the r2 between one predictor (as outcome) with all the other predictors (as predictors)
# let's use it just for practice. vif should be less than 10 for each predictor
vif(car_mod2)
# just a heads up: correlations among predictor variables, ie 'multicollinearity' is common in ecological data
# it can make your model unstable, ie, the coefficients of particular variables can change a lot when you add or remove other variables
# multicollinearity doesn't invalidate your overall model R2 though


# fourth, interpret results
# write code below to get your results
summary(car_mod2)



# note that for multiple regression, you get both a whole model R-squared and a whole model adjusted R-squared
# it is better to use the adjusted one, which adjusts for the number of predictor variables in the model
# R-squared will keep increasing with the number of predictors added, even if they don't really explain anything
# Q: given that wt and hp look similarly predictive on the scatterplots, and both are highly significant, why are the beta values (the slopes) so different? 
# Q: Does the much steeper slope of wt mean that it is a more important variable in this analysis?

# fifth, plot results onto data picture
# we can do this pretty well with 2 predictors, by using a 3D plot and adding our multiple regression fit as a plane
# with more than 2 predictors, forget it, tho
plot3d <-scatterplot3d(mtcars$wt,mtcars$hp,mtcars$mpg, pch=16, type="p")  # type 'p' gives the points
plot3d$plane3d(car_mod2)  # this syntax seems a little weird, but what's it's saying is, add a 3d plane representing the fit of car_mod2, to the existing plot 'plot3d'

# Q: Go back to the question that motivated the analysis and answer it, using the model coefficients (ie, your results) in your answer.
# remember that once you have more than one predictor variable (multiple regression, ancova, etc), when you interpret the results for any one variable you need to add 'while holding (the other variable(s)) constant'




## ANCOVA
# this exercise is modified from Beckerman et al 2017, Getting started with R
# our ancova question: does the mean number of eggs laid by limpets vary with limpet density and/or season?
# remember ancova is just a linear model with both continuous and categorical variables

# get some data on egg-laying by limpets
limpet <- read_csv("3_limpet.csv")  
glimpse(limpet)  # just to be clear, 'eggs' is eggs laid per limpet

# first, data picture
ggplot(limpet, aes(x = DENSITY, y = EGGS, color = SEASON)) +
        geom_point() +
        scale_color_manual(values = c(spring="green", summer="orange")) +  # this is how you set colors manually in ggplot, if things like this are important to you
        theme_bw()  # this gets rid of the gray excel-like background which some find objectionable
# Q: what is your prediction about the answer to the ancova question?

# in a model like this with a continuous and a categorical variable, ggplot is super handy for eyeballing slopes and intercepts, because it can fit a line through each level of the categorical variable
ggplot(limpet, aes(x = DENSITY, y = EGGS, color = SEASON)) +
        geom_point() +
        scale_color_manual(values = c(spring="green", summer="orange")) +  
        geom_smooth(method = 'lm', se = FALSE) +  # remember geom_smooth fits a loess unless you specify a particular model fit
        theme_bw()
# Q: use the picture to estimate the slope for each season
# Q: do you think the effect of density depends on season? that is, do you predict an interaction? explain why or why not
# Q: estimate the intercept(s).  can the intercept be meaningfully interpreted in this model?
# some reminders about units
# units of intercept are units of Y variable
# units of slope are units of Y variable per unit of continuous X variable
# units of categorical X variable are same as intercept, bc it is an adjustment to the intercept


# second, run model
limpet_mod <- lm(EGGS ~ DENSITY + SEASON + DENSITY*SEASON, data = limpet)
# Q: how would have R interpreted a model specified as lm(EGGS ~ DENSITY*SEASON, data=limpet)?
# look at all the output info R stores for you!!   under environment / data
# if that felt overwhelming, you can just look at the names of the things that are stored
names(limpet_mod)
# fortunately, there are only 3 types of output you'll need to use often:
# coefficients: the estimated coefficients (the intercept and all the slopes, ie, Betas)
# residuals: the residual associated with each original datapoint.  They are in the same order as the original data
# fitted.values: the model-estimated y value associated with each original data point, based on its predictor values

# third, check assumptions
autoplot(limpet_mod)
# Q: state what assumption you are checking in each plot and what you are looking for. are there any assumptions of ANCOVA that you are not able to check here?

# fourth, interpret results
# this gets a bit hairy, for ancova. both summary() and anova() are relevant
# but don't panic. it's actually an easy model to visualize because it's just two parallel lines
anova(limpet_mod)
# here is an interpretation of the anova table from the top row down
# DENSITY: R estimated a joint slope of eggs laid as a function of density for both seasons, and it explained lots of variance, with MS 5.0
# the 'variance explained' is relative to an intercept-only model, which is a flat line at the mean value of eggs laid
# SEASON: R estimated different intercepts for the two seasons, and that explained some more variance, for a MS of 3.3
# what the 'variance explained' actually means in this case: R switched from measuring residuals for all data points based on a single fit line,
# to measuring the residuals of 'summer' data points from the 'summer' line, and residuals of 'spring' data points from the 'spring' line
# DENSITY:SEASON  R allowed the slopes to vary (the interaction), but doing so didn't explain much; MS 0.01
# it would be fine to drop the interaction term from the model at this point

summary(limpet_mod)
# coefficients table: here is where we get our intercept and slope
# Q: write the model equation for egg production in the spring in y = a + bx form, filling in all coefficients and variable names
# SEASONsummer estimate is the difference between spring and summer in terms of egg production. in other words, a shift in the intercept
# Q: write the model equation for egg production in the summer in y = a + bx form, filling in all coefficients and variable names
# if the interaction mattered (but it doesn't) you could use the DENSITY:SEASONsummer interaction coefficient to flatten the slope for summer by 0.003
# the bottom line: the model shows that egg production by limpets can be pretty well explained by season and how many other limpets are around: R2 of 0.67, p<0.0001
# but in the results table for the various coefficients, R is reporting t tests. why? 
# a t-test asks if the difference between 2 values is different from zero
# so, what two values? is the question.
# intercept: is intercept different from 0. yes
# density: is slope of density different from 0. yes
# SEASONsummer: is difference between the intercept of summer and spring different from 0. yes
# DENSITY:SEASONsummer: is difference between the slopes of density between spring and summer different from 0. no


# fifth, plot results onto data picture
# we can just plot the estimated lines on the original data picture, being sure to add the coefficients correctly
ggplot(limpet, aes(x = DENSITY, y = EGGS, color = SEASON)) +
        geom_point() +
        scale_color_manual(values = c(spring="green", summer="red")) +  
		geom_abline(intercept = coef(limpet_mod)[1], slope = coef(limpet_mod)[2], colour="green") +
		geom_abline(intercept = (coef(limpet_mod)[1]+coef(limpet_mod)[3]), slope = (coef(limpet_mod)[2]), colour="orange") +
        theme_bw()
# Q: in the geom_abline lines above, explain why the intercept and slope are defined as they are in each case
# hint: look at the Estimate column in your summary() output
# Q: state your conclusion from this analysis. as always, interpret your results (coefficient values) in terms of their biological meaning
# some concluding insights about model geometry, aka, visualizing what your models are doing
# in model geometry in general, the coefficients of categorical variables are intercepts, and the coefficients of numerical (continuous) variables are slopes
# a model with one numeric, one categorical variable is a parallel (or if an interaction, not parallel) lines model
# a model with two numeric variables is a plane
# a model with two numeric, one categorical variable is parallel planes
# Q: discuss these model geometries with your partner



## VARIABLE STANDARDIZATION
# often in more complex models, variables are 'standardized' to Z scores
# centering a variable means subtracting the mean from all data points, so that the mean of the 'new' variable is zero
# standardizing means changing both the mean and the sd - more on that below
# let's center the hp variable, so that we can interpret the intercept as the mpg of a car of average weight, as opposed to the mpg of a car of weight=0 
# Q: what do you predict about the intercept, slope, p value, and R2 of the model run on the centered variable - which will be the same as the uncentered version, and which will change?

# just as a baseline, let's run the univariate model on the original hp variable, and plot it
car_mod3 <- lm(mpg ~ hp, data = mtcars)
summary(car_mod3)
ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point()

# now let's try centering (but not standardizing) the variable hp
mtcars$hp_centered = scale(mtcars$hp, center=TRUE, scale=FALSE) # center=true subtracts the mean from all data values, scale=true divides all data values by the sd 
car_mod4 = lm(mpg ~ hp_centered, data = mtcars)
summary(car_mod4)
# Q: were you right in your predictions above?

# let's try plotting the data points with the model fit added to the plot
ggplot(mtcars, aes(x = hp_centered, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = coef(car_mod4)[1], slope = coef(car_mod4)[2]) +  # check environment/data/car_mod4 to confirm we filled in the right values for coefficients
        geom_vline(xintercept = 0, linetype="dashed") # this just draws a line at 0, for reference

# now in addition to centering the variable, we are going to standardize it
# 'standardizing' means subtracting the mean AND dividing by the standard deviation
# so x axis tick marks are now in units of standard deviations
# this is also called a z score
# Q: what are the units of a z score? why? how does this make a z score particularly useful (or not)?
# Q: what do you predict about the intercept slope, p value,  and R2 of the model run on the standardized variable - will they all be the same as the unstandardized version?

mtcars$hp_standardized = scale(mtcars$hp, center=TRUE, scale=TRUE) 
car_mod5 = lm(mpg ~ hp_standardized, data = mtcars)
summary(car_mod5)
# Q: were you right in your predictions above?

# plotting the data points for the standardized variable, with the model fit added to the plot
# write code to plot this below
ggplot(mtcars, aes(x = hp_standardized, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = coef(car_mod5)[1], slope = coef(car_mod5)[2]) +  
        geom_vline(xintercept = 0, linetype="dashed")




# say we want to standardize both variables (x and y), i.e., do the entire analysis in terms of z scores
# clearly now the slope and intercept will both be different because the y axis values have changed, but the R2 and significance should be the same

mtcars$hp_z = scale(mtcars$hp, center = TRUE, scale = TRUE) 
mtcars$mpg_z = scale(mtcars$mpg, center = TRUE, scale = TRUE)
car_mod6 = lm(mpg_z ~ hp_z, data = mtcars)
summary(car_mod6)

# Q: what is the value of the intercept?  why is the p value for the intercept what it is?
# Q: how do the overall R2, F, and p value compare to the original (uncentered) version of the model?

# take a look at this model
ggplot(mtcars, aes(x = hp_z, y = mpg_z)) + 
        geom_point() + 
        geom_abline(intercept = coef(car_mod6)[1], slope = coef(car_mod6)[2]) +
        geom_vline(xintercept = 0, linetype="dashed") +
        geom_hline(yintercept = 0, linetype="dashed")

# now let's run the multiple regression with both wt and hp, with all 3 variables standardized

# write code to do that below
mtcars$wt_z = scale(mtcars$wt, center = TRUE, scale = TRUE) 
car_mod7 <- lm(mpg_z ~ wt_z + hp_z,data = mtcars)

summary(car_mod7)



# Q: how do the slopes of both hp and hp compare to the original (uncentered) version?
# Q: what can you conclude about comparing slopes between variables when variables are expressed in their original units, versus expressed as z scores? 



## EXERCISE 1
# goal of this is to build your intuition about the relationship between sample size, p value, and effect size, which in this case is r
# first create two vectors, one for x values and one for y values
# runif_x randomly samples 10 values from a uniform distribution with min = 0 and max = 100
unif_x <- runif(100, 0, 100)
# complete the code in unif_y so that it does the same thing
# write code below to create random y values the same way
######  delete code below #######
unif_y <- runif(100, 0, 100)
# these numbers should not be correlated except by chance
plot(unif_x, unif_y)
cor(unif_x, unif_y)
cor.test(unif_x, unif_y)
# run the code above repeatedly until you get a significant positive correlation and/or r >= 0.60, which is a rule of thumb for a 'reasonably strong' correlation
# Q: how many tries did it take?  
# Q: what do you predict will happen if you repeat the exercise with sampling only 5 values? 100 values?
# Q: try it and see. suggestion:  don't spend too long on the n=100 version  
# Q: explain what you think is going on here, in terms of sample size (n), effect size (r), p value, and the relationship among them




## EXERCISE 2
# the goal of this exercise is to develop your intuition about why the the sampling distribution of the mean (ie, the standard error) is what it is, and why the central limit theorem works

# part one
# on what the standard error is
# first let's plot a distribution of data for a standard normal distribution, just to compare it to the distribution of means later
hist(rnorm(1000), xlim = c(-3,3), breaks = 30, main = 'distribution of data')

# then let's take a bunch of samples from a normal distribution and see how the means of these samples are distributed
# you don't need to worry about how to code this part; the point is just to understand what distribution it's creating
means <- function(sample_size, number_samples) {            # defines the function 'means' with arguments sample_size (ie number of random draws per sample) and number_samples (this will be the number of means plotted in the end)
        output <- vector("double", length = number_samples)    # define a numerical vector to store the output, long enough to hold one value per sample, which will be the mean of that sample
        for(i in 1:number_samples) {                       # do the stuff between brackets once for each sample
        g <- rnorm(sample_size)                            # randomly draw n values from a normal distribution and call this set of numbers 'g'
        output[[i]] <- (mean(g))                          # take the mean of g, put it in a vector called output, and then draw a new sample
        }
     hist(output, breaks = 30, xlim = c(-3,3), main = 'distribution of means')
}
means(50, 1000)  # this takes 1000 samples of 50 data points each from a normal distribution with mean = 0 and standard deviation = 1

# Q: how is the distribution of the means different from the distribution of the data?
# Q: was this what you expected? why or why not?
# Q: what do you predict will be the effect of increasing sample_size?  ie, how will the histogram change
# Q: what do you predict will be the effect of increasing number_samples?  ie, how will the histogram change
# now test your predictions:
# Q: first run means() with sample_size = 5, then = 50, then = 500, both times using number_samples = 10000
# Q: then run means() with sample_size = 50 and number_samples = 10, then number_samples = 1000, then = 10,000
# Q: were your predictions right?  why or why not?
# note that this picture shows the situation you are in when you are estimating the true mean of a population based on a sample of one mean taken from that population
# in that case, you have ONE of the sample means that are plotted here, and are trying to choose CI around your mean that are likely to include the true mean (which is 0 in this case)
# Q: what equation would you use for the CI?


# part two, on the central limit theorem
# Q: what is the CLT?  that is, what does it say
# Q: what do you predict will happen when we change the distribution to a uniform one? 
# Q: try it with the code below and see
# Q: any thoughts about why the distribution of means is normal even if the values going into the means come from a distribution that is not at all normal?
# there is a hint below if you need it

# distribution of data for uniform distribution
hist(runif(1000), xlim = c(0,1), breaks = 30, main = 'distribution of data')
# distribution of means for uniform data distribution
means <- function(sample_size, number_samples) {            # defines the function 'means' with arguments sample_size (ie number of random draws per sample) and number_samples (this will be the number of means plotted in the end)
        output <- vector("double", length = number_samples)    # define a numerical vector to store the output, long enough to hold one value per sample, which will be the mean of that sample
        for(i in 1:number_samples) {                       # do the stuff between brackets once for each sample
                g <- runif(sample_size)                            # randomly draw n values from a uniform distribution and call this set of numbers 'g'
                output[[i]] <- (mean(g))                          # take the mean of g, put it in a vector called output, and then draw a new sample
        }
        hist(output, breaks = 30, xlim = c(0,1), main = 'distribution of means from uniform data')
}
means(50, 1000)

# because runif is defined from 0 to 1, the mean is 0.5
# if you aren't sure why the distribution of means ends up normal even for a uniform distribution, draw this picture for yourself on a piece of paper:
# both axes are equally likely values in a uniform distribution, eg 1,2,3,4,5,6,7,8
# cell values are the sum of the two axes
# this is a picture of what you are doing when you draw sample sizes of 2 from a uniform distribution and then take the mean (well, it's the sum actually, but the distribution will be the same because the mean is just the sum divided by a constant)
# ask yourself what the distribution of the cell values will look like and why



