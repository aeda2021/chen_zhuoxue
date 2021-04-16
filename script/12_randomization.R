## Goal of this assignment
# To learn how to use randomization methods: bootstrapping, permutation, and simulation for error propagation

## How to complete this assignment
# read through the comments and run the code below
# whenever you see a comment line that begins with Q
# answer the question in a comment line
# if answering the question requires you to write code, do so
# when you are finished, push your completed code to your Github repo

## Outline
# bootstrap
# permutation
# simulation
# the package infer, which can do all of these

rm(list = ls()) 
# load libraries
library(tidyverse) 
library(infer) # tidyverse way to do randomization tests
library(fitdistrplus) # for fitdist, which fits distributions to data

## WHAT DO RANDOMIZATION TESTS DO?
# randomization tests that use a null model ask the same general question that traditional stats tests do:
# is the effect/difference in our observed data real, or due to chance? 
# same as with traditional stats tests, we start by assuming that the observed data came from some world where “nothing is going on” 
# i.e. the observed effect was simply due to random chance
# and call this assumption our null hypothesis
# we then calculate a p value for the observed results
# bootstrapping is a randomization method, but it does not use a null model
# bootstrapping is used to estimate population parameters
# such as mean, median, sd, etc
# and more crucially to estimate the CI around those point estimates

## BOOTSTRAP
# bootstrapping is a method for drawing inferences about a population based on having only one sample
# for example, you can estimate the population mean and CI
# in this case you would get basically the same answer by using 1.96*SE
# but there are some population parameters for which no formula exists
# for example, the median
# so bootstrapping is super useful for these
# you can even use bootstrap to estimate things like odds ratios, regression coefficients, proportions, etc
# a big advantage of bootstrapping is that it has NO assumptions
# other than that your data are an unbiased sample from the population
# lastly bootstrapping useful when your sample size is too small for traditional stat methods (rule of thumb, n<30).
# a term to know: 'point estimate' is used for single-value estimates such as mean, median, sd, etc
# usually people report a point estimate along with the CI around it

# a bootstrapping example: say we want to estimate the frequency of pennies by mint year
# in the true population of all pennies in the USA
# we were too lazy to survey them all
# so we took a sample of 50 pennies instead
# we now want to estimate the mean and CI for the true population
pennies_sample <- tibble(
  year = c(1976, 1962, 1976, 1983, 2017, 2015, 2015, 1962, 2016, 1976, 
           2006, 1997, 1988, 2015, 2015, 1988, 2016, 1978, 1979, 1997, 
           1974, 2013, 1978, 2015, 2008, 1982, 1986, 1979, 1981, 2004, 
           2000, 1995, 1999, 2006, 1979, 2015, 1979, 1998, 1981, 2015, 
           2000, 1999, 1988, 2017, 1992, 1997, 1990, 1988, 2006, 2000)
)
hist(pennies_sample$year, breaks = 30)
# a bootstrap sample takes a sample of the same size as the observed
# sampling from the exact same distribution each time it draws a penny
# ie, with replacement
pennies_resample <- pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 1)
hist(pennies_resample$year, breaks = 30) # I'm going to standardize the number of breaks to 30 to make it easier to compare histograms
# Q: how do the two distributions compare?
# A: We can see those two distributions are similar to each other. They all show some high frequencies in later years.

# Q: do the two distributions have the same mean? write code below to find out
mean(pennies_sample$year)
mean(pennies_resample$year)
# A: No, these two means are slightly different. In my case, I saw the resample data is around 1993 and the original one is around 1994.

# Q: what would happen if you sampled without replacement?
pennies_resample1 <- pennies_sample %>% 
  rep_sample_n(size = 50, replace = FALSE, reps = 1)
hist(pennies_resample1$year, breaks = 30)
# A: Without replacement, our resample data will have the same mean as the original one. The histogram distribution
# will be exactly the same as the original one.

# now let's increase our number of reps
pennies_resample <- pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 100)
pennies_resample <- group_by(pennies_resample,replicate)
pennies_resample_means <- summarize(pennies_resample,mean_yr = mean(year))
# take a look at what R gives you for output here
# Q: plot the distribution of means for these 100 replicate re-samples

pennies_resample_means <- pennies_resample %>% 
  group_by(replicate) %>% 
  summarise(mean_yr = mean(year))
hist(pennies_resample_means$mean_yr, breaks = 30)

# the distribution of means that you just plotted is a bootstrap distribution
# Q: explain how you can get a pretty spread-out distribution of means by re sampling a single sample
# A: The sample size of resample data is too low to explain the situation. In this case, some
# extreme values can screw up our mean distribution.

# it turns out the bootstrap distribution of the mean approximates the sampling distribution of the mean
# ie, it approximates what you would get if you actually resampled from the population
# so long as the sample is randomly selected from the population,
# it will represent both the central tendency and the variation within that population
# and thus repeated draws from the sample will approximate repeated draws from the actual population
# Q: write code to check what happens as you decrease the number of reps 
# make a prediction first
# If we decrease the number of replicates, the mean distribution will be less likely to show the true situation.
pennies_resample_d <- pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 20)
pennies_resample_d_mean <- group_by(pennies_resample_d,replicate)
pennies_resample_d_mean <- summarize(pennies_resample_d_mean,mean_yr = mean(year))
hist(pennies_resample_d_mean$mean_yr, breaks = 30)

# fortunately for us, we can always run lots of reps (often people run 10,000 or so) and overcome the above problem
# however the accuracy of the bootstrap estimate will also depend on the sample size of the original sample
# Q: write code to check what happens as you decrease the sample size
pennies_resample_ds <- pennies_sample %>% 
  rep_sample_n(size = 5, replace = TRUE, reps = 100)
pennies_resample_ds_mean <- group_by(pennies_resample_ds,replicate)
pennies_resample_ds_mean <- summarize(pennies_resample_ds_mean,mean_yr = mean(year))
hist(pennies_resample_ds_mean$mean_yr, breaks = 30)
# looks like a normal distribution...

# now let's get the bootstrapped CI
CI <- pennies_resample_means %>%
  summarize(
    l = quantile(mean_yr, prob = 0.025), 
    u = quantile(mean_yr, prob = 0.975)
  )
# wait, what? what did we just do above?
# this is a cool thing!
# because we have a distribution of estimated mean values, we can just include 95% of those in the 95% CI
# recall that one definition of the CI is that it represents the range of values for the mean
# that you would get if you took a bunch of samples from the same population and calculated the mean of each sample
# specifically, for a 95% CI on your sample mean
# you would expect 95% of the newly-sampled means to fall within that 95% CI
# this is essentially what we just did with the bootstrap.
# let's make a picture of our bootstrapping results
ggplot(data = pennies_resample_means, aes(x=mean_yr)) +
  geom_histogram(binwidth = 1, color = "white") +
  geom_vline(xintercept = CI$l, color = "green") +
  geom_vline(xintercept = CI$u, color = "green")
# nice. we now have a distribution of possible population means, with CI, from our single sample of 50 pennies
# Q: write code below to bootstrap the median year
# make the histogram, and add quantiles that include 95% of the estimated medians
pennies_resample_median <- pennies_resample %>%
  group_by(replicate)%>%
  summarize(median_yr = median(year))

CI1 <- pennies_resample_median %>%
  summarize(
    l = quantile(median_yr, prob = 0.025), 
    u = quantile(median_yr, prob = 0.975)
  )

ggplot(data = pennies_resample_median, aes(x=median_yr)) +
  geom_histogram(binwidth = 1, color = "white") +
  geom_vline(xintercept = CI1$l, color = "green") +
  geom_vline(xintercept = CI1$u, color = "green")

# as you can see, medians to not have some of the nice properties that means do (think central limit theorem)
# okay, that's all on bootstrapping
# jackknife is a related, older technique that was used more before computers were powerful enough to bootstrap
# in a jackknife, you create the resampled datasets by dropping data points (rather than by resampling with replacement)
# in general, bootstrapping does a better job, so most people use that

## PERMUTATION
# permutation is a way of creating your own null distribution to test against
# to use it, you need to have data in matrix format
# a big advantage of permutation is that is has NO assumptions
# other than that the data points are independent of each other and are an unbiased sample from the populaton
# so you can use it when your data don't meet the assumptions of any stats tests
# basic steps in doing a permutation are:
# measure the thing you are interested in
# e.g. body masses of male and female bees
# reshuffle the data to break apart the relationship you are interested in
# e.g., reshuffle the sex column with respect to the body mass column
# for each reshuffle, calculate the measurement you want out of the analysis
# e.g. female mass - male mass
# repeat this reshuffling and calculating thousands of times
# calculate the p value as the proportion of iterations having a calculated value as or more extreme than your empirical value
# so in this example, the interpretation of the p value is:
# if male and female bees did not have different body masses, how likely would it be for me to get the result I got just by chance?
# always good to plot the histogram of your output too
# in this case, a distribution of differences between mean body masses

# let's explore permutation using a data set the number of carpenter bees (Xylocopa virginica)
# visiting an experimental array of 15 plant species (rows) over four years (columns)
# we aren't testing a hypothesis here, but just seeing how permutations work
bee_prefs <- read.csv("12_bee_prefs.csv")
head(bee_prefs)
# Q: are these data tidy?  should they be?
# A: If we just look at the data, it's not tidy because the year is not separated from the bee visits.
# But in this case, we are going to look at the matrix for plant and bee count in years. So, it could be look like this,

# to start, let's get this as a matrix and drop the row and column names, which could cause confusion when permuting
bee_prefs <- as.matrix(bee_prefs[2:5])
colnames(bee_prefs) <- NULL
bee_prefs
# permuting the cell values within each column erases (randomizes) the bee's preference for different plants
# create a matrix to hold the permutation output
bee_prefs_perm = matrix(0,15,4)
bee_prefs_perm
# let's write a for loop to permute the columns one at a time
for (i in 1:4) {
  bee_prefs_perm[,i] = sample(bee_prefs[,i], replace=F)		# note here we are using sample column by column within the loop
}
bee_prefs_perm
# Q: why does sample do a permutation if you set replace to F?
# A: The values will be the same but be shuffled around and put for different plant species.

# check whether column and row sums are maintained
colSums(bee_prefs)
colSums(bee_prefs_perm)
# Q: are column sums maintained?  should they be?
# A: Yes, they are the same and they should be, because we are interested in changing values for plant species not for years.
rowSums(bee_prefs)
rowSums(bee_prefs_perm)
# Q: are row sums maintained?  should they be?
# No, because we are trying to randomize the visits for different plant species.

# Q: write code below to do a permutation similar to that above,
# but now permute the rows instead of the columns
bee_prefs_perm_r = matrix(0,15,4)
bee_prefs_perm_r
for (i in 1:15) {
  bee_prefs_perm_r[i,] = sample(bee_prefs[i,], replace=F)		# note here we are using sample column by column within the loop
}

bee_prefs_perm_r

colSums(bee_prefs)
colSums(bee_prefs_perm_r)

rowSums(bee_prefs)
rowSums(bee_prefs_perm_r)
## SIMULATION
# the term simulation has a fuzzier meaning than bootstrapping or permutation
# people sometimes use it more generally - permutation can be considered a type of simulation, for instance
# here we are going to use a simple simulation to do error propagation
# our goal is to combine data on bee visits to flowers
# and pollen deposited per flower visit
# to estimate the pollination (ie, total number of pollen grains) received by the flowers
# in other words we want to calculate: visits x pollen grains per visit =  pollination
# while also propagating the uncertainty associated with each of these measurements

# first, get a dataset on the number of Bombus (bumblebee) visits to Monarda fistulosa flowers
visits <- read_csv("12_visits.csv")
# date and site are what they seem
# visits is a count of Bombus visits to a standardized array of 9 blooming M. fistulosa plants, that were observed on that site-date
# there are several Bombus species in here, but we are just going to analyze at the level of genus
# the visits data also have units of time, since we observed flowers for set time intervals, but we are not going to worry about that for this analysis
hist(visits$visits, breaks = 30)
# next, get a dataset on the number of M. fistulosa pollen grains deposited in one visit from a Bombus
pollen <- read_csv("12_pollen.csv")
# pollinator_id and plant_sp confirm that we have the right bee genus and plant species
# conspec_grains is the number of conspecific (ie, Monarda fistulosa) pollen grains deposited on the stigma of one flower during one bee visit
hist(pollen$consp_grains, breaks = 50)
# looks like we have a lot of zeros here! is that what we expect?
# in fact yes, because Monarda has flower heads made up of dozens of tiny flowers
# and a 'bee visit' is actually a visit to a flower head
# when a bumblebee visits a flower head and crawls around on it a bit, it only contacts some of the flowers
# but we count grains (or lack thereof) on all the dozens of tiny stigmas that were available to be pollinated during that visit

# to estimate the distribution of pollination per site-day 
# we randomly draw a value from the visits distribution
# and then multiply by randomly drawn values from the pollen distribution

# method 1: sampling from the data itself ('nonparametric', to use the confusing jargon)
# first, set up an empty vector for the output of pollination per site-date
pollination_site_date = c()	
# here is a simple function to estimate pollination for lots of site-dates
# each iteration is one site-date
for (i in 1:10000) {
  visit_draw <- sample(visits$visits, size = 1) 
  pollen_draws <- sample(pollen$consp_grains, size = visit_draw)
  pollination_site_date[i] = sum(pollen_draws)
}
# Q: briefly explain what the function above does
# A: This function simulated the bee visit with possible pollen draws on 10000 site dates.

# Q: why did we use size=visit_draw?
# A: Because for each visit, the bee could carry a random number of pollen.

# looking at our distribution of pollination per site-date
# the width of this distribution now contains the uncertainty from both of the variables we measured in the field
hist(pollination_site_date, breaks = 30, main = "pollination site-date non parametric")
median(pollination_site_date)
mean(pollination_site_date)
quantile(pollination_site_date, prob = 0.025)
quantile(pollination_site_date, prob = 0.975)


# method two: sampling from a distribution based on the data ('parametric')
# so first, we have to fit distributions to our data
# and then use those distributions to sample from in the simulation
# let's do some visual fits first
# we have count data, so poisson is a good first guess
hist(visits$visits, breaks = 50)
mean_vis <- mean(visits$visits)
mean_vis
hist(rpois(173, mean_vis), breaks = 50)
# looks like our bee visits data are too skewed for a poisson
# too many zeros and too many really large values
# we could try a gamma distribution, which has two parameters, and can fit just about anything
# shape parameter = (mean/sd)^2
# scale parameter = sd^2 / mean
sd_vis <- sd(visits$visits)
sd_vis
shape <- (mean_vis/sd_vis)^2
shape
scale <- sd_vis^2 / mean_vis
scale
# Q: write code to look at a few gamma distributions below, and compare to our data
hist(rgamma(173, shape,scale), breaks = 50)
hist(visits$visits, breaks = 50)
# that is a lot better; it seems running it a few times can give a distribution similar to ours

# we could also use fitdist to do the fitting and compare AIC values
# let's try the poisson and gamma from above, plus two others that are plausible
visits_pois <- fitdist(visits$visits, "pois")
summary(visits_pois) # summary gives AIC for the fit, just like for a model fit
visits_gamma <- fitdist(visits$visits, "gamma")
summary(visits_gamma)  # hmm this also got somewhat different parameter values using maximum likelihood, than we did using the formula
visits_nb <- fitdist(visits$visits, "nbinom")
summary(visits_nb)
visits_exp <- fitdist(visits$visits, "exp")
summary(visits_exp)
# Q: which distribution is the best fit, in the end?
# A: By looking at the AIC values, gamma distribution has the lowest, which means it is the best fit.
# Q: write code below to choose a distribution for the pollen data
hist(pollen$consp_grains, breaks = 50)
# pois
mean_p <- mean(pollen$consp_grains)
mean_p
hist(rpois(173, mean_p), breaks = 50)
# gamma looks better
sd_p <- sd(pollen$consp_grains)
sd_p
shape_p <- (mean_p/sd_p)^2
shape_p
scale_p <- sd_p^2 / mean_p
scale_p
hist(rgamma(173, shape=shape_p, scale=scale_p), breaks = 50)

# can also compare AIC
pollen_pois <- fitdist(pollen$consp_grains, "pois")
summary(pollen_pois) 
pollen_gamma <- fitdist(pollen$consp_grains, "gamma")
summary(pollen_gamma) 
# the gamma fit will probably fail to run
# likely because a gamma distribution technically cannot include 0 values, altho it can include values extremely close to 0
# so fitdist won't fit a gamma to our data which contain many zeros
# but it will run with the parameters generated by our data, and will produce values very close to zero, which show up in the zero bar on the histogram
pollen_nb <- fitdist(pollen$consp_grains, "nbinom")
summary(pollen_nb)
pollen_exp <- fitdist(pollen$consp_grains, "exp")
summary(pollen_exp)
# so what to do given that the gamma, which looked great visually, can't be fit with fitdist?
# you could either trust your visual judgment and use a gamma in the simulation, below
# or alternatively, you could use a negative binomial which is pretty similar and can be fit above

# now we are ready to build the simulation
# set up an empty vector for the output of pollination per site-date
pollination_site_date1 = c()	
# a simple function to estimate pollination for lots of site-dates
# each iteration is one site-date
for (i in 1:10000) {
  visit_draw1 <- rgamma(1, scale = scale_p, shape = shape_p) 
  pollen_draws1 <- rgamma(visit_draw1, scale = scale_p, shape = shape_p)
  pollination_site_date1[i] = sum(pollen_draws1)
}

hist(pollination_site_date1, breaks = 50, main = "pollination site-date parametric")
median(pollination_site_date1)
mean(pollination_site_date1)
quantile(pollination_site_date1, prob = 0.025)
quantile(pollination_site_date1, prob = 0.975)

# some last comments on the parametric vs non parametric choice:
# first, in this case, given the large-ish sample size for each distribution, personally I would just use the data
# which saves the bother of fitting distributions
# and based on the mean and median values, it looks like the distributions might have under-estimated anyway
# second, when you have a large data set, both methods generally work well
# the non parametric because you have a fully developed distribution of data to draw from
# and the parametric because you can accurately parameterize this distribution
# sadly, both can work poorly when you have a small data set
# the non parametric because you are re-drawing a small set of specific values each time
# whereas in reality, if you sampled more you would not keep getting these same values
# and the parametric because it's hard to know what the distribution is
# and in that situation, there isn't an obvious reason to choose one over the other
# third, one situation in which you might want to use the parametric versions:
# when you have an outlier in your dataset
# because resampling an outlier can really change your result
# however you also need to be confident in this case that it is indeed an outlier
# and not actually an indication that the real distribution is long-tailed



##  PACKAGE INFER
# infer is a tidymodels / Hadleyverse package for statistical inference
# it can do bootstrap, permutation, and simulation
# it uses the same basic syntax for all three thereby reducing cognitive load
# infer has five main verbs 
# specify() specifies the variable, or relationship between variables, that you’re interested in
# hypothesize() declares the null hypothesis
# generate()  generates data reflecting the null hypothesis
# calculate() calculates a distribution of statistics from the generated data to form the null distribution
# visualize() plots what you did 

# first, get a dataset on the body mass of queens and workers of Bombus griseocollis
# queens overwinter as adults and each founds a colony in the spring
# she then lays successive broods of workers
bee_mass <- read_csv("12_bee_mass.csv")
glimpse(bee_mass)
# first a few examples of what each verb does, then we'll put it all together

# specify can identify the outcome variable
bee_mass %>%
  specify(response = dry_mass_mg)
# if you run just this code, it gives you the tbl, like select would
# more generally, as you run infer code snippets
# the console displays the model object that is being built up
# so that you can check that you have what you thought you had
# specify can also say what relationship between predictor and outcome you want to analyze
bee_mass %>%
  specify(dry_mass_mg ~ caste)

# hypothesize specifies the null hypothesis
bee_mass %>%
  specify(dry_mass_mg ~ caste) %>%
  hypothesize(null = "independence") 
# use "independence" if you you want to ask whether two variables are independent
# ie, one does not predict the other
# or in our case, bees of different castes don't have different body masses

# generate constructs the null distribution based on the null hypothesis
# use the "type" argument to indicate the type of randomization test to use: bootstrap, permutation, or simulation
# bootstrap: samples the same size as your input dataset are drawn from the input dataset with replacement
# ie, R does a bootstrap
# permute: each input value will be randomly reassigned (without replacement) to a new output value
# ie, values are permuted within one of the columns
# you can think of this as the 'labels' (in this case the caste) being randomly assigned to the data values
# simulate: a value will be sampled from a theoretical distribution with parameters specified in hypothesize()
# in infer, the simulate option is currently only applicable for testing point estimates
# so we won't be using it here, since ecologists don't test against point estimates very often

# an example of generate, using bootstrap
bee_mass %>%
  specify(response = dry_mass_mg) %>%
  generate(reps = 100, type = "bootstrap")
# note that R has done the bootstrap already - 66 x 100 rows in the tibble
# note also that I didn't use a hypothesize line, because that isn't necessary for bootstrapping

# an example of generate, using permute
bee_mass %>%
  specify(dry_mass_mg ~ caste) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute")

# calculate tells R what statistic you want to calculate
# use stat = 
# the options are
# “mean”, “median”, “sum”, “sd”
# “prop”, “count”, “diff in means”, “diff in medians”, “diff in props”
# “Chisq”, “F”, “t”, “z”, “slope”, “correlation”
# if you use a stat that takes a difference, you also need to indicate the order for doing the subtraction
bee_mass %>%
  specify(response = dry_mass_mg) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "mean")
# Q: what did R just return here?
# A: R just returned the mean for 100 bootstrapped samples.

# of course, there is no reason to bootstrap to get the mean
# you could just take the mean of the sample (which is a good thing to check against btw)
# the real point of bootstrapping is to get the CI for the mean
# Q: write code below to get the 95% CI for the bootstrapped mean
# you may want to use 10,000 reps which is standard for randomization tests (we were using 100 above just to look at the output)
# an example of generate, using bootstrap

b_b <- bee_mass %>%
  specify(response = dry_mass_mg) %>%
  generate(reps = 10000, type = "bootstrap")%>%
  calculate(stat = "mean")

b_b_q <- b_b %>% summarize( l = quantile(stat, prob = 0.025), 
    u = quantile(stat, prob = 0.975))

# compare bootstrap mean to the data mean
mean(bee_mass$dry_mass_mg)
mean(b_b$stat)
# These two means are close (~29)

# now let's use calculate with a permutation
bee_mass %>%
  specify(dry_mass_mg ~ caste) %>%
  hypothesize(null = "independence")  %>%
  generate(reps = 10000, type = "permute")  %>%
  calculate (stat = 'diff in means', order = c("queen", "worker"))
# Q: explain what the quantities in the output column 'stat' are
# A: The difference of the mean dry mass between queen and worker.
# Q: how will you use these numbers? what are we trying to test here?
# A: I will use these numbers to see if there's any significant difference between queen and workers, will the caste influence
# the dry mass.

# to gain some more intuition about what is going on, let's look at some values
# first re run the permutation above but this time saving the output
null_dist <- bee_mass %>%
  specify(dry_mass_mg ~ caste) %>%
  hypothesize(null = "independence")  %>%
  generate(reps = 10000, type = "permute")  %>%
  calculate (stat = 'diff in means', order = c("queen", "worker"))
mean(null_dist$stat)
# Q: how do you interpret the result for the mean of the replicates? was it what you expected?
# A: The result shows a mean number close to zero. Which is what I expected that the null distribution shows no difference in mean
# for queen and worker.

# now let's look at the data itself without using the permutation
data_means <- bee_mass %>%
  group_by(caste) %>%
  summarize(mean_mass = mean(dry_mass_mg))
data_means
# Q:  what do you expect the conclusion to be from this permutation test?
# A: I expect the conclusion to be there's difference between queen and worker's body mass, the null can be rejected.

# Q: thinking back to our bootstrap exercise: how do you now interpret the 
# bootstrapped results in light of what we just learned about body mass by caste?
# A: The bootstrapped results don't show the difference between queen and worker. It only give us the mean dry mass, which
# could be insuffcient to interpret the data in our case.

# lastly, visualize lets you see what you did
# let's look at our null distribution
null_dist %>%
  visualize()
# note in the plot title infer calls this 'simulation-based' when it's a permutation . . an example of using the term 'simulation' somewhat generally
# let's calculate our observed difference between the mean mass of queens and workers
obs_stat = 36.8 - 18.9  # just retyping the numbers we got above
# with visualize we are now in ggplot world so use the + at end of lines
null_dist %>%
  visualize(binwidth = 50) +
  shade_p_value(obs_stat = obs_stat, direction = "two-sided")
# shade_p_value shades the p value region for our observed value
# ie, the area that is equally or more extreme than the observed value
# and also plots the observed value in red
# in this case tho, it's hard to see these features because queens are SSOO much bigger than workers
# Q: run the code above again after setting obs_stat to something within the distribution, so that you can see how shade_p_value works
obs_stat = 5
null_dist %>%
  visualize(binwidth = 50) +
  shade_p_value(obs_stat = obs_stat, direction = "two-sided")
# Q: write code to calculate the p value for your observed value (ie, your arbitrarily chosen new value of obs_stat) above
p_value <- count(filter(null_dist,stat>5))/nrow(null_dist)
p_value
# p = 0.0257

get_p_value(x=null_dist, obs_stat=obs_stat, direction="two-sided")

