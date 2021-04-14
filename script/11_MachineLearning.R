#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('ML_helpers.R')

set.seed(100)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

a <- amniote_data %>% 
    rename(y = litter_or_clutch_size_n)%>%
    select(where(~!all(is.na(.x)))) %>%
    mutate(y = log1p(y)) %>%
    filter(!is.na(y)) %>% 
    filter(Class == "Reptilia")%>%
    dplyr::select(-weaning_d, -weaning_weight_g, -fledging_age_d, -fledging_mass_g, -female_body_mass_at_maturity_g)


##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.
ggplot(a, aes(y)) + geom_histogram(color = "black",fill = "white")
ggplot(a, aes(x = Order, y = y)) + geom_boxplot()


##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.

a <- a %>%
    dplyr::select(-Class,-Order,-Genus,-Species,-Family,-scientificNameStd, -common_name)

preprocesses <- preProcess(a, method = 'medianImpute')
a_impute <- predict(preprocesses, a)

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?
# A: We need the log-transform to get the normal distribution for linear models(base model) and the elastic net model.
dim(a_impute)
names(a_impute)

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(a_impute)){
            hist(a_impute[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}

log_cols <- c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,17,18,20,21,22,23)

a_impute[, log_cols] <- log1p(a_impute[, log_cols])

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(a_impute)){
            hist(a_impute[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}

##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?
# A: Comparing to the mammal model, the reptile model got the similar result. The R squre value for reptile and mammal are
# close. For reptile, R square is 0.329 and for mammal is 0.336.The variables for mammal and reptile might work differently in 
# the model (some have negative coefficient in mammal(AdultBodyMass_g) but some have positive in reptile...)
folds <- createFolds(a$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

apriori_formula <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + litters_or_clutches_per_y + maximum_longevity_y
rep_r0_lm <- train(apriori_formula, data = a_impute, method = 'lm', trControl = trcntrl, na.action = na.omit)

plotCV(rep_r0_lm)

rep_r0_lm

summary(rep_r0_lm$finalModel)

##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?
# A: By looking at the max R squared value or the plot, we can say this model is a little bit better than the lm model.
# The R sq value for this model is 0.357, a little bit higher than lm model.
enet_gr <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
rep_r1_enet <- train(y ~ ., data = a_impute, method = 'enet', tuneGrid = enet_gr, trControl = trcntrl, na.action = na.omit)
plotCV(rep_r1_enet)

rep_r1_enet$results$Rsquared %>% max
rep_r1_enet$results %>%
    ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
    geom_line() +
    geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')

##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?
# A: Yes, in this case, the R squared vlue is 0.465, the Gaussian process model perform much better than lm.
gp_gr <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
rep_r2_gp <- train(y ~ ., data = a_impute, method = 'gaussprRadial', tuneGrid = gp_gr, trControl = trcntrl, na.action = na.omit)

rep_r2_gp$results %>% ggplot(aes(sigma, Rsquared)) +
    geom_line() + geom_point() + xlab('Sigma')
plotCV(rep_r2_gp)
rep_r2_gp
rep_r2_gp$results$Rsquared %>% max


##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model?
# A: The smallest value of min node size is selected (5, with highest R sq) means the noise in the model is small.
##What does the number of random predictors selected indicate about interaction depth?
# A: The selected random predictors is 5 and it's small, which indicates little or no interactions between covariate.
rf_gr <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
rep_r3_rf <- train(y ~ ., data = a_impute, method = 'ranger', tuneGrid = rf_gr, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

##Plot # of random predictors and minimum node size vs R2 (recreate figure 1c)

rep_r3_rf$results %>%
    ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
    geom_line() +
    geom_point() +
    labs(colour = 'min.node.size')

plotCV(rep_r3_rf)

rep_r3_rf
rep_r3_rf$results$Rsquared %>% max


##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?
compare_models(rep_r2_gp, rep_r3_rf)
compare_models(rep_r1_enet, rep_r3_rf)
compare_models(rep_r0_lm, rep_r3_rf)

# By looking at the R sq values for the three models, we can know that the random forest is the best model, with the highest 
# R sq value (0.65). The linear model perform the worst, with the lowest R sq value. But I also want to say the Elastic net model is 
# as bad as the linear model...Both Elastic net and linear models have lower correlation with random forest and lower R sq around 0.3.
# For the mammal analysis, the best model is still random forest. But in this case, the Gaussian process model also have a good 
# R sq (around 0.6) and showed high correlation with random forest. I think it depends on the data and the special cases. I cannot
# say it's universally good for all the data, but it performed really well in these two cases.


##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 
varImp(rep_r1_enet)
varImp(rep_r2_gp)
varImp(rep_r3_rf)
# elastic net: adult_body_mass_g
# gaussian process: adult_body_mass_g
# random forest: no_sex_svl_cm 

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.
##How do they differ?
##What does this say about the likely relation ship between litter/clutch size and the best predictor variable?

partial(rep_r2_gp,pred.var = c('adult_body_mass_g'),plot = TRUE)
partial(rep_r3_rf,pred.var = c('no_sex_svl_cm'),plot = TRUE)
# The Gaussian process model gave us a smooth nonlinear relationship between adult body mass and litter/clutch size.
# The random forest model shows a line with more ups and downs, which is not as smooth as the gaussian process.
# For the gaussian process, we can see a positive relationship before body mass = 10, after that, I see a little drop.
# For the random forest, the positive relationship reaches stable after svl = 3.




