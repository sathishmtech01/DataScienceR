# Linear Regression Use case
# Problem Definition: Predicting the prize of the building using sqft,bedrooms and bathrooms
# Data Gathering
house.data = read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/Linear/2018-05-12_155104.csv")

#library

library(ggplot2)
# Data Undersatanding
colnames(house.data)
# Description of each column column
# address - 
# city -
# state -
# zip -
# sqft -
# bedrooms -
# bathrooms -
# days_on_zillow -
# sales_type -
# url -



# Data Interpretation
summary(house.data)
# Summary of each column of the house data
plot(house.data$price,house.data$sqft)
ggplot(data=house.data, aes(x=sqft, y=price))+
  geom_line() + scale_x_continuous(name="sqft", limits=c(0, mean(house.data$sqft))) +
    scale_y_continuous(name="price", limits=c(0, max(house.data$price)))

#Data Cleaning
# found outlier in sqft
is.na(house.data$bedrooms)

house.data1<-house.data[!is.na(house.data$bedrooms) & !is.na(house.data$bathrooms),]

ggplot(data=house.data1, aes(x=sqft, y=price))+
  geom_line() + scale_x_continuous(name="sqft", limits=c(0, mean(house.data1$sqft))) +
  scale_y_continuous(name="price", limits=c(0, max(house.data1$price)))

summary(house.data1)
# in sqft there are 25 NAs so removing
house.data1<- house.data1[!(is.na(house.data1$sqft)),]

# Data Selection
# The data is seletected based on the perception
house.data.sel<- subset(house.data1, select = c("price", "sqft","bedrooms","bathrooms"))
summary(house.data.sel)
# correlation between price sqft bedroom bathrooms
cor(house.data.sel) 

# scatter plot of price sqft bedroom bathrooms
plot(house.data.sel)

# Model Building
# Fit our regression model
# . means all predictor variables
sat.mod <- lm(price ~., # regression formula
              data=house.data.sel) # data set
sat.mod1 <- lm(price ~sqft+bathrooms, # regression formula
              data=house.data.sel)
sat.mod1 <- lm(price ~sqft, # regression formula
               data=house.data.sel)
# Summarize and print the results
summary(sat.mod) # show regression coefficients table
summary(sat.mod1) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ~~~~~~~~~~~~~~~~~~~~~~~~

##   OK, we fit our model. Now what?
##   - Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   - Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   - Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.
##   - Investigate these assumptions visually by plotting your model:
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ~~~~~~~~~~~~~~~~

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:
# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise 0: least squares regression
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## Interactions and factors
## ========================

## Modeling interactions
## ~~~~~~~~~~~~~~~~~~~~~

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?
#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.
# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise 1: interactions and factors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?
