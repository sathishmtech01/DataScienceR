
## Regression with binary outcomes
## ===============================

## Logistic regression
## ~~~~~~~~~~~~~~~~~~~

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited--in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("Rstatistics/dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

## [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise 2: logistic regression
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

## Multilevel Modeling
## ===================

## Multilevel modeling overview
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   - Multi-level (AKA hierarchical) models are a type of mixed-effects
##     models
##   - Used to model variation due to group membership where the goal is to
##     generalize to a population of groups
##   - Can model different intercepts and/or slopes for each group
##   - Mixed-effecs models include two types of predictors: fixed-effects
##     and random effects
##     - Fixed-effects -- observed levels are of direct interest (.e.g,
##       sex, political party...)
##     - Random-effects -- observed levels not of direct interest: goal is
##       to make inferences to a population represented by observed levels
##   - In R the lme4 package is the most popular for mixed effects models
##     - Use the `lmer' function for liner mixed models, `glmer' for
##       generalized mixed models

library(lme4)

## The Exam data
## ~~~~~~~~~~~~~

##   The Exam data set contans exam scores of 4,059 students from 65
##   schools in Inner London. The variable names are as follows:

##    variable  Description                                                                                        
##   --------------------------------------------------------------------------------------------------------------
##    school    School ID - a factor.                                                                              
##    normexam  Normalized exam score.                                                                             
##    schgend   School gender - a factor.  Levels are 'mixed', 'boys', and 'girls'.                                
##    schavg    School average of intake score.                                                                    
##    vr        Student level Verbal Reasoning (VR) score band at intake - 'bottom 25%', 'mid 50%', and 'top 25%'. 
##    intake    Band of student's intake score - a factor.  Levels are 'bottom 25%', 'mid 50%' and 'top 25%'./     
##    standLRT  Standardised LR test score.                                                                        
##    sex       Sex of the student - levels are 'F' and 'M'.                                                       
##    type      School type - levels are 'Mxd' and 'Sngl'.                                                         
##    student   Student id (within school) - a factor                                                              

Exam <- readRDS("Rstatistics/dataSets/Exam.rds")

## The null model and ICC
## ~~~~~~~~~~~~~~~~~~~~~~

##   As a preliminary step it is often useful to partition the variance in
##   the dependent variable into the various levels. This can be
##   accomplished by running a null model (i.e., a model with a random
##   effects grouping structure, but no fixed-effects predictors).

# null model, grouping by school but not fixed effects.
Norm1 <-lmer(normexam ~ 1 + (1|school),
             data=Exam, REML = FALSE)
summary(Norm1)

##   The is .169/(.169 + .848) = .17: 17% of the variance is at the school
##   level.

## Adding fixed-effects predictors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Predict exam scores from student's standardized tests scores

Norm2 <-lmer(normexam~standLRT + (1|school),
             data=Exam,
             REML = FALSE) 
summary(Norm2) 

## Multiple degree of freedom comparisons
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   As with `lm' and `glm' models, you can compare the two `lmer' models
##   using the `anova' function.

anova(Norm1, Norm2)

## Random slopes
## ~~~~~~~~~~~~~

##   Add a random effect of students' standardized test scores as well. Now
##   in addition to estimating the distribution of intercepts across
##   schools, we also estimate the distribution of the slope of exam on
##   standardized test.

Norm3 <- lmer(normexam~standLRT + (standLRT|school), data=Exam,
              REML = FALSE) 
summary(Norm3) 

## Test the significance of the random slope
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   To test the significance of a random slope just compare models with
##   and without the random slope term

anova(Norm2, Norm3) 

## Exercise 3: multilevel modeling
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   Use the dataset, bh1996: `data(bh1996, package="multilevel")'

##   From the data documentation:
##         Variables are Cohesion (COHES), Leadership Climate (LEAD),
##         Well-Being (WBEING) and Work Hours (HRS). Each of these
##         variables has two variants - a group mean version that
##         replicates each group mean for every individual, and a
##         within-group version where the group mean is subtracted
##         from each individual response. The group mean version is
##         designated with a G. (e.g., G.HRS), and the within-group
##         version is designated with a W. (e.g., W.HRS).

##   1. Create a null model predicting wellbeing ("WBEING")
##   2. Calculate the ICC for your null model
##   3. Run a second multi-level model that adds two individual-level
##      predictors, average number of hours worked ("HRS") and leadership
##      skills ("LEAD") to the model and interpret your output.
##   4. Now, add a random effect of average number of hours worked ("HRS")
##      to the model and interpret your output. Test the significance of
##      this random term.

## Exercise solutions                                           :prototype:
## ==================

## Exercise 0 prototype
## ~~~~~~~~~~~~~~~~~~~~

##   Use the /states.rds/ data set.
states <- readRDS("Rstatistics/dataSets/states.rds")

##   Fit a model predicting energy consumed per capita (energy) from the
##   percentage of residents living in metropolitan areas (metro). Be sure
##   to
##   1. Examine/plot the data before fitting the model
states.en.met <- subset(states, select = c("metro", "energy"))
summary(states.en.met)
plot(states.en.met)
cor(states.en.met, use="pairwise")

##   2. Print and interpret the model `summary'
mod.en.met <- lm(energy ~ metro, data = states)
summary(mod.en.met)

##   3. `plot' the model to look for deviations from modeling assumptions
plot(mod.en.met)

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
states.en.met.pop.wst <- subset(states, select = c("energy", "metro", "pop", "waste"))
summary(states.en.met.pop.wst)
plot(states.en.met.pop.wst)
cor(states.en.met.pop.wst, use = "pairwise")
mod.en.met.pop.waste <- lm(energy ~ metro + pop + waste, data = states)
summary(mod.en.met.pop.waste)
anova(mod.en.met, mod.en.met.pop.waste)

## Exercise 1: prototype
## ~~~~~~~~~~~~~~~~~~~~~

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
mod.en.metro.by.waste <- lm(energy ~ metro * waste, data = states)

##   1. Try adding a region to the model. Are there significant differences
##      across the four regions?
mod.en.region <- lm(energy ~ metro * waste + region, data = states)
anova(mod.en.region)

## Exercise 2 prototype
## ~~~~~~~~~~~~~~~~~~~~

##   Use the NH11 data set that we loaded earlier. Note that the data is
##   not perfectly clean and ready to be modeled. You will need to clean up
##   at least some of the variables before fitting the model.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
nh11.wrk.age.mar <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
summary(nh11.wrk.age.mar)
NH11 <- transform(NH11,
                  everwrk = factor(everwrk,
                                   levels = c("1 Yes", "2 No")),
                  r_maritl = droplevels(r_maritl))

mod.wk.age.mar <- glm(everwrk ~ age_p + r_maritl, data = NH11,
                      family = "binomial")

summary(mod.wk.age.mar)

##   2. Predict the probability of working for each level of marital
##      status.
library(effects)
data.frame(Effect("r_maritl", mod.wk.age.mar))

## Exercise 3 prototype
## ~~~~~~~~~~~~~~~~~~~~

##   Use the dataset, bh1996:
data(bh1996, package="multilevel")

##   From the data documentation:
##         Variables are Cohesion (COHES), Leadership Climate (LEAD),
##         Well-Being (WBEING) and Work Hours (HRS). Each of these
##         variables has two variants - a group mean version that
##         replicates each group mean for every individual, and a
##         within-group version where the group mean is subtracted
##         from each individual response. The group mean version is
##         designated with a G. (e.g., G.HRS), and the within-group
##         version is designated with a W. (e.g., W.HRS).
##   Note that the group identifier is named "GRP".
##   1. Create a null model predicting wellbeing ("WBEING")
library(lme4)
mod.grp0 <- lmer(WBEING ~ 1 + (1 | GRP), data = bh1996)
summary(mod.grp0)

##   => library(lme4) > mod.grp0 <- lmer(WBEING ~ 1 + (1 | GRP), data =
##   bh1996) > summary(mod.grp0) Linear mixed model fit by REML ['lmerMod']
##   Formula: WBEING ~ 1 + (1 | GRP) Data: bh1996

##   REML criterion at convergence: 19347

##   Scaled residuals: Min 1Q Median 3Q Max -3.322 -0.648 0.031 0.718 2.667

##   Random effects: Groups Name Variance Std.Dev. GRP (Intercept) 0.0358
##    0.189 Residual 0.7895 0.889 Number of obs: 7382, groups: GRP, 99

##   Fixed effects: Estimate Std. Error t value (Intercept) 2.7743 0.0222
##               125 > =2. [@2] Calculate the ICC for your null model `ICC
##               = .0358/(.0358 + .7895) = .04'
##   3. Run a second multi-level model that adds two individual-level
##      predictors, average number of hours worked ("HRS") and leadership
##      skills ("LEAD") to the model and interpret your output.
mod.grp1 <- lmer(WBEING ~ HRS + LEAD + (1 | GRP), data = bh1996)
summary(mod.grp1)

##   4. Now, add a random effect of average number of hours worked ("HRS")
##      to the model and interpret your output. Test the significance of
##      this random term.
mod.grp2 <- lmer(WBEING ~ HRS + LEAD + (1 + HRS | GRP), data = bh1996)
anova(mod.grp1, mod.grp2)

## Wrap-up
## =======

## Help us make this workshop better!
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##   - Please take a moment to fill out a very short
##   feedback form
##   - These workshops exist for you -- tell us what you need!
##   - [http://tinyurl.com/RstatisticsFeedback]

## Additional resources
## ~~~~~~~~~~~~~~~~~~~~

##   - IQSS workshops:
##     [http://projects.iq.harvard.edu/rtc/filter_by/workshops]
##   - IQSS statistical consulting: [http://dss.iq.harvard.edu]
##   - Zelig
##     - Website: [http://gking.harvard.edu/zelig]
##     - Documentation: [http://r.iq.harvard.edu/docs/zelig.pdf]
##   - Ameila
##     - Website: [http://gking.harvard.edu/Amelia/]
##     - Documetation: [http://r.iq.harvard.edu/docs/amelia/amelia.pdf]
