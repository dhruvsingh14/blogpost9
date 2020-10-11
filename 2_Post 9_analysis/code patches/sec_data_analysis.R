#############
# libraries #
#############
library(manipulate)

###############
# directories #
###############
getwd()
setwd("C:/Dhruv/Misc/Personal/writing/Blogging/2_posts/3_September/wk2_post9/2_Post 9_analysis")

########
# data #
########
sec <- read.csv("final_SEC_data.csv")


###################################
# regression models: introduction #
###################################


# measurement error adjusts plots to make them appear the same
plot(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ OperatingIncomeLoss, subset(sec, (OperatingIncomeLoss < 5.0e+08)  
                                                                                   & (AccumulatedOtherComprehensiveIncomeLossNetOfTax > (-1.0e+08))
                                                                                   & (AccumulatedOtherComprehensiveIncomeLossNetOfTax < 2.0e+08)))

# jitter means disrupting near similar points w/ m.e. to make them appear dispersed
plot(jitter(AccumulatedOtherComprehensiveIncomeLossNetOfTax,4) ~ OperatingIncomeLoss, subset(sec, (OperatingIncomeLoss < 5.0e+08)  
                                                                                   & (AccumulatedOtherComprehensiveIncomeLossNetOfTax > (-1.0e+08))
                                                                                   & (AccumulatedOtherComprehensiveIncomeLossNetOfTax < 2.0e+08)))
# overlaying regression line: subsetted
regrline <- lm(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ OperatingIncomeLoss, subset(sec, (OperatingIncomeLoss < 5.0e+08)  
                                                                                             & (AccumulatedOtherComprehensiveIncomeLossNetOfTax > (-1.0e+08))
                                                                                             & (AccumulatedOtherComprehensiveIncomeLossNetOfTax < 2.0e+08)))
abline(regrline, lwd = 3, col = 'red')

# examining fitted regline
summary(regrline)

#   Coefficients:
#                         Estimate    Std. Error  t value Pr(>|t|)    
#   (Intercept)           -6.776e+06  1.516e+06  -4.468   9.70e-06 ***
#   OperatingIncomeLoss   -6.648e-02  1.372e-02  -4.847   1.66e-06 ***

# overlaying regression line: full dataset
regrline <- lm(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ OperatingIncomeLoss, sec)
abline(regrline, lwd = 3, col = 'red')

# examining fitted regline
summary(regrline)

#   Coefficients:
#                       Estimate    Std. Error  t value Pr(>|t|)    
#   (Intercept)         -1.662e+08  2.737e+07  -6.072   2.13e-09 ***
#   OperatingIncomeLoss -6.083e-02  1.558e-02  -3.905   0.000104 ***

# recall coeff +- 2*se yields confidence bands

# Operating Incomes are negatively related with the bottom line
# this is an interesting insights, since it is gross profit less the operating expenses, before taxes.
# ofcourse the higher the operating income, the higher the tax, -> thus the negative relation to the bottom line


#####################
# part 2: residuals #
#####################

# likelihood of residuals (or errors) being positive or negative same
# residuals must be uncorrelated with predictors

# reg line
fit <- lm(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ OperatingIncomeLoss, sec)

# residual vector
fit$residuals

# regression diagnostics
summary(fit)

# hyp: residuals mean = 0
mean(fit$residuals)
# 3.827504e-08

# checking residual - predictor correlation
# cov(fit$residuals[na.], sec$OperatingIncomeLoss[!is.na(sec["OperatingIncomeLoss"]),])

# residuals = est(ht) - actual(ht)
# deviance function helps calculate this

# extracting intercept
ols.ic <- fit$coef[1]

# extracting slope
ols.slope <- fit$coef[2]

# calculating variation in bottom line
varBLine <- var(sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax)

# calculating variance in residuals
varRes <- var(fit$residuals)

# mulivar

# generating reg line, and saving as a var, generates all subcomponents

# eg: residuals, coefficients, all as variables, or elements of reg var

efit <- lm(ComprehensiveIncomeNetOfTax ~ AccountsPayableCurrent + AccountsReceivableNetCurrent , subset(sec, !is.na(AccountsPayableCurrent) 
                                                                                                        & !is.na(AccountsReceivableNetCurrent)))
mean(efit$residuals) # mean is damn near 0

# cov(efit$residuals, sec$AccountsPayableCurrent) # corr resid vs payable predictor
# cov(efit$residuals, sec$AccountsReceivableNetCurrent) # corr resid vs receivable predictor

######################
# Residual Variation #
######################

fit <- lm(ComprehensiveIncomeNetOfTax ~ CommonStockParOrStatedValuePerShare, sec)

n = 6028

# taking square root of sum of squared residuals / qty
sqrt(sum(fit$residuals^2)/(n-2))

# looking at the sigma portion of fit summary
summary(fit)$sigma

sqrt(deviance(fit)/(n-2))

# storing away bottom line
mu <- mean(sec$ComprehensiveIncomeNetOfTax)

# centering data => subtracting the mean from each data point
sTot <- sum((sec$ComprehensiveIncomeNetOfTax - mu)^2)

sRes <- deviance(fit)

# regression variation
1- sRes/sTot

# extracting generated reg variation as comparison
summary(fit)$r.squared

cor(sec$ComprehensiveIncomeNetOfTax, sec$CommonStockParOrStatedValuePerShare)^2


######################################################################################




########################################
# lesson 2: Intro to Multivar Reg    #
########################################

ones <- rep(1, nrow(galton))

# using -1 suppresses the intercept
lm(child ~ ones + parent - 1, galton)

lm(child ~ ones + parent, galton)

1

# Gaussian Elimination
# subtracting mean from regressor and regressed vars, eliminates slope
# and passes regression line through the origin

# sometimes replaces variables w/ their residuals (kind of like errors) as a form of estimation

1

# the mean of a variable = coeff from regressing var against 1
# mean(var) = lm(var ~ 1, data)$coeff
lm(child ~ 1, galton)

head(trees)
3

# storing reg for later use
fit <- lm(Volume ~ Girth + Height + Constant - 1, trees)

# eliminating girth from dataset
trees2 <- eliminate("Girth", trees)
head(trees2)

2

# plotting reduced model, fewer regressors, to see size of constant unchanged
fit2 <- lm(Volume ~ Height + Constant - 1, trees2)

lapply(list(fit, fit2), coef)

2


#####################################
#   lesson 3: Multivar Reg  Eg.s    #
#####################################
4

# predicting fertility
all <- lm(Fertility ~ ., swiss)

# summarizing all predictor model
summary(all)

1

# generating model in one regressor
summary(lm(Fertility ~ Agriculture, swiss))
4
1

# checking correlations between variables
cor(swiss$Examination, swiss$Education)
cor(swiss$Agriculture, swiss$Education)

all

# running makelms()
makelms()

1

# model with added extraneous information remains unchanged
ec <- swiss$Examination + swiss$Catholic
efit <- lm(Fertility ~ . + ec, swiss)

# verifying
all$coefficients - efit$coefficients
3
3

getwd()

library(swirl)
# install_from_swirl("Regression Models")

swirl()

#################################
#   regression models week 3    #
#################################

#################################
#   lesson 1: Multivar Egs.2    #
#################################

# insect spray data
6
B

# dimension of the data
dim(InsectSprays)

# displaying first 15 rows
head(InsectSprays, 15)

# displaying array
sC

# checking column counts
summary(InsectSprays[,2])

# checking column classes
sapply(InsectSprays, class)

# generating linear model to predict count
fit <- lm(count ~ spray, InsectSprays)
summary(fit)$coef

est <- summary(fit)$coef[,1]

mean(sA)

4

mean(sB)

# model without intercept
nfit <- lm(count ~ spray - 1, InsectSprays)

# when intercept included, sA is base reference group
# when intercept ommitted, sA has explicit estimate
summary(nfit)$coef

1

# w/o intercepts, estimates are w reference to 0 as base group
# est are means, and test for sign diff from 0

1

# using r function relevel to set diff baseline from sA
# created a new predictor variable, reordering factor to make C baseline
spray2 <- relevel(InsectSprays$spray, "C")

fit2 <- lm(count ~ spray2, InsectSprays)
summary(fit2)$coef

# since sray C doesn't doesn't have coef, we see it is the baseline
3

# verifying answer matching intercept
mean(sC)

# now estimator for spray A = coeff A + intercept

# calculating spray2B's t value
(fit$coef[2] - fit$coef[3])/1.6011


#################################
#   lesson 2: Multivar Egs.3    #
#################################

dim(hunger)

# each row represents a sample
948

# checking column names
names(hunger)

# predicting hunger rates
fit <- lm(Numeric ~ Year, hunger)
summary(fit)$coef
1
3
1

# subsetting model prediction to gender = female
lmF <- lm(Numeric[hunger$Sex == "Female"] ~ Year[hunger$Sex == "Female"], hunger)
lmM <- lm(Numeric[hunger$Sex == "Male"] ~ Year[hunger$Sex == "Male"], hunger)

3

lmBoth <- lm(Numeric ~ Year + Sex, hunger)
summary(lmBoth)

3
1
1

# adding interacted variable to model
lmInter <- lm(Numeric ~ Year + Sex + Sex*Year, hunger)
summary(lmInter)
1
3
3
2
1

# some notes on interacted coefficients, when predictors held constant

# Suppose we have two interacting predictors and one of them is held constant. 
# The expected change in the outcome for a unit change in the other predictor is the
# coefficient of that changing predictor + 
# the coefficient of the interaction * the value of the predictor held constant.


########################################################
#   lesson 3: Residuals, Diagnostics, and Variation    #
########################################################

# addressing influential outlier
fit <- lm(y ~ x, out2)

# residuals vs. fitted plot
plot(fit, which = 1)

1
1

# creating a model without the influential outlier
fitno <- lm(y ~ x, out2[-1,])

# residuals vs. fitted plot
plot(fitno, which = 1)

# checking change induced by including influential
coef(fit) - coef(fitno)

head(dfbeta(fit))

# preparing to calculate influence from residuals ratio
resno <- out2[1, "y"] - predict(fitno, out2[1,])

# calculating influence of outlier
1 - resid(fit)[1]/resno

# hatvalues
head(hatvalues(fit))

# calculating sample standard deviation of fit's residual
sigma <- sqrt(deviance(fit)/df.residual(fit))

# computing the standardized residual
rstd <- resid(fit)/(sigma*sqrt(1-hatvalues(fit))) 

# comparing two columns side by side using a simple head + cbind
head(cbind(rstd, rstandard(fit)))

plot(fit, which = 3)

# diagnostic plot
plot(fit, which=2)
1

# calculating the sample deviation of residual
sigma1 <- sqrt(deviance(fitno)/df.residual(fitno))

# calculating studentized residual
resid(fit)[1] / (sigma1 * sqrt(1-hatvalues(fit)[1]))

# comparing the studentized residual with those of the other sample
head(rstudent(fit))

# cook's distance - tells how much a sample changes a model
# comparing models with and without outlier included, we have
dy <- predict(fitno, out2) - predict(fit, out2)

# calculating outlier's cook's distance
sum(dy^2)/(2*sigma^2) 

# displaying diagnostic plot for cook's distance
plot(fit, which=5)

getwd()

library(swirl)
# install_from_swirl("Regression Models")

swirl()

#################################
#   regression models week 4    #
#################################

#############################################
#   lesson 1: variance inflation factors    #
#############################################

# issue i: including too few vars --> bias
# issue ii: including too many vars --> errors

# issue ii is also called variance inflation
# note: variance inflation is due to correlated regressors
3
1
2

# vifSims is an excellent example of calculating multiple simulations of 
# a dependent variable, and the associated coefficients for variables used to simulate it

# here, a 1000 simulations, to be later aggregated to produce one estimate
rgp1()
2

rgp2()

# using car package to estimate vif's: variance inflation estimators
head(swiss)

# regressing fertility outcomes on all 5 measures
mdl <- lm(Fertility ~ Infant.Mortality + Catholic + Education + Examination + Agriculture, swiss)

# calculating vif's for each of 5 regressors
vif(mdl)
# vif's can be used to mentally scale down our estimators in the case of correlation within

# ommitting examination from our model
mdl2 <- lm(Fertility ~ Infant.Mortality + Catholic + Education + Agriculture, swiss)
vif(mdl2)
# lower vif for coefficient is good
# highly correlated vars can be ommitted to lower vif score per coefficient
2
2
2


##############################################
#  lesson 2: Overfitting and Underfitting    #
##############################################
1
3

# 2 x 150 matrix, row1: x1 coef est sans x3, row2: x1 coef sans x2
x1c <- simbias()

# finding mean regressor estimates of each row
apply(x1c, 1, mean)

# exemplifying analysis of variance # swiss data
fit1 <- lm(Fertility ~ Agriculture, swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, swiss)

# checking significance
anova(fit1, fit3)

1
3

deviance(fit3)

# calculating F value manually
# denom
d <- deviance(fit3)/43

# num
n <- (deviance(fit1) - deviance(fit3))/2

# F value
n/d

# calculating p value 
pf(n/d, 2, 43, lower.tail = FALSE)

# shapiro wilk to test the residuals
shapiro.test(fit3$residuals)

# checking each successive models significance wrt to prev
anova(fit1, fit3, fit5, fit6)

2

# including more regressors decreases residual sum of squares
3
3

##################################
#   lesson 3: binary outcomes    #
##################################
1
4

ravenData

# using glm model to predict wins from points scored
mdl <- glm(ravenWinNum ~ ravenScore, family = binomial, ravenData)

# predicting log odds of wins
lodds <- predict(mdl, data.frame(ravenScore=c(0, 3, 6)))

# converting log odds to probabilities to increase readability
exp(lodds)/(1+exp(lodds))

# checking sd of fit
summary(mdl)

# yields 95% upper and lower bounds of confidence interval
exp(confint(mdl))

1

# lower bound of coef log odds suggest slight decrease in prob of winning at the lower end
2

# using reg to minim var
# using negative log likelihoods in place of variance
anova(mdl)

# using chi square to test whether coef is diff from 0
qchisq(0.95, 1)


#################################
#   lesson 4: count outcomes    #
#################################

# expected rate of occurence: lambda
# mean of poisson = var of poisson = lambda

# checking sample var vs theor var
var(rpois(1000, 50))
3
Yes

View(hits)

# checking date type 
class(hits[,'date'])

# performing operations on dates
as.integer(head(hits[,'date']))

# using poisson to predict site visits by date
mdl <- glm(visits ~ date, poisson, hits)

# checking coefficients and significance
summary(mdl)

# getting the 95 % conf int
exp(confint(mdl, 'date'))

# checking date of max hits to website 
# the following prints the row num / index - where row num indicates sample number
which.max(hits[,'visits'])

# printing max hits row
hits[704,]

# extracting row 704 obs for fitted.values for comparison
lambda <- mdl$fitted.values[704] 

# checking 95th percentile of poisson distribution
qpois(.95, lambda)

# using log(visits+1) in order to avoid division by 0
# setting offset parameter to log(visits+1)
mdl2 <- glm(simplystats ~ date, poisson, hits, offset = log(visits+1))

# checking mdl2's 95th percentile to see if the actual visits - 64 - are approached by our predicted model
# prediction at 95th percentile shows 47 visits, a ways off from 64
qpois(.95, mdl2$fitted.values[704])

2
3
3



