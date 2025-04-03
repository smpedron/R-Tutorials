## Tutorial: Hypothesis Testing - Coding Models
## Stephanie Pedron (pedron.2@osu.edu)

## Note: many of these codes will not run on their own. Please exchange any data with your own as needed

library(dplyr)
library(tidyverse)
library(ggplot2)


########################  Correlation   #############################

## Correlation (cov() for covariance) to check collinearity
cor(CCES10$pid7, CCES10$ideo5) # one correlation coefficient
cor(CCES10) # everything

corr_function <- function(data){
  x <- round(cor(data, use = "pairwise.complete.obs"), 2)
  y <- corrplot(x)
  return(y)
} # this is a correlation function

## Sometimes it's easier to view a heatmap rather than a table
library(reshape2)
library(ggplot2)
heatmap <- sat %>%
  select(total, expend, salary, ratio, takers) %>%
  cor() %>% # make your correlation matrix
  round(4) %>% # up to what number you want it rounded to
  melt() %>% # make the heatmap
  ggplot(aes(x=Var1, y=Var2, fill = value)) + geom_tile(color = "white") + theme_light() +
  scale_fill_gradient2(high = "red", mid = "pink", low = "white") +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.5) +
  labs(title = "Correlation")

########################   Hypotheses Testing and Model Diagnostics   #############################

## Hypothesis Testing - T-tests and ANOVA
t.test(data, alternative = "two.sided") # alternative for one sided is "less" or "greater"
chisq.test() # chi-squared contingency table tests and goodness-of-fit tests

prop.test(x, alternative = "two.sided") # categorical variables differences in proportions, need it in table first
library(catspec) # categorical variables differences in proportions alternative, not needed in table
a <- ctab(factor(nes$south), factor(nes$white)) # independence
summary(a)

ks.test() # Kolmogorov-Smirnov Test
# one sample tests if it comes from a specific distribution
# two samples tests if they come from the same distribution

aov() # anova
TukeyHSD() # Tukey

# You can make a funciton if you need more specific info from anova and Tukey
anova_test_function <- function(anova){
  x <- aov(racialresentment ~ race + pid7 + immstat, data = anova)
  y <- summary(x)
  return(y)
}
tukey_function <- function(tukey){
  x <- aov(racialresentment ~ race + pid7 + immstat, data = tukey)
  t <- TukeyHSD(x)
  print(t$race[, "p adj"])
  print(t$pid7[, "p adj"])
  print(t$immstat[, "p adj"])
  return(t)
}



## Hypothesis Testing - Regression
lm() # basic OLS
confint(model, level = 0.95) # confidence intervals for coefficients
residuals(model) # residuals
predict(model) # fitted values (yhat)

plot(yourmodel) # diagnostics of OLS
## residuals vs fitted: looking for homoskedasticity and non-linearity, wanna see evenly spread residuals around the horizontal line at 0
## QQ Plot: checking normality, want them hugging the line
## Scale-Location Plot: checking homoskedasticity, don't wanna see patterns
## Residuals vs Leverage/Cook's D: assessing influence, looking for obs outside of Cook's D lines

## Other ways of plotting diagnostics
library(moderndive)
get_regression_table(model) # has confindence intervals
get_regression_summaries(model) # has Mean-Squared Error
get_regression_points(model) # residuals
residual_plot <- regression %>%
  ggplot(aes(x = residual,)) + geom_histogram(color = "white", fill = "violetred2", bins = 25) +
  theme_light() + labs(title = "Regression Residuals", x = "Residuals", y = "Counts")

## Even more diagnostics stuff
tail(sort(residuals(model))) # see obs with highest residuals
cor(fitted.values(model), residuals(model)) # correlation of residuals with fitted values
plot(fitted.values(model), residuals(model), xlab = "Fitted Values", ylab = "Residuals") # fitted vs residuals, check for heteroskedasticity

res <- residuals(model)
fit <- fitted.values(model)
summary(lm(sqrt(abs(res)) ~ fit)) # constant variance assumption, want an R-squared close to 0
mean(res) # check expectation of error, want it near 0 for the assumption
hist(res) # want to see if errors are normally distributed



## EXTRA: How to make a bunch of linear models using the same data but different DVs
lm_noimpute <- no_imputation %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    mod_col0 = data %>%  # Racial Resentment DV
      map(
        \(i)
        lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + pew_bornagain + ideo5 + race, data = i)),
    mod_col1 = data %>%  # Political Participation (Non-voting) DV
      map(
        \(i)
        lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
             race + race*racialresentment + race*immstat, data = i)),
    mod_col2 = data %>% # Immigration DV
      map(
        \(i)
        lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + ideo5 + 
             race + race*racialresentment + race*immstat, data = i))
  )
## How to do coefficient plots using the above? You call "lm_noimpute$mod_col1" -- alter mod_col number as needed


## Coefficient Plot Function for basic OLS
library(ggplot2)
coeff_plot_creator <- function(model1) {
  coefficients <- data.frame(
    vars = names(coef(model1)),
    estimate = coef(model1),
    std_error = summary(model1)$coefficients[, "Std. Error"]
  )
  plot <- ggplot(coefficients, aes(x = vars, y = estimate)) +
    geom_point(color = "black") +
    geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error),
                  width = 0.3, color = "black", alpha = 0.3) +
    coord_flip() +  # Flipping coordinates for horizontal plot
    labs(title = "Coefficient Plot",
         x = "Coefficient",
         y = "Estimate") +
    theme_minimal()
  return(plot)
}
coeff_plot_creator(model1)



## Generalized Linear Models
glm(family = "binomial"(link = "logit")) # logistic reg
glm(family = "binomial"(link = "probit")) # probit reg
glm(formula = cbind(X1, X2) ~ IVS, family = "quasibinomial"(link = "logit")) # binomial, each obs needs 2 variables for success/failure
glm(family = "poisson") # poisson
glm(family = "quasipoisson") # poisson but with dispersion parameter

library(MASS)
glm.nb() # negative binomial
polr() # ordered response model

library(nnet)
multinom() # unordered multinomial response model

library(MASS)
library(survival)
survfit(Surv(timevar, status) ~ x) # Survival model, Kaplan-Merier estimate
survdiff(Surv(timevar, status) ~ x) # tests for differences between groups. Should be adjusting for covariates because difference may reflect that
survreg(Surv(timevar, status) ~ x) # parametric survival model; can change distribution (default is Weibull), exponential, etc.. Wrap anova around it for a likelihood ratio test for differences between groups
survreg(Surv(timevar, status) ~ x, dist = "loglogistic") # accelerated life model
coxph(Surv(timevar, status) ~ x) # Cox Proportional Hazards Model



## Multilevel Model
## use survey data from a high level (e.g. national) and use coefficients from models on data to estimate attitudes at a lower level (e.g. state)
library(lmerTest)
library(afex) # for p values
library(MuMIn) # for R-squared
lmer() # linear mixed effects (y ~ x + (b | c)) = left of | random effects. right of | grouping variable across which effects vary
# random intercept (1 | Rvar); random slope (0|keyIV + Rvar)
glmer() # glmm
# complete pooling is literally just normal ols
# no pool is normal regression but the random effects you use factor (e.g. factor(country))
r.squaredGLMM(model) # r2m = marginal or variance explained by FE; r2c = conditional or variance explained by FE and RE
ranef(model)


## Outliers, Leverage, Influence
## Leverage
model_hatvalues <- hatvalues(model)
qqnorm(rstandard(model_hatvalues))
abline(0,1)
halfnorm(model_hatvalues, ylab = "Leverage") # check to see what leverage points are (e.g. data[40,0])
## Outliers
outliers <- rstudent(model)
outliers[which.max(abs(outliers))] # max outlier
## Influential Obs
cooks_d <- cooks.distance(model)
halfnorm(cooks_d, ylab = "Cook's D")
halfnorm(cooks_d, labs = data$var, ylab = "cook's D") # to turn obs numbers into what's in the var

########################   Regularization Techniques   #############################

## Ridge, Lasso, and Elastic Net - your data needs to be in model matrix form to use this!
library(glmnet)
cv.glmnet(x, y, alpha = 0, penalty.factor = penalty_factor) # ridge
cv.glmnet(x, y, alpha = 1, penalty.factor = penalty_factor) # lasso
cv.glmnet(x, y, alpha = 0 < 1, penalty.factor = penalty_factor) # elastic net
## NOTE: remember to exclude treatment from penalization (lasso might throw it out). Do this for theoretically important stuff too
penalty_factor <- rep(1, ncol(DX))
penalty_factor[2] <- 0



########################   Model Evaluation Methods   #############################

## REMEMBER TO SET SEED
## Method 1: Run models on training set, then predict on test set
index <- rbinom(nrow(data), 1, 0.3)
test_set <- subset(data, index == 1)
training_set <- subset(data, index == 0)
predictions <- predict(training_model, new = test_set, interval = "prediction")
mean(predictions[fit, 1]) # mean prediction error

## Method 2: LOOCV
library(boot)
model <- cv.glm(data, glmmodel, K = nrow(data)) # delta represents the mean errors (lower = better)

## Method 3: K-Fold Cross Validation
model <- cv.glm(data, glmmodel, K =)  # delta represents the mean errors (lower = better)

## Method 4: Step function (if you wanna throw away theory)
step(logitmodel)

## AIC and BIC
AIC(model1, model2) # linear penalty on parsimony, minimized the incentive to throw a bunch of shit in to minimize deviance
BIC(model1, model2) # nonlinear penalty. Same thing, but multiplies number of parameters by the log of obs - problematic if large dataset

## Evaluation of model fit
model_fit <- -2 * (logLik(model2) - logLik(model1))
pchisq(model_fit, df = 5, lower.tail = F) # if small first model is better

## Deviance
dev1 <- -2 * logLik(model1)
dev2 <- -2 * logLik(model2) # want the model with lower deviance
deviance(model2) - deviance(model1) # difference in deviance between two models
pchisq(deviancenumber, model1$edf - model2$edf, lower = F) # if dev is low or high, check if greater than 0.05 to justify removal of vars
# when new variables added, deviance always decreases. If it's random noise, deviance increases by 1.
# if it's a meaningful variable, deviance decreases by more

########################   Other Important Info   #############################

library(performance)
icc() # interclass correlation coefficient for multilevel models
check_overdispersion() # poisson
check_zeroinflation() # quasipoisson when observerd zeros is larger than predicted zeroes, there's underiftting - do neg binom
check_heteroskedasticity()
check_model() # assumption plots for lm

## Linear Probability Model (aka OLS with a binary DV) 
## this is how to fix heteroskedasticity (https://murraylax.org/rtutorials/linearprob.html?fbclid=IwAR1LMkVTI4NT7_rrqtPxceSz5Uhlefm_psqz0pnhurP8YhTVmOeG2hD_Rvo)
library(sandwich)
fix <- vcovHC(model, type = "HC1")
coeftest(model, vcov = fix)

## zero inflated - clustering
library(pscl)
library(sandwich)
library(lmtest)
zeroinfl(dist = "negbin") # zeroinflated neg binomial
coeftest(model, vcov = vcovCL(model, cluster = clustervariable))

## How to get robust standard errors
## Mostly for GLMs. Can just use "lm_robust" if OLS (estimatr package)
robust_cov <- vcovHC(model1, type = "HC3") # alter type as needed based on what you need (use ?vcovHC function to see types)
robust_se <- sqrt(diag(robust_cov)) # robust standard errors
robust_stats <- coeftest(model1, vcov = robust_cov) # robust coefficients
