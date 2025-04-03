## Tutorial: Coding DAGs and Models for Causal Inference
## Stephanie Pedron (pedron.2@osu.edu)

## Note: many of these codes will not run on their own. Please exchange any data/variables with your own as needed

library(data.table)

########################   Directed Acyclic Graphs   ######################

library(ggdag)
simple_dag <- dagify(Y ~ X) # x points at Y
ggdag(simple_DAG) + theme_dag_blank() 

covariate_DAG <- dagify(
  Y ~ X + Z, # X and Z point at Y
  X ~ Z # Z points at X
)
ggdag(covariate_DAG) + theme_dag_blank()

experimental_DAG <- dagify(
  Y ~ X + C2,
  X ~ Z + U,
  C1 ~ Z,
  C2 ~ C1
)
ggdag(experimental_DAG) + theme_dag_blank()


########################   Models for Causal Inference   ######################

## Diff-in-Diff
## Need a binary variable identifying treatment & control, plus another that identifies onset of whatever you're looking at (e.g. policy) then just interact them
lm(DV ~ binary1 + binary2 + binary1 * binary2)


## Fixed Effects
library(lfe)
felm(DV ~ covariates | state + year | 0 | state, data)
# part 1 = regular lm formula with DV and IVS
# part 2 = the fixed effects
# part 3 = not used so specified to 0. This is meant for instruments. (e.g. Q|W ~ x3 = covariates instrumented by x3)
# part 4 = clustering standard errors at the state level
# Note: only specify parts to 0 if you're not at the end of the formula. Can use weights = argument here for survey weights


## Instrumental Variables (2SLS)
## Step 1 predict some model with the instrumental variable, make the endogenous one the DV
step_one <- lm(endogenousvar ~ instrument + controls, data, na.action = na.exclude)
step_one_pred <- fitted(step_one) # save the predicted values
## Step 2 regress your actual DV using predicted values
step_two <- lm(DV ~ step_one_pred + controls, data, na.action = na.exclude)

## Alternative process and the standard errors auto adjusted for you!
library(AER)
iv_model <- ivreg(DV ~ endogenousvar + controls # step one is stage 2 in manual version
                  | IV + controls, # step two is stage 1 in manual version
                  data =)


## Regression Discontinuity Design (RDD) - reference rhelp ?RDestimate for specific arguments
library(rdd) # note: if you use recode, this package masks it! 
RDestimate(Y ~ X, cutpoint =) # sharp RDD
RDestimate(Y ~ X + Z, cutpoint =) # fuzzy RDD
DCdensity(runvar, cutpoint) # McCrary Density test - check discontinuity in the density function at either side of cutoff
# e.g. score > 90 means skip grade. Grader rounds scores so everyone with an 89 gets a 90, so you don't see 89s. Cutoff manipulation.
