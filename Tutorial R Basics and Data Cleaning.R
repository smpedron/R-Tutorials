## Tutorial: R Basics and Data Cleaning
## Stephanie Pedron (pedron.2@osu.edu)

## Note: many of these codes will not run on their own. Please exchange any data$var with your own as needed

########################   BASICS   #############################

rm(list = ls()) # remove everything in your global environment

RNGkind("L'Ecuyer-CMRG") # rngsus
sample(.Machine$integer.max, 1) 
set.seed(271179340) # IMPORTANT: used to make random number generation reproducible

getwd() # working directory path
setwd("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data") # set working directory path

if(!require(foreign)) install.packages("foreign") # check if a package is installed, and if not, install it
library(foreign) # loading packages
library(dplyr)
library(magrittr)
library(tidyverse)
library(data.table)
library(ggplot2)

## Loading R data from my computer
CCES10 <- read.dta("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2010.dta")

## Loading data from a website
anes_data_original <- "https://github.com/thomasjwood/ps7160/raw/master/anes_cdf_20211118.rds" %>%
  url %>%
  gzcon %>%
  readRDS

anes_data <- "https://github.com/thomasjwood/ps7160/raw/master/anes_pilot_18.rds" %>% 
  url %>% 
  gzcon %>% 
  readRDS

## Working with probabilities and distributions (example below is normal distribution, but can use other distributions: rexp, qbinom, dpois, plogis, etc.)
dnorm(x) # pdf of normal distribution for x
pnorm(x) # cdf - probability that a random value is less than x
qnorm(x) # inverse cdf - find z-scores
rnorm(90, mean = 5, sd = 2) # generate RVs

## Make your own dataframe with multiple variables and observations
cor_data2 <- data.frame(
  year = c(2010, 2012, 2014, 2016, 2018, 2020),
  asian_RR_PID = c(0.35, 0.32, 0.23, 0.20, 0.46, 0.48),
  white_RR_PID = c(0.57, 0.54, 0.47, 0.43, 0.63, 0.64)
)

## ifelse statements example code
a <- 50
if (a > 20){ print("a greater than 20")} else {print("a less than 20")}
QoG_2010$fh_status <- ifelse(QoG_2010$fh_status == 1, "Free", QoG_2010$fh_status)
QoG_2010$fh_status <- ifelse(QoG_2010$fh_status == 2, "Partly Free", QoG_2010$fh_status)
QoG_2010$fh_status <- ifelse(QoG_2010$fh_status == 3, "Not Free", QoG_2010$fh_status)

## multipanel graphical display
par(mfrow = c(2, 2)) # use \n to move things down a line like in titles for plots
library(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol = 2) # can also use grid.arrange!

## Making implicit missing values explicit
data %>% 
  complete(var1, var2)

## Looking for NAs
sum(is.na(CCES10$CC301b))
summary(CCES10$CC301b)
sum(CCES10$CC301b[CCES10$white == 0 & CCES10$gender = 0]) # example of getting NAs for specific cases

## Wide and Long
pivot_wider() # turn rows into variables
pivot_longer() # turn variables into rows

## Libraries for LaTeX output
library(texreg)
texreg(list(model1, model2, model3, stars = numeric(0), bold = 0.05))
library(xtable)
library(stargazer)

########################   CLEANING   #############################

## Selecting variables examples
CCES10 = select(CCES10, V208, V211, V212d, V213, V243, V246, V263, CC422a, CC422b, CC320, CC324, CC327,
                CC322_1:CC322_5, CC417a_1:CC417a_4, CC401, V214, V215, CC316)

CCES10 %>% 
  select(V208, starts_with("V2"), starts_with("V4")) # selecting columns starting with specific names

CCES10 %>% 
  select(age < 20, V208 == "A" | V208 == "B") %>% 
  distinct(id, age, V208) # unique info

## Deselecting variables
CCES10 <- subset(CCES10, select = -c(imm_grant_legal, imm_guest_workers, imm_bp, imm_fine, imm_police_ques, 
                                     pp_polimeeting, pp_polisign, pp_donate, pp_work, RR_Q1, RR_Q2))

## Subsetting data
A <- subset(CCES10, select = c(year, race, racialresentment, immstat, pid7))
corr_2014 <- CCES14[, c(1, 3:7, 10, 13)] # Subset specific columns

## Making new variables
CCES10$year <- 2010

## Adding new columns and rows in between others
library(berryFunctions)
ACA <- insertRows(ACA, 17:18, new = NA, rcurrent = FALSE) # Inserts new row at the row numbers specified
ACA[nrow(ACA) + 2,] <- NA  # Inserts row after all data

ACA <- add_column(ACA, "Model 1" = NA, .before = "Model 2" ) # columns before
aff_act <- add_column(aff_act, "Model 4" = NA, "Model 5" = NA, "Model 6" = NA, .after = "Model 3") # columns after

## Renaming variables
CES10 <- rename(CCES10,
                gender = V208, race = V211, pid7 = V212d, educ = V213, ideo5 = V243)

## Recoding variables
levels(CCES10$race) # check what the variable looks like first
CCES10$race %<>%
  plyr::mapvalues(
    c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other", "Middle Eastern", "Skipped", "Not Asked"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA, NA, NA)
  ) %>%
  as.vector()

CCES10$gender %<>%
  plyr::mapvalues(
    c("Male", "Female", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

anes <- anes %>%
  mutate(age_category_num = cut(age, c(9, 20, 30, 40, 50, 60, 70, 100),
                                labels = c('1', '2', '3', '4', '5', '6', '7'))) # wanna cut variables

## Standardizing variables
scale()
# OR
minmax <- function(x){
  (x - min(x)) /(max(x)-min(x))
} 

nest1$pid7 <- minmax(nest1$pid7) # want variables to go from 0 to 1

## Applying classes to all variables
factor_2 <- sapply(factor_2, as.numeric)

## How to use recode function instead of plyr::mapvalues for recoding variables
x <- c(1,1,1,34,456,5,5,5,5,NA) # 10 in here

a <- recode(x,
            "1" = "a",
            "5" = "b",
            "34" = "z",
            "456" = "y",
            .default = "changed")
a %<>% as.factor()
levels(a) # it auto sorts the factors alphabetically
table(a)

b <- recode(x,
            "1" = "12",
            "5" = "13",
            "34" = "14",
            "456" = "15",
            .default = "changed")
b %<>% as.numeric()

c <- recode(x,
            "1" = "new",
            "5" = "old", # if it's not included in the recode it gets changed to .default value
            .default = "NA",
            .missing = "NA") # actual NAs aren't affected by .default
table(c)

