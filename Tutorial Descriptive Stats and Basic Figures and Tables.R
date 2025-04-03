## Tutorial: Descriptive Stats and Basic Figures & Tables
## Stephanie Pedron (pedron.2@osu.edu)

## Note: many of these codes will not run on their own. Please exchange any data with your own as needed

library(dplyr)
library(tidyverse)
library(ggplot2)

########################   DESCRIPTIVE STATS   #############################

## Quick looks
head(CCES10) # first 6 obs, specify variable if needed (e.g. head(CCES10$race))
tail(CCES10) # last 6 obs
dim(CCES10) # data dimensions
CCES10 %>% 
  slice(1:20) # look at more than 6
colnames(CCES10) # column names
rownames(CCES10) # row names
CCES10[7,] # rows
CCES10[,4] # columns
CCES10[1:5, "pid7", "immstat"] # accessing multiple things
str(CCES10) # lots of variable details

## Data Type
class(CCES10$pid7)

## Descriptive stats
summary(CCES10)

mean(CCES10$pid7, na.rm = T) # mean
max(CCES10$pid7, na.rm = T) # maximum
min(CCES10$pid7, na.rm = T) # minimum
median(CCES10$pid7, na.rm = T) # mediaan
qunatile(CCES10$pid7, probs = c(0.25, 0.5, 0.75), na.rm = T)
IQR(CCES10$pid7, na.rm = T) # interquartile range
sd(CCES10$pid7, na.rm = T) # standard deviation
sum(complete.cases(CCES10$pid7)) # all obs without NA
abc <- rowMeans(cbind(CCES10$pid7, CCES10$ideo5), na.rm = T) # new variable from mean of two variables with NA removed


########################   Basic Figures / Tables   #############################

## Histograms and Density
hist(CCES10$pid7) # base R
hist1 <- ggplot(variables, aes( x = X1, y = ..density..)) + 
  geom_histogram(binwidth = 0.5, color = "white", fill ="pink") + theme_light() + labs(title = "Random Draws from X") # Density, ggplot example

hist1 + geom_density(aes(y=..density..), color = "black") # this adds a density curve

D <- rchisq(10000, df = 9999)
D_Density <- plot(density(D)) # just density

## layering density plots on top of each other
practice1 <- evals %>%
  ggplot(aes(x = score, y = ethnicity, fill = ethnicity)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 3, color = "black") +
  scale_fill_manual(values = c("pink", "blue"), guide = FALSE) +
  labs(x = "Average Course and Professor Evaluations", y = "Minority Status") +
  theme_minimal() + theme(panel.grid.minor = element_blank())

## Sample Means Histogram 
my_draws <- rexp(10000, 20)
sample3 <- replicate(10000, sample(my_draws, 5000, replace = T))
means3 <- colMeans(sample3)
plot3 <- hist(means3, 
              main = "Histogram of Sample Means (5,000 Draws)", 
              xlab="Sample Means", ylab="Frequency", col="purple")



## Barplot 
barplot(D1_POLGR, main = "Distribution of Political Grievance",
        xlab = "Groups' Levels of Political Grievance", col = "pink") # base R

ggplot(data = anes_data, aes(y = VCF0803)) + geom_bar(fill = "orchid") + theme_minimal() +
  labs(title = "Ideology") # ggplot



## Line graph: to add points just put + geom_point()
ggplot(data = House_Majority_Knowledge, aes(VCF0004, pct, color = education)) + geom_line() +
  labs(x = "Year", y = "Percent", title = "Correct House Majority Knowledge") + theme_minimal()
ggplot(data = WorldStanding_Improved, aes(VCF0004, pct, color = pid3)) +  geom_line() + 
  labs(x = "Year", y = "Percent", title = "U.S. Belief that World Standing Improved") + theme_minimal()



## Boxplots 
ggplot(data = QoG_2010, aes(x = undp_gdp)) + geom_boxplot() +
  labs(title = "GDP", x = "per capita GDP") # one variable

QoG_2010$fh_status <- ifelse(QoG_2010$fh_status == 1, "Free", QoG_2010$fh_status)
QoG_2010$fh_status <- ifelse(QoG_2010$fh_status == 2, "Partly Free", QoG_2010$fh_status)
QoG_2010$fh_status <- ifelse(QoG_2010$fh_status == 3, "Not Free", QoG_2010$fh_status)
plot2 <- ggplot(data = QoG_2010, aes(x = fh_status, y = undp_gini)) + geom_boxplot() +
  labs(title = "Gini Coefficients by Freedom Status", x = "Freedom Status", y =
         "Gini Coefficients")
plot2 # two variables, three boxplots



## Scatterplot - jitter() usually helpful to add noise if points are bunched together
plot(a$b, a$c, col = "pink", xlab = "", ylab = "") # basic scatterplot using base R

plot3 <- ggplot(data = QoG_2010, aes(x = wdi_gris, y = log(undp_gdp))) +
  geom_point(shape = 20, colour = "pink", size = 4) + theme_light() +
  labs(title = "Scatterplot", x = "Percentage of Girls to Boys Enrolled",
       y = "per capita GDP") + geom_abline() # ggplot example
plot3

ggplot(data2, aes(x=inst_quality, y= log(settler_mortality))) + geom_point(shape = 20, colour = "pink", size = 4) + 
  labs(title = "Institutional Quality and Settler Mortality", x = "Institutional Quality", y = "Settler Mortality") + 
  theme_minimal() + geom_smooth(method = lm) # ggplot example with a a trend line



## Tables
table(d1$VCF0320, d1$affect_gop_party)
a <- table(CCES10$pid7)
prop.table(a) # proportions

## Crosstabs
library(gmodels)
library(xtable)
crosstab1 <- CrossTable(data_prob4$POLGR, data_prob4$EXECREP, chisq = TRUE, prop.r = FALSE, 
                        prop.chisq = FALSE)
xtable(crosstab1)

## Make even better tables
table(
  d1$affect_gop_party, d1$pres_vote
) %>%
  prop.table(1) %>% # prop.table(1) means row percentages; prop.table(2) means column percentages
  multiply_by(100) %>%
  round()

## How to add weights!
xtabs(
  d1$VCF0009x ~ d1$affect_gop_party + d1$pres_vote
) %>%
  prop.table(1) %>%
  multiply_by(100) %>%
  round()

