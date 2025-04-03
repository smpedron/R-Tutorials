## Tutorial Structural Topic Model
## Stephanie Pedron (pedron.2@osu.edu)

# Load package and data
library(stm)
set.seed(964851236)
NAES <- read.csv("C:/Users/steph/Documents/Courses/PhD/4th Sem/Comm Data/Data/annenberg_content_analysis.csv")

## Prep the open-ended response variable for auto content analysis
## Need this stuff for effects estimates to work later on
NAES <- NAES[complete.cases(NAES$obama_over_mccain), ] # Remove missing values NAs
NAES <- NAES[complete.cases(NAES$Rwhite), ]
NAES <- NAES[complete.cases(NAES$Rfemale), ]
NAES <- NAES[rowSums(is.na(NAES)) == 0, ] # Remove observations that don't match in all three variables

## Use this to see a lot of examples at once to see what needs to be cleaned up a bit in your data
change <- rep(0, length(NAES$obama_over_mccain))
change[grep("change", tolower(NAES$obama_over_mccain))] <- 1 # chose a word that's in a lot of responses
NAES$obama_over_mccain[change==1] # lets us look at lots of responses at once

## Prepare the variables
NAES$obama_over_mccain <- gsub("\\\n\\\n", " ", tolower(NAES$obama_over_mccain)) # don't need all this extra stuff
NAES$obama_over_mccain <- gsub("\\\n", " ", tolower(NAES$obama_over_mccain))
NAES$obama_over_mccain <- gsub("\\\"", " ", tolower(NAES$obama_over_mccain))
NAES$obama_over_mccain <- gsub("i think", "", tolower(NAES$obama_over_mccain)) # this phrase is unnecessary
NAES$obama_over_mccain <- gsub("\\$", " money", tolower(NAES$obama_over_mccain)) # want an actual word (can be problematic for those that specify a number after though)
NAES$obama_over_mccain <- gsub("[^[:alnum:] ]", "", tolower(NAES$obama_over_mccain))

## Check the variable
NAES$obama_over_mccain[change==1]

## Processing data
process <- textProcessor(NAES$obama_over_mccain, metadata = NAES)

## Document output
output <- prepDocuments(process$documents, process$vocab, process$meta, 
                        lower.thresh = 900) 
# lots of observations. Let's just get words included in at least 900 (about 5%) responses as they may be more meaningful
# this helps us narrow down things by getting rid of uncommon words, but it also reduces variability

docs <- output$documents
vocab <- output$vocab
meta <- output$meta

## Estimate the topic model
## Searching for right number of topics first
topicnumbers <- c(2, 4, 6) # I am purposely limiting the number of topics for this, but you'd want to check a large number of topics
topicresult <- searchK(docs,vocab, topicnumbers, prevalence =~ Rwhite + Rfemale, data = meta)

plot(topicresult)
# 5 topics seems to be ideal based on the combination between high held out likelihood and semantic coherence as well as lower residuals compared to 6 topics

## Topic model
topic_model <- stm(output$documents, output$vocab, K = 5, data = output$meta, 
                   prevalence =~ Rwhite + Rfemale, init.type = "Spectral",
                   seed = 964851236) # setting the seed in the model as an extra precaution even though we set the seed above
# NOTE: If you are running multiple topic models, remember to set the seed for each!

## Validating
## 1: checking word prevalence
labelTopics(topic_model)

## 2: Reading examples
t1 <- findThoughts(topic_model, texts = substr(NAES$obama_over_mccain, 1, 400)[as.numeric(names(output$documents))], 
                   n = 2, topics = 1)$docs[[1]]
t2 <- findThoughts(topic_model, texts = substr(NAES$obama_over_mccain, 1, 400)[as.numeric(names(output$documents))], 
                   n = 2, topics = 2)$docs[[1]]
t3 <- findThoughts(topic_model, texts = substr(NAES$obama_over_mccain, 1, 400)[as.numeric(names(output$documents))], 
                   n = 2, topics = 3)$docs[[1]]
t4 <- findThoughts(topic_model, texts = substr(NAES$obama_over_mccain, 1, 400)[as.numeric(names(output$documents))], 
                   n = 2, topics = 4)$docs[[1]]
t5 <- findThoughts(topic_model, texts = substr(NAES$obama_over_mccain, 1, 400)[as.numeric(names(output$documents))], 
                   n = 2, topics = 5)$docs[[1]]

# plots of the examples
par(mfrow = c(1,2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(t1, width = 30, main = "Topic 1") # change = Obama
plotQuote(t2, width = 30, main = "Topic 2") # Barack > McCain
plotQuote(t3, width = 30, main = "Topic 3") # want an establishment change
plotQuote(t4, width = 30, main = "Topic 4") # Bush and McCain alike
plotQuote(t5, width = 30, main = "Topic 5") # Turnout of Democrats

## topic labels
topic_labels <- c("Obama Represents Change", "Obama Better Candidate", 
                  "Want Establishment Change", "Bush-McCain Similar",
                  "More Democrats Turned Out")

## Prevalence of topics
plot.STM(topic_model, type = "summary", labeltype="frex")

## Effects Estimate
# effect of being white on topic prevalence
estimate1 <- estimateEffect(1:5 ~ Rwhite, topic_model, metadata = meta, uncertainty = "Global")
summary(estimate1) # checking coefficients

estimate2 <- estimateEffect(1:5 ~ Rfemale, topic_model, metadata = meta, uncertainty = "Global")
summary(estimate2)

# plotting estimates
plot.estimateEffect(estimate1, 
                    covariate = "Rwhite", 
                    topics = 1:5, 
                    model = topic_model, 
                    method = "difference", 
                    cov.value1 = 1, 
                    cov.value2 = 0, 
                    xlab = "Change in topic likelihood when respondent is White", 
                    main = "Difference between Whites and Non-Whites in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

plot.estimateEffect(estimate2, 
                    covariate = "Rfemale", 
                    topics = 1:5, 
                    model = topic_model, 
                    method = "difference", 
                    cov.value1 = 1, 
                    cov.value2 = 0, 
                    xlab = "Change in topic likelihood when respondent is Female", 
                    main = "Difference between Females and Males in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))