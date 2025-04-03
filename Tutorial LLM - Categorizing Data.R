## Tutorial: Using an LLM to categorize data
## Stephanie Pedron (pedron.2@osu.edu)

rm(list=ls())

library(openai)
library(readxl)
library(tidyverse)
library(dplyr)

test_data <- read_xlsx("C:/Users/steph/Documents/Courses/PhD/Research Projects/AI (Kennedy)/LLM/test_data.xlsx")

Sys.setenv(OPENAI_API_Key ="PUT YOUR API HERE")

## making categories and subcategories based on some keywords
## note: can also just let the model choose the categories
categories <- list(
  "Reliability" = c("accuracy", "consistency", "dependability", "stability", "predictability"),
  "Usability" = c("ease of use", "user-friendly", "intuitive", "accessibility", "simplicity"),
  "Transparency" = c("transparency", "clarity", "understandability", "openness"),
  "Performance" = c("efficiency", "performance", "speed", "responsiveness", "functionality"),
  "Security" = c("data protection", "privacy", "encryption", "safety", "confidentiality"),
  "Ethical Decision-Making" = c("ethics", "fairness", "bias", "accountability"),
  "Interpersonal Trust" = c("empathy", "interaction quality", "communication", "personalization"),
  "Adaptability" = c("learning", "adaptability", "improvement", "evolution"),
  "Risk Tolerance" = c("risk", "uncertainty", "comfort", "assurance", "willingness"),
  "Support and Maintenance" = c("customer service", "support", "maintenance", "upgrades")
)


## categorize questions function
categorize_with_llm <- function(question, categories) {
  prompt <- paste0(
    "Categorize the following statement using the options provided below, selecting one category and one subcategory.\n",
    "The categories and subcategories are as follows:\n\n", # your prompt or what you want it to do
    paste0(
      names(categories), ": ", 
      sapply(categories, function(subs) paste(subs, collapse = ", ")), 
      collapse = "\n"
    ),
    "\n\nStatement: ", question, 
    "\n\nRespond in the format: Category: [Category], Subcategory: [Subcategory]." # how you want it to return the data, not always perfect!
  )
  
  # OpenAI API request
  response <- tryCatch({
    openai::create_chat_completion(
      model = "gpt-4",  
      messages = list(
        list(role = "system", content = "You are a research assistant and an expert at categorizing questions based on predefined categories and subcategories."),
        list(role = "user", content = paste0("Question: ", question)), # the role of the AI
        list(role = "system", content = paste0("Categories and subcategories:\n", 
                                               paste0(names(categories), ": ", 
                                                      sapply(categories, function(subs) paste(subs, collapse = ", ")), 
                                                      collapse = "\n"))
        )
      ),
      max_tokens = 150,  
      temperature = 0
    )
  }, error = function(e) {
    return(list(choices = list(list(message = list(content = "Error: API Request Failed")))))
  })
  
  # FOR DEBUGGING PURPOSES: Print the response structure
  print(str(response))
  
  # Access message content based on the actual response structure
  if (!is.null(response$choices) && nrow(response$choices) > 0) {
    result <- tryCatch({
      content <- response$choices$message.content[1]
      trimws(content)
    }, error = function(e) {
      return(NA)
    })
    
    return(result)
  } else {
    return(NA)
  }
}

## Notes:
# prompt = tell it what to do (ask chatGPT to categorize the question into specific subcategories)
# model = can change to gpt-3.5-turbo or other models
# max_tokens = max response length
# temperature = 0: output will be more consistent. The model will likely provide the same output for the same prompt every time, making it more reliable
# temperature = 1: output will be more creative and varied, which might be useful for tasks like writing or brainstorming


## let's pull 15 random observations from our list of survey questions for a brief categorization test
set.seed(100)
test_data2 <- test_data[sample(nrow(test_data), 15), ]


# apply categorization function to each question
test <- test_data2 %>%
  mutate(category = sapply(Question, categorize_with_llm, categories = categories))

head(test)


