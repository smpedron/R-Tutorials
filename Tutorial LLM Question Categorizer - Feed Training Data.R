## LLM for categorizing survey questions with training data
## Stephanie Pedron (pedron.2@osu.edu)

rm(list=ls())

library(openai)
library(tidyverse)
library(dplyr)

Sys.setenv(OPENAI_API_Key ="API KEY HERE")

## making sample training data
training_data <- data.frame(
  question = c(
    "I find it easy to understand this visualization",
    "The visualization transparently includes all important elements of the data",
    "I would likely use this visualization and its information in my daily life"),
  category = c("Usability", "Transparency", "Usability"),
  subcategories = c(
    "ease of use, understandability",
    "clarity, completeness",
    "ease of use, intuitive"
  ),
  stringsAsFactors = FALSE
)

## Check map
category_map <- split(training_data$subcategories, training_data$category)

## LLM
categorize_with_llm_nested <- function(question, training_data) {
  # Split subcategories into lists
  training_data$subcat_list <- strsplit(training_data$subcategories, ",\\s*")
  
  # Category map
  category_map <- split(unlist(training_data$subcat_list), training_data$category)
  category_map <- lapply(category_map, function(x) unique(trimws(x)))
  
  # Format training examples
  training_examples <- paste0(
    apply(training_data, 1, function(row) {
      paste0(
        "Question: ", row["question"], "\n",
        "Category: ", row["category"], "\n",
        "Subcategories: ", row["subcategories"]
      )
    }),
    collapse = "\n\n"
  )
  
  # format category-to-subcategory map
  category_structure <- paste0(
    names(category_map), ": ",
    sapply(category_map, function(subs) paste(subs, collapse = ", ")),
    collapse = "\n"
  )
  
  # prompt
  prompt <- paste0(
    "You are a research assistant and an expert at categorizing statements based on predefined categories and subcategories.\n",
    "Each question belongs to one CATEGORY.\n",
    "Each category contains several SUBCATEGORIES.\n\n",
    "Here are valid categories and their subcategories:\n\n",
    category_structure,
    "\n\nHere are labeled examples:\n\n",
    training_examples,
    "\n\nNow, classify the following statement into a category and at least one subcategory:\n",
    question,
    "\n\nRespond in this format:\nCategory: [Category]\nSubcategory: [Subcategory]"
  )
  
  # OpenAI API call
  response <- tryCatch({
    openai::create_chat_completion(
      model = "gpt-4",
      messages = list(
        list(role = "system", content = "You categorize questions into a category and valid subcategories."),
        list(role = "user", content = prompt)
      ),
      max_tokens = 200,
      temperature = 0
    )
  }, error = function(e) {
    return(list(choices = list(list(message = list(content = "Error: API Request Failed")))))
  })
  
  # FOR DEBUGGING: Print the response structure
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

## Testing LLM
test_data <- tibble(
  question = c(
    "How easy was it for you to complete your task using the system?",
    "The system was easy to navigate",
    "How quickly were you able to learn how to use the tool?",
    "I am confident in the system",
    "Were the functions of the system understandable to you?"
  )
)

test_data$categorization <- apply(test_data, 1, function(row) {
  question <- row["question"]
  categorize_with_llm_nested(question, training_data) 
})

# Create new columns for Category and Subcategory by extracting from 'categorization'
test_data$Category <- sapply(test_data$categorization, function(x) {
  # Extract text after Category: and before Subcategory:
  category <- sub("Category:\\s*(.*)\\s*Subcategory:.*", "\\1", x)
  return(category)
})

test_data$Subcategory <- sapply(test_data$categorization, function(x) {
  # Extract text after Subcategory: 
  subcategory <- sub(".*Subcategory:\\s*(.*)", "\\1", x)
  return(subcategory)
})

