# LECTURE 10: Shiny - more interesting 
# Wednesday, January 26, 2022

# ----------------------------------- CODE -----------------------------------


# PACKAGES 

#install.packages("")
library("tidyverse")
library("dplyr")
library("tidytext")
library("tidyr")
library("rvest")
library("rnaturalearth")
library("shiny")


# ----------------------------- EXPLORE APPS ----------------------------------

# FINISH THESE NOTES TOMORROW




# Do all coding that doesn't need user input outside the app

# GOAL - Using the OKCupid profiles data set
# Allow the user to enter a word or phrase
# and display two things:
#   1) A random essay response containing that word or phrase
#   2) The number of times that word or phrase appears in each essay 


library(shiny)
library("tidyverse")

profiles <- read_csv("profiles.csv") 


# more efficient just to save this then read it in 
profiles.essays <- profiles %>%
  select(contains("essay")) %>%         # selects column names with essay in it
  pivot_longer(cols = everything(),
               names_to = "essayNumber",
               values_to = "essayResponse")     






ui <- fluidPage(
  
  textInput(inputId = "word",                 # let user select the word
            label = "Enter a word or phrase."),
  dataTableOutput(outputId  = "counts"),
  textOutput(outputId = "randomEssay"),
  
  
)

server <- function(input, output, session) {
  
  output$randomEssay <- renderText({ profiles.essays %>%
    filter(str_detect(essayResponse, input$word)) %>%      # filters only responses with word
    sample_n(1) %>%                                        # get's random response
    pull(essayResponse)                                    # pulls the random response
  })
  
  output$counts <- renderDataTable({ 
    profiles.essays %>% 
      mutate(word.count = str_count(essayResponse, input$word)) %>%
      group_by(essayNumber) %>%
      summarize(total = sum(word.count, na.rm = TRUE))
    
    
    })
  

  
  
}

shinyApp(ui, server)


