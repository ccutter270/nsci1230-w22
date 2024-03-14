# LECTURE 10: Shiny!
# Wednesday, January 26, 2022

# ----------------------------------- START -----------------------------------

# INSTALLATIONS 
library(shiny)
library("tidyverse")
library("dplyr")
library("tidytext")
library("tidyr")





# ----------------------------------- APP -----------------------------------


# Break up lines with comments 

ui <- fluidPage(     # user interface (what users see)

  selectInput(inputId = "colorChoice",
              label = "Choose a color!",
              choices = c("red", "green", "blue")),  # let user select color 
  textInput(inputId = "titleChoice",                 # let user select title 
            label = "Choose a title!"),
  actionButton(inputId = "button",
               label = "Click to make graph!"), 
  plotOutput(outputId = "graph1")                   # display price vs carat graph 
  
  
  
  
)

server <- function(input, output, session) {   # number crunching R stuff

  
  output$graph1 <- renderPlot({
    
    input$button                     # when clicked, something changes and it runs
    
    diamonds %>%
      ggplot() +
      geom_point(aes(x = carat, y = price),
                 color = isolate(input$colorChoice)) +   # isolate
      labs(title = isolate(input$titlechoice))           # isolate 
  })
    
  
  
  
}

shinyApp(ui, server)
