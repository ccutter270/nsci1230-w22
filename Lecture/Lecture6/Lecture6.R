# LECTURE 6: Profiles Data Continued 
# Wednesday, January 19, 2022

# ----------------------------------- CODE -----------------------------------


# PACKAGES 

#install.packages("tidytext")
library("tidyverse")
library("dplyr")
library("tidytext")

setwd("~/Desktop/NSCI Data Science/Lecture/Lecture6")

profiles <- read_csv("profiles.csv")                              # data file


# ------------------------- NEW TOOLS ------------------------------



# "TOKENIZING" TEXT - with large blocks texts, it is going break it into 
#                     different tokens based on the word inside the text 


essay2.tokens <- profiles %>%
  unnest_tokens(input = essay2,          # takes input column to tokenize
                output = "word")         # makes new column in new data set 
  

essay2.tokens %>%                     
  count(word) %>%          # count the words to see most popular 
  arrange(-n) %>%          # arrange in decreasing order to see the most common
  #filter(! (word %in% c("a", "the", "to", "at"))) # inefficient way 
  anti_join(stop_words)    # takes away the stop words (things that don't change the meaning)





# TOKENIZE ESSAY 7, "On a typical Friday night"


essay7.tokens <- profiles %>%
  unnest_tokens(input = essay7,          # takes input column to tokenize
                output = "word")         # makes new column in new data set 


essay7.tokens %>%                     
  count(word) %>%          # count the words to see most popular 
  arrange(-n) %>%          # arrange in decreasing order to see the most common
  #filter(! (word %in% c("a", "the", "to", "at"))) # inefficient way 
  anti_join(stop_words)    # takes away the stop words (things that don't change the meaning)



# Comparing Drinking categories with all vs people who mentioned it in essay 7

graph1 <- essay7.tokens %>%
  filter(word == "drinking") %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = drinks),
           color = "black") +            # show proportions instead of counts
  theme_bw()


graph2 <- profiles %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = drinks),
           color = "black") +            # show proportions instead of counts
  theme_bw()


grid.arrange(graph1, graph2, nrow = 2)




# ------------------------- WEB SCRAPING ------------------------------

# Something exists on the internet, I want it! We can bring it to R! 

install.packages("rvest")
library("rvest")



# READ IN A WEBPAGE 


# Joe Biden's Wiki Page! 
url <- "https://en.wikipedia.org/wiki/Joe_Biden"

biden.text <- url %>% 
  read_html() %>%             # reads HTML code of given URL / webpage
  html_elements("p")  %>%     # takes HTML, filters it for stuff we care about
                              # almost every website stores in p element 
  html_text()                 # converts html text to useful R text 



# Lets look at Biden's 10th paragraph 
biden.text[10]


# Looking at most common words
# biden.text is a vector, not a data frame, so we cannot use the same format 

# We can turn it into a data set though! 

biden.data <- tibble(text = biden.text)


biden.data %>%
  unnest_tokens(input = "text",        
                output = "word")  %>% 
  count(word) %>%         
  arrange(-n) %>%         
  anti_join(stop_words)    




# SCRAPING IN A DATASET 


brady.url <- "https://en.wikipedia.org/wiki/Tom_Brady"


brady.data <- brady.url %>% 
  read_html() %>%
  # We want a specific element, right click, inspect, copy xpath 
  html_element(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]')  %>%
  html_table()


# THE FIRST ROW IS WHAT THE TITLES SHOULD BE! Lets fix this 

colnames(brady.data) <- brady.data[1, ]

# brady.data <- brady.data[-1, ]     # Get rid of first row now - BAD WAY 

brady.data2 <- brady.data[-1, ]    # INSTEAD DO THIS! 
  


# GRAPH THIS DATA - Brady's passing yards over time!

# OH NO - we have multiple columns with the same name! We need to fix this 
#         before we do the next thing - we actually want to reverse what we 
#         did before and paste together original column names 







# --------------------- FINAL GRAPH ---------------------

brady.url <- "https://en.wikipedia.org/wiki/Tom_Brady"


brady.data <- brady.url %>% 
  read_html() %>%
  # We want a specific element, right click, inspect, copy xpath 
  html_element(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]')  %>%
  html_table()

colnames(brady.data) <- paste(colnames(brady.data), brady.data[1, ])


# Two problems - recognizes passing yards as text & has total passing yards
#                does this because the comma in the numbers, Lets fix this...

brady.data2 <- brady.data[-c(1, 24), ]    # INSTEAD DO THIS! (first and last row) 

# Make a graph with this data 
brady.data2 %>% 
  mutate(passing.yards.numeric = as.numeric(str_remove_all(`Passing Yds`, 
                                                           "[:punct:]"))) %>%
  ggplot() + 
  geom_line(mapping = aes(x = as.numeric(`Year Year`),
                          y= passing.yards.numeric))





















