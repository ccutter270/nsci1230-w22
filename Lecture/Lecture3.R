# LECTURE 3: Data Wrangling 
# Wednesday, January 12, 2022

# ----------------------------------- NOTES -----------------------------------


# -------------------- DATA WRANGLING -------------------------


# INSTALLATIONS 
#install.packages('tidyverse')

library("tidyverse")
library("dplyr")

diamonds 


# group_by():

    # Filters data set based on the conditions it gives
      # only 1 required variable - .data
     group_by(.data = diamonds)
    
    # can insert names of columns that you want to group by
      grouped.diamonds <- group_by(.data = diamonds, color)
      
    # Now lets summarize data using the grouped data...
      # Outputs mean and median by group!
      summary.stats <- summarize(.data = grouped.diamonds, 
                meanPrice = mean(price), 
                medainCarat = median(carat))
      
    # What is the average carat of expensive diamos (<10000) with colors E and F? 
      
      # SLOW AND COMPLICATED 
      x <- filter(.data = diamonds, price > 10000, color %in% c("E", "F"))
      grouped.x <- group_by(.data = x, color)
      sumarized.x <- summarize(grouped.x, mean = mean(carat))
      sumarized.x
      
      # We can make this faster using the pipe operator....
      

# EFFICIENT - pipe operator %>%

      vector1 <- c(1, 2, 3)
      
      # Using the pipe operator, we can change this...
      mean(vector1)
      
      # to this....
      vector1 %>% mean()
      
      # The pipe says take the thing on the left side and make it the first
      #     argument on the left side calculations 
      
      # This is useful because we do not have to store our data set multiple times
      
      # Back to the example - EFFICIENT WAY! This outputs the same as above!
      
      diamonds %>% 
        filter(price > 10000, color %in% c("E", "F")) %>% 
        group_by( color) %>% 
        summarize( mean = mean(carat))
               

  
# ACTIVITY 

#install.packages('nycflights13')
library("nycflights13")

# QUESTION: What is the average delay of flights in to Burlington Airport and logan Airport 
  
flights
View(flights)
  
flights %>% 
        filter(dest %in% c("BTV", "BOS"), arr_delay > 0) %>% 
        group_by(dest) %>% 
        summarize( mean = mean(arr_delay, na.rm = TRUE))  

# BONUS: What PROPORTION of flights are delayed into these airports?

flights %>% 
  filter(dest %in% c("BTV", "BOS")) %>% 
  group_by(dest) %>% 
  summarize( mean = mean(arr_delay > 0, na.rm = TRUE)) 


  # This works because :

  x <- c(0, 0, 1)    # Average of this is 0


  # Example:
    x2 <- c (1, 1, 0, 1, 0) # this turns into c(TRUE  TRUE FALSE  TRUE FALSE)
    
    #Then it takes the average of all 1's and 0's which represent delayed or not


    
# ACTIVITY - Recreating graphs 
    
# IMPORTs 
library(tidyverse)
library(gridExtra)
library(nycflights13)

    
# Graph 1:
    
    graph1.data <- flights %>% 
      filter(origin %in% c("JFK", "LGA")) 
    
    ggplot(data = graph1.data) +                         
      geom_violin(mapping = aes(x = origin, y = dep_time, fill = origin)) + 
      ggtitle("Graph 1") + 
      labs(x ="Airport of Origin", y = "Departure Time")

 
# Graph 2: 
    
    # Setting the data set 
    graph2.data <- flights %>% 
                    group_by(carrier) %>%
                    summarize(average.travel = mean(distance))

    # Graphing the data
    ggplot(data = graph2.data) +                         
      geom_col(mapping = aes(x = carrier, y = average.travel, fill = average.travel)) + 
      ggtitle("Graph 2") + 
      labs(x ="Carrier", y = "Average Distance Traveled", fill = "Average Distance Traveled")
    

# Graph 3: 
    
    # Setting the data set 
    graph3.data <- flights %>%
                    group_by(carrier) %>%
                    summarize(average.travel = mean(distance))
    
   
    # Graphing the data
    ggplot(data = graph3.data) +                         
      geom_col(mapping = aes(x = reorder(carrier, -average.travel), y = average.travel, fill = average.travel)) + 
      ggtitle("Graph 3") + 
      labs(x ="Carrier", y = "Average Distance Traveled", fill = "Average Distance Traveled")

  

# Graph 4: 
    
    # Setting the data set 
    graph4.data <- flights %>%
      filter(carrier %in% c("DL", "HA", "OO")) %>%
      group_by(carrier, month) %>%
      summarize(mean = mean(dep_delay, na.rm = TRUE))
  
    ggplot(data = graph4.data) +                         
      geom_col(mapping = aes(x = carrier, y = mean, fill = carrier), color = "black") +
      geom_hline(yintercept = 0, size = 1, color = "red") + 
      facet_wrap(~month) + 
      labs(title = "Graph 4 (Departure Delays by Month)",
           x = "Carrier",
           y = "Average Departure Delay of All Flights") 
    
    
