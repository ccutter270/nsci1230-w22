# Partners: Diana Xu and Caroline Cutter
# Worksheet 1: Exploring Spike Trains and Firing Rates
# Tuesday, January 11, 2022



# -------------------------------- CODE -----------------------------------

# IMPORTS

library("ggplot2")                                   # ggplot2 library
library("tidyverse")                                 # tidyverse library
library("gridExtra")                                 # gridExtra library


setwd("~/Desktop/NSCI Data Science/Lab/Homework/HW 5")
spike.df <- read_csv("spikeTimes_example5_FR.csv")   # data file




# *****************************************************************************


# Question 2: geom_tile plot of spike train 
ggplot(data = spike.df) +                             
  geom_tile(mapping = aes(x = sp1, y = "Neuron 1", width = 20, height = .5)) +
  ggtitle("Spike Times Neuron 1") + 
  labs(x ="Spike Times (ms)", y = "Neuron")
 


# *****************************************************************************
# Question 3: geom_histogram plot of spike train

# There are 105 spikes 
# The average firing rate is 10.5 hz  (105 spikes per 10 seconds)
 
ggplot(data = spike.df) +                                 # create plot             
  geom_histogram(mapping = aes(x = sp1),                  # create histogram with sp1
                 bins = 10,                               # change bins
                 color="black",                           # change color of lines
                 fill="blue") +                           # change color of fill
  labs(x ="Time (ms)", y = "Average Firing Rate (Hz)") +  # add axis labels 
  ggtitle("Average Firing Rates of Bins (1000 ms)")       # add title
  





# *****************************************************************************
# Question 4: 


# For graph 2 and 3 we did not relabel the y axis
# We made the bars shorter rather than making y labels taller (or vice versa) 
  

# Graph 1
graph1 <- ggplot(data = spike.df) +                             
  geom_tile(mapping = aes(x = sp1, y = "Neuron 1", width = 20, height = .5)) +
  ggtitle("Graph 1: Spike Times Neuron 1") + 
  labs(x ="Spike Times (ms)", y = "Neuron")

# Graph 2
graph2 <- ggplot(data = spike.df) +                          # create plot             
  geom_histogram(mapping = aes(x = sp1, y = ..count../.5),   # create histogram with sp1
                 bins = 20,                                  # change bins
                 color="black",                              # change color of lines
                 fill="pink") +                              # change color of fill
  ggtitle("Graph 2: Firing Rate, Bin = 500 ms") +                     # add title 
  labs(x ="Time (ms)", y = "Firing Rate (Hz)")               # add axis labels


# Graph 3
graph3 <- ggplot(data = spike.df) +                          # create plot             
  geom_histogram(mapping = aes(x = sp1, y = ..count../.1),   # create histogram with sp1
                 bins = 100,                                 # change bins
                 color="black",                              # change color of lines
                 fill="pink") +                              # change color of fill
  ggtitle("Graph 3: Firing Rate, Bin = 100 ms") +                     # add title 
  labs(x ="Time (ms)", y = "Firing Rate (Hz)")               # add axis labels


# Arranging grids using gridExtra
grid.arrange(graph1, graph2, graph3, nrow = 3)


