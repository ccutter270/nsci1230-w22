# Caroline Cutter
# Partner: Diana Xu
# Worksheet 2: Computing the Fano Factor (FF)
# Wednesday, January 12, 2022

# -------------------------------- CODE -----------------------------------

# IMPORTS

library("ggplot2")                                   # ggplot2 library
library("tidyverse")                                 # tidyverse library
library("gridExtra")                                 # gridExtra library


# QUESTION 1: import files (comment out the one not being used)

setwd("~/Desktop/NSCI Data Science/Lab/Homework/HW 3")

spike.df <- read_csv("spikeTime_sim1.csv")            # data file
spike.df <- read_csv("spikeTimes_example1_FR.csv")    # data file



# --------------------------- WS 2: Fano Factor  ------------------------------


# QUESTION 2: Plotting Spike Times

# Graph 1


graph1 <- ggplot(data = spike.df) +                             
  geom_tile(mapping = aes(x = sp1, y = "Neuron 1", width = 20, height = .5)) +
  ggtitle("Spike Times Neuron 1") + 
  labs(x ="Spike Times (ms)", y = "Neuron")

graph1 


# *****EXTRA GRAPHS FOR THIS SECTION ****

# Graph 2
graph2 <- ggplot(data = spike.df) +                                     
  geom_histogram(mapping = aes(x = sp1, y = ..count../.5),   # create histogram with sp1
                 bins = 20,                                  # change bins
                 color="black",                              # design the graph
                 fill="slateblue1") +                             
  ggtitle("Firing Rate, Bin = 500 ms") +                      
  labs(x ="Time (ms)", y = "Firing Rate (Hz)")               


# Graph 3
graph3 <- ggplot(data = spike.df) +                                   
  geom_histogram(mapping = aes(x = sp1, y = ..count../.1),   # create histogram with sp1
                 bins = 100,                                 # change bins
                 color="black",                              # design graph
                 fill="slateblue1") +                              
  ggtitle("Firing Rate, Bin = 100 ms") +                  
  labs(x ="Time (ms)", y = "Firing Rate (Hz)")         


# Arranging grids using gridExtra
grid.arrange(graph1, graph2, graph3, nrow = 3)



# *****************************************************************************

# QUESTION 3: Compute the Average Firing Rate, r

num.spikes <- nrow(spike.df)   # Counts number of spikes
r <- num.spikes / 10           # number of spikes / seconds = frequency (Hz)
r                              



# *****************************************************************************

# QUESTION 4 / 5 : Bin the spikes and create a data frame with # spikes in each 
#                    bin, then add frequecy to the table  


# For SIM data 
spike.data <- data.frame(bin = cut(spike.df$sp1,    # cut spikes to bin variable
                   breaks = 500*(0:20),             # make 20 breaks of 500 ms
                   dig.lab = 5) )%>%                # no scientific notation
                   group_by(bin) %>%                # group by bins
                   summarise(count=n()) %>%         # count spikes in each bin
                   mutate(frequency = count/.5)     # add frequency 


# For EXAMPLE data 
spike.data <- data.frame(bin = cut(spike.df$sp1,    # cut spikes to bin variable
                                   breaks = 200*(0:50),             # make 20 breaks of 500 ms
                                   dig.lab = 5) )%>%                # no scientific notation
  group_by(bin) %>%                # group by bins
  summarise(count=n()) %>%         # count spikes in each bin
  mutate(frequency = count/.5)     # add frequency 

# *****************************************************************************

# QUESTION 6: make a histogram using geom_col

ggplot(data = spike.data) +                         
  geom_col(mapping = aes(x = bin, y = frequency),
           fill = "slateblue1",
           color = "black",
           alpha = .75 ) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  ggtitle("Average Firing Rates of Bins (500 ms)") + 
  labs(x ="Time (ms)", y = "Firing Rate (Hz)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8))    # adjust text


# *****************************************************************************

# QUESTION 7: compute the Fano Factor of the spike train (Variance / Mean) 


Fano.Factor <- var(spike.data$count) / mean(spike.data$count)
Fano.Factor   #


# *****************************************************************************
# QUESTION 8: repeat w/ the "example" file 





# ---------------------------- WS 3: ISI & CV  -------------------------------


# QUESTION 2: successive Spike Time Differences Data

spike.differences <- data.frame(difference = diff(spike.df$sp1))
spike.differences


# *****************************************************************************

# QUESTION 3: Create an ISI plot 

# Sim5 data 
ggplot(data = spike.differences) +                               
  geom_histogram(mapping = aes(x = difference ),      # create histogram with sp1
                 bins = 30,                           # change bins
                 color ="black",                      # design graph
                 fill ="pink",                   
                 alpha =  .75) +                   
  ggtitle("Interspike Interval") +                   
  labs(x ="Time Difference (ms)", y = "Count")     


# Example5 data 
ggplot(data = spike.differences) +                               
  geom_histogram(mapping = aes(x = difference ),      # create histogram with sp1
                 bins = 50,                           # change bins
                 color ="black",                      # design graph
                 fill ="pink",                   
                 alpha =  .75) +                   
  ggtitle("Interspike Interval") +                   
  labs(x ="Time Difference (ms)", y = "Count")       



# *****************************************************************************

# QUETION 4: Compute the coefficient of variation (CV) (variance / mean)

CV <- sd(spike.differences$difference) / mean(spike.differences$difference)
CV    # CV = 1.096702


# *****************************************************************************

# QUESTION 5: Repeat steps 1-4 with “spikeTime_simX_FR.csv”  



