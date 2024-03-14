# Caroline Cutter
# HW 4: Networks
# Wednesday, January 18, 2022

# -------------------------------- CODE -----------------------------------

# IMPORTS

library("ggplot2")                                   # ggplot2 library
library("tidyverse")                                 # tidyverse library
library("gridExtra")                                 # gridExtra library



setwd("~/Desktop/NSCI Data Science/Lab/Homework/HW 4")

# Initialize data frame --> column 1 = spike times, column 2 = neuron number
allSpikes.df <- data.frame(spikeTimes = NULL, cellID = NULL)


# Read in data frames then bind them to the big one 
spike.df <- read_csv("spikeTime_sim1.csv") %>% mutate(cellID = factor(1)) 
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTime_sim2.csv") %>% mutate(cellID = factor(2)) 
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTime_sim3.csv") %>% mutate(cellID = factor(3))
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTime_sim4.csv") %>% mutate(cellID = factor(4))
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTime_sim5.csv") %>% mutate(cellID = factor(5))
allSpikes.df <- rbind(allSpikes.df, spike.df)



# --------------------------- WS 4: Networks  ------------------------------


# PART 1: CREATE SOME RASTER PLOTS 

ggplot(data = allSpikes.df) +                               
  geom_point(mapping = aes(x = spikes, 
                          y = cellID,
                          color = cellID,
                          shape = cellID),  
                          size = .1) + 
  ggtitle("Raster Plot Neurons 1 - 5") +                   
  labs(x ="Spike Times", y = "Neuron Number")   




# FAVORITE
ggplot(data = allSpikes.df) +                               
  geom_tile(mapping = aes(x = spikes, 
                          y = cellID,
                          fill = cellID ), 
            width = 15) + 
  ggtitle("Raster Plot Neurons 1 - 5") +                   
  labs(x ="Spike Times", y = "Neuron Number")  + 
  theme_dark()





# *****************************************************************************



# PART 2: CREATE FIRING RATE & ISI PLOT // CALCULATE FF & CV

allSpikes.df


# FIRING RATE PLOT 

spike.data <- data.frame(bin = cut(allSpikes.df$spikes,              # cut spikes to bin variable
                                   breaks = 500*(0:20),              # make 20 breaks of 500 ms
                                   dig.lab = 5) )%>%                 # no scientific notation
                                   group_by(bin) %>%                 # group by bins
                                   summarise(count=n()) %>%          # count spikes in each bin
                                   mutate(frequency = count/(.5* 5)) # add frequency - num spikes / time * N

ggplot(data = spike.data) +                         
  geom_col(mapping = aes(x = bin, y = frequency),
           fill = "slateblue1",
           color = "black",
           alpha = .75 ) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  ggtitle("Average Firing Rates of Bins (500 ms)") + 
  labs(x ="Time (ms)", y = "Firing Rate per Neuron (Hz)") + 
  theme(axis.text.x = element_text(angle = 45, vjust = .5),    # adjust text
        axis.text.y = element_text(angle = 45))





# FANO FACTOR

spike.fano <- allSpikes.df %>%
  group_by(cellID) %>%
  mutate(bin = cut(spikes,
                       breaks = 250*(0:40),    # make 20 breaks of 500 ms
                       dig.lab = 5)) %>%
  summarise(count = as.vector(table(bin)), bin = as.vector(names(table(bin)))) %>%
  summarize(fano = ( var(count) / mean(count)))

spike.fano
             



# CHECK INDIVIDUAL FANO FACTORS 

spike.check <- allSpikes.df %>%
  group_by(cellID) %>%
  mutate(bin = cut(spikes,
                   breaks = 500*(0:20),
                   dig.lab = 5)) %>%
  count(cellID, bin)


ggplot(data = spike.check) +                         
  geom_col(mapping = aes(x = bin,y = n ),
           fill = "slateblue1",
           color = "black",
           alpha = .75 ) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  facet_wrap(~cellID) + 
  ggtitle("Average Firing Rates of Bins (500 ms)") + 
  labs(x ="Time (ms)", y = "Firing Rate per Neuron (Hz)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5))







# ISI PLOT 

spike.differences <- allSpikes.df %>%
  group_by(cellID) %>%
  summarize(difference = diff(spikes))


ggplot(data = spike.differences) +                               
  geom_histogram(mapping = aes(x = difference ),      # create histogram with sp1
                 bins = 50,                           # change bins
                 color ="black",                      # design graph
                 fill ="pink",                   
                 alpha =  .75) +                   
  ggtitle("Interspike Interval") +                   
  labs(x ="Time Difference (ms)", y = "Count")       





# CV 



CV <- sd(spike.differences$difference) / mean(spike.differences$difference)
CV  # CV = 0.5096443






# *****************************************************************************


# QUESTION 3: Synchronous 


min(spike.differences$difference)    # find min bin width = 20.34 
                                     # SO, bin width 20 & none will be the same




spike.probability <- data.frame(bin = cut(allSpikes.df$spikes,  # cut spikes to bin variable
                                   breaks = 20*(0:500),         # make 500 breaks of 20 ms
                                   dig.lab = 5) )%>%            # no scientific notation
              group_by(bin) %>%                                 # group by bins
              summarise(count=n()) %>%                          # count spikes in each bin
              mutate(probability = count / 5)  %>%
              mutate(firing.rate = probability / (.02))           # probability / seconds



# Synchrony Column Graph 

ggplot(data = spike.probability) +                         
  geom_col(mapping = aes(x = bin, y = probability),
           fill = "slateblue1",
           color = "slateblue1",
           alpha = .75,
           width = .001) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  ggtitle("Synchrony of Network Firing") + 
  labs(x ="Time (ms)", y = "Probability of Firing") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8)) +
  scale_x_discrete(breaks = c("(20,40]","(1020,1040]","(2020,2040]","(3020,3040]",
                            "(4020,4040]","(5020,5040]", "(6020,6040]", "(7000,7020]",
                            "(8000,8020]", "(9000,9020]", "(9960,9980]"))



# Synchrony Line graph 

ggplot(data = spike.probability) +                         
  geom_line(mapping = aes(x = bin, y = firing.rate, group = 1),
            size = .5,
            color = "slateblue1",
            alpha = .75,) +
  geom_hline(yintercept = 0, size = .25, color = "black") +
  ggtitle("Firing Rate") + 
  labs(x ="Time (ms)", y = "Firing Rate (Hz)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8)) + 
  scale_x_discrete(breaks = c("(20,40]","(1020,1040]","(2020,2040]","(3020,3040]",
                            "(4020,4040]","(5020,5040]", "(6020,6040]", "(7000,7020]",
                            "(8000,8020]", "(9000,9020]", "(9960,9980]"))
  
 

  