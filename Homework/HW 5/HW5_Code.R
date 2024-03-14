# Caroline Cutter
# HW 5: Networks
# Wednesday, January 18, 2022

# -------------------------------- CODE -----------------------------------

# IMPORTS

library("ggplot2")                                   # ggplot2 library
library("tidyverse")                                 # tidyverse library
library("gridExtra")                                 # gridExtra library


setwd("~/Desktop/NSCI Data Science/Lab/Homework/HW 5")


# Initialize data frame --> column 1 = spike times, column 2 = neuron number
allSpikes.df <- data.frame(spikeTimes = NULL, cellID = NULL)

# Read in data frames then bind them to the big one 
spike.df <- read_csv("spikeTimes_neuron1_M1cortex.csv") %>% mutate(cellID = factor(1)) 
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron2_M1cortex.csv") %>% mutate(cellID = factor(2)) 
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron3_M1cortex.csv") %>% mutate(cellID = factor(3))
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron4_M1cortex.csv") %>% mutate(cellID = factor(4))
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron5_M1cortex.csv") %>% mutate(cellID = factor(5))
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron6_M1cortex.csv") %>% mutate(cellID = factor(6)) 
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron7_M1cortex.csv") %>% mutate(cellID = factor(7)) 
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron8_M1cortex.csv") %>% mutate(cellID = factor(8))
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron9_M1cortex.csv") %>% mutate(cellID = factor(9))
allSpikes.df <- rbind(allSpikes.df, spike.df)
spike.df <- read_csv("spikeTimes_neuron10_M1cortex.csv") %>% mutate(cellID = factor(10))
allSpikes.df <- rbind(allSpikes.df, spike.df)


allSpikes.df <- allSpikes.df[c("spikes", "cellID")]



# --------------------------- WS 5: Networks  ------------------------------


# PART 1:  Plot one of the graphs you designed 

# Raster Plot 
ggplot(data = allSpikes.df) +                               
  geom_tile(mapping = aes(x = spikes, 
                          y = cellID,
                          fill = cellID ), 
            width = .05) + 
  ggtitle("Spike Times over Time") +                   
  labs(x ="Spike Times (seconds)", y = "Neuron Number")  + 
  annotate("rect", xmin= 3.5, xmax = 4.5,
           ymin = 0 , ymax = 11.5, 
           alpha=0.2, color="blue", fill="blue") +
  annotate("text", x= 4, y = 11, label = "Stimulus", size = 3 ) + 
  theme_bw()




# *****************************************************************************

# PART 2: Plot the Three graphs we talked about in class


# FIRING RATE GRAPH (PER NEURON)

# Data for .5 second bins 
spike.data <- data.frame(bin = cut(allSpikes.df$spikes,     
                                   breaks = .5*(0:20),           # make 20 breaks of .5 seconds each
                                   dig.lab = 5) )%>%        
  group_by(bin) %>%                      # group by bins
  summarise(count = as.vector(table(bin)), bin = as.vector(names(table(bin)))) %>%    
  mutate(frequency = count/(.5 * 10))    # Frequency PER NEURON  = # spikes / time * N  


# Graph .5 second bins 
ggplot(data = spike.data) +                         
  geom_col(mapping = aes(x = bin, y = frequency),
           fill = "slateblue1",
           color = "black",
           alpha = .75 ) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  ggtitle("Average Firing Rates of Bins (0.5 Seconds)") + 
  labs(x ="Time (seconds)", y = "Average Firing Rate per Neuron (Hz)") + 
  theme(axis.text.x = element_text(angle = 45, vjust = .5),    # adjust text
        axis.text.y = element_text(angle = 45)) +
  annotate("rect", xmin= 7.5, xmax = 9.5,
         ymin = 0 , ymax = 10, 
         alpha=0.2, color="blue", fill="blue") + 
  annotate("text", x= 8.5, y = 9.5, label = "Stimulus", size = 4 )




# Data for .2 second bins 
spike.data <- data.frame(bin = cut(allSpikes.df$spikes,   
                                   breaks = .2*(0:50),    # make 50 breaks of .2 seconds each
                                   dig.lab = 5) )%>%
  group_by(bin) %>%                      # group by bins
  summarise(count = as.vector(table(bin)), bin = as.vector(names(table(bin)))) %>%        
  mutate(frequency = count/(.2 * 10))    # Frequency PER NEURON  = # spikes / time * N



# Graph .2 second bins  
ggplot(data = spike.data) +                         
  geom_col(mapping = aes(x = bin, y = frequency),
           fill = "slateblue1",
           color = "black",
           alpha = .75 ) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  ggtitle("Average Firing Rates of Bins (0.2 Seconds)") + 
  labs(x ="Time (seconds)", y = "Average Firing Rate per Neuron (Hz)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8),    # adjust text
        axis.text.y = element_text(angle = 45)) +
  annotate("rect", xmin= 17, xmax = 22.5,
           ymin = 0 , ymax = 12, 
           alpha=0.2, color="blue", fill="blue") + 
  annotate("text", x= 19.75, y = 11.5, label = "Stimulus", size = 4 )




# *****************************************************************************

# FANO FACTORS 


# FF FOR EACH CELL NUMBER 

spike.fano <- allSpikes.df %>%
  group_by(cellID) %>%
  mutate(bin = cut(spikes,
                   breaks = .25*(0:40),
                   dig.lab = 5)) %>%
  summarise(count = as.vector(table(bin)), bin = as.vector(names(table(bin)))) %>%
  summarize(fano = ( var(count) / mean(count)))

spike.fano


# FF FOR LABELED CELLS  


# Label all the cells based on before, during, after
before <- allSpikes.df %>% 
  filter(spikes < 3.5)  %>%
  mutate(label = "before")

during <- allSpikes.df %>% 
  filter(spikes >= 3.5 & spikes <= 4.5 )  %>%
  mutate(label = "during")

after <- allSpikes.df %>% 
  filter(spikes > 4.5) %>%
  mutate(label = "after")

allSpikes.df <- rbind(before, during, after)



# Calculate the FF 
spike.fano <- allSpikes.df %>% 
  group_by(label) %>%
  mutate(bin = cut(spikes,
                   breaks = .25*(0:40),
                   dig.lab = 5)) %>%
  summarise(count = as.vector(table(bin)), bin = as.vector(names(table(bin)))) %>% 
  mutate(countPerNeuron = count / 10 ) %>%
  mutate(firing.rate = count/(.25 * 10)) %>%
  group_by(label) %>%
  summarize(var(countPerNeuron)/mean(countPerNeuron))
  
spike.fano





# *****************************************************************************

# SYNCHRONY GRAPH 


# Spike differnces data 
spike.differences <- allSpikes.df %>%
  group_by(cellID) %>%
  summarize(difference = diff(spikes))


# Find min width - this doesn't work because it is too small 
min(spike.differences$difference)    # find min bin width = .002


spike.probability <- data.frame(bin = cut(allSpikes.df$spikes,
                                   breaks = .05*(0:200),       # make 200 of .05 seconds each
                                   dig.lab = 5) )%>%          
  group_by(bin) %>%                      # group by bins
  summarise(count = as.vector(table(bin)), bin = as.vector(names(table(bin)))) %>%  
  mutate(probability = count / 5)  %>%
  mutate(firing.rate = probability / (.05))     # R =  probability / seconds



 

# Graph 
ggplot(data = spike.probability) +                         
  geom_col(mapping = aes(x = bin, y = firing.rate),
           fill = "slateblue1",
           color = "slateblue1",
           alpha = .75,
           width = 1) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  ggtitle("Synchrony of Network Firing in 0.05 Second Bins") + 
  labs(x ="Time (seconds)", y = "Average Firing Rate (Hz)") +
  theme_bw() + 
  scale_x_discrete(breaks = c("(0,0.05]","(1.05,1.1]","(2.1,2.15]","(3.25,3.3]",
                              "(4.3,4.35]","(5.45,5.5]", "(6.65,6.7]", "(7.7,7.75]",
                              "(8.85,8.9]", "(9.95,10]")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8)) + 
  annotate("rect", xmin= 70, xmax = 90,
           ymin = 0 , ymax = 32, 
           alpha=0.2, color="blue", fill="blue") + 
  annotate("text", x= 80, y = 31, label = "Stimulus", size = 4 )
 



# *****************************************************************************

# ISI GRAPHS 


# Splitting data set 
before <- allSpikes.df %>% 
  filter(spikes < 3.5)

during <- allSpikes.df %>% 
  filter(spikes >= 3.5 & spikes <= 4.5 ) 

after <- allSpikes.df %>% 
  filter(spikes > 4.5)



# Differences of data sets 
before.differences <- before %>%
  group_by(cellID) %>%
  summarize(differences = diff(spikes))

during.differences <- during %>% 
  group_by(cellID) %>%
  summarize(differences = diff(spikes)) 


after.differences <- after %>% 
  group_by(cellID) %>%
  summarize(differences = diff(spikes)) 

  


# Graphing ISI plots 

before.ISI <- ggplot(data = before.differences) +                               
  geom_histogram(mapping = aes(x = differences ),    
                 bins = 50,                           
                 color ="black",                    
                 fill ="slateblue2",                   
                 alpha =  .75) +                   
  ggtitle("Before Stimulus ISI") +                   
  labs(x ="Time Difference (s)", y = "Spike Count") + 
  theme_bw() + 
  annotate("text", x= 1.25, y = 27, label = "CV = 1.38", size = 4 )

during.ISI <- ggplot(data = during.differences) +                               
  geom_histogram(mapping = aes(x = differences ),    
                 bins = 50,                           
                 color ="black",                    
                 fill ="slateblue2",                   
                 alpha =  .75) +                   
  ggtitle("During Stimulus ISI") +                   
  labs(x ="Time Difference (s)", y = "Spike Count") + 
  theme_bw() +
  annotate("text", x= .3, y = 5.5, label = "CV = 0.772", size = 4 )
  


after.ISI <- ggplot(data = after.differences) +                               
  geom_histogram(mapping = aes(x = differences ),    
                 bins = 50,                           
                 color ="black",                    
                 fill ="slateblue2",                   
                 alpha =  .75) +                   
  ggtitle("After Stimulus ISI") +                   
  labs(x ="Time Difference (s)", y = "Spike Count") + 
  theme_bw() +
  annotate("text", x= 1.25, y = 44, label = "CV = 1.28", size = 4 )


# Put them all together 
grid.arrange(before.ISI, during.ISI, after.ISI, nrow = 1) 
  
  
  

# *****************************************************************************

# CV'S FOR BEFORE, DURING, AFTER STIMULUS 


# Before CV = 1.376483
before.CV <- sd(before.differences$differences) / mean(before.differences$differences)

# During CV = 0.7717036
during.CV <- sd(during.differences$differences) / mean(during.differences$differences)


# After CV = 1.279079
after.CV <- sd(after.differences$differences) / mean(after.differences$differences)




