################################ 
## This script will read in the M1 cortex data from worksheet 5 and create several plots:
############################################
# set directory:
wdd <- setwd('~/Downloads')

library('ggplot2')
library(tidyverse)

# set up empty data frame to hold spike times and cell ID
allSpikes.df <- data.frame(spikes = NULL, cellID = NULL)
# read in data 
for (i in seq(1,10))
{ 
  spike.df <- read_csv(paste0("spikeTimes_neuron",i,"_M1cortex.csv")) %>% 
    mutate(cellID = factor(i))
  allSpikes.df <- rbind(allSpikes.df, spike.df)
}
# only keep the two columns I want: spikes and cellID
allSpikes.df <- allSpikes.df[c("spikes","cellID")]

# trial info
start <- 3.5 #start of stim  -- seconds 
stop <- 4.5 #end of stim  -- seconds 
T <- 10 # total time of trial -- seconds
N <- 10 # number of spike trains 
##########################
### Spike raster plot ####
##########################

spikeTrain <- allSpikes.df %>% 
  ggplot() +
  geom_tile(aes(x = spikes, y = cellID), width = 0.02, height = 0.6) +
  annotate("rect", xmin = start, xmax = stop, ymin = 0, ymax = N+0.5, fill = "purple", color ="black", alpha = 0.4)+
  labs(x = "Time (seconds)", y = "Cell ID", title = "Spike raster") +
  theme_classic() 
spikeTrain

##############################
### Firing-rate histogram ####
##############################

# population level FR histogram
# first, add a column to our data frame with a label for before, during, and after
before.df <- allSpikes.df %>% 
  filter(spikes<start) %>%
  mutate(stimID = "before")
during.df <- allSpikes.df %>% 
  filter(spikes>=start & spikes <= stop) %>%
  mutate(stimID = "during")
after.df <- allSpikes.df %>% 
  filter(spikes>stop) %>%
  mutate(stimID = "after")

# put that column onto allSpikes
allSpikes.df <- rbind(before.df, during.df, after.df)

# bin width
dt <- 0.25; 
# create new data frame with a column of counts, a column of bins, and a column of firing rate
pop.firingRate.df <- allSpikes.df %>% 
  group_by(stimID) %>%
   mutate(bins = cut(spikes, breaks = seq(0,T,dt), dig.lab = 5)) %>%
  summarise(count = as.vector(table(bins)), bins = as.vector(names(table(bins))))%>% 
  mutate(countPerNeuron = count/N) %>%
  mutate(FR = count/(dt*N))

popFR_histogram <- pop.firingRate.df %>% ggplot() +
  geom_col(aes(x = bins, y = FR), fill = "grey", color = "black") +
  annotate("rect", xmin = 4*start+dt, xmax = 4*stop+dt, ymin = 0, ymax = N+0.5, fill = "purple", color ="black", alpha = 0.4)+
  xlab("Time (s)") + ylab("Firing rate (Hz)") +
  theme_classic()+
  #facet_wrap(~stimID)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.05, hjust=1))
popFR_histogram
ggsave("M1cortex_popFRhistogram.png",popFR_histogram)

# calculate the Fano factor 
FF_all <- pop.firingRate.df %>% 
  group_by(stimID) %>%
  summarize(var(countPerNeuron)/mean(countPerNeuron))

####################
#### Synchrony:####
####################
dt_small <- 0.1;
synch.df <- allSpikes.df %>% 
  #group_by(stimID) %>%
  mutate(bins = cut(spikes, breaks = seq(0,T,by = dt_small), dig.lab = 5)) %>%
  summarise(count = as.vector(table(bins)), bins = as.vector(names(table(bins))))%>% 
  mutate(countPerNeuron = count/N) %>%
  mutate(FR = count/(dt_small*N))

# plot firing rate as a curve instead of a histogram
synchPlot<- synch.df %>% ggplot() +
  geom_line(aes(x = seq(0+dt_small,T, by = dt_small), y=FR)) +
  annotate("rect", xmin = start, xmax = stop, ymin = 0, ymax = 15, fill = "purple", alpha = 0.3)+
  geom_segment(aes(x = start, xend = stop, y = 0, yend = 0), size = 6, color = "black")+
  xlab("Time (s)") + ylab("Firing rate per neuron (Hz)") +
  theme_classic()
ggsave("M1cortex_synchronyPlot.png",synchPlot)

####################
######## ISI: ######
####################


beforeStim.df <- allSpikes.df %>% filter(stimID == "before")
duringStim.df <- allSpikes.df %>% filter(stimID == "during")
afterStim.df <- allSpikes.df %>% filter(stimID == "after")
# check that have all the data
nrow(allSpikes.df)
nrow(beforeStim.df) + nrow(duringStim.df) + nrow(afterStim.df)


# make ISI for each:
# ISI per cell ID
ISIbefore.df <- beforeStim.df %>% 
  group_by(cellID) %>%
  summarise(ISI = diff(spikes))

# calculate CV
cv.before <- sd(ISIbefore.df$ISI)/mean(ISIbefore.df$ISI)

# plot ISI
ISIbefore.graph <- ISIbefore.df %>%
  ggplot() +
  geom_histogram(aes(x = ISI), color = "black", fill = "darkgreen", alpha = 0.6, 
                          bins = 30) +
  labs(x = "Time difference (ms)", y = "Count", title = paste("ISI - before stimulus, CV = ", round(cv.before, digits = 2))) +
  theme_classic()
ISIbefore.graph

# during
ISIduring.df <- duringStim.df %>% 
  group_by(cellID) %>%
  summarise(ISI = diff(spikes))

# calculate CV
cv.during <- sd(ISIduring.df$ISI)/mean(ISIduring.df$ISI)

ISIduring.graph <- ISIduring.df %>%
  ggplot() +
  geom_histogram(aes(x = ISI), color = "black", fill = "purple", alpha = 0.6, 
                 bins = 30) +
  scale_x_continuous(limits = c(0,2)) +
  labs(x = "Time difference (ms)", y = "Count", title = paste("ISI - during stimulus, CV = ", round(cv.during, digits = 2)))+
  theme_classic()
ISIduring.graph

# after
ISIafter.df <- afterStim.df %>% 
  group_by(cellID) %>%
  summarise(ISI = diff(spikes))

# calculate CV
cv.after <- sd(ISIafter.df$ISI)/mean(ISIafter.df$ISI)

ISIafter.graph <- ISIafter.df %>%
  ggplot() +
  geom_histogram(aes(x = ISI), color = "black", fill = "darkgreen", alpha = 0.6, 
                 bins = 30) +
  labs(x = "Time difference (ms)", y = "Count", title = paste("ISI - after stimulus, CV = ", round(cv.after, digits = 2)))+
  theme_classic()
ISIafter.graph

# use grid.arrange to put them together
library("gridExtra")
finalISI <- grid.arrange(ISIbefore.graph,ISIduring.graph,ISIafter.graph,nrow=3)
ggsave("M1cortex_ISIplots.png",finalISI)

