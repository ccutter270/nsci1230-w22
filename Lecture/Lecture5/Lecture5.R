# LECTURE 5: Profiles Data
# Tuesday, January 18, 2022

# ----------------------------------- CODE -----------------------------------


# PACKAGES 

library("tidyverse")
library("dplyr")

setwd("~/Desktop/NSCI Data Science/Lecture/Lecture5")

profiles <- read_csv("profiles.csv")                              # data file


# ------------------------- REVIEW  ------------------------------


# BOXPLOT of height  

profiles %>%
  ggplot() +
  geom_boxplot(aes(x = height, y = sex)) +    # another way to split by sex
  xlim(48, 96)                                # adjust for unreasonable heights
# facet_grid(~sex) +                          # using facet to split by sex
  

# HISTOGRAM of height
#   histograms are used or continuous variables 

profiles %>%
  ggplot() +
  geom_histogram(aes(x = height, fill = sex)) +   # another way to split by sex
  xlim(48, 96)                                    # adjust for unreasonable heights


# BAR of height
#   - geom_bar is used for non-continuous, height isn't really continuous 
#   - This shows that there are more men than females
#   - This also shows that people are rounding heights because more at the 
#     even numbers and at the feet marks

profiles %>%
  ggplot() +
  geom_bar(aes(x = height, fill = sex)) + 
  xlim(48, 96) +                            # adjust for unreasonable heights
 facet_wrap(~sex)                           # using facet to split by sex

    

# COUNTS - men vs female, there are way more males
 profiles %>%
   count(sex)

 
 
 
 # ------------------------- NEW! ------------------------------
 
 # Most of the profiles have text in them instead of just variables
 
 
 # BODY TYPE VARIABLE - exploration 
 
 #  - How many different body types are entered? 13 
 #  - Can't use Geom_bar with count because it already does that for us 
 #  - SO if you have the count, you need to use geom_col and pass the count as y
 #    
 profiles %>%
   count(body_type) %>%       # Groups by body type response 
   arrange(-n) %>%            # arranges them in de/increasing order
   ggplot() +
     geom_col(mapping = aes(x = reorder(body_type, n), y = n))  # graph it!
 
 
 
 
 # ESSAYS - exploration 
 
#  ******  STRING HASHING  ****** 
 
toy.vector <- c("Alex", "Becky", "Charlie")

# str_detect() - detects if a given string is present, case sensitive
str_detect(toy.vector, "a")
 
# str_replace() - replaces first variable given with a given string
  # str_replace_all() - replaces all 
 str_replace(toy.vector, "a", ":)")
 
 

#   QUESTION 1: do people whoo like to play soccer have different body types
#               than those who don't mention soccer at all in essay 0?
 
 
 soccer.data <- profiles %>% 
   mutate(soccer = str_detect(essay0, "soccer"))    # creates new variable 

 
 # Check that this worked 
  yes.soccer <- soccer.data %>%
    filter(soccer == TRUE)
 
 
# Lets make our graph, first count combinations of body type and soccer
  soccer.data %>% 
    count(body_type, soccer) %>%
    na.omit() %>%
    ggplot() + 
    geom_col(mapping = aes(x = body_type, y = n, fill = soccer))
 

  # This graph is kind of lame - counts are so different because not nearly
  # as many people that metion soccer vs not, so instead of counts lets use 
  # proportions of people who like soccer and their body types 
  
# QUESTION 2: What proportion of people like soccer - use the mean() trick!
  # This gives us the proportion of... what proportion of the people who reported
  # a specific body type reported like soccer
  
  soccer.data %>%
    group_by(body_type) %>%
    summarize(prop = mean(soccer, na.rm = TRUE))

  
# BUT, we want out of the people who liked soccer have what body type
  
  soccer.data %>%
    group_by(soccer)           

    
    # Above code is trickier than expected, lets make geom_bar do it for us
  
  
  # position = "fill" = proportion
  # position = "dodge" = puts bars next to each other
    
soccer.data %>%
  filter(!is.na(soccer)) %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = soccer, fill = body_type),
           color = "black",
           position = "fill") +            # show proportions instead of counts
  theme_bw()
   


# CHOOSE ESSAY PROMPT, DIG INTO ESSAY 7 "On a typical Friday night   



friday.data <- profiles %>% 
  mutate(drinking = str_detect(essay7, "drinking"))  %>%   
  mutate(sleeping = str_detect(essay7, "sleep")) %>%   
  mutate(eating = str_detect(essay7, "eat"))   
   


# DRINKING AND DRUGS
friday.data %>%
  filter(!is.na(drinking)) %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = drinking, fill = drugs),
           color = "black",
           position = "fill") +            # show proportions instead of counts
  theme_bw()



# SLEEP AND DRUGS 
friday.data %>%
  filter(!is.na(sleeping)) %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = sleeping, fill = drugs),
           color = "black",
           position = "fill") +            # show proportions instead of counts
  theme_bw()

# EATING AND DRUGS 
friday.data %>%
  filter(!is.na(eating)) %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = eating, fill = drugs),
           color = "black",
           position = "fill") +            # show proportions instead of counts
  theme_bw()



# DRINK AND SEX
friday.data %>%
  filter(!is.na(drinking)) %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = drinking, fill = sex),
           color = "black",
           position = "fill") +            # show proportions instead of counts
  theme_bw()


 # SLEEP AND SEX - not useful 
friday.data %>%
  filter(!is.na(sleeping)) %>%
  ggplot()+ 
  geom_bar(mapping = aes(x = sleeping, fill = sex),
           color = "black",
           position = "fill") +            # show proportions instead of counts
  theme_bw()


 
 




#### look this over 
profiles %>% mutate(internet = str_detect(essay5, "internet")) %>%
  filter(str_detect(job, "computer'")) %>%
  ggplot() +
  geom_bar(aes(x = internet))
 
 



