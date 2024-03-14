# LECTURE 7: Income and Life Expecatncy Data
# Thursday, January 20, 2022

# ----------------------------------- CODE -----------------------------------


# PACKAGES 

install.packages("tidyr")
library("tidyverse")
library("dplyr")
library("tidytext")
library("tidyr")

setwd("~/Desktop/NSCI Data Science/Lecture/Lecture7")

life <- read_csv("life_expectancy_years.csv")                              # data file
income <- read_csv("income.csv") 

# ------------------------- NEW TOOLS ------------------------------



# LIFE EXPECTANCY GRAPH 

# Reshape the graph to switch the columns to rows 
life.long <- life %>%
  pivot_longer(-country, 
               names_to = "Year",
               values_to = "LifeExpectancy") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)


life.long %>%
  filter(country %in% c("China", "Brazil")) %>%
  ggplot() + 
  geom_line(aes(x = Year, y = LifeExpectancy, color = country ),
            size = 2) 
  # facet_wrap(~country)          # could do this but color is better  
  


# INCOME GRAPH 

# Switch data
income.long <- income %>%
  mutate(across(.cols = -country,
                 .fns= ~as.numeric(str_replace(.x, "k", "e3")))) %>%
  pivot_longer(-country, 
               names_to = "Year",
               values_to = "Income") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)


income.long %>%
  filter(country %in% c("China", "Brazil")) %>%
  ggplot() + 
  geom_line(aes(x = Year, y = income, color = country ),
            size = 2) 


# ------------------------- COMBINING DATA SETS ------------------------------


# FULL JOIN - Join all unique names, then go through and add the details
#             for each person and use NA for everyone who doesn't have info 
#             there will be all the info, none is omitted


# INNNER JOIN - only join the rows where you have complete information 

# LEFT JOIN - Take all of the left, then check the right hand data set and
#             add any information that you can from that to the left 


# ANTI JOIN - starts the same as the left data set, start with a data set
#             then the anti join deletes information from the right hand data



# JOIN INCOME AND LIFE EXPECTANCY 

joined.data <- income.long %>%
  inner_join(life.long, by = c("country", "Year"))



# Only start using scientific notation above this number
options(scripen = 100000)


joined.data %>%
  filter(Year == 1850) %>%
  ggplot() +
  geom_point(aes(x = Income, y = LifeExpectancy)) +
  xlim(0, 100000) +
scale_x_log10()





# SCRAPE IN DATA 

url <- "https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification"

region.data <- url %>% 
  read_html() %>%
  html_element("table")%>% 
  html_table()



# How can we join joined data with region data? 

joined.region.data <- joined.data %>%
  left_join(region.data, by = c("country" = "Country"))


joined.region.data %>%
  filter(Year == 2000) %>%
  ggplot() +
  geom_point(aes(x = Income, y = LifeExpectancy, color = Region)) +
  #xlim(0, 100000) +
  scale_x_log10()
options(scripen = 100000)



# How can I figure out which countries are being "delete"
# with my inner_join (which countries don't have mathcing region)

joined.region.data <- joined.data %>%
  anti_join(region.data, by = c("country" = "Country")) %>%
  filter(Year == 2000)




