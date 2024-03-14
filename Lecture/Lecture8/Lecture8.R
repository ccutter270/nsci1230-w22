# LECTURE 8: POPULATION, INCOME, LIFE Data
# Monday, January 24, 2022

# ----------------------------------- CODE -----------------------------------


# PACKAGES 

install.packages("tidyr")
library("tidyverse")
library("dplyr")
library("tidytext")
library("tidyr")
library("rvest")

setwd("~/Desktop/NSCI Data Science/Lecture/Lecture8")


# ------------------------- OLD DATA  ------------------------------

life <- read_csv("life_expectancy_years.csv")                       
income <- read_csv("income.csv") 


# LIFE EXPECTANCY DATA 
life.long <- life %>%
  pivot_longer(-country, 
               names_to = "Year",
               values_to = "LifeExpectancy") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)


# INCOME DATA 

income.long <- income %>%
  mutate(across(.cols = -country,
                 .fns= ~as.numeric(str_replace(.x, "k", "e3")))) %>%
  pivot_longer(-country, 
               names_to = "Year",
               values_to = "Income") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)


# JOIN INCOME AND LIFE EXPECTANCY 

joined.data <- income.long %>%
  inner_join(life.long, by = c("country", "Year"))



# REGION DATA 

url <- "https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification"

region.data <- url %>% 
  read_html() %>%
  html_element("table") %>% 
  html_table()


# JOIN REGION, INCOME & LIFE EXPECTANCY 

# BAD 
joined.region.data <- joined.data %>%
  anti_join(region.data, by = c("country" = "Country")) %>%
  filter(Year == 2000)

# GOOD 
joined.region.data <- joined.data %>%
  left_join(region.data, by = c("country" = "Country"))








# ---------------------------- NEW NOTES ---------------------------------

pop <- read_csv("population_total.csv")


pop.long <- pop %>%
  mutate(across(.cols = -country,
                .fns = ~str_replace(.x, "k", "e3"))) %>%
  mutate(across(.cols = -country,
                .fns = ~str_replace(.x, "M", "e6"))) %>%
  mutate(across(.cols = -country,
                .fns = ~as.numeric(str_replace(.x, "B", "e9")))) %>%
  pivot_longer(-country, 
               names_to = "Year",
               values_to = "pop") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)

  

final.joined.data <- joined.region.data %>%
  inner_join(pop.long)



# ANIMATION 


#install.packages("gganimate")
#install.packages("gifski")
#install.packages("av")
#install.packages("pg")

library("gganimate")
library("gifski")
library("av")
library("pg")


graph1 <- final.joined.data %>% 
  #filter(Year == 2000) %>% 
  ggplot() +
  geom_point(aes(x = Income, y = LifeExpectancy, color = Region, size = pop)) +
  scale_x_log10() + 
  theme_bw() +
  transition_time(Year) +
  labs(title = "Year: {frame_time}")

animation1 <- animate(graph1, nframes = 223)




# Visualize population changes over time in 3 countries 

graph2 <- final.joined.data %>%
  filter(country %in% c("Canada", "China", "Angola")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = Income, color = country),
            size = 2) + 
  theme_bw() +
  transition_reveal(Year)

animation2 <- animate(graph2)



# Look back at diamonds

diamonds %>%
  ggplot() +
  geom_point(aes(x = carat, y = price, color = color)) +
  transition_states(color, state_length = 1, transition_length = 1) +
  enter_fade() +
  exit_fade()


# ---------------------------- 3D ANIMATION ---------------------------------


# 3D ANIMATION 

#install.packages("rayshader")

library("rayshader")


diamonds.graph1 <- diamonds %>% 
  ggplot() + 
  geom_hex(aes(x = carat, y = price)) +
  scale_fill_viridis_c() +
  theme_bw() 

plot_gg(diamonds.graph1)



# ---------------------------- MORE DATA ---------------------------------


# install.packages("fivethirtyeight")

library("fivethirtyeight")

bechdel 


# Can we visualize the change in representation of women in movies over time? 
# NOTE: Counts change over years so must do percentages 

# Visualize clean test in a given year?
bechdel.graph1 <- bechdel %>%
  filter(year == 2000) %>%
  ggplot() + 
  geom_bar(aes( x = binary))


# For all years?
bechdel %>% 
  mutate(decade = str_sub(year, start = 1, end = 3))  %>% 
  ggplot() + 
  geom_bar(aes(x = decade, fill = binary),
           position =  "fill")

  





