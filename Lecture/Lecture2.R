# LECTURE 2: Graphing and New Functions
# Tuesday, January 11, 2022


# ----------------------------------- NOTES -----------------------------------

#   - Look at cheat sheet to figure out which graph to use with variables
#   - One = makes things equal, == is a Boolean and checks if two things are equal
#   - Only difference between double and single quotes is when using quotes in quotes

# ------------------------------------ CODE ------------------------------------


#IMPORTS 
library("ggplot2")     # library for ggplot 
diamonds               # data set 


# -------------------- 2 VARIABLE GRAPHS -------------------------

# GRAPH FOR COLOR AND PRICE OF DIAMOND
#       VARIABLES: color (discrete) and price (continuous)
#       OBSERVATIONS:
#         - Black lines are the outliers, color goes from best (D) to worst (J)
#         - Graph shows that color of diamond is positively skewed 
#         - I and J have higher non-outlier values 
#         - This seems like the opposite that J should be the most expensive
#           because they are the ugliest - but they have other better traits

ggplot(data = diamonds) +                             # 1. Start with Canvas
  geom_boxplot(mapping = aes(x = color, y = price))  # 2. Add graph type, x, y

  
# GRAPH FOR CLARITY AND PRICE OF DIAMOND
#       VARIABLES: clarity (discrete) and price (continuous)
#       OBSERVATIONS:
#         - Worst clarity (I1) to best (IF)
#         - Basically same graph as above but in reverse
#         - Suggest that people paying more for worse clarity than better 
#           which probably means there is something else causing this trend

ggplot(data = diamonds) +                             # 1. Start with Canvas
  geom_boxplot(mapping = aes(x = clarity, y = price))   # 2. Add graph type, x, y


# GRAPH FOR CARAT AND COLOR OF DIAMOND
#       VARIABLES: carat (quantitative) and color (categorical)
#       OBSERVATIONS:
#         - This graphs shows that least desirable color tend to be the 
#           heaviest (carat) diamonds, so that is why they are more expensive
#         - Doesn't tell us causality, but tells 

ggplot(data = diamonds) +                             # 1. Start with Canvas
  geom_boxplot(mapping = aes(x = color, y = carat))   # 2. Add graph type, x, y




# -------------------- 3+ VARIABLE GRAPHS -------------------------



# GRAPH FOR CARAT, PRICE, AND COLOR - 3 variable 
#       VARIABLES: carat (quantitative), color (categorical), price (continuous)
#       OBSERVATIONS:
#         - Now we can see color bands in the graph that reveal trends
#         - Can do the same with clarity 

ggplot(data = diamonds) +                         
  geom_point(mapping = aes(x = carat, y = price, color = color))



# GRAPH FOR CARAT, PRICE, CLARITY AND COLOR - bad
#       VARIABLES: carat (quantitative), color (categorical), price (continuous), clarity (categorical)
#       OBSERVATIONS:
#         - Can change color for color and shape for clairty
#         - This graph is horrible, too confusing and hard to tell

ggplot(data = diamonds) +                         
  geom_point(mapping = aes(x = carat, y = price, color = color, shape = clarity))




# FACETING 
# GRAPH FOR CARAT, PRICE, CLARITY AND COLOR - good!
#       VARIABLES: carat (quantitative), color (categorical), price (continuous), clarity (categorical)
#       OBSERVATIONS:
#         - We can use facet to create multiple graphs to make it more clear
#         - The first one splits by color, the seconds splits by color and clarity
#         - We can see different trends in this graph easier than all in one graph


ggplot(data = diamonds) +                         
  geom_point(mapping = aes(x = carat, y = price, color = color)) +
  facet_grid(~color)

ggplot(data = diamonds) +                         
  geom_point(mapping = aes(x = carat, y = price, color = color)) +
  facet_grid(clarity~color)






# -------------------- 1 VARIABLE GRAPHS -------------------------



# GRAPH FOR PRICE - DENSTIY GRAPH  
#       VARIABLES: price (continuous)
#       OBSERVATIONS:
#         - Basic and ugly looking graph
#         - But shows that there are more diamonds at lower price than higher

ggplot(data = diamonds) +                         
  geom_density(mapping = aes(x = price))



# GRAPH FOR PRICE - HISTORGRAM 
#       VARIABLES: price (continuous)
#       OBSERVATIONS:
#         - Shows same info as above but in bins of different prices 
#         - You can change the amount of bins you group them into
#           or you can just leave it as a default (take away "bins = 100")
#         - Illuminates buying tendencies - there is a sweet spot 

ggplot(data = diamonds) +                         
  geom_histogram(mapping = aes(x = price), bins = 100)


# GRAPH FOR PRICE - HISTORGRAM with X-LIMITS, only focusing on cheaper diamonds 
#       VARIABLES: price (continuous)
#       OBSERVATIONS:
#         - Can add X-limits (aka only looking at certain prices)
#         - Shows us a sweet spot of best diamond prices 
#         - Can change 5000 to 2000 to narrow sweet spot even more
#         - Can see that there are no diamonds at 1500 range - this can be 
#           a marketing technique 
#         - So how do you know when to stop looking? Technically never

ggplot(data = diamonds) +                         
  geom_histogram(mapping = aes(x = price), bins = 100) +
  xlim(0, 5000)





# -------------------- DATA WRANGLING -------------------------


# INSTALLATIONS 
#install.packages('tidyverse')

library("tidyverse")
library("dplyr")

# INTRODUCING FUNCTIONS
#   - There may be multiple functions with the same names in 
#      different packages, so make sure you are using the same one 

# filter(): 

    # Filters data set based on the conditions it gives
        # only 1 required variable - .data
      filter(.data = diamonds)

    # Filtering only really expensive diamonds - more than $10,000
      filter(.data = diamonds, price > 10000)

    # Filtering most expensive of best color 
    #   make sure that you label D, because its not a column so use quotes
     filter(.data = diamonds, price > 10000, color == "D")
     
   # COOL USES
     
     # Suppose we want heavy diamonds ( carat > 1.5 ) with the 3 worst clarities
       # this would return an empty data set - needs ALL of these requirents 
       filter(.data = diamonds, carat > 1.5, 
              clarity == "I1",
              clarity == "SI1",
              clarity == "SI2")
       
       # here is how to fix that - use & and |
       filter(.data = diamonds, 
              carat > 1.5 &
              clarity == "I1" |
              clarity == "SI1" |
              clarity == "SI2") 
       
       # here is a more efficient way to do it - %in% takes a vector 
       filter(.data = diamonds, carat > 1.5, clarity %in% c("I1","SI1", "SI2"))
    
      # even more readable - store the vector! 
       desirable.clarities <- c("I1","SI1", "SI2")
       filter(.data = diamonds, carat > 1.5, clarity %in% desirable.clarities)
       
       # this does not change the original data set - would need to store this 
       #  in a new data set to store it - now it is stored in good.diamonds
       desirable.clarities <- c("I1","SI1", "SI2")
       good.diamonds <- filter(.data = diamonds, carat > 1.5, clarity %in% desirable.clarities)
       

# summarize():
       
       # Summarizes all the observations based on inputs 
            # only 1 required variable - .data
       summarize(.data = diamonds)
     
      # Calculate mean price of diamonds 
       summarize(.data = diamonds, mean(price))
       
      # Can calculate as many statistics as you want
       summarize(.data = diamonds, mean(price), median(carat))
       
      # Good to give names to the columns 
       summarize(.data = diamonds, 
                 meanPrice = mean(price), 
                 medainCarat = median(carat))
       
      # If you ever want to reference these, you have to store them
       
       meanPrice <- summarize(.data = diamonds, mean(price))
       
       meanPrice
      
       
